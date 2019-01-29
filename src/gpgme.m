% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred gpgme_init(io::di, io::uo) is det.

% Protocols and Engines

:- type protocol
    --->    openpgp.

:- pred gpgme_engine_check_version(protocol::in, maybe_error::out,
    io::di, io::uo) is det.

% Algorithms

:- type hash_algo.

:- func gpgme_hash_algo_name(hash_algo) = maybe(string).

% Contexts

:- type ctx.

:- pred gpgme_new(maybe_error(ctx)::out, io::di, io::uo) is det.

:- pred gpgme_release(ctx::in, io::di, io::uo) is det.

:- pred gpgme_set_protocol(ctx::in, protocol::in, maybe_error::out,
    io::di, io::uo) is det.

:- type armor
    --->    no_armor
    ;       ascii_armor.

:- pred gpgme_set_armor(ctx::in, armor::in, io::di, io::uo) is det.

% Key Management

:- type key.

:- type key_info
    --->    key_info(
                key_revoked         :: bool,
                key_expired         :: bool,
                key_disabled        :: bool,
                key_invalid         :: bool,
                key_can_encrypt     :: bool,
                key_can_sign        :: bool,
                key_can_certify     :: bool,
                key_can_authenticate:: bool,
                key_is_qualified    :: bool,
                key_secret          :: bool,
                % protocol
                % issuer_serial
                % issuer_serial
                % chain_id
                key_owner_trust     :: validity,
                key_subkeys         :: list(subkey),
                key_userids         :: list(user_id)
            ).

:- type subkey
    --->    subkey(
                subkey_revoked          :: bool,
                subkey_expired          :: bool,
                subkey_disabled         :: bool,
                subkey_invalid          :: bool,
                subkey_can_encrypt      :: bool,
                subkey_can_sign         :: bool,
                subkey_can_certify      :: bool,
                subkey_can_authenticate :: bool,
                subkey_is_qualified     :: bool,
                subkey_secret           :: bool,
                % pubkey_algo
                subkey_length           :: int, % bits
                subkey_keyid            :: string,
                subkey_fingerprint      :: string,
                subkey_timestamp        :: subkey_timestamp,
                subkey_expires          :: maybe(timestamp)
            ).

:- type subkey_timestamp
    --->    invalid
    ;       unavailable
    ;       creation(timestamp).

:- type user_id
    --->    user_id(
                uid_revoked     :: bool,
                uid_invalid     :: bool,
                uid_validity    :: validity,
                uid             :: string,
                name            :: maybe(string),
                comment         :: maybe(string),
                email           :: maybe(string)
                % signatures
            ).

:- type validity
    --->    validity_unknown
    ;       validity_undefined
    ;       validity_never
    ;       validity_marginal
    ;       validity_full
    ;       validity_ultimate.

% Data buffers

:- type data.

:- include_module gpgme.data.

% Crypto Operations

:- type invalid_key
    --->    invalid_key(
                fingerprint     :: string,
                reason          :: string
            ).

:- include_module gpgme.encrypt.
:- include_module gpgme.key.
:- include_module gpgme.sign.
:- include_module gpgme.signer.

% Misc

    % XXX gpgme_signature_t has unsigned long timestamps, other places use long
    % int timestamps. Either way not guaranteed to fit in Mercury int.
:- type timestamp == int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module store.

:- include_module gpgme.invalid_key.
:- include_module gpgme.key_array.

:- pragma foreign_decl("C", "
    #include <gpgme.h>
").

:- pragma foreign_decl("C", local, "
    #include <locale.h>
").

:- pragma foreign_enum("C", protocol/0, [
    openpgp - "GPGME_PROTOCOL_OpenPGP"
]).

:- pragma foreign_type("C", hash_algo, "gpgme_hash_algo_t").

:- pragma foreign_type("C", ctx, "gpgme_ctx_t").

:- type key
    --->    key(
                key_info,
                io_mutvar(maybe(gpgme_key))
            ).

:- type gpgme_key.

:- pragma foreign_type("C", gpgme_key, "gpgme_key_t").

:- pragma foreign_enum("C", validity/0, [
    validity_unknown - "GPGME_VALIDITY_UNKNOWN",
    validity_undefined - "GPGME_VALIDITY_UNDEFINED",
    validity_never - "GPGME_VALIDITY_NEVER",
    validity_marginal - "GPGME_VALIDITY_MARGINAL",
    validity_full - "GPGME_VALIDITY_FULL",
    validity_ultimate - "GPGME_VALIDITY_ULTIMATE"
]).

:- type data
    --->    data(
                real_data   :: gpgme_data,
                retain      :: string % prevent GC
            ).

:- type gpgme_data.

:- pragma foreign_type("C", gpgme_data, "gpgme_data_t").

:- type gpgme_invalid_key.

:- pragma foreign_type("C", gpgme_invalid_key, "gpgme_invalid_key_t").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
MR_String
_gpgme_error_to_string(gpgme_error_t err, MR_AllocSiteInfoPtr alloc_id);
").

:- pragma foreign_code("C", "
MR_String
_gpgme_error_to_string(gpgme_error_t err, MR_AllocSiteInfoPtr alloc_id)
{
    char buf[128];

    gpgme_strerror_r(err, buf, sizeof(buf));
    return MR_make_string(alloc_id, ""%s"", buf);
}
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gpgme_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_check_version(NULL);
    gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_CTYPE, NULL));
#ifdef LC_MESSAGES
    gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_MESSAGES, NULL));
#endif
").

%-----------------------------------------------------------------------------%

gpgme_engine_check_version(Proto, Res, !IO) :-
    gpgme_engine_check_version_2(Proto, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_engine_check_version_2(protocol::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_engine_check_version_2(Proto::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_engine_check_version(Proto);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

gpgme_hash_algo_name(Algo) = MaybeName :-
    gpgme_hash_algo_name_2(Algo, Ok, Name),
    (
        Ok = yes,
        MaybeName = yes(Name)
    ;
        Ok = no,
        MaybeName = no
    ).

:- pred gpgme_hash_algo_name_2(hash_algo::in, bool::out, string::out) is det.

:- pragma foreign_proc("C",
    gpgme_hash_algo_name_2(Algo::in, Ok::out, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const char *s = gpgme_hash_algo_name(Algo);
    if (s != NULL) {
        Ok = MR_YES;
        MR_make_aligned_string_copy_msg(Name, s, MR_ALLOC_ID);
    } else {
        Ok = MR_NO;
        Name = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

gpgme_new(Res, !IO) :-
    gpgme_new_2(Ok, Ctx, Error, !IO),
    (
        Ok = yes,
        Res = ok(Ctx)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_new_2(bool::out, ctx::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    gpgme_new_2(Ok::out, Ctx::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_new(&Ctx);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gpgme_release(Ctx::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_release(Ctx);
").

%-----------------------------------------------------------------------------%

gpgme_set_protocol(Ctx, Proto, Res, !IO) :-
    gpgme_set_protocol_2(Ctx, Proto, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_set_protocol_2(ctx::in, protocol::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_set_protocol_2(Ctx::in, Proto::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_set_protocol(Ctx, Proto);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

gpgme_set_armor(Ctx, Armor, !IO) :-
    (
        Armor = no_armor,
        Ascii = no
    ;
        Armor = ascii_armor,
        Ascii = yes
    ),
    gpgme_set_armor_2(Ctx, Ascii, !IO).

:- pred gpgme_set_armor_2(ctx::in, bool::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_set_armor_2(Ctx::in, Ascii::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_set_armor(Ctx, Ascii);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
