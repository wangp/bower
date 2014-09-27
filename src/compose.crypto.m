% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module compose.crypto.
:- interface.

:- type crypto_info
    --->    crypto_info(
                ci_context      :: crypto,
                ci_encrypt      :: bool,
                ci_encrypt_keys :: map(addr_spec, key_userid)
            ).

:- type key_userid
    --->    key_userid(gpgme.key, gpgme.user_id).

:- func init_crypto_info(crypto, bool) = crypto_info.

:- pred unref_keys(crypto_info::in, io::di, io::uo) is det.

:- pred maintain_encrypt_keys(parsed_headers::in,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

:- pred get_encrypt_keys(crypto_info::in, parsed_headers::in,
    list(gpgme.key)::out, list(addr_spec)::out) is det.

:- pred encrypt(crypto_info::in, list(gpgme.key)::in, prog_config::in,
    message_type::in, maybe_error(string)::out, list(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

:- import_module gpgme.data.
:- import_module gpgme.encrypt.

%-----------------------------------------------------------------------------%

init_crypto_info(Crypto, EncryptInit) =
    crypto_info(Crypto, EncryptInit, map.init).

%-----------------------------------------------------------------------------%

unref_keys(crypto_info(_, _, EncryptKeys), !IO) :-
    unref_keys(map(fst, values(EncryptKeys)), !IO).

:- func fst(key_userid) = gpgme.key.

fst(key_userid(Key, _)) = Key.

%-----------------------------------------------------------------------------%

maintain_encrypt_keys(ParsedHeaders, CryptoInfo0, CryptoInfo, !IO) :-
    CryptoInfo0 = crypto_info(Crypto, Encrypt, EncryptKeys0),
    (
        Encrypt = yes,
        maintain_encrypt_keys_map(Crypto, ParsedHeaders,
            EncryptKeys0, EncryptKeys, !IO),
        CryptoInfo = crypto_info(Crypto, Encrypt, EncryptKeys)
    ;
        Encrypt = no,
        CryptoInfo = CryptoInfo0
    ).

:- pred maintain_encrypt_keys_map(crypto::in, parsed_headers::in,
    map(addr_spec, key_userid)::in, map(addr_spec, key_userid)::out,
    io::di, io::uo) is det.

maintain_encrypt_keys_map(Crypto, ParsedHeaders, !EncryptKeys, !IO) :-
    ParsedHeaders = parsed_headers(From, To, Cc, Bcc, _ReplyTo),
    Addresses = From ++ To ++ Cc ++ Bcc,
    solutions(addr_specs(Addresses), AddrSpecs),
    list.foldl2(maintain_encrypt_key(Crypto), AddrSpecs, !EncryptKeys, !IO).

:- pred maintain_encrypt_key(crypto::in, addr_spec::in,
    map(addr_spec, key_userid)::in, map(addr_spec, key_userid)::out,
    io::di, io::uo) is det.

maintain_encrypt_key(Crypto, AddrSpec, !EncryptKeys, !IO) :-
    ( map.contains(!.EncryptKeys, AddrSpec) ->
        true
    ;
        addr_spec_to_string(AddrSpec, Email, _Valid),
        find_suitable_key(Crypto, Email, not_secret_only,
            pick_encrypt_key(Email), ResKey, !IO),
        (
            ResKey = ok(KeyUserId),
            map.det_insert(AddrSpec, KeyUserId, !EncryptKeys)
        ;
            ResKey = error(_) % display error?
        )
    ).

:- pred find_suitable_key(crypto, string, secret_only,
    pred(list(gpgme.key), key_userid), maybe_error(key_userid), io, io).
:- mode find_suitable_key(in, in, in,
    pred(in, out) is semidet, out, di, uo) is det.

find_suitable_key(Context, Email, SecretOnly, Pick, Res, !IO) :-
    gpgme_op_keylist(Context, yes(Email), SecretOnly, ResKeys, !IO),
    (
        ResKeys = ok(Keys),
        ( Pick(Keys, KeyUserId) ->
            Res = ok(KeyUserId),
            KeyUserId = key_userid(Key, _UserId),
            list.delete_all(Keys, Key, UnusedKeys)
        ;
            Res = error("no key found"),
            UnusedKeys = Keys
        ),
        unref_keys(UnusedKeys, !IO)
    ;
        ResKeys = error(Error),
        Res = error(Error)
    ).

:- pred pick_encrypt_key(string::in, list(gpgme.key)::in, key_userid::out)
    is semidet.

pick_encrypt_key(Email, [Key | Keys], KeyUserId) :-
    KeyInfo = get_key_info(Key),
    (
        KeyInfo ^ key_revoked = no,
        KeyInfo ^ key_expired = no,
        KeyInfo ^ key_disabled = no,
        KeyInfo ^ key_invalid = no,
        KeyInfo ^ key_can_encrypt = yes,
        list.find_first_match(suitable_user_id(Email),
            KeyInfo ^ key_userids, UserId)
    ->
        KeyUserId = key_userid(Key, UserId)
    ;
        pick_encrypt_key(Email, Keys, KeyUserId)
    ).

:- pred suitable_user_id(string::in, user_id::in) is semidet.

suitable_user_id(Email, UserId) :-
    UserId ^ uid_revoked = no,
    UserId ^ uid_invalid = no,
    UserId ^ email = yes(Email).

%-----------------------------------------------------------------------------%

get_encrypt_keys(CryptoInfo, ParsedHeaders, SelectedKeys, Missing) :-
    EncryptKeys = CryptoInfo ^ ci_encrypt_keys,
    ParsedHeaders = parsed_headers(From, To, Cc, Bcc, _ReplyTo),
    Addresses = From ++ To ++ Cc ++ Bcc,
    solutions(addr_specs(Addresses), AddrSpecs),
    list.foldr2(get_encrypt_key(EncryptKeys), AddrSpecs,
        [], SelectedKeys, [], Missing).
        % deduplicate SelectedKeys?

:- pred get_encrypt_key(map(addr_spec, key_userid)::in, addr_spec::in,
    list(gpgme.key)::in, list(gpgme.key)::out,
    list(addr_spec)::in, list(addr_spec)::out) is det.

get_encrypt_key(EncryptKeys, AddrSpec, !Keys, !Missing) :-
    ( map.search(EncryptKeys, AddrSpec, key_userid(Key, _UserId)) ->
        cons(Key, !Keys)
    ;
        cons(AddrSpec, !Missing)
    ).

%-----------------------------------------------------------------------------%

:- pred addr_specs(list(address)::in, addr_spec::out) is nondet.

addr_specs(Addresses, AddrSpec) :-
    list.member(Address, Addresses),
    addr_specs_2(Address, AddrSpec).

:- pred addr_specs_2(address::in, addr_spec::out) is nondet.

addr_specs_2(Address, AddrSpec) :-
    require_complete_switch [Address]
    (
        Address = mailbox(Mailbox)
    ;
        Address = group(_DisplayName, Mailboxes),
        list.member(Mailbox, Mailboxes)
    ),
    addr_specs_3(Mailbox, AddrSpec).

:- pred addr_specs_3(mailbox::in, addr_spec::out) is semidet.

addr_specs_3(Mailbox, AddrSpec) :-
    require_complete_switch [Mailbox]
    (
        Mailbox = mailbox(_DisplayName, AddrSpec)
    ;
        Mailbox = bad_mailbox(_),
        fail
    ).

%-----------------------------------------------------------------------------%

encrypt(CryptoInfo, Keys, Config, MessageType, Res, Warnings, !IO) :-
    Ctx = CryptoInfo ^ ci_context,
    gpgme_data_new(ResPlainData, !IO),
    (
        ResPlainData = ok(PlainData),
        write_message_type(PlainData, Config, MessageType, ResPlain, !IO),
        (
            ResPlain = ok,
            gpgme_data_rewind(PlainData, ResRewind, !IO),
            (
                ResRewind = ok,
                encrypt_2(Ctx, Keys, PlainData, Res, Warnings, !IO)
            ;
                ResRewind = error(Error),
                Res = error(Error),
                Warnings = []
            )
        ;
            ResPlain = error(Error),
            Res = error(Error),
            Warnings = []
        ),
        gpgme_data_release(PlainData, !IO)
    ;
        ResPlainData = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred encrypt_2(crypto::in, list(gpgme.key)::in, gpgme.data::in,
    maybe_error(string)::out, list(string)::out, io::di, io::uo) is det.

encrypt_2(Ctx, Keys, PlainData, Res, Warnings, !IO) :-
    gpgme_data_new(ResCipherData, !IO),
    (
        ResCipherData = ok(CipherData),
        encrypt_3(Ctx, Keys, PlainData, CipherData, Res, Warnings, !IO),
        gpgme_data_release(CipherData, !IO)
    ;
        ResCipherData = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred encrypt_3(crypto::in, list(gpgme.key)::in, gpgme.data::in,
    gpgme.data::in, maybe_error(string)::out, list(string)::out,
    io::di, io::uo) is det.

encrypt_3(Ctx, Keys, PlainData, CipherData, Res, Warnings, !IO) :-
    % Not sure about always_trust.
    gpgme_op_encrypt(encrypt_only, Ctx, Keys, [always_trust, no_encrypt_to],
        PlainData, CipherData, ResEncrypt, !IO),
    (
        ResEncrypt = ok(encrypt_result(InvalidRecipients)),
        gpgme_data_to_string(CipherData, Res, !IO),
        list.map(invalid_recipient_to_warning, InvalidRecipients, Warnings)
    ;
        ResEncrypt = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred invalid_recipient_to_warning(invalid_key::in, string::out) is det.

invalid_recipient_to_warning(invalid_key(Fingerprint, Reason), Warning) :-
    Warning = "Key " ++ string.right_by_codepoint(Fingerprint, 8) ++
        " is invalid (" ++ Reason ++ ").".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
