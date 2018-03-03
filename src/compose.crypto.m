% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module compose.crypto.
:- interface.

:- import_module pair.

:- type crypto_info
    --->    crypto_info(
                ci_context      :: crypto,
                ci_encrypt      :: bool,
                ci_encrypt_keys :: map(addr_spec, key_userid),
                ci_sign         :: bool,
                ci_sign_keys    :: map(addr_spec, key_userid)
            ).

:- type key_userid
    --->    key_userid(gpgme.key, gpgme.user_id).

:- func init_crypto_info(crypto, bool, bool) = crypto_info.

:- pred unref_keys(crypto_info::in, io::di, io::uo) is det.

:- pred maintain_encrypt_keys(parsed_headers::in,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

:- pred maintain_sign_keys(parsed_headers::in,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

:- type encrypt_for_whom
    --->    from_only
    ;       from_and_recipients.

:- pred get_encrypt_keys(crypto_info::in, parsed_headers::in,
    encrypt_for_whom::in, list(gpgme.key)::out, list(addr_spec)::out,
    list(addr_spec)::out) is det.

:- pred get_sign_keys(crypto_info::in, parsed_headers::in,
    list(gpgme.key)::out) is det.

:- pred encrypt(crypto_info::in, maybe(list(gpgme.key))::in,
    list(gpgme.key)::in, prog_config::in, mime_part::in, i_paused_curses::in,
    maybe_error(string)::out, list(string)::out, io::di, io::uo) is det.

:- pred sign_detached(crypto_info::in, list(gpgme.key)::in, prog_config::in,
    mime_part::in, i_paused_curses::in, maybe_error(pair(string, micalg))::out,
    list(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

:- import_module gpgme.data.
:- import_module gpgme.encrypt.
:- import_module gpgme.sign.
:- import_module gpgme.signer.

:- instance write_message.writer(gpgme.data) where [].
:- instance write_message.writer(gpgme.data.data_crlf) where [].

%-----------------------------------------------------------------------------%

init_crypto_info(Crypto, EncryptInit, SignInit) =
    crypto_info(Crypto, EncryptInit, map.init, SignInit, map.init).

%-----------------------------------------------------------------------------%

unref_keys(crypto_info(_, _, EncryptKeys, _, SignKeys), !IO) :-
    unref_keys(map(fst, values(EncryptKeys)), !IO),
    unref_keys(map(fst, values(SignKeys)), !IO).

:- func fst(key_userid) = gpgme.key.

fst(key_userid(Key, _)) = Key.

%-----------------------------------------------------------------------------%

maintain_encrypt_keys(ParsedHeaders, !CryptoInfo, !IO) :-
    Encrypt = !.CryptoInfo ^ ci_encrypt,
    (
        Encrypt = yes,
        Crypto = !.CryptoInfo ^ ci_context,
        EncryptKeys0 = !.CryptoInfo ^ ci_encrypt_keys,
        maintain_encrypt_keys_map(Crypto, ParsedHeaders,
            EncryptKeys0, EncryptKeys, !IO),
        !CryptoInfo ^ ci_encrypt_keys := EncryptKeys
    ;
        Encrypt = no
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
        find_suitable_key(Crypto, Email, pick_encrypt_key(Email), ResKey, !IO),
        (
            ResKey = ok(KeyUserId),
            map.det_insert(AddrSpec, KeyUserId, !EncryptKeys)
        ;
            ResKey = error(_) % display error?
        )
    ).

%-----------------------------------------------------------------------------%

maintain_sign_keys(ParsedHeaders, !CryptoInfo, !IO) :-
    Sign = !.CryptoInfo ^ ci_sign,
    (
        Sign = yes,
        Crypto = !.CryptoInfo ^ ci_context,
        SignKeys0 = !.CryptoInfo ^ ci_sign_keys,
        maintain_sign_keys_map(Crypto, ParsedHeaders, SignKeys0, SignKeys,
            !IO),
        !CryptoInfo ^ ci_sign_keys := SignKeys
    ;
        Sign = no
    ).

:- pred maintain_sign_keys_map(crypto::in, parsed_headers::in,
    map(addr_spec, key_userid)::in, map(addr_spec, key_userid)::out,
    io::di, io::uo) is det.

maintain_sign_keys_map(Crypto, ParsedHeaders, !SignKeys, !IO) :-
    ParsedHeaders = parsed_headers(From, _To, _Cc, _Bcc, _ReplyTo),
    solutions(addr_specs(From), AddrSpecs),
    list.foldl2(maintain_sign_key(Crypto), AddrSpecs, !SignKeys, !IO).

:- pred maintain_sign_key(crypto::in, addr_spec::in,
    map(addr_spec, key_userid)::in, map(addr_spec, key_userid)::out,
    io::di, io::uo) is det.

maintain_sign_key(Crypto, AddrSpec, !SignKeys, !IO) :-
    ( map.contains(!.SignKeys, AddrSpec) ->
        true
    ;
        addr_spec_to_string(AddrSpec, Email, _Valid),
        find_suitable_key(Crypto, Email, pick_sign_key(Email), ResKey, !IO),
        (
            ResKey = ok(KeyUserId),
            map.det_insert(AddrSpec, KeyUserId, !SignKeys)
        ;
            ResKey = error(_) % display error?
        )
    ).

%-----------------------------------------------------------------------------%

:- pred find_suitable_key(crypto, string, pred(list(gpgme.key), key_userid),
    maybe_error(key_userid), io, io).
:- mode find_suitable_key(in, in, pred(in, out) is semidet, out, di, uo)
    is det.

find_suitable_key(Context, Email, Pick, Res, !IO) :-
    gpgme_op_keylist(Context, yes(Email), not_secret_only, ResKeys, !IO),
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

:- pred pick_sign_key(string::in, list(gpgme.key)::in, key_userid::out)
    is semidet.

pick_sign_key(Email, [Key | Keys], KeyUserId) :-
    KeyInfo = get_key_info(Key),
    (
        KeyInfo ^ key_revoked = no,
        KeyInfo ^ key_expired = no,
        KeyInfo ^ key_disabled = no,
        KeyInfo ^ key_invalid = no,
        KeyInfo ^ key_can_sign = yes,
        list.find_first_match(suitable_user_id(Email),
            KeyInfo ^ key_userids, UserId)
    ->
        KeyUserId = key_userid(Key, UserId)
    ;
        pick_sign_key(Email, Keys, KeyUserId)
    ).

:- pred suitable_user_id(string::in, user_id::in) is semidet.

suitable_user_id(Email, UserId) :-
    UserId ^ uid_revoked = no,
    UserId ^ uid_invalid = no,
    UserId ^ email = yes(Email).

%-----------------------------------------------------------------------------%

get_encrypt_keys(CryptoInfo, ParsedHeaders, EncryptForWhom, SelectedKeys,
        Missing, LeakedBccs) :-
    EncryptKeys = CryptoInfo ^ ci_encrypt_keys,
    ParsedHeaders = parsed_headers(From, To, Cc, Bcc, _ReplyTo),
    (
        EncryptForWhom = from_only,
        Addresses = From
    ;
        EncryptForWhom = from_and_recipients,
        Addresses = From ++ To ++ Cc ++ Bcc
    ),
    solutions(addr_specs(Addresses), AddrSpecs),
    list.foldl2(get_key(EncryptKeys), AddrSpecs,
        [], RevSelectedKeys, [], RevMissing),
    % deduplicate SelectedKeys?
    list.reverse(RevSelectedKeys, SelectedKeys),
    list.reverse(RevMissing, Missing),
    (
        EncryptForWhom = from_only,
        LeakedBccs = []
    ;
        EncryptForWhom = from_and_recipients,
        solutions(addr_specs(Bcc), BccAddrSpecs),
        filter(key_selected(EncryptKeys, SelectedKeys),
            BccAddrSpecs, LeakedBccs)
    ).

get_sign_keys(CryptoInfo, ParsedHeaders, SelectedKeys) :-
    SignKeys = CryptoInfo ^ ci_sign_keys,
    ParsedHeaders = parsed_headers(From, _To, _Cc, _Bcc, _ReplyTo),
    solutions(addr_specs(From), AddrSpecs),
    list.foldl2(get_key(SignKeys), AddrSpecs,
        [], RevSelectedKeys, [], _RevMissing),
    % deduplicate SelectedKeys?
    list.reverse(RevSelectedKeys, SelectedKeys).

:- pred get_key(map(addr_spec, key_userid)::in, addr_spec::in,
    list(gpgme.key)::in, list(gpgme.key)::out,
    list(addr_spec)::in, list(addr_spec)::out) is det.

get_key(KeyMap, AddrSpec, !Keys, !Missing) :-
    ( map.search(KeyMap, AddrSpec, key_userid(Key, _UserId)) ->
        cons(Key, !Keys)
    ;
        cons(AddrSpec, !Missing)
    ).

:- pred key_selected(map(addr_spec, key_userid)::in, list(gpgme.key)::in,
    addr_spec::in) is semidet.

key_selected(KeyMap, SelectedKeys, AddrSpec) :-
    map.search(KeyMap, AddrSpec, key_userid(Key, _UserId)),
    list.contains(SelectedKeys, Key).

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

encrypt(CryptoInfo, MaybeSignKeys, EncryptKeys, Config, PartToEncrypt,
        PausedCurs, Res, Warnings, !IO) :-
    Ctx = CryptoInfo ^ ci_context,
    (
        MaybeSignKeys = yes(SignKeys),
        gpgme_signers_clear(Ctx, !IO),
        add_signers(Ctx, SignKeys, ResSigners, !IO),
        Op = encrypt_sign
    ;
        MaybeSignKeys = no,
        ResSigners = ok,
        Op = encrypt_only
    ),
    (
        ResSigners = ok,
        encrypt_1(Ctx, Op, EncryptKeys, Config, PartToEncrypt, PausedCurs,
            Res, Warnings, !IO)
    ;
        ResSigners = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred encrypt_1(crypto::in, encrypt_op::in, list(gpgme.key)::in,
    prog_config::in, mime_part::in, i_paused_curses::in,
    maybe_error(string)::out, list(string)::out, io::di, io::uo) is det.

encrypt_1(Ctx, Op, Keys, Config, PartToEncrypt, PausedCurs,
        Res, Warnings, !IO) :-
    gpgme_data_new(ResPlainData, !IO),
    (
        ResPlainData = ok(PlainData),
        write_mime_part(PlainData, Config, PartToEncrypt, PausedCurs,
            ResPlain, !IO),
        (
            ResPlain = ok,
            gpgme_data_rewind(PlainData, ResRewind, !IO),
            (
                ResRewind = ok,
                encrypt_2(Ctx, Op, Keys, PlainData, PausedCurs,
                    Res, Warnings, !IO)
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

:- pred encrypt_2(crypto::in, encrypt_op::in, list(gpgme.key)::in,
    gpgme.data::in, i_paused_curses::in,
    maybe_error(string)::out, list(string)::out, io::di, io::uo) is det.

encrypt_2(Ctx, Op, Keys, PlainData, PausedCurs, Res, Warnings, !IO) :-
    gpgme_data_new(ResCipherData, !IO),
    (
        ResCipherData = ok(CipherData),
        encrypt_3(Ctx, Op, Keys, PlainData, CipherData, PausedCurs,
            Res, Warnings, !IO),
        gpgme_data_release(CipherData, !IO)
    ;
        ResCipherData = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred encrypt_3(crypto::in, encrypt_op::in, list(gpgme.key)::in,
    gpgme.data::in, gpgme.data::in, i_paused_curses::in,
    maybe_error(string)::out, list(string)::out, io::di, io::uo) is det.

encrypt_3(Ctx, Op, Keys, PlainData, CipherData, PausedCurs,
        Res, Warnings, !IO) :-
    PausedCurs = i_paused_curses,
    % Not sure about always_trust.
    gpgme_op_encrypt(Op, Ctx, Keys, [always_trust, no_encrypt_to],
        PlainData, CipherData, ResEncrypt, !IO),
    (
        ResEncrypt = ok(encrypt_result(InvalidRecipients)),
        gpgme_data_to_string(CipherData, Res, !IO),
        list.map(invalid_key_to_warning, InvalidRecipients, Warnings)
    ;
        ResEncrypt = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred invalid_key_to_warning(invalid_key::in, string::out) is det.

invalid_key_to_warning(invalid_key(Fingerprint, Reason), Warning) :-
    Warning = "Key " ++ string.right_by_codepoint(Fingerprint, 8) ++
        " is invalid (" ++ Reason ++ ").".

%-----------------------------------------------------------------------------%

sign_detached(CryptoInfo, SignKeys, Config, SignedPart, PausedCurs,
        Res, Warnings, !IO) :-
    Ctx = CryptoInfo ^ ci_context,
    gpgme_signers_clear(Ctx, !IO),
    add_signers(Ctx, SignKeys, ResSigners, !IO),
    (
        ResSigners = ok,
        sign_detached_2(Ctx, Config, SignedPart, PausedCurs, Res, Warnings,
            !IO)
    ;
        ResSigners = error(Error),
        Res = error(Error),
        Warnings = []
    ).

:- pred sign_detached_2(crypto::in, prog_config::in, mime_part::in,
    i_paused_curses::in, maybe_error(pair(string, micalg))::out,
    list(string)::out, io::di, io::uo) is det.

sign_detached_2(Ctx, Config, SignedPart, PausedCurs, Res, Warnings, !IO) :-
    gpgme_data_new(ResPlainData, !IO),
    (
        ResPlainData = ok(PlainData),
        % Must use CR/LF line terminators when generating the signature.
        write_mime_part(data_crlf(PlainData), Config, SignedPart, PausedCurs,
            ResPlain, !IO),
        (
            ResPlain = ok,
            gpgme_data_rewind(PlainData, ResRewind, !IO),
            (
                ResRewind = ok,
                gpgme_data_new(ResSigData, !IO),
                (
                    ResSigData = ok(SigData),
                    PausedCurs = i_paused_curses,
                    gpgme_op_sign_detached(Ctx, PlainData, SigData, ResSign,
                        !IO),
                    (
                        ResSign = ok(SignResult),
                        ( sign_result_to_micalg(SignResult, MicAlg) ->
                            gpgme_data_to_string(SigData, ResSig, !IO),
                            (
                                ResSig = ok(Sig),
                                Res = ok(Sig - MicAlg),
                                sign_result_to_warnings(SignResult, Warnings)
                            ;
                                ResSig = error(Error),
                                Res = error(Error),
                                Warnings = []
                            )
                        ;
                            Res = error("Cannot determine hash algorithm."),
                            Warnings = []
                        )
                    ;
                        ResSign = error(Error),
                        Res = error(Error),
                        Warnings = []
                    ),
                    gpgme_data_release(SigData, !IO)
                ;
                    ResSigData = error(Error),
                    Res = error(Error),
                    Warnings = []
                )
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

:- pred add_signers(ctx::in, list(key)::in, maybe_error::out, io::di, io::uo)
    is det.

add_signers(_Ctx, [], Res, !IO) :-
    Res = ok.
add_signers(Ctx, [Key | Keys], Res, !IO) :-
    gpgme_signers_add(Ctx, Key, Res0, !IO),
    (
        Res0 = ok,
        add_signers(Ctx, Keys, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred sign_result_to_micalg(sign_result::in, micalg::out) is semidet.

sign_result_to_micalg(SignResult, MicAlg) :-
    SignResult = sign_result(_InvalidSigners, NewSigs),
    solutions(
        (pred(X::out) is nondet :-
            list.member(NewSig, NewSigs),
            X = gpgme_hash_algo_name(NewSig ^ hash_algo)
        ), Xs),
    Xs = [yes(HashAlgo)],
    MicAlg = micalg("pgp-" ++ to_lower(HashAlgo)).

:- pred sign_result_to_warnings(sign_result::in, list(string)::out) is det.

sign_result_to_warnings(sign_result(InvalidSigners, _NewSigs), Warnings) :-
    list.map(invalid_key_to_warning, InvalidSigners, Warnings).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
