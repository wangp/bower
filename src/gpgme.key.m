% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.key.
:- interface.

:- type secret_only
    --->    not_secret_only
    ;       secret_only.

    % The caller is responsible for unref'ing each key.
    %
:- pred gpgme_op_keylist(ctx::in, maybe(string)::in, secret_only::in,
    maybe_error(list(key))::out, io::di, io::uo) is det.

:- pred unref_keys(list(key)::in, io::di, io::uo) is det.

:- func get_key_info(key) = key_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type gpgme_subkey.

:- pragma foreign_type("C", gpgme_subkey, "gpgme_subkey_t").

:- type gpgme_user_id.

:- pragma foreign_type("C", gpgme_user_id, "gpgme_user_id_t").

%-----------------------------------------------------------------------------%

gpgme_op_keylist(Ctx, MaybePattern, SecretOnly, Res, !IO) :-
    gpgme_op_keylist_start(Ctx, MaybePattern, SecretOnly, Res0, !IO),
    (
        Res0 = ok,
        keylist_loop(Ctx, Res1, [], RevKeys, !IO),
        (
            Res1 = ok,
            gpgme_op_keylist_end(Ctx, Res2, !IO),
            (
                Res2 = ok,
                Res = ok(reverse(RevKeys))
            ;
                Res2 = error(Error),
                Res = error(Error),
                unref_keys(RevKeys, !IO)
            )
        ;
            Res1 = error(Error),
            Res = error(Error),
            unref_keys(RevKeys, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred keylist_loop(ctx::in, maybe_error::out, list(key)::in, list(key)::out,
    io::di, io::uo) is det.

keylist_loop(Ctx, Res, !RevKeys, !IO) :-
    gpgme_op_keylist_next(Ctx, Res0, !IO),
    (
        Res0 = ok(yes(Key)),
        cons(Key, !RevKeys),
        keylist_loop(Ctx, Res, !RevKeys, !IO)
    ;
        Res0 = ok(no),
        Res = ok
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred gpgme_op_keylist_start(ctx::in, maybe(string)::in, secret_only::in,
    maybe_error::out, io::di, io::uo) is det.

gpgme_op_keylist_start(Ctx, MaybePattern, SecretOnly, Res, !IO) :-
    (
        MaybePattern = yes(Pattern),
        HavePattern = yes
    ;
        MaybePattern = no,
        HavePattern = no,
        Pattern = ""
    ),
    gpgme_op_keylist_start_2(Ctx, HavePattern, Pattern,
        secret_only_bool(SecretOnly), Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_op_keylist_start_2(ctx::in, bool::in, string::in,
    bool::in, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_keylist_start_2(Ctx::in, HavePattern::in, Pattern::in,
        SecretOnly::in, Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_keylist_start(Ctx,
        (HavePattern ? Pattern : NULL),
        (SecretOnly ? 1 : 0));
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

:- func secret_only_bool(secret_only) = bool.

secret_only_bool(not_secret_only) = no.
secret_only_bool(secret_only) = yes.

%-----------------------------------------------------------------------------%

:- pred gpgme_op_keylist_end(ctx::in, maybe_error::out, io::di, io::uo) is det.

gpgme_op_keylist_end(Ctx, Res, !IO) :-
    gpgme_op_keylist_end_2(Ctx, Ok, Error, !IO),
    (
        Ok = yes,
        % Worth checking keylist result?
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_op_keylist_end_2(ctx::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_keylist_end_2(Ctx::in, Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_keylist_end(Ctx);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

:- pred gpgme_op_keylist_next(ctx::in, maybe_error(maybe(key))::out,
    io::di, io::uo) is det.

gpgme_op_keylist_next(Ctx, Res, !IO) :-
    gpgme_op_keylist_next_2(Ctx, RC, GpgmeKey, Error, !IO),
    ( RC = 1 ->
        make_key_info(GpgmeKey, KeyInfo, !IO),
        new_mutvar(yes(GpgmeKey), Mutvar, !IO),
        Key = key(KeyInfo, Mutvar),
        Res = ok(yes(Key))
    ; RC = 0 ->
        Res = ok(no)
    ;
        Res = error(Error)
    ).

:- pred gpgme_op_keylist_next_2(ctx::in, int::out, gpgme_key::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_keylist_next_2(Ctx::in, RC::out, Key::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_keylist_next(Ctx, &Key);
    if (err == GPG_ERR_NO_ERROR) {
        RC = 1;
        Error = MR_make_string_const("""");
    } else if (gpg_err_code(err) == GPG_ERR_EOF) {
        RC = 0;
        Error = MR_make_string_const("""");
    } else {
        RC = -1;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

unref_keys(Keys, !IO) :-
    list.foldl(gpgme_key_unref, Keys, !IO).

%-----------------------------------------------------------------------------%

:- pred gpgme_key_unref(key::in, io::di, io::uo) is det.

gpgme_key_unref(key(_Info, Mutvar), !IO) :-
    get_mutvar(Mutvar, Maybe, !IO),
    (
        Maybe = yes(Key),
        set_mutvar(Mutvar, no, !IO),
        gpgme_key_unref_2(Key, !IO)
    ;
        Maybe = no
    ).

:- pred gpgme_key_unref_2(gpgme_key::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_key_unref_2(Key::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_key_unref(Key);
").

%-----------------------------------------------------------------------------%

get_key_info(key(Info, _)) = Info.

:- pred make_key_info(gpgme_key::in, key_info::out, io::di, io::uo) is det.

make_key_info(Key, Info, !IO) :-
    promise_pure (
        semipure gpgme_key_fields(Key, Revoked, Expired, Disabled, Invalid,
            CanEncrypt, CanSign, CanCertify, CanAuthenticate,
            IsQualified, Secret, OwnerTrust, SubKeys0, UserIds0),
        semipure convert_subkeys(SubKeys0, SubKeys),
        semipure convert_user_ids(UserIds0, UserIds)
    ),
    Info ^ key_revoked = Revoked,
    Info ^ key_expired = Expired,
    Info ^ key_disabled = Disabled,
    Info ^ key_invalid = Invalid,
    Info ^ key_can_encrypt = CanEncrypt,
    Info ^ key_can_sign = CanSign,
    Info ^ key_can_certify = CanCertify,
    Info ^ key_can_authenticate = CanAuthenticate,
    Info ^ key_is_qualified = IsQualified,
    Info ^ key_secret = Secret,
    Info ^ key_owner_trust = OwnerTrust,
    Info ^ key_subkeys = SubKeys,
    Info ^ key_userids = UserIds.

:- semipure pred gpgme_key_fields(gpgme_key::in,
    bool::out, bool::out, bool::out, bool::out,
    bool::out, bool::out, bool::out, bool::out,
    bool::out, bool::out, validity::out,
    gpgme_subkey::out, gpgme_user_id::out) is det.

:- pragma foreign_proc("C",
    gpgme_key_fields(Key::in,
        Revoked::out, Expired::out, Disabled::out, Invalid::out,
        CanEncrypt::out, CanSign::out, CanCertify::out, CanAuthenticate::out,
        IsQualified::out, Secret::out, OwnerTrust::out,
        SubKeys::out, UserIds::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    Revoked = Key->revoked ? MR_YES : MR_NO;
    Expired = Key->expired ? MR_YES : MR_NO;
    Disabled = Key->disabled ? MR_YES : MR_NO;
    Invalid = Key->invalid ? MR_YES : MR_NO;
    CanEncrypt = Key->can_encrypt ? MR_YES : MR_NO;
    CanSign = Key->can_sign ? MR_YES : MR_NO;
    CanCertify = Key->can_certify ? MR_YES : MR_NO;
    CanAuthenticate = Key->can_authenticate ? MR_YES : MR_NO;
    IsQualified = Key->is_qualified ? MR_YES : MR_NO;
    Secret = Key->secret ? MR_YES : MR_NO;
    OwnerTrust = Key->owner_trust;
    SubKeys = Key->subkeys;
    UserIds = Key->uids;
").

:- semipure pred convert_subkeys(gpgme_subkey::in, list(subkey)::out)
    is det.

convert_subkeys(SubKey0, SubKeys) :-
    ( semipure convert_subkey(SubKey0, SubKey, Next0) ->
        semipure convert_subkeys(Next0, Next),
        SubKeys = [SubKey | Next]
    ;
        SubKeys = []
    ).

:- semipure pred convert_subkey(gpgme_subkey::in, subkey::out,
    gpgme_subkey::out) is semidet.

convert_subkey(SubKey0, SubKey, Next) :-
    semipure subkey_fields(SubKey0, Next,
        Revoked, Expired, Disabled, Invalid,
        CanEncrypt, CanSign, CanCertify, CanAuthenticate, IsQualified, Secret,
        Length, KeyId, Fingerprint, Timestamp0, Expires0),
    ( Timestamp0 = -1 ->
        Timestamp = invalid
    ; Timestamp0 = 0 ->
        Timestamp = unavailable
    ;
        Timestamp = creation(Timestamp0)
    ),
    ( Expires0 = 0 ->
        Expires = no
    ;
        Expires = yes(Expires0)
    ),
    SubKey = subkey(Revoked, Expired, Disabled, Invalid,
        CanEncrypt, CanSign, CanCertify, CanAuthenticate,
        IsQualified, Secret, Length, KeyId, Fingerprint, Timestamp, Expires).

:- semipure pred subkey_fields(gpgme_subkey::in, gpgme_subkey::out,
    bool::out, bool::out, bool::out, bool::out,
    bool::out, bool::out, bool::out, bool::out, bool::out, bool::out,
    int::out, string::out, string::out, timestamp::out, timestamp::out)
    is semidet.

:- pragma foreign_proc("C",
    subkey_fields(SubKey::in, Next::out,
        Revoked::out, Expired::out, Disabled::out, Invalid::out,
        CanEncrypt::out, CanSign::out, CanCertify::out, CanAuthenticate::out,
        IsQualified::out, Secret::out,
        Length::out, KeyId::out, Fingerprint::out,
        Timestamp::out, Expires::out),
    [will_not_call_mercury, promise_semipure, thread_safe, tabled_for_io],
"
    if (SubKey != NULL) {
        SUCCESS_INDICATOR = MR_TRUE;
        Next = SubKey->next;
        Revoked = SubKey->revoked ? MR_YES : MR_NO;
        Expired = SubKey->expired ? MR_YES : MR_NO;
        Disabled = SubKey->disabled ? MR_YES : MR_NO;
        Invalid = SubKey->invalid ? MR_YES : MR_NO;
        CanEncrypt = SubKey->can_encrypt ? MR_YES : MR_NO;
        CanSign = SubKey->can_sign ? MR_YES : MR_NO;
        CanCertify = SubKey->can_certify ? MR_YES : MR_NO;
        CanAuthenticate = SubKey->can_authenticate ? MR_YES : MR_NO;
        IsQualified = SubKey->is_qualified ? MR_YES : MR_NO;
        Secret = SubKey->secret ? MR_YES : MR_NO;
        Length = SubKey->length;
        MR_make_aligned_string_copy_msg(KeyId, SubKey->keyid, MR_ALLOC_ID);
        MR_make_aligned_string_copy_msg(Fingerprint, SubKey->fpr, MR_ALLOC_ID);
        Timestamp = SubKey->timestamp;
        Expires = SubKey->expires;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
        Next = NULL;
        Revoked = MR_YES;
        Expired = MR_YES;
        Disabled = MR_YES;
        Invalid = MR_YES;
        CanEncrypt = MR_NO;
        CanSign = MR_NO;
        CanCertify = MR_NO;
        CanAuthenticate = MR_NO;
        IsQualified = MR_NO;
        Secret = MR_NO;
        Length = 0;
        KeyId = MR_make_string_const("""");
        Fingerprint = MR_make_string_const("""");
        Timestamp = -1;
        Expires = -1;
    }
").

:- semipure pred convert_user_ids(gpgme_user_id::in, list(user_id)::out)
    is det.

convert_user_ids(UserId0, UserIds) :-
    ( semipure convert_user_id(UserId0, UserId, Next0) ->
        semipure convert_user_ids(Next0, Next),
        UserIds = [UserId | Next]
    ;
        UserIds = []
    ).

:- semipure pred convert_user_id(gpgme_user_id::in, user_id::out,
    gpgme_user_id::out) is semidet.

convert_user_id(UserId0, UserId, Next) :-
    semipure user_id_fields(UserId0, Next, Revoked, Invalid, Validity, UID,
        HaveName, Name, HaveComment, Comment, HaveEmail, Email),
    UserId = user_id(Revoked, Invalid, Validity, UID, maybe(HaveName, Name),
        maybe(HaveComment, Comment), maybe(HaveEmail, Email)).

:- semipure pred user_id_fields(gpgme_user_id::in, gpgme_user_id::out,
    bool::out, bool::out, validity::out, string::out,
    bool::out, string::out, bool::out, string::out, bool::out, string::out)
    is semidet.

:- pragma foreign_proc("C",
    user_id_fields(UserId::in, Next::out,
        Revoked::out, Invalid::out, Validity::out, UID::out,
        HaveName::out, Name::out, HaveComment::out, Comment::out,
        HaveEmail::out, Email::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    Next = NULL;
    Revoked = MR_YES;
    Invalid = MR_YES;
    Validity = GPGME_VALIDITY_UNKNOWN;
    UID = MR_make_string_const("""");
    HaveName = MR_NO;
    Name = MR_make_string_const("""");
    HaveComment = MR_NO;
    Comment = MR_make_string_const("""");
    HaveEmail = MR_NO;
    Email = MR_make_string_const("""");

    if (UserId == NULL) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
        Next = UserId->next;
        Revoked = UserId->revoked;
        Invalid = UserId->invalid;
        Validity = UserId->validity;
        MR_make_aligned_string_copy_msg(UID, UserId->uid, MR_ALLOC_ID);
        if (UserId->name != NULL) {
            HaveName = MR_YES;
            MR_make_aligned_string_copy_msg(Name, UserId->name, MR_ALLOC_ID);
        }
        if (UserId->comment != NULL) {
            HaveComment = MR_YES;
            MR_make_aligned_string_copy_msg(Comment, UserId->comment,
                MR_ALLOC_ID);
        }
        if (UserId->email != NULL) {
            HaveEmail = MR_YES;
            MR_make_aligned_string_copy_msg(Email, UserId->email, MR_ALLOC_ID);
        }
    }
").

:- func maybe(bool, T) = maybe(T).

maybe(yes, X) = yes(X).
maybe(no, _) = no.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
