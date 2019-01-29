% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.signer.
:- interface.

:- pred gpgme_signers_clear(ctx::in, io::di, io::uo) is det.

:- pred gpgme_signers_add(ctx::in, key::in, maybe_error::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gpgme_signers_clear(Ctx::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_signers_clear(Ctx);
").

%-----------------------------------------------------------------------------%

gpgme_signers_add(Ctx, Key, Res, !IO) :-
    Key = key(_KeyInfo, Mutvar),
    get_mutvar(Mutvar, MaybeKey, !IO),
    (
        MaybeKey = yes(GpgmeKey),
        gpgme_signers_add_2(Ctx, GpgmeKey, Ok, Error, !IO),
        (
            Ok = yes,
            Res = ok
        ;
            Ok = no,
            Res = error(Error)
        )
    ;
        MaybeKey = no,
        unexpected($module, $pred, "key already unref'd")
    ).

:- pred gpgme_signers_add_2(ctx::in, gpgme_key::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_signers_add_2(Ctx::in, Key::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_signers_add(Ctx, Key);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
