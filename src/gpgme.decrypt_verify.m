% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.decrypt_verify.
:- interface.

:- import_module gpgme.decrypt.
:- import_module gpgme.verify.

:- pred gpgme_op_decrypt_verify(ctx::in, data::in, data::in,
    maybe_error(decrypt_result)::out, maybe(verify_result)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

gpgme_op_decrypt_verify(Ctx, data(Cipher, _), data(Plain, _),
        ResDecrypt, MaybeVerifyResult, !IO) :-
    gpgme_op_decrypt_verify_2(Ctx, Cipher, Plain, Ok, Error, MaybeSigned, !IO),
    (
        Ok = yes,
        gpgme_op_decrypt_result(Ctx, DecryptResult, !IO),
        ResDecrypt = ok(DecryptResult)
    ;
        Ok = no,
        ResDecrypt = error(Error)
    ),
    (
        MaybeSigned = yes,
        gpgme_op_verify_result(Ctx, VerifyResult0, !IO),
        (
            VerifyResult0 = ok(VerifyResult),
            MaybeVerifyResult = yes(VerifyResult)
        ;
            VerifyResult0 = error(_),
            MaybeVerifyResult = no
        )
    ;
        MaybeSigned = no,
        MaybeVerifyResult = no
    ).

:- pred gpgme_op_decrypt_verify_2(ctx::in, gpgme_data::in, gpgme_data::in,
    bool::out, string::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_decrypt_verify_2(Ctx::in, Cipher::in, Plain::in,
        Ok::out, Error::out, MaybeSigned::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_decrypt_verify(Ctx, Cipher, Plain);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
        MaybeSigned = MR_YES;
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
        MaybeSigned = (gpgme_err_code(err) == GPG_ERR_NO_DATA) ? MR_YES : MR_NO;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
