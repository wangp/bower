% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.decrypt.
:- interface.

:- type decrypt_result
    --->    decrypt_result(
                unsupported_algorithm   :: string,
                wrong_key_usage         :: bool,
                % recipients
                dr_file_name            :: maybe(string)
            ).

:- pred gpgme_op_decrypt(ctx::in, data::in, data::in,
    maybe_error(decrypt_result)::out, io::di, io::uo) is det.

    % Only for gpgme.decrypt_verify
    %
:- pred gpgme_op_decrypt_result(ctx::in, decrypt_result::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

gpgme_op_decrypt(Ctx, data(Cipher, _), data(Plain, _), Res, !IO) :-
    gpgme_op_decrypt_2(Ctx, Cipher, Plain, Ok, Error, !IO),
    (
        Ok = yes,
        gpgme_op_decrypt_result(Ctx, DecryptResult, !IO),
        Res = ok(DecryptResult)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_op_decrypt_2(ctx::in, gpgme_data::in, gpgme_data::in, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_decrypt_2(Ctx::in, Cipher::in, Plain::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_decrypt(Ctx, Cipher, Plain);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%

gpgme_op_decrypt_result(Ctx, DecryptResult, !IO) :-
    gpgme_op_decrypt_result_2(Ctx, UnsupportedAlgorithm, WrongKeyUsage,
        FileName, !IO),
    ( FileName = "" ->
        MaybeFileName = no
    ;
        MaybeFileName = yes(FileName)
    ),
    DecryptResult = decrypt_result(UnsupportedAlgorithm, WrongKeyUsage,
        MaybeFileName).

:- pred gpgme_op_decrypt_result_2(ctx::in, string::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_decrypt_result_2(Ctx::in, UnsupportedAlgorithm::out,
        WrongKeyUsage::out, FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_decrypt_result_t r;

    r = gpgme_op_decrypt_result(Ctx);

    if (r != NULL && r->unsupported_algorithm != NULL) {
        UnsupportedAlgorithm = MR_make_string(MR_ALLOC_ID, ""%s"",
            r->unsupported_algorithm);
    } else {
        UnsupportedAlgorithm = MR_make_string_const("""");
    }

    if (r != NULL && r->wrong_key_usage != 0) {
        WrongKeyUsage = MR_YES;
    } else {
        WrongKeyUsage = MR_NO;
    }

    if (r != NULL && r->file_name != NULL) {
        FileName = MR_make_string(MR_ALLOC_ID, ""%s"", r->file_name);
    } else {
        FileName = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
