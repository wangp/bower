% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.encrypt.
:- interface.

:- import_module io.

:- type encrypt_op
    --->    encrypt_only
    ;       encrypt_sign.

:- type encrypt_flag
    --->    always_trust
    ;       no_encrypt_to.

:- type encrypt_result
    --->    encrypt_result(
                invalid_recipients :: list(invalid_key)
            ).

:- pred gpgme_op_encrypt(encrypt_op::in, ctx::in, list(key)::in,
    list(encrypt_flag)::in, data::in, data::in,
    maybe_error(encrypt_result)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module gpgme.invalid_key.
:- import_module gpgme.key_array.

:- pragma foreign_enum("C", encrypt_flag/0, [
    always_trust - "GPGME_ENCRYPT_ALWAYS_TRUST",
    no_encrypt_to - "GPGME_ENCRYPT_NO_ENCRYPT_TO"
]).

:- type gpgme_encrypt_result.

:- pragma foreign_type("C", gpgme_encrypt_result, "gpgme_encrypt_result_t").

%-----------------------------------------------------------------------------%

gpgme_op_encrypt(Op, Ctx, Recipients, Flags, data(Plain, _), data(Cipher, _),
        Res, !IO) :-
    FlagsInt = encrypt_flags_to_int(Flags),
    with_key_array(gpgme_op_encrypt_2(Op, Ctx, FlagsInt, Plain, Cipher),
        Recipients, {Ok, Error}, !IO),
    (
        Ok = yes,
        gpgme_op_encrypt_result(Ctx, Res, !IO)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- func encrypt_flags_to_int(list(encrypt_flag)) = int.

encrypt_flags_to_int(Flags) = list.foldl(or_encrypt_flag, Flags, 0).

:- func or_encrypt_flag(encrypt_flag, int) = int.

:- pragma foreign_proc("C",
    or_encrypt_flag(F::in, X0::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = X0 | F;
").

:- pred gpgme_op_encrypt_2(encrypt_op::in, ctx::in, int::in, gpgme_data::in,
    gpgme_data::in, gpgme_key_array::in, {bool, string}::out, io::di, io::uo)
    is det.

gpgme_op_encrypt_2(Op, Ctx, Flags, Plain, Cipher, Recp, {Ok, Error}, !IO) :-
    gpgme_op_encrypt_3(sign(Op), Ctx, Flags, Plain, Cipher, Recp, Ok, Error,
        !IO).

:- pred gpgme_op_encrypt_3(bool::in, ctx::in, int::in, gpgme_data::in,
    gpgme_data::in, gpgme_key_array::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_encrypt_3(Sign::in, Ctx::in, Flags::in, Plain::in, Cipher::in,
        Recp::in, Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    gpgme_error_t err;

    if (Sign) {
        err = gpgme_op_encrypt_sign(Ctx, Recp, Flags, Plain, Cipher);
    } else {
        err = gpgme_op_encrypt(Ctx, Recp, Flags, Plain, Cipher);
    }
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

:- func sign(encrypt_op) = bool.

sign(encrypt_only) = no.
sign(encrypt_sign) = yes.

%-----------------------------------------------------------------------------%

:- pred gpgme_op_encrypt_result(ctx::in, maybe_error(encrypt_result)::out,
    io::di, io::uo) is det.

gpgme_op_encrypt_result(Ctx, Res, !IO) :-
    promise_pure
    (
        gpgme_op_encrypt_result_2(Ctx, Ok, EncryptResult0, !IO),
        (
            Ok = yes,
            semipure convert_encrypt_result(EncryptResult0, EncryptResult),
            Res = ok(EncryptResult)
        ;
            Ok = no,
            Res = error("gpgme_op_encrypt_result failed")
        )
    ).

:- pred gpgme_op_encrypt_result_2(ctx::in, bool::out,
    gpgme_encrypt_result::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_encrypt_result_2(Ctx::in, Ok::out, EncryptResult::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    EncryptResult = gpgme_op_encrypt_result(Ctx);
    Ok = (EncryptResult != NULL) ? MR_YES : MR_NO;
").

:- semipure pred convert_encrypt_result(gpgme_encrypt_result::in,
    encrypt_result::out) is det.

convert_encrypt_result(EncryptResult0, EncryptResult) :-
    semipure encrypt_result_fields(EncryptResult0, InvalidRecipients0),
    semipure convert_invalid_keys(InvalidRecipients0, InvalidRecipients),
    EncryptResult = encrypt_result(InvalidRecipients).

:- semipure pred encrypt_result_fields(gpgme_encrypt_result::in,
    gpgme_invalid_key::out) is det.

:- pragma foreign_proc("C",
    encrypt_result_fields(EncryptResult::in, InvalidRecipients::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    InvalidRecipients = EncryptResult->invalid_recipients;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
