% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.sign.
:- interface.

:- import_module io.
:- import_module list.

:- type sign_result
    --->    sign_result(
                invalid_signers :: list(invalid_key),
                new_signatures  :: list(new_signature)
            ).

:- type new_signature
    --->    new_signature(
                % type
                % pubkey_algo
                hash_algo       :: hash_algo,
                % sig_class
                timestamp       :: timestamp,
                fingerprint     :: string
            ).

:- pred gpgme_op_sign_detached(ctx::in, data::in, data::in,
    maybe_error(sign_result)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module gpgme.invalid_key.

:- type gpgme_sign_result.

:- pragma foreign_type("C", gpgme_sign_result, "gpgme_sign_result_t").

:- type gpgme_new_signature.

:- pragma foreign_type("C", gpgme_new_signature, "gpgme_new_signature_t").

%-----------------------------------------------------------------------------%

gpgme_op_sign_detached(Ctx, data(Plain, _), data(Sig, _), Res, !IO) :-
    gpgme_op_sign_2(Ctx, Plain, Sig, Ok, Error, !IO),
    (
        Ok = yes,
        gpgme_op_sign_result(Ctx, Res, !IO)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_op_sign_2(ctx::in, gpgme_data::in, gpgme_data::in,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_sign_2(Ctx::in, Plain::in, Sig::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_op_sign(Ctx, Plain, Sig, GPGME_SIG_MODE_DETACH);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

:- pred gpgme_op_sign_result(ctx::in, maybe_error(sign_result)::out,
    io::di, io::uo) is det.

gpgme_op_sign_result(Ctx, Res, !IO) :-
    promise_pure
    (
        gpgme_op_sign_result_2(Ctx, Ok, SignResult0, !IO),
        (
            Ok = yes,
            semipure convert_sign_result(SignResult0, SignResult),
            Res = ok(SignResult)
        ;
            Ok = no,
            Res = error("gpgme_op_sign_result failed")
        )
    ).

:- pred gpgme_op_sign_result_2(ctx::in, bool::out, gpgme_sign_result::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_sign_result_2(Ctx::in, Ok::out, SignResult::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    SignResult = gpgme_op_sign_result(Ctx);
    Ok = (SignResult != NULL) ? MR_YES : MR_NO;
").

:- semipure pred convert_sign_result(gpgme_sign_result::in, sign_result::out)
    is det.

convert_sign_result(SignResult0, SignResult) :-
    semipure sign_result_fields(SignResult0, InvalidSigners0, Signatures0),
    semipure convert_invalid_keys(InvalidSigners0, InvalidSigners),
    semipure convert_signatures(Signatures0, Signatures),
    SignResult = sign_result(InvalidSigners, Signatures).

:- semipure pred sign_result_fields(gpgme_sign_result::in,
    gpgme_invalid_key::out, gpgme_new_signature::out) is det.

:- pragma foreign_proc("C",
    sign_result_fields(SignResult::in, InvalidSigners::out, Signatures::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    InvalidSigners = SignResult->invalid_signers;
    Signatures = SignResult->signatures;
").

:- semipure pred convert_signatures(gpgme_new_signature::in,
    list(new_signature)::out) is det.

convert_signatures(Signature0, Res) :-
    ( semipure convert_signature(Signature0, Next, Signature) ->
        semipure convert_signatures(Next, Signatures),
        Res = [Signature | Signatures]
    ;
        Res = []
    ).

:- semipure pred convert_signature(gpgme_new_signature::in,
    gpgme_new_signature::out, new_signature::out) is semidet.

convert_signature(Signature0, Next, Signature) :-
    semipure signature_fields(Signature0, Next, HashAlgo, Timestamp, Fingerprint),
    Signature = new_signature(HashAlgo, Timestamp, Fingerprint).

:- semipure pred signature_fields(gpgme_new_signature::in,
    gpgme_new_signature::out, hash_algo::out, int::out, string::out)
    is semidet.

:- pragma foreign_proc("C",
    signature_fields(Sig::in, Next::out, HashAlgo::out, Timestamp::out,
        Fingerprint::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    SUCCESS_INDICATOR = (Sig != NULL);
    if (SUCCESS_INDICATOR) {
        Next = Sig->next;
        HashAlgo = Sig->hash_algo;
        Timestamp = Sig->timestamp;
        MR_make_aligned_string_copy_msg(Fingerprint, Sig->fpr, MR_ALLOC_ID);
    } else {
        Next = NULL;
        HashAlgo = -1;
        Timestamp = 0;
        Fingerprint = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
