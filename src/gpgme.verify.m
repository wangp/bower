% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.verify.
:- interface.

:- import_module bool.
:- import_module list.

:- type verify_result
    --->    verify_result(
                signatures      :: list(signature),
                vr_file_name    :: maybe(string)
            ).

:- type signature
    --->    signature(
                summary         :: list(signature_summary_bit),
                fingerprint     :: string,
                % status,
                % notations
                timestamp       :: timestamp,
                exp_timestamp   :: maybe(timestamp),
                s_wrong_key_usage :: bool,
                pka_trust       :: pka_trust,
                chain_model     :: bool,
                validity        :: validity,
                validity_reason :: string % maybe?
                % pubkey_algo
                % hash_algo
            ).

:- type signature_summary_bit
    --->    valid
    ;       green
    ;       red
    ;       key_revoked
    ;       key_expired
    ;       sig_expired
    ;       key_missing
    ;       crl_missing
    ;       crl_too_old
    ;       bad_policy
    ;       sys_error.

:- type timestamp == int.   % XXX unsigned long

:- type pka_trust
    --->    pka_no_info
    ;       pka_verification_failed
    ;       pka_verification_succeeded
    ;       pka_reserved.   % for future use

:- type validity
    --->    validity_unknown
    ;       validity_undefined
    ;       validity_never
    ;       validity_marginal
    ;       validity_full
    ;       validity_ultimate.

:- pred gpgme_op_verify_detached(ctx::in, data::in, data::in,
    maybe_error(verify_result)::out, io::di, io::uo) is det.

:- pred gpgme_op_verify_clearsigned(ctx::in, data::in, data::in,
    maybe_error(verify_result)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

:- type gpgme_verify_result.

:- pragma foreign_type("C", gpgme_verify_result, "gpgme_verify_result_t").

:- type gpgme_signature.

:- pragma foreign_type("C", gpgme_signature, "gpgme_signature_t").

:- pragma foreign_enum("C", signature_summary_bit/0, [
    valid - "GPGME_SIGSUM_VALID",
    green - "GPGME_SIGSUM_GREEN",
    red - "GPGME_SIGSUM_RED",
    key_revoked - "GPGME_SIGSUM_KEY_REVOKED",
    key_expired - "GPGME_SIGSUM_KEY_EXPIRED",
    sig_expired - "GPGME_SIGSUM_SIG_EXPIRED",
    key_missing - "GPGME_SIGSUM_KEY_MISSING",
    crl_missing - "GPGME_SIGSUM_CRL_MISSING",
    crl_too_old - "GPGME_SIGSUM_CRL_TOO_OLD",
    bad_policy - "GPGME_SIGSUM_BAD_POLICY",
    sys_error - "GPGME_SIGSUM_SYS_ERROR"
]).

:- pragma foreign_enum("C", pka_trust/0, [
    pka_no_info - "0",
    pka_verification_failed - "1",
    pka_verification_succeeded - "2",
    pka_reserved - "3"
]).

:- pragma foreign_enum("C", validity/0, [
    validity_unknown - "GPGME_VALIDITY_UNKNOWN",
    validity_undefined - "GPGME_VALIDITY_UNDEFINED",
    validity_never - "GPGME_VALIDITY_NEVER",
    validity_marginal - "GPGME_VALIDITY_MARGINAL",
    validity_full - "GPGME_VALIDITY_FULL",
    validity_ultimate - "GPGME_VALIDITY_ULTIMATE"
]).

%-----------------------------------------------------------------------------%

:- func all_signature_summary_bits = list(signature_summary_bit).

all_signature_summary_bits = List :-
    solutions(pred(X::out) is multi :- signature_summary_bit(X), List).

:- pred signature_summary_bit(signature_summary_bit).
:- mode signature_summary_bit(in) is det.
:- mode signature_summary_bit(out) is multi.

signature_summary_bit(valid).
signature_summary_bit(green).
signature_summary_bit(red).
signature_summary_bit(key_revoked).
signature_summary_bit(key_expired).
signature_summary_bit(sig_expired).
signature_summary_bit(key_missing).
signature_summary_bit(crl_missing).
signature_summary_bit(crl_too_old).
signature_summary_bit(bad_policy).
signature_summary_bit(sys_error).

%-----------------------------------------------------------------------------%

gpgme_op_verify_detached(Ctx, data(Sig, _), data(SignedText, _), Res, !IO) :-
    promise_pure
    (
        gpgme_op_verify_detached_2(Ctx, Sig, SignedText, Ok, Error, !IO),
        (
            Ok = yes,
            semipure gpgme_op_verify_result(Ctx, Res)
        ;
            Ok = no,
            Res = error(Error)
        )
    ).

:- pred gpgme_op_verify_detached_2(ctx::in, gpgme_data::in, gpgme_data::in,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_verify_detached_2(Ctx::in, Sig::in, SignedText::in,
        Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    gpgme_error_t err;

    err = gpgme_op_verify(Ctx, Sig, SignedText, NULL);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%

gpgme_op_verify_clearsigned(Ctx, data(Sig, _), data(Plain, _), Res, !IO) :-
    promise_pure
    (
        gpgme_op_verify_clearsigned_2(Ctx, Sig, Plain, Ok, Error, !IO),
        (
            Ok = yes,
            semipure gpgme_op_verify_result(Ctx, Res)
        ;
            Ok = no,
            Res = error(Error)
        )
    ).

:- pred gpgme_op_verify_clearsigned_2(ctx::in, gpgme_data::in, gpgme_data::in,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_op_verify_clearsigned_2(Ctx::in, Sig::in, Plain::in,
        Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    gpgme_error_t err;

    err = gpgme_op_verify(Ctx, Sig, NULL, Plain);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%

:- semipure pred gpgme_op_verify_result(ctx::in,
    maybe_error(verify_result)::out) is det.

gpgme_op_verify_result(Ctx, Res) :-
    semipure gpgme_op_verify_result_2(Ctx, Ok, VerifyResult0),
    (
        Ok = yes,
        semipure convert_verify_result(VerifyResult0, VerifyResult),
        Res = ok(VerifyResult)
    ;
        Ok = no,
        Res = error("gpgme_op_verify_result failed")
    ).

:- semipure pred gpgme_op_verify_result_2(ctx::in, bool::out,
    gpgme_verify_result::out) is det.

:- pragma foreign_proc("C",
    gpgme_op_verify_result_2(Ctx::in, Ok::out, VerifyResult::out),
    [will_not_call_mercury, promise_semipure, thread_safe, may_not_duplicate],
"
    VerifyResult = gpgme_op_verify_result(Ctx);
    Ok = (VerifyResult != NULL) ? MR_YES : MR_NO;
").

:- semipure pred convert_verify_result(gpgme_verify_result::in,
    verify_result::out) is det.

convert_verify_result(VR0, VR) :-
    semipure verify_result_fields(VR0, Signatures0, HaveFileName, FileName),
    semipure convert_signatures(Signatures0, Signatures),
    (
        HaveFileName = yes,
        MaybeFileName = yes(FileName)
    ;
        HaveFileName = no,
        MaybeFileName = no
    ),
    VR = verify_result(Signatures, MaybeFileName).

:- semipure pred verify_result_fields(gpgme_verify_result::in,
    gpgme_signature::out, bool::out, string::out) is det.

:- pragma foreign_proc("C",
    verify_result_fields(VR::in, Signatures::out, HaveFileName::out,
        FileName::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    Signatures = VR->signatures;

    if (VR->file_name != NULL) {
        HaveFileName = MR_YES;
        MR_make_aligned_string_copy_msg(FileName, VR->file_name, MR_ALLOC_ID);
    } else {
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    }
").

:- semipure pred convert_signatures(gpgme_signature::in, list(signature)::out)
    is det.

convert_signatures(Signature0, Res) :-
    ( semipure convert_signature(Signature0, Next, Signature) ->
        semipure convert_signatures(Next, Signatures),
        Res = [Signature | Signatures]
    ;
        Res = []
    ).

:- semipure pred convert_signature(gpgme_signature::in, gpgme_signature::out,
    signature::out) is semidet.

convert_signature(Sig0, Next, Sig) :-
    semipure signature_fields(Sig0, Next, SummaryBits, Fingerprint, Timestamp,
        ExpTimestamp0, WrongKeyUsage, PkaTrust, ChainModel, Validity,
        ValidityReason),
    require_det
    (
        signature_summary_list(SummaryBits, Summary),
        ( ExpTimestamp0 = 0 ->
            ExpTimestamp = no
        ;
            ExpTimestamp = yes(ExpTimestamp0)
        ),
        Sig = signature(Summary, Fingerprint, Timestamp, ExpTimestamp,
            WrongKeyUsage, PkaTrust, ChainModel, Validity, ValidityReason)
    ).

:- semipure pred signature_fields(gpgme_signature::in, gpgme_signature::out,
    int::out, string::out, timestamp::out, timestamp::out, bool::out,
    pka_trust::out, bool::out, validity::out, string::out) is semidet.

:- pragma foreign_proc("C",
    signature_fields(Sig::in, Next::out, SummaryBits::out, Fingerprint::out,
        Timestamp::out, ExpTimestamp::out, WrongKeyUsage::out, PkaTrust::out,
        ChainModel::out, Validity::out, ValidityReason::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    SUCCESS_INDICATOR = (Sig != NULL);
    if (SUCCESS_INDICATOR) {
        Next = Sig->next;
        SummaryBits = Sig->summary;
        MR_make_aligned_string_copy_msg(Fingerprint, Sig->fpr, MR_ALLOC_ID);
        Timestamp = Sig->timestamp;
        ExpTimestamp = Sig->exp_timestamp;
        WrongKeyUsage = (Sig->wrong_key_usage) ? MR_YES : MR_NO;
        PkaTrust = Sig->pka_trust;
        ChainModel = (Sig->chain_model) ? MR_YES : MR_NO;
        Validity = Sig->validity;
        ValidityReason = _gpgme_error_to_string(Sig->validity_reason);
    } else {
        Next = NULL;
        SummaryBits = 0;
        Fingerprint = MR_make_string_const("""");
        Timestamp = 0;
        ExpTimestamp = 0;
        WrongKeyUsage = MR_NO;
        PkaTrust = 0;
        ChainModel = MR_NO;
        Validity = 0;
        ValidityReason = MR_make_string_const("""");
    }
").

:- pred signature_summary_list(int::in, list(signature_summary_bit)::out)
    is det.

signature_summary_list(Bits, List) :-
    list.filter(signature_summary_contains(Bits),
        all_signature_summary_bits, List).

:- pred signature_summary_contains(int::in, signature_summary_bit::in)
    is semidet.

:- pragma foreign_proc("C",
    signature_summary_contains(Bits::in, Bit::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Bits & Bit) != 0;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
