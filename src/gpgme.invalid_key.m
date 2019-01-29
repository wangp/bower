% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.invalid_key.
:- interface.

:- semipure pred convert_invalid_keys(gpgme_invalid_key::in,
    list(invalid_key)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

convert_invalid_keys(Key0, Res) :-
    ( semipure convert_invalid_key(Key0, Key1, Key) ->
        semipure convert_invalid_keys(Key1, Keys),
        Res = [Key | Keys]
    ;
        Res = []
    ).

:- semipure pred convert_invalid_key(gpgme_invalid_key::in,
    gpgme_invalid_key::out, invalid_key::out) is semidet.

convert_invalid_key(Key0, Next, Key) :-
    semipure invalid_key_fields(Key0, Next, Fingerprint, Reason),
    Key = invalid_key(Fingerprint, Reason).

:- semipure pred invalid_key_fields(gpgme_invalid_key::in,
    gpgme_invalid_key::out, string::out, string::out) is semidet.

:- pragma foreign_proc("C",
    invalid_key_fields(Key::in, Next::out, Fingerprint::out, Reason::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    SUCCESS_INDICATOR = (Key != NULL);
    if (SUCCESS_INDICATOR) {
        Next = Key->next;
        MR_make_aligned_string_copy_msg(Fingerprint, Key->fpr, MR_ALLOC_ID);
        Reason = _gpgme_error_to_string(Key->reason, MR_ALLOC_ID);
    } else {
        Next = NULL;
        Fingerprint = MR_make_string_const("""");
        Reason = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
