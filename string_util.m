% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module string_util.
:- interface.

:- import_module char.

:- pred strcase_equal(string::in, string::in) is semidet.

:- pred strcase_str(string::in, string::in) is semidet.

:- pred strrchr(string::in, char::in, int::out) is semidet.

:- pred verify_utf8(string::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- pragma foreign_proc("C",
    strcase_equal(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasecmp(SA, SB) == 0);
").

:- pragma foreign_proc("C",
    strcase_str(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasestr(SA, SB) != 0);
").

:- pragma foreign_proc("C",
    strrchr(S::in, C::in, I::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const char *p;

    p = strrchr(S, C);
    if (p != NULL) {
        SUCCESS_INDICATOR = MR_TRUE;
        I = (p - S);
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
        I = -1;
    }
").

verify_utf8(String) :-
    verify_utf8_2(String, 0, string.count_codepoints(String)).

:- pred verify_utf8_2(string::in, int::in, int::in) is semidet.

verify_utf8_2(String, Cur, End) :-
    ( Cur = End ->
        true
    ; string.unsafe_index_next(String, Cur, Next, _) ->
        verify_utf8_2(String, Next, End)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
