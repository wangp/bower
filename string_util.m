%-----------------------------------------------------------------------------%

:- module string_util.
:- interface.

:- pred strcase_equal(string::in, string::in) is semidet.

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
