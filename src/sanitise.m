% Bower - a frontend for the Notmuch email system
% Copyright (C) 2020 Peter Wang

:- module sanitise.
:- interface.

:- type presentable_string
    --->    presentable_string(string).

    % Replace all ASCII whitespace characters with SPACE (U+0020) and
    % ASCII unprintable characters with U+FFFD.
    %
:- func make_presentable(string) = presentable_string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module char_util.
:- import_module string_util.

make_presentable(S0) = presentable_string(S) :-
    ( string.all_match(is_printable, S0) ->
        S1 = S0
    ;
        % Not the most efficient but should be rarely reached.
        string.to_char_list(S0, Chars0),
        list.map(sanitise_char, Chars0, Chars),
        string.from_char_list(Chars, S1)
    ),
    collapse_spaces(S1, S).

:- pred sanitise_char(char::in, char::out) is det.

sanitise_char(C0, C) :-
    ( is_printable(C0) ->
        C = C0
    ; char.is_whitespace(C0) ->
        C = (' ')
    ;
        C = ('\ufffd')
    ).

:- pred collapse_spaces(string::in, string::out) is det.

collapse_spaces(S0, S) :-
    collapse_spaces_between(S0, 0, string.length(S0), S).

:- pred collapse_spaces_between(string::in, int::in, int::in, string::out)
    is det.

collapse_spaces_between(S0, Start, End, S) :-
    ( sub_string_search_start(S0, "  ", Start, SpIndex) ->
        skip_whitespace(S0, SpIndex, NonSpIndex),
        string.unsafe_between(S0, Start, SpIndex + 1, Head),
        collapse_spaces_between(S0, NonSpIndex, End, Tail),
        S = Head ++ Tail
    ;
        ( Start = 0 ->
            S = S0
        ; Start = End ->
            S = ""
        ;
            string.unsafe_between(S0, Start, End, S)
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
