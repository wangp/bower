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
:- import_module list.
:- import_module string.

make_presentable(S0) = presentable_string(S) :-
    ( string.all_match(isprint, S0) ->
        S = S0
    ;
        % Not the most efficient but should be rarely reached.
        string.to_char_list(S0, Chars0),
        list.map(sanitise_char, Chars0, Chars),
        string.from_char_list(Chars, S)
    ).

:- pred sanitise_char(char::in, char::out) is det.

sanitise_char(C0, C) :-
    ( isprint(C0) ->
        C = C0
    ; char.is_whitespace(C0) ->
        C = (' ')
    ;
        C = ('\ufffd')
    ).

:- pred isprint(char::in) is semidet.

:- pragma foreign_proc("C",
    isprint(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* The argument to isprint must be representable by an unsigned char
     * or equal to EOF.
     */
    SUCCESS_INDICATOR = (Char >= 0x80) || isprint(Char);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
