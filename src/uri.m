% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module uri.
:- interface.

:- type url_regex.

:- pred init_url_regex(url_regex::out) is det.

:- pred detect_url(url_regex::in, string::in, int::out, int::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module regex.
:- import_module rfc3986.

:- type url_regex == regex.

%-----------------------------------------------------------------------------%

init_url_regex(Reg) :-
    regcomp("(https?|gemini)://", [reg_extended], CompRes),
    (
        CompRes = ok(Reg)
    ;
        CompRes = error(Error),
        unexpected($pred, "regcomp failed: " ++ Error)
    ).

%-----------------------------------------------------------------------------%

detect_url(Reg, String, Start, End) :-
    detect_url_loop(Reg, String, 0, Start, End).

:- pred detect_url_loop(regex::in, string::in, int::in, int::out, int::out)
    is semidet.

detect_url_loop(Reg, String, BeginAt, Start, End) :-
    % We don't use start-of-line anchors so no need for reg_notbol even when
    % BeginAt > 0.
    EFlags = [],
    unsafe_regexec_offset(Reg, String, BeginAt, EFlags, ExecRes),
    require_complete_switch [ExecRes]
    (
        ExecRes = have_match(Matches),
        (
            Matches = [Match | _],
            Match = regmatch(Http, AfterSlashSlash),
            (
                is_start_of_word(String, Http),
                detect_url_end(String, AfterSlashSlash, End0)
            ->
                Start = Http,
                ( strip_url_trailing_chars(String, Start, End0, UrlEnd) ->
                    End = UrlEnd
                ;
                    End = End0
                )
            ;
                detect_url_loop(Reg, String, AfterSlashSlash, Start, End)
            )
        ;
            Matches = [],
            fail
        )
    ;
        ExecRes = no_match,
        fail
    ;
        ExecRes = error(_),
        fail
    ).

:- pred is_start_of_word(string::in, int::in) is semidet.

is_start_of_word(String, I) :-
    not (
        string.unsafe_prev_index(String, I, _, PrevChar),
        char.is_alnum_or_underscore(PrevChar)
    ).

:- pred detect_url_end(string::in, int::in, int::out) is det.

detect_url_end(String, I, End) :-
    (
        string.unsafe_index_next(String, I, J, Char),
        valid_uri_char(Char)
    ->
        detect_url_end(String, J, End)
    ;
        End = I
    ).

:- pred strip_url_trailing_chars(string::in, int::in, int::in, int::out)
    is semidet.

strip_url_trailing_chars(String, Start, End0, UrlEnd) :-
    string.unsafe_prev_index(String, End0, End1, LastChar),
    (
        ( LastChar = (')'), Open = ('(')
        ; LastChar = (']'), Open = ('[')
        ),
        % Smartly handle bracketed URLs.
        count_unbalanced_brackets(Open, LastChar, String, Start, End1,
            1, Unbalanced),
        ( Unbalanced > 0 ->
            UrlEnd = End1
        ;
            UrlEnd = End0
        )
    ;
        ( LastChar = ('!')
        ; LastChar = (',')
        ; LastChar = ('.')
        ; LastChar = (';')
        ; LastChar = ('?')
        ),
        ( string.unsafe_prev_index(String, End1, End2, ')') ->
            UrlEnd = End2
        ;
            UrlEnd = End1
        )
    ).

:- pred count_unbalanced_brackets(char::in, char::in,
    string::in, int::in, int::in, int::in, int::out) is det.

count_unbalanced_brackets(OpenChar, CloseChar, String, Start, Index0,
        Unbalanced0, Unbalanced) :-
    (
        string.unsafe_prev_index(String, Index0, Index1, Char),
        Index1 >= Start
    ->
        ( Char = CloseChar ->
            Unbalanced1 = Unbalanced0 + 1
        ; Char = OpenChar ->
            Unbalanced1 = Unbalanced0 - 1
        ;
            Unbalanced1 = Unbalanced0
        ),
        count_unbalanced_brackets(OpenChar, CloseChar, String, Start, Index1,
            Unbalanced1, Unbalanced)
    ;
        Unbalanced = Unbalanced0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
