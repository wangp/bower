% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module uri.
:- interface.

:- pred detect_url(string::in, int::out, int::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

:- import_module rfc3986.
:- import_module string_util.

%-----------------------------------------------------------------------------%

detect_url(String, Start, End) :-
    detect_url_2(String, 0, Start, End).

:- pred detect_url_2(string::in, int::in, int::out, int::out) is semidet.

detect_url_2(String, BeginAt, Start, End) :-
    % string.sub_string_search_start is unnecessarily safe.
    string_util.unsafe_strstr(String, "http", BeginAt, Http),
    AfterHttp = Http + 4,
    (
        is_start_of_word(String, Http),
        s_colon_slash_slash(String, AfterHttp, AfterSlashSlash),
        detect_url_end(String, AfterSlashSlash, End0)
    ->
        Start = Http,
        ( strip_url_trailing_chars(String, Start, End0, UrlEnd) ->
            End = UrlEnd
        ;
            End = End0
        )
    ;
        detect_url_2(String, AfterHttp, Start, End)
    ).

:- pred is_start_of_word(string::in, int::in) is semidet.

is_start_of_word(String, I) :-
    not (
        string.unsafe_prev_index(String, I, _, PrevChar),
        char.is_alnum_or_underscore(PrevChar)
    ).

:- pred s_colon_slash_slash(string::in, int::in, int::out) is semidet.

s_colon_slash_slash(String, !I) :-
    % Skip s in "https".
    ( string.unsafe_index_next(String, !I, 's') ->
        true
    ;
        true
    ),
    string.unsafe_index_next(String, !I, ':'),
    string.unsafe_index_next(String, !I, '/'),
    string.unsafe_index_next(String, !I, '/').

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
