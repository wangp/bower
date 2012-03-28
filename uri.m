%-----------------------------------------------------------------------------%

:- module uri.
:- interface.

:- pred detect_url(string::in, int::out, int::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

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
        ( detect_bracketed_url_end(String, Start, End0, UrlEnd) ->
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

:- pred valid_uri_char(char::in) is semidet.

valid_uri_char('!').
valid_uri_char('*').
valid_uri_char('''').
valid_uri_char('(').
valid_uri_char(')').
valid_uri_char(';').
valid_uri_char(':').
valid_uri_char('@').
valid_uri_char('&').
valid_uri_char('=').
valid_uri_char('+').
valid_uri_char('$').
valid_uri_char(',').
valid_uri_char('/').
valid_uri_char('?').
valid_uri_char('#').
valid_uri_char('[').
valid_uri_char(']').
valid_uri_char('A').
valid_uri_char('B').
valid_uri_char('C').
valid_uri_char('D').
valid_uri_char('E').
valid_uri_char('F').
valid_uri_char('G').
valid_uri_char('H').
valid_uri_char('I').
valid_uri_char('J').
valid_uri_char('K').
valid_uri_char('L').
valid_uri_char('M').
valid_uri_char('N').
valid_uri_char('O').
valid_uri_char('P').
valid_uri_char('Q').
valid_uri_char('R').
valid_uri_char('S').
valid_uri_char('T').
valid_uri_char('U').
valid_uri_char('V').
valid_uri_char('W').
valid_uri_char('X').
valid_uri_char('Y').
valid_uri_char('Z').
valid_uri_char('a').
valid_uri_char('b').
valid_uri_char('c').
valid_uri_char('d').
valid_uri_char('e').
valid_uri_char('f').
valid_uri_char('g').
valid_uri_char('h').
valid_uri_char('i').
valid_uri_char('j').
valid_uri_char('k').
valid_uri_char('l').
valid_uri_char('m').
valid_uri_char('n').
valid_uri_char('o').
valid_uri_char('p').
valid_uri_char('q').
valid_uri_char('r').
valid_uri_char('s').
valid_uri_char('t').
valid_uri_char('u').
valid_uri_char('v').
valid_uri_char('w').
valid_uri_char('x').
valid_uri_char('y').
valid_uri_char('z').
valid_uri_char('0').
valid_uri_char('1').
valid_uri_char('2').
valid_uri_char('3').
valid_uri_char('4').
valid_uri_char('5').
valid_uri_char('6').
valid_uri_char('7').
valid_uri_char('8').
valid_uri_char('9').
valid_uri_char('-').
valid_uri_char('_').
valid_uri_char('.').
valid_uri_char('~').
valid_uri_char('%').

:- pred detect_bracketed_url_end(string::in, int::in, int::in, int::out)
    is semidet.

detect_bracketed_url_end(String, Start, End0, UrlEnd) :-
    % Smartly handle bracketed URLs.
    string.unsafe_prev_index(String, Start, _, '('),
    string.unsafe_prev_index(String, End0, End1, LastChar),
    (
        LastChar = (')'),
        UrlEnd = End1
    ;
        ( LastChar = ('!')
        ; LastChar = (',')
        ; LastChar = ('.')
        ; LastChar = (';')
        ; LastChar = ('?')
        ),
        string.unsafe_prev_index(String, End1, UrlEnd, ')')
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
