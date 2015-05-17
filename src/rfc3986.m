% Bower - a frontend for the Notmuch email system
% Copyright (C) 2015 Peter Wang

:- module rfc3986.
:- interface.

:- import_module char.
:- import_module maybe.

:- type uri_components
    --->    uri_components(
                % All strings are left in percent-encoded form.
                scheme :: maybe(string),
                authority :: maybe(string),
                path :: string,
                query :: maybe(string),
                fragment :: maybe(string)
            ).

:- pred split_uri(string::in, uri_components::out) is semidet.

:- pred valid_uri_char(char::in) is semidet.

:- pred unreserved_char(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module parsing_utils.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%

% Emulate the regular expression from Appendix B:
% (or we could just use it...)
%
%   ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?

split_uri(Input, URI) :-
    promise_equivalent_solutions [Result] (
        parsing_utils.parse(Input, no_skip_whitespace, uri, Result)
    ),
    Result = ok(URI).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

:- pred uri(src::in, uri_components::out, ps::in, ps::out) is semidet.

uri(Src, URI, !PS) :-
    optional(scheme, Src, Scheme, !PS),
    optional(authority, Src, Authority, !PS),
    optional(path_and_query, Src, PathAndQuery, !PS),
    optional(fragment, Src, Fragment, !PS),
    eof(Src, _, !PS),
    (
        PathAndQuery = yes(Path - Query)
    ;
        PathAndQuery = no,
        Path = "",
        Query = no
    ),
    URI = uri_components(Scheme, Authority, Path, Query, Fragment).

:- pred scheme(src::in, string::out, ps::in, ps::out) is semidet.

scheme(Src, Scheme, !PS) :-
    one_or_more(scheme_char, Src, Chars, !PS),
    next_char(Src, ':', !PS),
    string.from_char_list(Chars, Scheme).

:- pred scheme_char(src::in, char::out, ps::in, ps::out) is semidet.

scheme_char(Src, Char, !PS) :-
    valid_char(Src, Char, !PS),
    Char \= (':'),
    Char \= ('/'),
    Char \= ('?'),
    Char \= ('#').

:- pred authority(src::in, string::out, ps::in, ps::out) is semidet.

authority(Src, Authority, !PS) :-
    next_char(Src, '/', !PS),
    next_char(Src, '/', !PS),
    zero_or_more(authority_char, Src, Chars, !PS),
    string.from_char_list(Chars, Authority).

:- pred authority_char(src::in, char::out, ps::in, ps::out) is semidet.

authority_char(Src, Char, !PS) :-
    valid_char(Src, Char, !PS),
    Char \= ('/'),
    Char \= ('?'),
    Char \= ('#').

:- pred path_and_query(src::in, pair(string, maybe(string))::out,
    ps::in, ps::out) is semidet.

path_and_query(Src, Path - Query, !PS) :-
    path(Src, Path, !PS),
    optional(query, Src, Query, !PS).

:- pred path(src::in, string::out, ps::in, ps::out) is semidet.

path(Src, Path, !PS) :-
    zero_or_more(path_char, Src, PathChars, !PS),
    string.from_char_list(PathChars, Path).

:- pred path_char(src::in, char::out, ps::in, ps::out) is semidet.

path_char(Src, Char, !PS) :-
    valid_char(Src, Char, !PS),
    Char \= ('?'),
    Char \= ('#').

:- pred query(src::in, string::out, ps::in, ps::out) is semidet.

query(Src, Query, !PS) :-
    next_char(Src, '?', !PS),
    zero_or_more(query_char, Src, Chars, !PS),
    string.from_char_list(Chars, Query).

:- pred query_char(src::in, char::out, ps::in, ps::out) is semidet.

query_char(Src, Char, !PS) :-
    valid_char(Src, Char, !PS),
    Char \= ('#').

:- pred fragment(src::in, string::out, ps::in, ps::out) is semidet.

fragment(Src, Fragment, !PS) :-
    next_char(Src, '#', !PS),
    zero_or_more(fragment_char, Src, Chars, !PS),
    string.from_char_list(Chars, Fragment).

:- pred fragment_char(src::in, char::out, ps::in, ps::out) is semidet.

fragment_char(Src, Char, !PS) :-
    valid_char(Src, Char, !PS).

:- pred valid_char(src::in, char::out, ps::in, ps::out) is semidet.

valid_char(Src, C, !PS) :-
    next_char(Src, C, !PS),
    valid_uri_char(C).

%-----------------------------------------------------------------------------%

valid_uri_char('!').    % sub-delims
valid_uri_char('*').    % sub-delims
valid_uri_char('''').   % sub-delims
valid_uri_char('(').    % sub-delims
valid_uri_char(')').    % sub-delims
valid_uri_char(';').    % sub-delims
valid_uri_char(':').    % gen-delims
valid_uri_char('@').    % gen-delims
valid_uri_char('&').    % sub-delims
valid_uri_char('=').    % sub-delims
valid_uri_char('+').    % sub-delims
valid_uri_char('$').    % sub-delims
valid_uri_char(',').    % sub-delims
valid_uri_char('/').    % gen-delims
valid_uri_char('?').    % gen-delims
valid_uri_char('#').    % gen-delims
valid_uri_char('[').    % gen-delims
valid_uri_char(']').    % gen-delims
valid_uri_char('%').    % percent-encoding
valid_uri_char(C) :-
    unreserved_char(C).

%-----------------------------------------------------------------------------%

unreserved_char('A').
unreserved_char('B').
unreserved_char('C').
unreserved_char('D').
unreserved_char('E').
unreserved_char('F').
unreserved_char('G').
unreserved_char('H').
unreserved_char('I').
unreserved_char('J').
unreserved_char('K').
unreserved_char('L').
unreserved_char('M').
unreserved_char('N').
unreserved_char('O').
unreserved_char('P').
unreserved_char('Q').
unreserved_char('R').
unreserved_char('S').
unreserved_char('T').
unreserved_char('U').
unreserved_char('V').
unreserved_char('W').
unreserved_char('X').
unreserved_char('Y').
unreserved_char('Z').
unreserved_char('a').
unreserved_char('b').
unreserved_char('c').
unreserved_char('d').
unreserved_char('e').
unreserved_char('f').
unreserved_char('g').
unreserved_char('h').
unreserved_char('i').
unreserved_char('j').
unreserved_char('k').
unreserved_char('l').
unreserved_char('m').
unreserved_char('n').
unreserved_char('o').
unreserved_char('p').
unreserved_char('q').
unreserved_char('r').
unreserved_char('s').
unreserved_char('t').
unreserved_char('u').
unreserved_char('v').
unreserved_char('w').
unreserved_char('x').
unreserved_char('y').
unreserved_char('z').
unreserved_char('0').
unreserved_char('1').
unreserved_char('2').
unreserved_char('3').
unreserved_char('4').
unreserved_char('5').
unreserved_char('6').
unreserved_char('7').
unreserved_char('8').
unreserved_char('9').
unreserved_char('-').
unreserved_char('.').
unreserved_char('_').
unreserved_char('~').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
