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
valid_uri_char('A').    % unreserved
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
valid_uri_char('-').    % unreserved
valid_uri_char('_').    % unreserved
valid_uri_char('.').    % unreserved
valid_uri_char('~').    % unreserved
valid_uri_char('%').    % percent-encoding

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
