% Bower - a frontend for the Notmuch email system
% Copyright (C) 2015 Peter Wang

:- module rfc5234.
:- interface.

:- import_module char.

:- pred 'ALPHA'(char::in) is semidet.

:- pred 'DIGIT'(char::in) is semidet.

:- func 'DQUOTE' = char.

:- pred 'HEXDIG'(char::in, int::out) is semidet.

:- pred 'VCHAR'(char::in) is semidet.

:- pred 'WSP'(char::in) is semidet.

:- pred not_WSP(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

'ALPHA'(C) :-
    char.is_alpha(C).

'DIGIT'(C) :-
    char.is_digit(C).

'DQUOTE' = '"'.

'HEXDIG'(C, I) :-
    char.is_hex_digit(C, I).

'VCHAR'(C) :-
    char.to_int(C, I),
    0x20 =< I, I =< 0x7e.

'WSP'(' ').
'WSP'('\t').

not_WSP(C) :-
    not 'WSP'(C).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
