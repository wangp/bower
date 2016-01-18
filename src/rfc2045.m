% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2045.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module pair.

:- import_module rfc5322.

:- type token
    --->    token(string).

:- type parameter == pair(attribute, value).

:- type attribute
    --->    attribute(string). % case-insensitive; keep in lowercase

:- type value
    --->    token(token)
    ;       quoted_string(quoted_string).

:- pred token_char(char::in) is semidet.

:- pred parameter_to_string(parameter::in, string::out, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- import_module rfc5322.writer.

:- type acc == list(string). % reverse

%-----------------------------------------------------------------------------%

token_char(Char) :-
    char.to_int(Char, Int),
    % Exclude SPACE, CTL (0-31 and DEL), and non-ASCII.
    0x20 < Int, Int < 0x7f,
    not tspecial(Char).

:- pred tspecial(char::in) is semidet.

tspecial('('). tspecial(')'). tspecial('<'). tspecial('>'). tspecial('@').
tspecial(','). tspecial(';'). tspecial(':'). tspecial('\\'). tspecial('"').
tspecial('/'). tspecial('['). tspecial(']'). tspecial('?'). tspecial('=').

%-----------------------------------------------------------------------------%

parameter_to_string(Attr - Value, String, !:Ok) :-
    some [!Acc] (
        !:Acc = [],
        !:Ok = yes,
        attribute(Attr, !Acc, !Ok),
        cons("=", !Acc),
        value(Value, !Acc, !Ok),
        list.reverse(!.Acc, Strings),
        string.append_list(Strings, String)
    ).

:- pred token(token::in, acc::in, acc::out, bool::in, bool::out) is det.

token(token(String), !Acc, !Ok) :-
    cons(String, !Acc).

:- pred attribute(attribute::in, acc::in, acc::out, bool::in, bool::out)
    is det.

attribute(attribute(String), !Acc, !Ok) :-
    token(token(String), !Acc, !Ok).

:- pred value(value::in, acc::in, acc::out, bool::in, bool::out) is det.

value(token(Token), !Acc, !Ok) :-
    token(Token, !Acc, !Ok).
value(quoted_string(QuotedString), !Acc, !Ok) :-
    quoted_string_ascii_only(QuotedString, !Acc, !Ok).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
