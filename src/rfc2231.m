% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2231.
:- interface.

:- import_module rfc2045.

:- pred encode_parameter(parameter::in, parameter::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.
:- use_module require.

:- import_module rfc5234.
:- import_module rfc5322.
:- import_module string_util.

% See also RFC 5987 (the application to HTTP) which is a whole lot more
% comprehensible than RFC 2231.

%-----------------------------------------------------------------------------%

encode_parameter(Parameter0, Parameter) :-
    Parameter0 = Attribute0 - Value0,
    (
        Value0 = token(_),
        Parameter = Parameter0
    ;
        Value0 = quoted_string(quoted_string(ascii(_))),
        Parameter = Parameter0
    ;
        Value0 = quoted_string(quoted_string(unicode(String0))),
        % We don't break up long values into multiple sections.  We only use
        % this to encode the filename parameter in Content-Disposition bodies.
        Attribute = Attribute0 ++ "*",
        encode_string(String0, String1),
        Token = token("UTF-8''" ++ String1),
        Parameter = Attribute - token(Token)
    ).

:- func attribute ++ string = attribute.

attribute(Attr) ++ Section = attribute(Attr ++ Section).

:- pred encode_string(string::in, string::out) is det.

encode_string(String, TokenString) :-
    foldr_code_units(encode_octet, String, [], Chars),
    string.from_char_list(Chars, TokenString).

:- pred encode_octet(int::in, list(char)::in, list(char)::out) is det.

encode_octet(Octet, TokenChars0, TokenChars) :-
    (
        Octet =< 0x7f,
        char.from_int(Octet, Char),
        unencoded_value_chars(Char)
    ->
        TokenChars = [Char | TokenChars0]
    ;
        Hi = (Octet /\ 0xf0) >> 4,
        Lo = (Octet /\ 0x0f),
        (
            char.int_to_hex_char(Hi, HiChar),
            char.int_to_hex_char(Lo, LoChar)
        ->
            TokenChars = ['%', HiChar, LoChar | TokenChars0]
        ;
            require.unexpected($module, $pred, "char.int_to_hex_char failed")
        )
    ).

    % RFC 5987 explicitly lists the characters that can appear without percent
    % encoding.  We add '{' and '}' from the RFC 2045 token production, which
    % are excluded from RFC 2616.
    %
:- pred unencoded_value_chars(char::in) is semidet.

unencoded_value_chars(C) :-
    ( 'ALPHA'(C)
    ; 'DIGIT'(C)
    ; C = ('!')
    ; C = ('#')
    ; C = ('$')
    ; C = ('&')
    ; C = ('+')
    ; C = ('-')
    ; C = ('.')
    ; C = ('^')
    ; C = ('_')
    ; C = ('`')
    ; C = ('|')
    ; C = ('~')
    ; C = ('{')
    ; C = ('}')
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
