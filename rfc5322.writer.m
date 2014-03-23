% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc5322.writer.
:- interface.

:- import_module bool.

:- pred address_list_to_string(address_list::in, string::out, bool::out)
    is det.

:- pred address_to_string(address::in, string::out, bool::out) is det.

:- pred mailbox_to_string(mailbox::in, string::out, bool::out) is det.

:- pred display_name_to_string(display_name::in, string::out, bool::out)
    is det.

:- pred addr_spec_to_string(addr_spec::in, string::out, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

:- type acc == list(string). % reverse

%-----------------------------------------------------------------------------%

address_list_to_string(Addresses, String, Ok) :-
    intersperse(address, ", ", Addresses, [], Acc, yes, Ok),
    String = to_string(Acc).

address_to_string(Address, String, Ok) :-
    address(Address, [], Acc, yes, Ok),
    String = to_string(Acc).

mailbox_to_string(Mailbox, String, Ok) :-
    mailbox(Mailbox, [], Acc, yes, Ok),
    String = to_string(Acc).

display_name_to_string(DisplayName, String, Ok) :-
    display_name(DisplayName, [], Acc, yes, Ok),
    String = to_string(Acc).

addr_spec_to_string(AddrSpec, String, Ok) :-
    addr_spec(AddrSpec, [], Acc, yes, Ok),
    String = to_string(Acc).

%-----------------------------------------------------------------------------%

:- pred add(string::in, acc::in, acc::out) is det.

add(X, L, [X | L]).

:- func to_string(acc) = string.

to_string(L) = string.append_list(list.reverse(L)).

:- pred intersperse(pred(T, acc, acc, bool, bool), string, list(T),
    acc, acc, bool, bool).
:- mode intersperse(in(pred(in, in, out, in, out) is det), in, in,
    in, out, in, out) is det.

intersperse(_P, _Sep, [], !Acc, !Ok).
intersperse(P, Sep, [X | Xs], !Acc, !Ok) :-
    P(X, !Acc, !Ok),
    (
        Xs = []
    ;
        Xs = [_ | _],
        add(Sep, !Acc),
        intersperse(P, Sep, Xs, !Acc, !Ok)
    ).

%-----------------------------------------------------------------------------%

:- pred ascii_unicode(ascii_unicode::in, acc::in, acc::out,
    bool::in, bool::out) is det.

ascii_unicode(ascii(String), !Acc, !Ok) :-
    add(String, !Acc).
ascii_unicode(unicode(String), !Acc, _Ok0, Ok) :-
    % In certain contexts we can encode to ASCII.
    add(String, !Acc),
    Ok = no.

:- pred atom(atom::in, acc::in, acc::out, bool::in, bool::out) is det.

atom(atom(Atom), !Acc, !Ok) :-
    ascii_unicode(Atom, !Acc, !Ok).

:- pred dot_atom(dot_atom::in, acc::in, acc::out, bool::in, bool::out) is det.

dot_atom(dot_atom(Atom), !Acc, !Ok) :-
    ascii_unicode(Atom, !Acc, !Ok).

:- pred quoted_string(quoted_string::in, acc::in, acc::out,
    bool::in, bool::out) is det.

quoted_string(quoted_string(QuotedString), !Acc, !Ok) :-
    RevChars0 = ['"'],
    (
        QuotedString = ascii(String),
        quoted_string_loop(String, 0, _, RevChars0, RevChars1)
    ;
        QuotedString = unicode(String),
        % Requires encoding.
        quoted_string_loop(String, 0, _, RevChars0, RevChars1),
        !:Ok = no
    ),
    RevChars = ['"' | RevChars1],
    string.from_rev_char_list(RevChars, EscString),
    add(EscString, !Acc).

:- pred quoted_string_loop(string::in, int::in, int::out,
    list(char)::in, list(char)::out) is det.

quoted_string_loop(String, !Pos, !RevChars) :-
    ( string.unsafe_index_next(String, !Pos, C) ->
        (
            ( C = ('\\')
            ; C = ('"')
            )
        ->
            !:RevChars = [C, '\\' | !.RevChars]
        ;
            !:RevChars = [C | !.RevChars]
        ),
        quoted_string_loop(String, !Pos, !RevChars)
    ;
        % End of string.
        true
    ).

:- pred word(word::in, acc::in, acc::out, bool::in, bool::out) is det.

word(word_atom(Atom), !Acc, !Ok) :-
    atom(Atom, !Acc, !Ok).
word(word_quoted_string(QuotedString), !Acc, !Ok) :-
    quoted_string(QuotedString, !Acc, !Ok).

:- pred phrase(display_name::in, acc::in, acc::out, bool::in, bool::out)
    is det.

phrase(Words, !Acc, !Ok) :-
    intersperse(word, " ", Words, !Acc, !Ok).

%-----------------------------------------------------------------------------%

:- pred address(address::in, acc::in, acc::out, bool::in, bool::out) is det.

address(mailbox(Mailbox), !Acc, !Ok) :-
    mailbox(Mailbox, !Acc, !Ok).
address(group(DisplayName, Mailboxes), !Acc, !Ok) :-
    display_name(DisplayName, !Acc, !Ok),
    add(": ", !Acc),
    intersperse(mailbox, ", ", Mailboxes, !Acc, !Ok),
    add(";", !Acc).

:- pred mailbox(mailbox::in, acc::in, acc::out, bool::in, bool::out) is det.

mailbox(Mailbox, !Acc, !Ok) :-
    (
        Mailbox = mailbox(yes(DisplayName), AddrSpec),
        display_name(DisplayName, !Acc, !Ok),
        add(" <", !Acc),
        addr_spec(AddrSpec, !Acc, !Ok),
        add(">", !Acc)
    ;
        Mailbox = mailbox(no, AddrSpec),
        addr_spec(AddrSpec, !Acc, !Ok)
    ;
        Mailbox = bad_mailbox(String),
        add(String, !Acc),
        !:Ok = no
    ).

:- pred display_name(display_name::in, acc::in, acc::out, bool::in, bool::out)
    is det.

display_name(Phrase, !Acc, !Ok) :-
    phrase(Phrase, !Acc, !Ok).

:- pred addr_spec(addr_spec::in, acc::in, acc::out, bool::in, bool::out)
    is det.

addr_spec(addr_spec(LocalPart, Domain), !Acc, !Ok) :-
    local_part(LocalPart, !Acc, !Ok),
    add("@", !Acc),
    domain(Domain, !Acc, !Ok).

:- pred local_part(local_part::in, acc::in, acc::out, bool::in, bool::out)
    is det.

local_part(lpart_atom(Atom), !Acc, !Ok) :-
    dot_atom(Atom, !Acc, !Ok).
local_part(lpart_quoted_string(QuotedString), !Acc, !Ok) :-
    quoted_string(QuotedString, !Acc, !Ok).

:- pred domain(domain::in, acc::in, acc::out, bool::in, bool::out) is det.

domain(domain_name(Atom), !Acc, !Ok) :-
    dot_atom(Atom, !Acc, !Ok).
domain(domain_literal(Literal), !Acc, !Ok) :-
    add("[", !Acc),
    (
        Literal = ascii(String),
        % Escaping not required unless obs-dtext accepted.
        add(String, !Acc)
    ;
        Literal = unicode(String),
        add(String, !Acc),
        !:Ok = no
    ),
    add("]", !Acc).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
