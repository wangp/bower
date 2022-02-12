% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc5322.writer.
:- interface.

:- import_module bool.

:- type options
    --->    no_encoding
    ;       rfc2047_encoding
    ;       for_display.    % no quotes or escaping

:- pred address_list_to_string(options::in, address_list::in,
    string::out, bool::out) is det.

:- pred address_to_string(options::in, address::in, string::out, bool::out)
    is det.

:- pred mailbox_to_string(options::in, mailbox::in, string::out, bool::out)
    is det.

:- pred display_name_to_string(options::in, display_name::in, string::out,
    bool::out) is det.

:- pred addr_spec_to_string(addr_spec::in, string::out, bool::out) is det.

:- pred domain_to_message_id_right(domain::in, string::out) is semidet.

%-----------------------------------------------------------------------------%

    % Exported for rfc2045.
    %
:- pred quoted_string_ascii_only(quoted_string::in,
    list(string)::in, list(string)::out, bool::in, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

:- use_module rfc2047.
:- use_module rfc2047.encoder.

:- type allow_unicode
    --->    ascii_only
    ;       unicode_allowed.

:- type acc == list(string). % reverse

%-----------------------------------------------------------------------------%

address_list_to_string(Opt, Addresses, String, Ok) :-
    intersperse(address(Opt), ", ", Addresses, [], Acc, yes, Ok),
    String = to_string(Acc).

address_to_string(Opt, Address, String, Ok) :-
    address(Opt, Address, [], Acc, yes, Ok),
    String = to_string(Acc).

mailbox_to_string(Opt, Mailbox, String, Ok) :-
    mailbox(Opt, Mailbox, [], Acc, yes, Ok),
    String = to_string(Acc).

display_name_to_string(Opt, DisplayName, String, Ok) :-
    display_name(Opt, DisplayName, [], Acc, yes, Ok),
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

:- pred atom(allow_unicode::in, atom::in, acc::in, acc::out,
    bool::in, bool::out) is det.

atom(AllowUnicode, atom(Atom), !Acc, !Ok) :-
    (
        Atom = ascii(String)
    ;
        Atom = unicode(String),
        AllowUnicode = ascii_only,
        !:Ok = no
    ;
        Atom = unicode(String),
        AllowUnicode = unicode_allowed
        % In a context where we could have encoded the Unicode string.
    ),
    add(String, !Acc).

:- pred dot_atom(allow_unicode::in, dot_atom::in, acc::in, acc::out,
    bool::in, bool::out) is det.

dot_atom(AllowUnicode, dot_atom(Atom), !Acc, !Ok) :-
    atom(AllowUnicode, atom(Atom), !Acc, !Ok).

:- pred quoted_string(allow_unicode::in, quoted_string::in, acc::in, acc::out,
    bool::in, bool::out) is det.

quoted_string(AllowUnicode, quoted_string(QuotedString), !Acc, !Ok) :-
    RevChars0 = ['"'],
    (
        QuotedString = ascii(String)
    ;
        QuotedString = unicode(String),
        AllowUnicode = ascii_only,
        !:Ok = no
    ;
        QuotedString = unicode(String),
        AllowUnicode = unicode_allowed
        % In a context where we could have encoded the Unicode string.
    ),
    quoted_string_loop(String, 0, _, RevChars0, RevChars1),
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

quoted_string_ascii_only(QuotedString, !Acc, !Ok) :-
    quoted_string(ascii_only, QuotedString, !Acc, !Ok).

:- pred word(allow_unicode::in, word::in, acc::in, acc::out,
    bool::in, bool::out) is det.

word(AllowUnicode, Word, !Acc, !Ok) :-
    (
        Word = word_atom(Atom),
        atom(AllowUnicode, Atom, !Acc, !Ok)
    ;
        Word = word_quoted_string(QuotedString),
        quoted_string(AllowUnicode, QuotedString, !Acc, !Ok)
    ).

:- pred word_for_display(word::in, acc::in, acc::out, bool::in, bool::out)
    is det.

word_for_display(Word, !Acc, !Ok) :-
    (
        Word = word_atom(Atom),
        atom(unicode_allowed, Atom, !Acc, !Ok)
    ;
        Word = word_quoted_string(quoted_string(QuotedString)),
        (
            QuotedString = ascii(String)
        ;
            QuotedString = unicode(String)
        ),
        add(String, !Acc)
    ).

:- pred phrase(options::in, phrase::in, acc::in, acc::out,
    bool::in, bool::out) is det.

phrase(Opt, Words0, !Acc, !Ok) :-
    (
        (
            Opt = no_encoding,
            Words = Words0
        ;
            Opt = rfc2047_encoding,
            rfc2047.encoder.encode_phrase(Words0, Words)
        ),
        intersperse(word(unicode_allowed), " ", Words, !Acc, !Ok)
    ;
        Opt = for_display,
        intersperse(word_for_display, " ", Words0, !Acc, !Ok)
    ).

%-----------------------------------------------------------------------------%

:- pred address(options::in, address::in, acc::in, acc::out,
    bool::in, bool::out) is det.

address(Opt, mailbox(Mailbox), !Acc, !Ok) :-
    mailbox(Opt, Mailbox, !Acc, !Ok).
address(Opt, group(DisplayName, Mailboxes), !Acc, !Ok) :-
    display_name(Opt, DisplayName, !Acc, !Ok),
    add(": ", !Acc),
    intersperse(mailbox(Opt), ", ", Mailboxes, !Acc, !Ok),
    add(";", !Acc).

:- pred mailbox(options::in, mailbox::in, acc::in, acc::out,
    bool::in, bool::out) is det.

mailbox(Opt, Mailbox, !Acc, !Ok) :-
    (
        Mailbox = mailbox(yes(DisplayName), AddrSpec),
        display_name(Opt, DisplayName, !Acc, !Ok),
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

:- pred display_name(options::in, display_name::in, acc::in, acc::out,
    bool::in, bool::out) is det.

display_name(Opt, Phrase, !Acc, !Ok) :-
    phrase(Opt, Phrase, !Acc, !Ok).

:- pred addr_spec(addr_spec::in, acc::in, acc::out, bool::in, bool::out)
    is det.

addr_spec(addr_spec(LocalPart, Domain), !Acc, !Ok) :-
    local_part(LocalPart, !Acc, !Ok),
    add("@", !Acc),
    domain(Domain, !Acc, !Ok).

:- pred local_part(local_part::in, acc::in, acc::out, bool::in, bool::out)
    is det.

local_part(lpart_atom(Atom), !Acc, !Ok) :-
    dot_atom(ascii_only, Atom, !Acc, !Ok).
local_part(lpart_quoted_string(QuotedString), !Acc, !Ok) :-
    quoted_string(ascii_only, QuotedString, !Acc, !Ok).

:- pred domain(domain::in, acc::in, acc::out, bool::in, bool::out) is det.

domain(domain_name(Atom), !Acc, !Ok) :-
    dot_atom(ascii_only, Atom, !Acc, !Ok).
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

domain_to_message_id_right(Domain, MessageIdRight) :-
    % id-right is like domain but does not include CFWS or FWS.
    % Whitespace is stripped when we parse domain.
    domain(Domain, [], Acc, yes, Ok),
    Ok = yes, % ASCII only
    MessageIdRight = to_string(Acc).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
