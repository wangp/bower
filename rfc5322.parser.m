% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc5322.parser.
:- interface.

:- pred parse_address_list(string::in, address_list::out) is det.

:- pred parse_address(string::in, address::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module parsing_utils.
:- import_module string.
:- import_module unit.

:- use_module rfc2047.
:- use_module rfc2047.decoder.

%-----------------------------------------------------------------------------%

parse_address_list(Input, Addresses) :-
    Stripped = strip(Input),
    ( Stripped = "" ->
        Addresses = []
    ;
        % All failures are equivalent.
        promise_equivalent_solutions [ParseResult] (
            parsing_utils.parse(Stripped, no_skip_whitespace,
                eof_after(obs_address_list), ParseResult)
        ),
        (
            ParseResult = ok(Addresses)
        ;
            ParseResult = error(_, _, _),
            Addresses = [mailbox(bad_mailbox(Stripped))]
        )
    ).

parse_address(Input, Address) :-
    Stripped = strip(Input),
    Stripped \= "",
    % All failures are equivalent.
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, no_skip_whitespace, eof_after(address),
            ParseResult)
    ),
    ParseResult = ok(Address).

%-----------------------------------------------------------------------------%

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

:- pred eof_after(parser(T)::in(parser), src::in, T::out, ps::in, ps::out)
    is semidet.

eof_after(P, Src, X, !PS) :-
    P(Src, X, !PS),
    eof(Src, _, !PS).

:- pred while0(pred(char, T, T), src, T, T, ps, ps).
:- mode while0(in(pred(in, in, out) is semidet), in, in, out, in, out) is det.

while0(P, Src, !State, !PS) :-
    (
        next_char(Src, C, !PS),
        P(C, !State)
    ->
        while0(P, Src, !State, !PS)
    ;
        true
    ).

:- pred while1(pred(char, T, T), src, T, T, ps, ps).
:- mode while1(in(pred(in, in, out) is semidet), in, in, out, in, out)
    is semidet.

while1(P, Src, !State, !PS) :-
    current_offset(Src, Start, !PS),
    while0(P, Src, !State, !PS),
    current_offset(Src, End, !PS),
    End > Start.

:- pred one_or_more_chars(pred(char, T, T), src, string, T, T, ps, ps).
:- mode one_or_more_chars(in(pred(in, in, out) is semidet), in, out, in, out,
    in, out) is semidet.

one_or_more_chars(P, Src, String, !State, !PS) :-
    current_offset(Src, Start, !PS),
    while1(P, Src, !State, !PS),
    current_offset(Src, End, !PS),
    input_substring(Src, Start, End, String).

:- pred separated_list_skip_nulls(string::in, parser(maybe(T))::in(parser),
    src::in, list(T)::out, ps::in, ps::out) is semidet.

separated_list_skip_nulls(Sep, P, Src, Xs, !PS) :-
    separated_list_skip_nulls_2(Sep, P, Src, [], RevXs, !PS),
    list.reverse(RevXs, Xs).

:- pred separated_list_skip_nulls_2(string::in, parser(maybe(T))::in(parser),
    src::in, list(T)::in, list(T)::out, ps::in, ps::out) is semidet.

separated_list_skip_nulls_2(Sep, P, Src, !RevXs, !PS) :-
    ( P(Src, MaybeX, !PS) ->
        (
            MaybeX = yes(X),
            cons(X, !RevXs)
        ;
            MaybeX = no
        ),
        ( punct(Sep, Src, _, !PS) ->
            separated_list_skip_nulls_2(Sep, P, Src, !RevXs, !PS)
        ;
            true
        )
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

% 3.1. Syntax

% ALPHA
% DIGIT
% WSP

:- pred 'VCHAR'(char::in) is semidet.

'VCHAR'(C) :-
    char.to_int(C, I),
    0x21 =< I, I =< 0x7e.

:- pred 'DQUOTE'(src::in, ps::in, ps::out) is semidet.

'DQUOTE'(Src, !PS) :-
    next_char(Src, '"', !PS).

%-----------------------------------------------------------------------------%

% 3.2.1. Quoted characters

:- pred quoted_pair_tail(src::in, char::out, bool::in, bool::out,
    ps::in, ps::out) is semidet.

quoted_pair_tail(Src, Char, !AllAscii, !PS) :-
    next_char(Src, Char, !PS),
    ( 'VCHAR'(Char) ->
        true
    ; 'WSP'(Char) ->
        true
    ;
        nonascii(Char),
        !:AllAscii = no
    ).

%-----------------------------------------------------------------------------%

% 3.2.2. Folding white space and comments

:- pred skip_FWS(src::in, ps::in, ps::out) is det.

skip_FWS(Src, !PS) :-
    % We assume unfolding has already occurred.
    skip_WSP_chars(Src, !PS).

:- pred skip_WSP_chars(src::in, ps::in, ps::out) is det.

skip_WSP_chars(Src, !PS) :-
    (
        next_char(Src, C, !PS),
        'WSP'(C)
    ->
        skip_WSP_chars(Src, !PS)
    ;
        true
    ).

:- pred skip_CFWS(src::in, ps::in, ps::out) is det.

skip_CFWS(Src, !PS) :-
    skip_FWS(Src, !PS),
    ( skip_comment(Src, !PS) ->
        skip_CFWS(Src, !PS)
    ;
        true
    ).

:- pred skip_comment(src::in, ps::in, ps::out) is semidet.

skip_comment(Src, !PS) :-
    next_char(Src, '(', !PS),
    comment_tail(Src, !PS).

:- pred comment_tail(src::in, ps::in, ps::out) is semidet.

comment_tail(Src, !PS) :-
    next_char(Src, C, !PS),
    ( C = (')') ->
        % End of comment.
        true
    ;
        ( C = ('\\') ->
            quoted_pair_tail(Src, _, yes, _AllAscii, !PS)
        ; C = ('(') ->
            % Nested comment.
            comment_tail(Src, !PS)
        ;
            'WSP'(C)
        ;
            % ctext, and we will accept non-ASCII.
            char.to_int(C, I),
            I >= 33
        ),
        comment_tail(Src, !PS)
    ).

%-----------------------------------------------------------------------------%

% 3.2.3. Atom

:- type atom_opt
    --->    normal
    ;       allow_dot_for_obs_phrase.

% atext
% atext_or_nonascii

:- pred atext_or_dot_or_nonascii(char::in, bool::in, bool::out) is semidet.

atext_or_dot_or_nonascii(C, !AllAscii) :-
    ( C = ('.') ->
        true
    ;
        atext_or_nonascii(C, !AllAscii)
    ).

:- pred atom(atom_opt::in, src::in, atom::out, ps::in, ps::out)
    is semidet.

atom(Opt, Src, atom(Atom), !PS) :-
    skip_CFWS(Src, !PS),
    (
        Opt = normal,
        Pred = atext_or_nonascii
    ;
        Opt = allow_dot_for_obs_phrase,
        Pred = atext_or_dot_or_nonascii
    ),
    one_or_more_chars(Pred, Src, String, yes, AllAscii, !PS),
    skip_CFWS(Src, !PS),
    ascii_unicode(AllAscii, String, Atom).

:- pred dot_atom(src::in, dot_atom::out, ps::in, ps::out) is semidet.

dot_atom(Src, dot_atom(Atom), !PS) :-
    skip_CFWS(Src, !PS),
    dot_atom_text(Src, String, yes, AllAscii, !PS),
    ascii_unicode(AllAscii, String, Atom),
    skip_CFWS(Src, !PS).

:- pred dot_atom_text(src::in, string::out, bool::in, bool::out,
    ps::in, ps::out) is semidet.

dot_atom_text(Src, String, !AllAscii, !PS) :-
    current_offset(Src, Start, !PS),
    while1(atext_or_nonascii, Src, !AllAscii, !PS),
    dot_atom_tail(Src, !AllAscii, !PS),
    current_offset(Src, End, !PS),
    input_substring(Src, Start, End, String).

:- pred dot_atom_tail(src::in, bool::in, bool::out, ps::in, ps::out)
    is semidet.

dot_atom_tail(Src, !AllAscii, !PS) :-
    (
        next_char(Src, '.', !PS),
        while1(atext_or_nonascii, Src, !AllAscii, !PS)
    ->
        dot_atom_tail(Src, !AllAscii, !PS)
    ;
        true
    ).

:- pred ascii_unicode(bool::in, string::in, ascii_unicode::out) is det.

ascii_unicode(AllAscii, String, Wrap) :-
    (
        AllAscii = yes,
        Wrap = ascii(String)
    ;
        AllAscii = no,
        Wrap = unicode(String)
    ).

%-----------------------------------------------------------------------------%

% 3.2.4. Quoted strings

:- pred quoted_string(src::in, quoted_string::out, ps::in, ps::out) is semidet.

quoted_string(Src, quoted_string(QuotedString), !PS) :-
    skip_CFWS(Src, !PS),
    'DQUOTE'(Src, !PS),
    zero_or_more(quoted_string_body, Src, Chars, yes, AllAscii, !PS),
    'DQUOTE'(Src, !PS),
    skip_CFWS(Src, !PS),
    string.from_char_list(Chars, String),
    ascii_unicode(AllAscii, String, QuotedString).

:- pred quoted_string_body(src::in, char::out, bool::in, bool::out,
    ps::in, ps::out) is semidet.

quoted_string_body(Src, Char, !AllAscii, !PS) :-
    % We assume unfolding has already occurred.
    next_char(Src, Char0, !PS),
    ( Char0 = ('\\') ->
        quoted_pair_tail(Src, Char, !AllAscii, !PS)
    ; 'WSP'(Char0) ->
        Char = Char0
    ;
        qtext_extended(Char0, !AllAscii),
        Char = Char0
    ).

:- pred qtext_extended(char::in, bool::in, bool::out) is semidet.

qtext_extended(C, !AllAscii) :-
    ( nonascii(C) ->
        !:AllAscii = no
    ;
        char.to_int(C, I),
        ( I = 33
        ; 35 =< I, I =< 91
        ; 93 =< I, I =< 126
        )
        % or obs-qtext
    ).

%-----------------------------------------------------------------------------%

% 3.2.5. Miscellaneous tokens

:- pred word(atom_opt::in, src::in, word::out, ps::in, ps::out) is semidet.

word(Opt, Src, Word, !PS) :-
    ( atom(Opt, Src, Atom, !PS) ->
        Word = word_atom(Atom)
    ;
        quoted_string(Src, QuotedString, !PS),
        Word = word_quoted_string(QuotedString)
    ).

%-----------------------------------------------------------------------------%

% 3.4. Address Specification

:- pred address(src::in, address::out, ps::in, ps::out) is semidet.

address(Src, Address, !PS) :-
    ( mailbox(Src, Mailbox, !PS) ->
        Address = mailbox(Mailbox)
    ;
        group(Src, Address, !PS)
    ).

:- pred mailbox(src::in, mailbox::out, ps::in, ps::out) is semidet.

mailbox(Src, Mailbox, !PS) :-
    ( name_addr(Src, MailboxPrime, !PS) ->
        Mailbox = MailboxPrime
    ;
        addr_spec(Src, AddrSpec, !PS),
        Mailbox = mailbox(no, AddrSpec)
    ).

:- pred name_addr(src::in, mailbox::out, ps::in, ps::out) is semidet.

name_addr(Src, Mailbox, !PS) :-
    optional(display_name, Src, MaybeDisplayName, !PS),
    angle_addr(Src, AddrSpec, !PS),
    Mailbox = mailbox(MaybeDisplayName, AddrSpec).

:- pred angle_addr(src::in, addr_spec::out, ps::in, ps::out) is semidet.

angle_addr(Src, AddrSpec, !PS) :-
    skip_CFWS(Src, !PS),
    next_char(Src, '<', !PS),
    addr_spec(Src, AddrSpec, !PS),
    next_char(Src, '>', !PS),
    skip_CFWS(Src, !PS).
    % or obs-angle-addr

:- pred group(src::in, address::out, ps::in, ps::out) is semidet.

group(Src, group(DisplayName, Mailboxes), !PS) :-
    display_name(Src, DisplayName, !PS),
    next_char(Src, ':', !PS),
    obs_group_list(Src, Mailboxes, !PS),
    ( next_char(Src, ';', !PS) ->
        true
    ;
        % Syntax error.
        true
    ),
    skip_CFWS(Src, !PS).

:- pred display_name(src::in, display_name::out, ps::in, ps::out) is semidet.

display_name(Src, DisplayName, !PS) :-
    display_name_2(Src, Phrase, !PS),
    rfc2047.decoder.decode_phrase(Phrase, DisplayName).

:- pred display_name_2(src::in, display_name::out, ps::in, ps::out) is semidet.

display_name_2(Src, DisplayName, !PS) :-
    word(allow_dot_for_obs_phrase, Src, FirstWord, !PS),
    zero_or_more(word(allow_dot_for_obs_phrase), Src, RestWords, !PS),
    Words = [FirstWord | RestWords],
    (
        list.member(Word, Words),
        obsolete_word(Word)
    ->
        DisplayName = [word_quoted_string(quoted_string_from_words(Words))]
    ;
        DisplayName = Words
    ).

:- pred obs_group_list(src::in, list(mailbox)::out, ps::in, ps::out)
    is semidet.

obs_group_list(Src, Mailboxes, !PS) :-
    separated_list_skip_nulls(",", mailbox_or_null, Src, Mailboxes, !PS).

:- pred mailbox_or_null(src::in, maybe(mailbox)::out, ps::in, ps::out)
    is semidet.

mailbox_or_null(Src, MaybeMailbox, !PS) :-
    skip_CFWS(Src, !PS),
    ( next_char(Src, (','), !.PS, _) ->
        % Ignore null mailbox (obsolete).
        MaybeMailbox = no
    ; mailbox_or_bad_mailbox(Src, Mailbox, !PS) ->
        MaybeMailbox = yes(Mailbox)
    ;
        MaybeMailbox = no,
        semidet_true
    ).

:- pred obs_address_list(src::in, list(address)::out, ps::in, ps::out)
    is semidet.

obs_address_list(Src, Addresses, !PS) :-
    separated_list_skip_nulls(",", address_or_null, Src, Addresses, !PS),
    Addresses = [_ | _].

:- pred address_or_null(src::in, maybe(address)::out, ps::in, ps::out)
    is semidet.

address_or_null(Src, MaybeAddress, !PS) :-
    skip_CFWS(Src, !PS),
    ( next_char(Src, (','), !.PS, _) ->
        % Ignore null address (obsolete).
        MaybeAddress = no
    ; address_or_bad_address(Src, Address, !PS) ->
        MaybeAddress = yes(Address)
    ;
        MaybeAddress = no,
        semidet_true
    ).

%-----------------------------------------------------------------------------%

% 3.4.1. Addr-spec specification

:- pred addr_spec(src::in, addr_spec::out, ps::in, ps::out) is semidet.

addr_spec(Src, addr_spec(LocalPart, Domain), !PS) :-
    skip_CFWS(Src, !PS),
    local_part(Src, LocalPart, !PS),
    next_char(Src, '@', !PS),
    domain(Src, Domain, !PS).

:- pred local_part(src::in, local_part::out, ps::in, ps::out) is semidet.

local_part(Src, LocalPart, !PS) :-
    ( dot_atom(Src, Atom, !PS) ->
        LocalPart = lpart_atom(Atom)
    ;
        quoted_string(Src, QuotedString, !PS),
        LocalPart = lpart_quoted_string(QuotedString)
    ).
    % or obs-local-part

:- pred domain(src::in, domain::out, ps::in, ps::out) is semidet.

domain(Src, Domain, !PS) :-
    ( dot_atom(Src, Atom, !PS) ->
        Domain = domain_name(Atom)
    ;
        domain_literal(Src, Domain, !PS)
    ).
    % or obs-domain

:- pred domain_literal(src::in, domain::out, ps::in, ps::out) is semidet.

domain_literal(Src, domain_literal(Literal), !PS) :-
    skip_CFWS(Src, !PS),
    next_char(Src, '[', !PS),

    current_offset(Src, Start, !PS),
    domain_literal_body(Src, yes, AllAscii, !PS),
    current_offset(Src, End, !PS),

    next_char(Src, ']', !PS),
    skip_CFWS(Src, !PS),

    input_substring(Src, Start, End, String),
    ascii_unicode(AllAscii, String, Literal).

:- pred domain_literal_body(src::in, bool::in, bool::out, ps::in, ps::out)
    is semidet.

domain_literal_body(Src, !AllAscii, !PS) :-
    skip_FWS(Src, !PS),
    ( while1(dtext_or_nonascii, Src, !AllAscii, !PS) ->
        domain_literal_body(Src, !AllAscii, !PS)
    ;
        true
    ).

:- pred dtext_or_nonascii(char::in, bool::in, bool::out) is semidet.

dtext_or_nonascii(C, !AllAscii) :-
    ( dtext(C) ->
        true
    ;
        % Just for consistency...
        nonascii(C),
        !:AllAscii = no
    ).

:- pred dtext(char::in) is semidet.

dtext(C) :-
    char.to_int(C, I),
    ( 33 =< I, I =< 90
    ; 94 =< I, I =< 126
    % or obs-dtext (but update writer then)
    ).

%-----------------------------------------------------------------------------%

% 4.1. Miscellaneous Obsolete Tokens

:- pred obsolete_word(word::in) is semidet.

obsolete_word(word_atom(atom(Atom))) :-
    ( Atom = ascii(String)
    ; Atom = unicode(String)
    ),
    string.sub_string_search(String, ".", _).

:- func quoted_string_from_words(list(word)) = quoted_string.

quoted_string_from_words(Words) = quoted_string(Wrap) :-
    String = string.join_list(" ", list.map(word_to_string, Words)),
    ( string.all_match(ascii, String) ->
        Wrap = ascii(String)
    ;
        Wrap = unicode(String)
    ).

%-----------------------------------------------------------------------------%

:- pred address_or_bad_address(src::in, address::out, ps::in, ps::out)
    is semidet.

address_or_bad_address(Src, Address, !PS) :-
    ( address(Src, AddressPrime, !PS) ->
        Address = AddressPrime
    ;
        bad_address(Src, Address, !PS)
    ).

:- pred bad_address(src::in, address::out, ps::in, ps::out) is semidet.

bad_address(Src, Address, !PS) :-
    bad_mailbox(Src, Mailbox, !PS),
    Address = mailbox(Mailbox).

:- pred mailbox_or_bad_mailbox(src::in, mailbox::out, ps::in, ps::out)
    is semidet.

mailbox_or_bad_mailbox(Src, Mailbox, !PS) :-
    ( mailbox(Src, MailboxPrime, !PS) ->
        Mailbox = MailboxPrime
    ;
        bad_mailbox(Src, Mailbox, !PS)
    ).

:- pred bad_mailbox(src::in, mailbox::out, ps::in, ps::out) is semidet.

bad_mailbox(Src, bad_mailbox(String), !PS) :-
    skip_FWS(Src, !PS),
    one_or_more_chars(bad_mailbox_char, Src, String0, unit, unit, !PS),
    String = string.strip(String0).

:- pred bad_mailbox_char(char::in, unit::in, unit::out) is semidet.

bad_mailbox_char(C, unit, unit) :-
    C \= (','),
    C \= (';').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
