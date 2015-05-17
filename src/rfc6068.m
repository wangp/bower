% Bower - a frontend for the Notmuch email system
% Copyright (C) 2015 Peter Wang

:- module rfc6068.
:- interface.

:- import_module assoc_list.

%-----------------------------------------------------------------------------%

:- type hfname == string.   % case-insensitive

:- type hfvalue == string.  % case-sensitive
                            % May contain CR LFs or RFC 2047 encoded words.

:- pred is_mailto_uri(string::in) is semidet.

:- pred parse_mailto_uri(string::in, assoc_list(hfname, hfvalue)::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module parsing_utils.
:- import_module string.
:- import_module unit.

:- import_module rfc3986.
:- import_module rfc5234.
:- import_module rfc5322.
:- import_module rfc5322.writer.

%-----------------------------------------------------------------------------%

is_mailto_uri(Input) :-
    string.prefix(Input, "mailto:").

parse_mailto_uri(Input, Hfields) :-
    rfc3986.split_uri(Input, URI),
    URI = uri_components(Scheme, Authority, Path, MaybeQuery, _Fragment),
    Scheme = yes("mailto"),
    Authority = no,

    promise_equivalent_solutions [Result0] (
        parsing_utils.parse(Path, no_skip_whitespace, maybe_to, Result0)
    ),
    Result0 = ok(MaybeTo),

    (
        MaybeQuery = yes(Query),
        promise_equivalent_solutions [Result1] (
            parsing_utils.parse(Query, no_skip_whitespace, hfields, Result1)
        ),
        Result1 = ok(Hfields0)
    ;
        MaybeQuery = no,
        Hfields0 = []
    ),

    (
        MaybeTo = yes(To),
        Hfields = ["To" - To | Hfields0]
    ;
        MaybeTo = no,
        Hfields = Hfields0
    ).

%-----------------------------------------------------------------------------%

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

%-----------------------------------------------------------------------------%

:- pred maybe_to(src::in, maybe(hfvalue)::out, ps::in, ps::out) is semidet.

maybe_to(Src, Res, !PS) :-
    optional(to, Src, Res, !PS),
    eof(Src, _, !PS).

:- pred to(src::in, hfvalue::out, ps::in, ps::out) is semidet.

to(Src, Value, !PS) :-
    comma_separated_list(addr_spec, Src, AddrSpecs, !PS),
    AddrSpecs = [_ | _],
    map(addr_spec_to_string, AddrSpecs, Strings),
    Value = join_list(", ", Strings).

:- pred addr_spec_to_string(addr_spec::in, string::out) is det.

addr_spec_to_string(AddrSpec, String) :-
    rfc5322.writer.addr_spec_to_string(AddrSpec, String, _Ok).

%-----------------------------------------------------------------------------%

:- pred hfields(src::in, assoc_list(hfname, hfvalue)::out, ps::in, ps::out)
    is semidet.

hfields(Src, Hfields, !PS) :-
    separated_list("&", hfield, Src, Hfields, !PS),
    eof(Src, _, !PS).

:- pred hfield(src::in, pair(hfname, hfvalue)::out, ps::in, ps::out)
    is semidet.

hfield(Src, Name - Value, !PS) :-
    zero_or_more(qchar, Src, Name0, !PS),
    next_char(Src, '=', !PS),
    zero_or_more(qchar, Src, Value0, !PS),
    percent_decode(Name0, Name),
    percent_decode(Value0, Value).

:- pred qchar(src::in, char::out, ps::in, ps::out) is semidet.

qchar(Src, C, !PS) :-
    next_char(Src, C, !PS),
    ( rfc3986.unreserved_char(C)
    ; C = ('%')
    ; some_delims(C)
    ).

:- pred some_delims(char::in) is semidet.

some_delims(C) :-
    ( C = ('!')
    ; C = ('$')
    ; C = ('''')
    ; C = ('(')
    ; C = (')')
    ; C = ('*')
    ; C = ('+')
    ; C = (',')
    ; C = (';')
    ; C = (':')
    ; C = ('@')
    ).

%-----------------------------------------------------------------------------%

:- pred percent_decode(list(char)::in, string::out) is semidet.

percent_decode(Chars, String) :-
    percent_decode_2(Chars, [], RevOctets),
    list.reverse(RevOctets, Octets),
    % Newer Mercury has string.from_utf8_code_unit_list.
    string.from_code_unit_list(Octets, String).

:- pred percent_decode_2(list(char)::in, list(int)::in, list(int)::out)
    is semidet.

percent_decode_2([], !RevOctets).
percent_decode_2([C | Cs], !RevOctets) :-
    ( C = ('%') ->
        Cs = [Hi, Lo | Rest],
        char.is_hex_digit(Hi, Int_Hi),
        char.is_hex_digit(Lo, Int_Lo),
        Octet = (Int_Hi << 4) \/ Int_Lo
    ;
        char.to_int(C, Octet),
        Octet =< 0x7f,
        Rest = Cs
    ),
    cons(Octet, !RevOctets),
    percent_decode_2(Rest, !RevOctets).

:- pred percent_decode_ascii_unicode(list(char)::in, ascii_unicode::out)
    is semidet.

percent_decode_ascii_unicode(Chars, Wrap) :-
    percent_decode(Chars, String),
    ( string.all_match(rfc5322.ascii, String) ->
        Wrap = rfc5322.ascii(String)
    ;
        Wrap = rfc5322.unicode(String)
    ).

%-----------------------------------------------------------------------------%

% The rest is mostly modified from rfc5322.m

%-----------------------------------------------------------------------------%

:- pred rfc6068.atext(src::in, char::out, ps::in, ps::out) is semidet.

rfc6068.atext(Src, C, !PS) :-
    next_char(Src, C, !PS),
    rfc5322.atext(C).

:- pred rfc6068.dot_atom(src::in, dot_atom::out, ps::in, ps::out) is semidet.

rfc6068.dot_atom(Src, dot_atom(Atom), !PS) :-
    rfc6068.dot_atom_text(Src, Chars, !PS),
    percent_decode_ascii_unicode(Chars, Atom).

:- pred rfc6068.dot_atom_text(src::in, list(char)::out, ps::in, ps::out)
    is semidet.

rfc6068.dot_atom_text(Src, Head ++ Tail, !PS) :-
    one_or_more(rfc6068.atext, Src, Head, !PS),
    rfc6068.dot_atom_tail(Src, Tail, !PS).

:- pred rfc6068.dot_atom_tail(src::in, list(char)::out, ps::in, ps::out)
    is semidet.

rfc6068.dot_atom_tail(Src, Chars, !PS) :-
    (
        next_char(Src, '.', !PS),
        one_or_more(rfc6068.atext, Src, Head, !PS)
    ->
        rfc6068.dot_atom_tail(Src, Tail, !PS),
        Chars = ['.' | Head ++ Tail]
    ;
        Chars = []
    ).

%-----------------------------------------------------------------------------%

:- pred rfc6068.quoted_string(src::in, quoted_string::out, ps::in, ps::out)
    is semidet.

rfc6068.quoted_string(Src, quoted_string(QuotedString), !PS) :-
    next_char(Src, 'DQUOTE', !PS),
    zero_or_more(rfc6068.quoted_string_body, Src, Chars, !PS),
    next_char(Src, 'DQUOTE', !PS),
    percent_decode_ascii_unicode(Chars, QuotedString).

:- pred rfc6068.quoted_string_body(src::in, char::out, ps::in, ps::out)
    is semidet.

rfc6068.quoted_string_body(Src, Char, !PS) :-
    % We assume unfolding has already occurred.
    next_char(Src, Char0, !PS),
    ( Char0 = ('\\') ->
        rfc6068.quoted_pair_tail(Src, Char, !PS)
    ; 'WSP'(Char0) ->
        Char = Char0
    ;
        rfc5322.qtext(Char0),
        Char = Char0
    ).

:- pred rfc6068.quoted_pair_tail(src::in, char::out, ps::in, ps::out)
    is semidet.

rfc6068.quoted_pair_tail(Src, Char, !PS) :-
    next_char(Src, Char, !PS),
    ( 'VCHAR'(Char) ->
        true
    ; 'WSP'(Char) ->
        true
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

:- pred rfc6068.addr_spec(src::in, addr_spec::out, ps::in, ps::out) is semidet.

rfc6068.addr_spec(Src, addr_spec(LocalPart, Domain), !PS) :-
    rfc6068.local_part(Src, LocalPart, !PS),
    next_char(Src, '@', !PS),
    rfc6068.domain(Src, Domain, !PS).

:- pred rfc6068.local_part(src::in, local_part::out, ps::in, ps::out)
    is semidet.

rfc6068.local_part(Src, LocalPart, !PS) :-
    ( rfc6068.dot_atom(Src, Atom, !PS) ->
        LocalPart = lpart_atom(Atom)
    ;
        rfc6068.quoted_string(Src, QuotedString, !PS),
        LocalPart = lpart_quoted_string(QuotedString)
    ).

:- pred rfc6068.domain(src::in, domain::out, ps::in, ps::out) is semidet.

rfc6068.domain(Src, Domain, !PS) :-
    ( rfc6068.dot_atom(Src, Atom, !PS) ->
        Domain = domain_name(Atom)
    ;
        next_char(Src, '[', !PS),
        zero_or_more(rfc6068.dtext_no_obs, Src, Chars, !PS),
        next_char(Src, ']', !PS),
        percent_decode_ascii_unicode(Chars, Literal),
        Domain = domain_literal(Literal)
    ).

:- pred rfc6068.dtext_no_obs(src::in, char::out, ps::in, ps::out) is semidet.

rfc6068.dtext_no_obs(Src, C, !PS) :-
    next_char(Src, C, !PS),
    rfc5322.dtext_no_obs(C).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
