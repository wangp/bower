% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014, 2022 Peter Wang

:- module shell_word.
:- interface.

:- import_module list.
:- import_module parsing_utils.

:- type shell_token
    --->    whitespace
    ;       gmeta(string)       % non-empty sequence of |&;()<>
    ;       word(shell_word).

:- type shell_word == list(shell_word_segment).

:- type shell_word_segment
    --->    unquoted(
                % This string may contain "unsafe" characters, i.e. characters
                % that may have special meaning to the shell, but are not
                % graphic metacharacters (which separate words).
                string
            )
    ;       quoted(
                % This string can be:
                %   'single quoted string'
                %   "double quoted string"
                %   \x (backslash escape sequence)
                string
            ).

    % Also trims whitespace.
    %
:- pred tokenise(string::in, parse_result(list(shell_token))::out)
    is cc_multi.

:- pred contains_graphic_metachars(list(shell_token)::in) is semidet.

:- func trim_whitespace(list(shell_token)) = list(shell_token).
:- func ltrim_whitespace(list(shell_token)) = list(shell_token).
:- func rtrim_whitespace(list(shell_token)) = list(shell_token).

:- pred serialise_as_is(list(shell_token)::in, string::out) is det.

    % Serialise the list of tokens, quoting everything as required to prevent
    % interpretation of characters as metacharacters.
    %
:- pred serialise_quote_all(list(shell_token)::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.
:- import_module unit.

:- import_module quote_arg.

%-----------------------------------------------------------------------------%

:- pred is_shell_metachar(char::in) is semidet.

is_shell_metachar(C) :- is_shell_whitespace(C).
is_shell_metachar(C) :- is_shell_graphic_metachar(C).

:- pred is_shell_whitespace(char::in) is semidet.

is_shell_whitespace(' ').
is_shell_whitespace('\t').
is_shell_whitespace('\n').
is_shell_whitespace('\r'). % not in bash manual

:- pred is_shell_graphic_metachar(char::in) is semidet.

is_shell_graphic_metachar('|').
is_shell_graphic_metachar('&').
is_shell_graphic_metachar(';').
is_shell_graphic_metachar('(').
is_shell_graphic_metachar(')').
is_shell_graphic_metachar('<').
is_shell_graphic_metachar('>').

%-----------------------------------------------------------------------------%

tokenise(Input, ParseResult) :-
    parsing_utils.parse(Input, no_skip_whitespace, trim_tokens_eof,
        ParseResult).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_, unit, !PS) :-
    semidet_true.

:- pred skip_while_char(pred(char), src, ps, ps).
:- mode skip_while_char(in(pred(in) is semidet), in, in, out) is semidet.

skip_while_char(P, Src, PS0, PS) :-
    ( next_char(Src, Char, PS0, PS1) ->
        ( P(Char) ->
            skip_while_char(P, Src, PS1, PS)
        ;
            PS = PS0
        )
    ;
        eof(Src, _, PS0, PS)
    ).

:- pred trim_tokens_eof(src::in, list(shell_token)::out, ps::in, ps::out)
    is semidet.

trim_tokens_eof(Src, Tokens, !PS) :-
    zero_or_more(token, Src, Tokens0, !PS),
    eof(Src, _, !PS),
    Tokens = trim_whitespace(Tokens0).

:- pred token(src::in, shell_token::out, ps::in, ps::out) is semidet.

token(Src, Token, !PS) :-
    ( metachar_token(Src, Token0, !PS) ->
        Token = Token0
    ;
        one_or_more(word_segment, Src, Segments, !PS),
        Token = word(Segments)
    ).

:- pred metachar_token(src::in, shell_token::out, ps::in, ps::out)
    is semidet.

metachar_token(Src, Token, !PS) :-
    current_offset(Src, Offset0, !PS),
    next_char(Src, C, !PS),
    ( is_shell_whitespace(C) ->
        skip_while_char(is_shell_whitespace, Src, !PS),
        Token = whitespace
    ; is_shell_graphic_metachar(C) ->
        skip_while_char(is_shell_graphic_metachar, Src, !PS),
        current_offset(Src, Offset, !PS),
        input_substring(Src, Offset0, Offset, Str),
        Token = gmeta(Str)
    ;
        fail
    ).

:- pred word_segment(src::in, shell_word_segment::out, ps::in, ps::out)
    is semidet.

word_segment(Src, Segment, !PS) :-
    PS0 = !.PS,
    current_offset(Src, Offset0, !PS),
    next_char(Src, C, !PS),
    not is_shell_metachar(C),
    ( C = ('"') ->
        ( dquote_tail(Src, [], _RevCs, !PS) ->
            %string.from_rev_char_list(RevCs, Unescaped),
            current_offset(Src, Offset, !PS),
            input_substring(Src, Offset0, Offset, OrigStr),
            Segment = quoted(OrigStr)
        ;
            fail_with_message("unmatched double quote",
                Src, Segment, PS0, !:PS)
        )
    ; C = ('''') ->
        ( squote_tail(Src, [], _RevCs, !PS) ->
            %string.from_rev_char_list(RevCs, Unescaped),
            current_offset(Src, Offset, !PS),
            input_substring(Src, Offset0, Offset, OrigStr),
            Segment = quoted(OrigStr)
        ;
            fail_with_message("unmatched single quote",
                Src, Segment, PS0, !:PS)
        )
    ; C = ('\\') ->
        ( escape(Src, _Unescaped, !PS) ->
            current_offset(Src, Offset, !PS),
            input_substring(Src, Offset0, Offset, OrigStr),
            Segment = quoted(OrigStr)
        ;
            fail_with_message("missing escaped character",
                Src, Segment, PS0, !:PS)
        )
    ;
        unquoted(Src, C, String, !PS),
        Segment = unquoted(String)
    ).

:- pred dquote_tail(src::in, list(char)::in, list(char)::out, ps::in, ps::out)
    is semidet.

dquote_tail(Src, !RevCs, !PS) :-
    next_char_no_progress(Src, C0, !PS),
    ( C0 = ('"') ->
        true
    ; C0 = ('\\') ->
        next_char_no_progress(Src, C, !PS),
        cons(C, !RevCs),
        dquote_tail(Src, !RevCs, !PS)
    ;
        C = C0,
        cons(C, !RevCs),
        dquote_tail(Src, !RevCs, !PS)
    ).

:- pred squote_tail(src::in, list(char)::in, list(char)::out, ps::in, ps::out)
    is semidet.

squote_tail(Src, !RevCs, !PS) :-
    next_char_no_progress(Src, C, !PS),
    ( C = ('''') ->
        true
    ;
        cons(C, !RevCs),
        squote_tail(Src, !RevCs, !PS)
    ).

:- pred escape(src::in, string::out, ps::in, ps::out) is semidet.

escape(Src, String, !PS) :-
    next_char_no_progress(Src, C, !PS),
    String = string.from_char(C).

:- pred unquoted(src::in, char::in, string::out, ps::in, ps::out)
    is semidet.

unquoted(Src, C, String, !PS) :-
    zero_or_more(unquoted_char, Src, Cs, !PS),
    string.from_char_list([C | Cs], String).

:- pred unquoted_char(src::in, char::out, ps::in, ps::out) is semidet.

unquoted_char(Src, C, !PS) :-
    next_char_no_progress(Src, C, !PS),
    not is_shell_metachar(C),
    C \= ('\\'),
    C \= ('"'),
    C \= ('''').

%-----------------------------------------------------------------------------%

contains_graphic_metachars(Tokens) :-
    member(gmeta(_), Tokens).

%-----------------------------------------------------------------------------%

trim_whitespace(Tokens) =
    rtrim_whitespace(ltrim_whitespace(Tokens)).

ltrim_whitespace([]) = [].
ltrim_whitespace([Token | Tokens]) =
    ( if Token = whitespace then
        ltrim_whitespace(Tokens)
    else
        [Token | Tokens]
    ).

rtrim_whitespace(Tokens) =
    reverse(ltrim_whitespace(reverse(Tokens))).

%-----------------------------------------------------------------------------%

:- type quoting
    --->    quote_as_is
    ;       quote_all.

serialise_as_is(Tokens, Str) :-
    serialise_tokens(quote_as_is, Tokens, Strs),
    string.append_list(Strs, Str).

serialise_quote_all(Tokens, Str) :-
    serialise_tokens(quote_all, Tokens, Strs),
    string.append_list(Strs, Str).

:- pred serialise_tokens(quoting::in, list(shell_token)::in, list(string)::out)
    is det.

serialise_tokens(Quoting, Tokens0, Strs) :-
    (
        Tokens0 = [],
        Strs = []
    ;
        Tokens0 = [Head | TailTokens],
        (
            Head = whitespace,
            HeadStr = " "
        ;
            Head = gmeta(HeadStr0),
            HeadStr = maybe_quote_arg(Quoting, HeadStr0)
        ;
            Head = word(HeadWord),
            HeadStr = serialise_word(Quoting, HeadWord)
        ),
        serialise_tokens(Quoting, TailTokens, TailStrs),
        Strs = [HeadStr | TailStrs]
    ).

:- func serialise_word(quoting, shell_word) = string.

serialise_word(Quoting, Segments) =
    string.append_list(list.map(serialise_word_segment(Quoting), Segments)).

:- func serialise_word_segment(quoting, shell_word_segment) = string.

serialise_word_segment(Quoting, Segment) = Str :-
    (
        Segment = unquoted(Str0),
        Str = maybe_quote_arg(Quoting, Str0)
    ;
        Segment = quoted(Str)
    ).

:- func maybe_quote_arg(quoting, string) = string.

maybe_quote_arg(quote_all, S) = quote_arg(S).
maybe_quote_arg(quote_as_is, S) = S.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
