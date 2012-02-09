% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module search_term.
:- interface.

:- import_module bool.
:- import_module io.

:- pred string_to_search_terms(string::in, string::out, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module parsing_utils.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module callout.
:- import_module popen.
:- import_module quote_arg.

:- type token
    --->    literal(string)
    ;       macro(string)   % includes ~ prefix
    ;       date_range(string, string).

:- inst macro
    --->    macro(ground).

%-----------------------------------------------------------------------------%

string_to_search_terms(String, Terms, ApplyLimit, !IO) :-
    det_parse(String, tokens, Tokens0),
    check_apply_default_limit(Tokens0, Tokens1, ApplyLimit),
    Seen = set.init,
    expand_words(Seen, Tokens1, Words2, !IO),
    ( should_apply_default_filter(Words2) ->
        add_default_filters(Words2, Words)
    ;
        Words = Words2
    ),
    Terms = string.join_list(" ", Words).

:- pred det_parse(string::in, parser(T)::in(parser), T::out) is det.

det_parse(Input, Parser, X) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, Parser, ParseResult)
    ),
    (
        ParseResult = ok(X)
    ;
        ParseResult = error(_MaybeError, _Line, Column),
        unexpected($module, $pred,
            "parse error at column " ++ string.from_int(Column))
    ).

:- pred tokens(src::in, list(token)::out, ps::in, ps::out) is semidet.

tokens(Src, Words, !PS) :-
    whitespace(Src, _, !PS),
    zero_or_more(token, Src, Words, !PS),
    eof(Src, _, !PS).

:- pred token(src::in, token::out, ps::in, ps::out) is semidet.

token(Src, Token, !PS) :-
    ( char_in_class("()", Src, Char, !PS) ->
        Word = string.from_char(Char),
        Token = literal(Word)
    ;
        current_offset(Src, Start, !PS),
        word_chars(Src, !PS),
        current_offset(Src, End, !PS),
        End > Start,
        input_substring(Src, Start, End, Word),
        (
            string.remove_prefix("~d", Word, Suffix),
            Words = string.split_at_string("..", Suffix),
            Words = [FromWord, ToWord]
        ->
            Token = date_range(FromWord, ToWord)
        ;
            string.prefix(Word, "~")
        ->
            Token = macro(Word)
        ;
            Token = literal(Word)
        )
    ),
    whitespace(Src, _, !PS).

:- pred word_chars(src::in, ps::in, ps::out) is semidet.

word_chars(Src, PS0, PS) :-
    ( next_char(Src, Char, PS0, PS1) ->
        ( is_word_char(Char) ->
            word_chars(Src, PS1, PS)
        ;
            PS = PS0
        )
    ;
        eof(Src, _, PS0, PS)
    ).

:- pred is_word_char(char::in) is semidet.

is_word_char(C) :-
    C \= '(',
    C \= ')',
    not char.is_whitespace(C).

:- pred check_apply_default_limit(list(token)::in, list(token)::out,
    bool::out) is det.

check_apply_default_limit(Tokens0, Tokens, ApplyLimit) :-
    ( list.contains(Tokens0, macro("~A")) ->
        Tokens = list.delete_all(Tokens0, macro("~A")),
        ApplyLimit = no
    ;
        Tokens = Tokens0,
        ApplyLimit = yes
    ).

:- pred expand_words(set(string)::in, list(token)::in, list(string)::out,
    io::di, io::uo) is det.

expand_words(Seen, Tokens0, Words, !IO) :-
    list.map_foldl(expand_word(Seen), Tokens0, Words1, !IO),
    list.condense(Words1, Words).

:- pred expand_word(set(string)::in, token::in, list(string)::out,
    io::di, io::uo) is det.

expand_word(Seen, Token0, Words, !IO) :-
    (
        Token0 = literal(Word),
        Words = [Word]
    ;
        Token0 = macro(Word0),
        expand_fixed_alias(Token0, Words1, !IO),
        (
            Words1 = [_ | _],
            Words = Words1
        ;
            Words1 = [],
            expand_config_alias(Seen, Token0, Words2, !IO),
            (
                Words2 = [_ | _],
                Words = Words2
            ;
                Words2 = [],
                Words = [Word0]
            )
        )
    ;
        Token0 = date_range(FromString, ToString),
        maybe_call_date(FromString, FromTime, !IO),
        maybe_call_date(ToString, ToTime, !IO),
        Words = [FromTime ++ ".." ++ ToTime]
    ).

:- pred expand_fixed_alias(token::in(macro), list(string)::out, io::di, io::uo)
    is det.

expand_fixed_alias(macro(Word0), Words, !IO) :-
    ( Word0 = "~D" ->
        Words = ["tag:deleted"]
    ; Word0 = "~F" ->
        Words = ["tag:flagged"]
    ; Word0 = "~U" ->
        Words = ["tag:unread"]
    ; date_macro(Word0, DateSpec) ->
        call_date(DateSpec, Time, !IO),
        Words = [Time ++ ".."]
    ;
        Words = [] % fail
    ).

:- pred date_macro(string::in, string::out) is semidet.

date_macro("~lw", "last week").
date_macro("~1w", "last week").
date_macro("~2w", "2 weeks ago").
date_macro("~3w", "3 weeks ago").
date_macro("~lm", "last month").
date_macro("~1m", "last month").
date_macro("~2m", "2 months ago").
date_macro("~3m", "3 months ago").
date_macro("~ly", "last year").
date_macro("~yesterday", "yesterday").
date_macro("~today", "today").

:- pred expand_config_alias(set(string)::in, token::in(macro),
    list(string)::out, io::di, io::uo) is det.

expand_config_alias(Seen0, macro(Word), Words, !IO) :-
    (
        string.remove_prefix("~", Word, MacroName),
        not set.contains(Seen0, MacroName)
    ->
        get_notmuch_config("search_alias", MacroName, Res, !IO),
        (
            Res = ok(Expansion),
            set.insert(MacroName, Seen0, Seen),
            det_parse(Expansion, tokens, ExpansionTokens),
            list.map_foldl(expand_word(Seen), ExpansionTokens, Wordss, !IO),
            list.condense(Wordss, Words0),
            Words = ["(" | Words0 ++ [")"]]
        ;
            Res = error(_),
            Words = [] % fail
        )
    ;
        Words = [] % fail
    ).

:- pred should_apply_default_filter(list(string)::in) is semidet.

should_apply_default_filter(Words) :-
    all [Word] (
        list.member(Word, Words)
    => (
        Word \= "tag:deleted",
        Word \= "tag:draft"
    )).

:- pred add_default_filters(list(string)::in, list(string)::out) is det.

add_default_filters(Words0, Words) :-
    Words = ["("] ++ Words0 ++ [") AND NOT ( tag:deleted OR tag:draft )"].

    % Lazy man's date parser, at least until notmuch implements one.
    %
:- pred call_date(string::in, string::out, io::di, io::uo) is det.

call_date(Arg, Res, !IO) :-
    args_to_quoted_command(["date", "+%s", "-d", Arg], Command),
    popen(Command, CallRes, !IO),
    (
        CallRes = ok(Line),
        Res = string.rstrip(Line)
    ;
        CallRes = error(_),
        Res = Arg
    ).

:- pred maybe_call_date(string::in, string::out, io::di, io::uo) is det.

maybe_call_date(Arg, Res, !IO) :-
    ( Arg = "" ->
        Res = ""
    ;
        call_date(Arg, Res, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
