% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module search_term.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- type token.

:- pred predigest_search_string(string::in, list(token)::out, io::di, io::uo)
    is det.

:- pred tokens_to_search_terms(list(token)::in, string::out, bool::out,
    io::di, io::uo) is det.

:- pred get_default_search_terms(string::out, io::di, io::uo) is det.

:- func search_alias_section = string.

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
:- import_module call_system.
:- import_module quote_arg.

:- type token
    --->    literal(string)             % pass through to notmuch
    ;       macro(string)               % includes ~ prefix
    ;       date_range(string, string)  % ~d FROM..TO
    ;       do_not_apply_limit.         % ~A

:- inst macro
    --->    macro(ground).

%-----------------------------------------------------------------------------%

predigest_search_string(String, Tokens, !IO) :-
    det_parse(String, tokens, Tokens0),
    Seen = set.init,
    expand_config_aliases(Seen, Tokens0, Tokens1, !IO),
    ( list.all_true(should_apply_default_filter, Tokens1) ->
        add_default_filters(Tokens1, Tokens)
    ;
        Tokens = Tokens1
    ).

%-----------------------------------------------------------------------------%

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
        next_char(Src, '~', !PS),
        next_char(Src, 'd', !PS),
        whitespace(Src, _, !PS),
        date_range(Src, DateRangeToken, !PS)
    ->
        Token = DateRangeToken
    ;
        macro_or_literal(Src, Token, !PS)
    ),
    whitespace(Src, _, !PS).

:- pred date_range(src::in, token::out, ps::in, ps::out) is semidet.

date_range(Src, date_range(FromString, ToString), !PS) :-
    date_string(Src, FromString, !PS),
    next_char(Src, '.', !PS),
    next_char(Src, '.', !PS),
    date_string(Src, ToString, !PS).

:- pred date_string(src::in, string::out, ps::in, ps::out) is semidet.

date_string(Src, DateString, !PS) :-
    ( next_char(Src, '{', !PS) ->
        current_offset(Src, Start, !PS),
        date_string_2(unify('}'), Src, Start, DateString, !PS),
        next_char(Src, '}', !PS)
    ;
        current_offset(Src, Start, !PS),
        date_string_2(not_unbracketed_date_char, Src, Start, DateString, !PS)
    ).

:- pred date_string_2(pred(char)::in(pred(in) is semidet),
    src::in, int::in, string::out, ps::in, ps::out) is semidet.

date_string_2(EndPred, Src, Start, DateString, PS0, PS) :-
    ( next_char(Src, C_prime, PS0, PS1_prime) ->
        C = C_prime,
        PS1 = PS1_prime
    ;
        % Treat eof as whitespace.
        eof(Src, _, PS0, PS1),
        C = ' '
    ),
    ( EndPred(C) ->
        current_offset(Src, Prev, PS0, PS), % backtrack one char
        input_substring(Src, Start, Prev, DateString)
    ;
        date_string_2(EndPred, Src, Start, DateString, PS1, PS)
    ).

:- pred not_unbracketed_date_char(char::in) is semidet.

not_unbracketed_date_char(C) :-
    (
        char.is_whitespace(C)
    ;
        C = ('.')
    ).

:- pred macro_or_literal(src::in, token::out, ps::in, ps::out) is semidet.

macro_or_literal(Src, Token, !PS) :-
    current_offset(Src, Start, !PS),
    word_chars(Src, !PS),
    current_offset(Src, End, !PS),
    End > Start,
    input_substring(Src, Start, End, Word),
    ( string.prefix(Word, "~") ->
        ( simple_alias(Word, AliasToken) ->
            Token = AliasToken
        ;
            Token = macro(Word)
        )
    ;
        Token = literal(Word)
    ).

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

:- pred simple_alias(string::in, token::out) is semidet.

simple_alias("~D", literal("tag:deleted")).
simple_alias("~F", literal("tag:flagged")).
simple_alias("~U", literal("tag:unread")).
simple_alias("~A", do_not_apply_limit).

%-----------------------------------------------------------------------------%

:- pred expand_config_aliases(set(string)::in, list(token)::in,
    list(token)::out, io::di, io::uo) is det.

expand_config_aliases(Seen, Tokens0, Tokens, !IO) :-
    list.map_foldl(expand_config_alias(Seen), Tokens0, Tokens1, !IO),
    list.condense(Tokens1, Tokens).

:- pred expand_config_alias(set(string)::in, token::in, list(token)::out,
    io::di, io::uo) is det.

expand_config_alias(Seen, Token0, Tokens, !IO) :-
    (
        Token0 = macro(Word0),
        expand_config_alias_macro(Seen, Token0, Tokens1, !IO),
        (
            Tokens1 = [_ | _],
            Tokens = Tokens1
        ;
            Tokens1 = [],
            Tokens = [literal(Word0)]
        )
    ;
        ( Token0 = literal(_)
        ; Token0 = date_range(_, _)
        ; Token0 = do_not_apply_limit
        ),
        Tokens = [Token0]
    ).

:- pred expand_config_alias_macro(set(string)::in, token::in(macro),
    list(token)::out, io::di, io::uo) is det.

expand_config_alias_macro(Seen0, macro(MacroName), Tokens, !IO) :-
    (
        string.remove_prefix("~", MacroName, Key),
        not set.contains(Seen0, Key)
    ->
        get_notmuch_config(search_alias_section, Key, Res, !IO),
        (
            Res = ok(Expansion),
            set.insert(Key, Seen0, Seen),
            det_parse(Expansion, tokens, Tokens0),
            expand_config_aliases(Seen, Tokens0, Tokens1, !IO),
            Tokens = [literal("(")] ++ Tokens1 ++ [literal(")")]
        ;
            Res = error(_),
            Tokens = [] % fail
        )
    ;
        Tokens = [] % fail
    ).

%-----------------------------------------------------------------------------%

:- pred should_apply_default_filter(token::in) is semidet.

should_apply_default_filter(Token) :-
    Token \= literal("tag:deleted"),
    Token \= literal("tag:draft").

:- pred add_default_filters(list(token)::in, list(token)::out) is det.

add_default_filters(Tokens0, Tokens) :-
    Tokens = [literal("(")] ++ Tokens0 ++
        [literal(") AND NOT ( tag:deleted OR tag:draft )")].

%-----------------------------------------------------------------------------%

tokens_to_search_terms(Tokens0, Terms, ApplyLimit, !IO) :-
    ( list.contains(Tokens0, do_not_apply_limit) ->
        ApplyLimit = no
    ;
        ApplyLimit = yes
    ),
    list.map_foldl(token_to_search_term, Tokens0, TermList, !IO),
    Terms = string.join_list(" ", TermList).

:- pred token_to_search_term(token::in, string::out, io::di, io::uo) is det.

token_to_search_term(Token, Term, !IO) :-
    (
        Token = literal(Term)
    ;
        Token = date_range(FromString, ToString),
        date_string_to_time(FromString, FromTime, !IO),
        date_string_to_time(ToString, ToTime, !IO),
        Term = FromTime ++ ".." ++ ToTime
    ;
        Token = do_not_apply_limit,
        Term = ""
    ;
        Token = macro(_),
        % At this stage all macros have been expanded out.
        unexpected($module, $pred, "macro should have been expanded")
    ).

:- pred date_string_to_time(string::in, string::out, io::di, io::uo)
    is det.

date_string_to_time(DateString, Time, !IO) :-
    ( DateString = "" ->
        Time = ""
    ;
        call_date(DateString, Time, !IO)
    ).

    % Lazy man's date parser, at least until notmuch implements one.
    %
:- pred call_date(string::in, string::out, io::di, io::uo) is det.

call_date(Arg, Res, !IO) :-
    args_to_quoted_command(["date", "+%s", "-d", Arg], Command),
    call_system_capture_stdout(Command, CallRes, !IO),
    (
        CallRes = ok(Line),
        Res = string.rstrip(Line)
    ;
        CallRes = error(_),
        Res = Arg
    ).

%-----------------------------------------------------------------------------%

get_default_search_terms(Terms, !IO) :-
    get_notmuch_config(search_alias_section, "default", Res, !IO),
    (
        Res = ok(Value),
        Value \= ""
    ->
        Terms = Value
    ;
        Terms = "~d {last week}.."
    ).

%-----------------------------------------------------------------------------%

search_alias_section = "bower:search_alias".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
