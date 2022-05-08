% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module search_term.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module notmuch_config.
:- import_module prog_config.

%-----------------------------------------------------------------------------%

:- type token.

:- pred parse_search_string_and_expand(prog_config::in,
    maybe(notmuch_config)::in, string::in, maybe_error(list(token))::out,
    io::di, io::uo) is det.

:- pred parse_search_string(string::in, maybe_error(list(token))::out) is det.

:- pred tokens_to_search_terms(list(token)::in, string::out) is det.

:- pred check_apply_limit(list(token)::in, bool::out) is det.

:- pred get_default_search_terms(notmuch_config::in, string::out) is det.

:- func search_alias_section = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module parsing_utils.
:- import_module set.
:- import_module string.
:- import_module unit.

:- type token
    --->    whitespace
    ;       open_paren
    ;       close_paren
    ;       open_brace
    ;       close_brace
    ;       literal(string)             % pass through to notmuch
    ;       macro(string)               % without ~ prefix
    ;       date_range(string, string)  % ~d FROM..TO
    ;       do_not_apply_limit.         % ~A

:- inst macro % for token/0
    --->    macro(ground).

:- type parse_context
    --->    tilde_special
    ;       tilde_literal.

:- type expand_alias_result
    --->    found(list(token))
    ;       not_found
    ;       error(string).

%-----------------------------------------------------------------------------%

parse_search_string_and_expand(Config, MaybeNotmuchConfig, Input, Res, !IO) :-
    parse_search_string(Input, Res0),
    (
        Res0 = ok(Tokens),
        ( contains_macro(Tokens) ->
            % At startup, use the notmuch config result we have got already.
            % Subsequently, call notmuch config afresh in case the user
            % modified the configuration.
            (
                MaybeNotmuchConfig = yes(NotmuchConfig0),
                ResConfig = ok(NotmuchConfig0)
            ;
                MaybeNotmuchConfig = no,
                get_notmuch_command(Config, Notmuch),
                get_notmuch_config(Notmuch, ResConfig, !IO)
            )
        ;
            ResConfig = ok(empty_notmuch_config)
        ),
        (
            ResConfig = ok(NotmuchConfig),
            expand_config_aliases(NotmuchConfig, set.init, Tokens, Res)
        ;
            ResConfig = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

parse_search_string(Input, Res) :-
    promise_equivalent_solutions [Res] (
        parsing_utils.parse(Input, no_skip_whitespace, tokens_eof,
            ParseResult),
        (
            ParseResult = ok(Tokens),
            Res = ok(Tokens)
        ;
            ParseResult = error(_MaybeError, _Line, _Column),
            Res = error("Error parsing search string.")
        )
    ).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

:- pred tokens_eof(src::in, list(token)::out, ps::in, ps::out) is semidet.

tokens_eof(Src, Tokens, !PS) :-
    whitespace(Src, _, !PS),
    tokens(Src, Tokens, tilde_special, _Context, !PS),
    eof(Src, _, !PS).

:- pred tokens(src::in, list(token)::out,
    parse_context::in, parse_context::out, ps::in, ps::out) is det.

tokens(Src, Tokens, !Context, !PS) :-
    ( token(Src, Token, !Context, !PS) ->
        tokens(Src, TailTokens, !Context, !PS),
        Tokens = [Token | TailTokens]
    ;
        Tokens = []
    ).

:- pred token(src::in, token::out, parse_context::in, parse_context::out,
    ps::in, ps::out) is semidet.

token(Src, Token, !Context, !PS) :-
    ( whitespace_token(Src, Token0, !Context, !PS) ->
        Token = Token0
    ; paren_token(Src, Token0, !Context, !PS) ->
        Token = Token0
    ; dquoted_string_token(Src, Token0, !Context, !PS) ->
        Token = Token0
    ; date_range_prefix(Src, !Context, !PS) ->
        % Commit to parsing date range after ~d.
        date_range_exprs(Src, FromExpr, ToExpr, !PS),
        Token = date_range(FromExpr, ToExpr)
    ; macro_token(Src, Token0, !Context, !PS) ->
        Token = Token0
    ;
        word_token(Src, Token, !Context, !PS)
    ).

%------------------%

:- pred whitespace_token(src::in, token::out,
    parse_context::in, parse_context::out, ps::in, ps::out) is semidet.

whitespace_token(Src, whitespace, _Context0, Context, !PS) :-
    next_char(Src, Char, !PS),
    char.is_whitespace(Char),
    whitespace(Src, _, !PS),    % eat consecutive whitespace
    Context = tilde_special.

%------------------%

:- pred paren_token(src::in, token::out, parse_context::in, parse_context::out,
    ps::in, ps::out) is semidet.

paren_token(Src, Token, _Context0, Context, !PS) :-
    next_char(Src, Char, !PS),
    (
        Char = '(',
        Token = open_paren,
        Context = tilde_special
    ;
        Char = ')',
        Token = close_paren,
        Context = tilde_literal
    ;
        Char = '{',
        Token = open_brace,
        Context = tilde_special
    ;
        Char = '}',
        Token = close_brace,
        Context = tilde_literal
    ).

%------------------%

:- pred dquoted_string_token(src::in, token::out,
    parse_context::in, parse_context::out, ps::in, ps::out) is semidet.

dquoted_string_token(Src, Token, _Context0, Context, !PS) :-
    current_offset(Src, Start, !PS),
    next_char(Src, '"', !PS),
    dquoted_string_chars(Src, !PS),
    current_offset(Src, End, !PS),
    input_substring(Src, Start, End, Lit),
    Token = literal(Lit),
    Context = tilde_literal.

:- pred dquoted_string_chars(src::in, ps::in, ps::out) is semidet.

dquoted_string_chars(Src, !PS) :-
    next_char(Src, Char, !PS),
    ( Char = ('"') ->
        % Xapian uses doubling of " to escape internal quotes.
        ( next_char(Src, '"', !PS) ->
            dquoted_string_chars(Src, !PS)
        ;
            true
        )
    ;
        dquoted_string_chars(Src, !PS)
    ).
    % Should we accept eof, i.e. unclosed strings?

%------------------%

:- pred date_range_prefix(src::in, parse_context::in, parse_context::out,
    ps::in, ps::out) is semidet.

date_range_prefix(Src, Context0, Context, !PS) :-
    Context0 = tilde_special,
    next_char(Src, '~', !PS),
    next_char(Src, 'd', !PS),
    % Disambiguate ~dDATE from alias beginning with ~d.
    date_range_prefix_2(Src, !PS),
    Context = tilde_literal.

:- pred date_range_prefix_2(src::in, ps::in, ps::out) is semidet.

date_range_prefix_2(Src, PS0, PS) :-
    next_char(Src, Char, PS0, PS1),
    ( char.is_whitespace(Char) ->   % ~d foo
        whitespace(Src, _, PS1, PS)
    ;
        (
            Char = ('{')            % ~d{foo}
        ;
            Char = ('.'),           % ~d..foo
            next_char(Src, '.', PS1, _PS2)
        ),
        PS = PS0
    ).

:- pred date_range_exprs(src::in, string::out, string::out, ps::in, ps::out)
    is semidet.

date_range_exprs(Src, FromExpr, ToExpr, !PS) :-
    date_expr(Src, FromExpr, !PS),
    (
        next_char(Src, '.', !PS),
        next_char(Src, '.', !PS)
    ->
        date_expr(Src, ToExpr, !PS)
    ;
        ToExpr = FromExpr
    ),
    % At least one expression must be non-empty.
    ( FromExpr \= ""
    ; ToExpr \= ""
    ).

:- pred date_expr(src::in, string::out, ps::in, ps::out) is semidet.

date_expr(Src, Expr, !PS) :-
    ( next_char(Src, '{', !PS) ->
        zero_or_more(bracketed_date_char, Src, Chars, !PS),
        next_char(Src, '}', !PS),
        Expr = string.strip(from_char_list(Chars))
    ;
        zero_or_more(non_bracketed_date_char, Src, Chars, !PS),
        Expr = string.from_char_list(Chars)
    ).

:- pred bracketed_date_char(src::in, char::out, ps::in, ps::out) is semidet.

bracketed_date_char(Src, C, !PS) :-
    next_char(Src, C, !PS),
    C \= ('}').

:- pred non_bracketed_date_char(src::in, char::out, ps::in, ps::out)
    is semidet.

non_bracketed_date_char(Src, C, !PS) :-
    next_char(Src, C, !PS),
    ( C = ('.') ->
        not next_char(Src, '.', !.PS, _PS)
    ;
        not char.is_whitespace(C)
    ).

%------------------%

:- pred macro_token(src::in, token::out, parse_context::in, parse_context::out,
    ps::in, ps::out) is semidet.

macro_token(Src, Token, Context0, Context, !PS) :-
    Context0 = tilde_special,
    next_char(Src, '~', !PS),
    current_offset(Src, Start, !PS),
    skip_while_char(is_macro_name_char, Src, !PS),
    current_offset(Src, End, !PS),
    End > Start,
    input_substring(Src, Start, End, MacroName), % excludes ~
    ( simple_alias(MacroName, Token0) ->
        Token = Token0
    ;
        Token = macro(MacroName)
    ),
    Context = tilde_literal.

:- pred is_macro_name_char(char::in) is semidet.

is_macro_name_char(C) :-
    is_word_char(C).

:- pred simple_alias(string::in, token::out) is semidet.

simple_alias("D", literal("tag:deleted")).
simple_alias("F", literal("tag:flagged")).
simple_alias("U", literal("tag:unread")).
simple_alias("A", do_not_apply_limit).

%------------------%

:- pred word_token(src::in, token::out, parse_context::in, parse_context::out,
    ps::in, ps::out) is semidet.

word_token(Src, Token, _Context0, Context, !PS) :-
    current_offset(Src, Start, !PS),
    skip_while_char(is_word_char, Src, !PS),
    current_offset(Src, End, !PS),
    End > Start,
    input_substring(Src, Start, End, Lit),
    Token = literal(Lit),
    Context = tilde_literal.

:- pred is_word_char(char::in) is semidet.

is_word_char(Char) :-
    not char.is_whitespace(Char),
    Char \= ('('),
    Char \= (')'),
    Char \= ('{'),
    Char \= ('}'),
    Char \= ('"').

%------------------%

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

%-----------------------------------------------------------------------------%

:- pred contains_macro(list(token)::in) is semidet.

contains_macro(Tokens) :-
    list.member(Token, Tokens),
    token_is_macro(Token).

:- pred token_is_macro(token::in) is semidet.

token_is_macro(Token) :-
    require_complete_switch [Token]
    (
        Token = macro(_)
    ;
        ( Token = whitespace
        ; Token = open_paren
        ; Token = close_paren
        ; Token = open_brace
        ; Token = close_brace
        ; Token = literal(_)
        ; Token = date_range(_, _)
        ; Token = do_not_apply_limit
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

:- pred expand_config_aliases(notmuch_config::in, set(string)::in,
    list(token)::in, maybe_error(list(token))::out) is det.

expand_config_aliases(NotmuchConfig, Seen, Tokens0, Res) :-
    (
        Tokens0 = [],
        Res = ok([])
    ;
        Tokens0 = [H0 | T0],
        expand_config_alias(NotmuchConfig, Seen, H0, ResH),
        (
            ResH = ok(H),
            expand_config_aliases(NotmuchConfig, Seen, T0, ResT),
            (
                ResT = ok(T),
                Res = ok(H ++ T)
            ;
                ResT = error(Error),
                Res = error(Error)
            )
        ;
            ResH = error(Error),
            Res = error(Error)
        )
    ).

:- pred expand_config_alias(notmuch_config::in, set(string)::in, token::in,
    maybe_error(list(token))::out) is det.

expand_config_alias(NotmuchConfig, Seen, Token0, Res) :-
    (
        Token0 = macro(MacroName),
        expand_config_alias_macro(NotmuchConfig, Seen, Token0, Res1),
        (
            Res1 = found(Tokens),
            Res = ok(Tokens)
        ;
            Res1 = not_found,
            Res = ok([literal("~" ++ MacroName)])
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        ( Token0 = whitespace
        ; Token0 = open_paren
        ; Token0 = close_paren
        ; Token0 = open_brace
        ; Token0 = close_brace
        ; Token0 = literal(_)
        ; Token0 = date_range(_, _)
        ; Token0 = do_not_apply_limit
        ),
        Res = ok([Token0])
    ).

:- pred expand_config_alias_macro(notmuch_config::in, set(string)::in,
    token::in(macro), expand_alias_result::out) is det.

expand_config_alias_macro(NotmuchConfig, Seen0, macro(Name), Res) :-
    (
        not set.contains(Seen0, Name)
    ->
        (
            search(NotmuchConfig, search_alias_section, Name, Expansion)
        ->
            set.insert(Name, Seen0, Seen),
            parse_search_string(Expansion, Res0),
            (
                Res0 = ok(Tokens0),
                expand_config_aliases(NotmuchConfig, Seen, Tokens0, Res1),
                (
                    Res1 = ok(Tokens1),
                    Tokens = [open_paren] ++ Tokens1 ++ [close_paren],
                    Res = found(Tokens)
                ;
                    Res1 = error(Error),
                    Res = error(Error)
                )
            ;
                Res0 = error(_Error),
                Res = error("Error parsing expansion of ~" ++ Name ++ ".")
            )
        ;
            contains(NotmuchConfig, "query", Name)
        ->
            Res = found([literal("query:" ++ Name)])
        ;
            Res = error("Could not expand ~" ++ Name ++ ".")
        )
    ;
        Res = error("Search alias ~" ++ Name ++ " is recursive.")
    ).

%-----------------------------------------------------------------------------%

tokens_to_search_terms(Tokens, Terms) :-
    list.foldl(add_token, Tokens, [], Acc),
    string.append_list(reverse(Acc), Terms).

:- pred add_token(token::in, list(string)::in, list(string)::out) is det.

add_token(Token, !Acc) :-
    (
        Token = whitespace,
        cons(" ", !Acc)
    ;
        Token = open_paren,
        cons("(", !Acc)
    ;
        Token = close_paren,
        cons(")", !Acc)
    ;
        Token = open_brace,
        cons("{", !Acc)
    ;
        Token = close_brace,
        cons("}", !Acc)
    ;
        Token = literal(Lit),
        cons(Lit, !Acc)
    ;
        Token = macro(MacroName),
        cons("~", !Acc),
        cons(MacroName, !Acc)
    ;
        Token = date_range(FromExpr0, ToExpr0),
        string.replace_all(FromExpr0, " ", "_", FromExpr),
        string.replace_all(ToExpr0, " ", "_", ToExpr),
        Term = "date:" ++ FromExpr ++ ".." ++ ToExpr,
        cons(Term, !Acc)
    ;
        Token = do_not_apply_limit
    ).

%-----------------------------------------------------------------------------%

check_apply_limit(Tokens, ApplyLimit) :-
    % It's a little strange that ~A has an effect inside a prefix,
    % e.g. body:( ~A ) but it would also be a little strange if we silently
    % ignored it in ( ~A ).
    ( list.contains(Tokens, do_not_apply_limit) ->
        ApplyLimit = no
    ;
        ApplyLimit = yes
    ).

%-----------------------------------------------------------------------------%

get_default_search_terms(Config, Terms) :-
    (
        search(Config, search_alias_section, "default", Value),
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
