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

%-----------------------------------------------------------------------------%

string_to_search_terms(String, Terms, ApplyLimit, !IO) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(String, tokens, ParseResult)
    ),
    some [!Words]
    (
        ParseResult = ok(!:Words),
        check_apply_default_limit(!Words, ApplyLimit),
        Seen = set.init,
        expand_words(Seen, !Words, !IO),
        ( should_apply_default_filter(!.Words) ->
            add_default_filters(!Words)
        ;
            true
        ),
        Terms = string.join_list(" ", !.Words)
    ;
        ParseResult = error(_MaybeError, _Line, Column),
        unexpected($module, $pred,
            "parse error at column " ++ string.from_int(Column))
    ).

:- pred tokens(src::in, list(string)::out, ps::in, ps::out) is semidet.

tokens(Src, Words, !PS) :-
    whitespace(Src, _, !PS),
    zero_or_more(token, Src, Words, !PS),
    eof(Src, _, !PS).

:- pred token(src::in, string::out, ps::in, ps::out) is semidet.

token(Src, Word, !PS) :-
    ( char_in_class("()", Src, Char, !PS) ->
        Word = string.from_char(Char)
    ;
        current_offset(Src, Start, !PS),
        word_chars(Src, !PS),
        current_offset(Src, End, !PS),
        End > Start,
        input_substring(Src, Start, End, Word)
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

:- pred check_apply_default_limit(list(string)::in, list(string)::out,
    bool::out) is det.

check_apply_default_limit(Words0, Words, ApplyLimit) :-
    ( list.contains(Words0, "~A") ->
        Words = list.delete_all(Words0, "~A"),
        ApplyLimit = no
    ;
        Words = Words0,
        ApplyLimit = yes
    ).

:- pred expand_words(set(string)::in, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

expand_words(Seen, Words0, Words, !IO) :-
    list.map_foldl(expand_word(Seen), Words0, Words1, !IO),
    list.condense(Words1, Words).

:- pred expand_word(set(string)::in, string::in, list(string)::out,
    io::di, io::uo) is det.

expand_word(Seen, Word0, Words, !IO) :-
    expand_fixed_alias(Word0, Words1, !IO),
    (
        Words1 = [_ | _],
        Words = Words1
    ;
        Words1 = [],
        expand_config_alias(Seen, Word0, Words2, !IO),
        (
            Words2 = [_ | _],
            Words = Words2
        ;
            Words2 = [],
            Words = [Word0]
        )
    ).

:- pred expand_fixed_alias(string::in, list(string)::out, io::di, io::uo)
    is det.

expand_fixed_alias(Word0, Words, !IO) :-
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
        string.remove_prefix("~d", Word0, Suffix),
        SplitWords = string.split_at_string("..", Suffix),
        SplitWords \= [_]
    ->
        ( SplitWords = ["", Word2] ->
            call_date(Word2, Time2, !IO),
            Words = [".." ++ Time2]
        ; SplitWords = [Word1, ""] ->
            call_date(Word1, Time1, !IO),
            Words = [Time1 ++ ".."]
        ; SplitWords = [Word1, Word2] ->
            call_date(Word1, Time1, !IO),
            call_date(Word2, Time2, !IO),
            Words = [Time1 ++ ".." ++ Time2]
        ;
            Words = [] % fail
        )
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

:- pred expand_config_alias(set(string)::in, string::in, list(string)::out,
    io::di, io::uo) is det.

expand_config_alias(Seen0, Word0, Words, !IO) :-
    (
        string.remove_prefix("~", Word0, Key),
        not set.contains(Seen0, Key)
    ->
        get_notmuch_config("search_alias", Key, Res, !IO),
        (
            Res = ok(Expansion),
            set.insert(Key, Seen0, Seen),
            ExpansionWords = ["("] ++ string.words(Expansion) ++ [")"],
            list.map_foldl(expand_word(Seen), ExpansionWords, Wordss, !IO),
            list.condense(Wordss, Words)
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
