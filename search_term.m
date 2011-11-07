%-----------------------------------------------------------------------------%

:- module search_term.
:- interface.

:- import_module io.

:- pred string_to_search_terms(string::in, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module popen.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

string_to_search_terms(String, Terms, !IO) :-
    Words0 = string.words(String),
    list.map_foldl(expand_word, Words0, Words1, !IO),
    ( should_apply_default_filter(Words1) ->
        add_default_filters(Words1, Words2)
    ;
        Words2 = Words1
    ),
    Terms = string.join_list(" ", Words2).

:- pred expand_word(string::in, string::out, io::di, io::uo) is det.

expand_word(Word0, Word, !IO) :-
    ( Word0 = "~D" ->
        Word = "tag:deleted"
    ; Word0 = "~F" ->
        Word = "tag:flagged"
    ; Word0 = "~U" ->
        Word = "tag:unread"
    ; Word0 = "~lw" ->
        call_date("last week", Time, !IO),
        Word = Time ++ ".."
    ; Word0 = "~lm" ->
        call_date("last month", Time, !IO),
        Word = Time ++ ".."
    ; Word0 = "~yesterday" ->
        call_date("yesterday", Time, !IO),
        Word = Time ++ ".."
    ; Word0 = "~today" ->
        call_date("today", Time, !IO),
        Word = Time ++ ".."
    ; string.remove_prefix("~d", Word0, Suffix) ->
        SplitWords = string.split_at_string("..", Suffix),
        ( SplitWords = [Word1] ->
            call_date(Word1, Time, !IO),
            Word = Time ++ ".."
        ; SplitWords = ["", Word2] ->
            call_date(Word2, Time2, !IO),
            Word = ".." ++ Time2
        ; SplitWords = [Word1, ""] ->
            call_date(Word1, Time1, !IO),
            Word = Time1 ++ ".."
        ; SplitWords = [Word1, Word2] ->
            call_date(Word1, Time1, !IO),
            call_date(Word2, Time2, !IO),
            Word = Time1 ++ ".." ++ Time2
        ;
            Word = Word0
        )
    ;
        Word = Word0
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
