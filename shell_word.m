% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module shell_word.
:- interface.

:- import_module list.
:- import_module parsing_utils.

    % Delimited words.
:- type word
    --->    word(list(segment)).

    % Undelimited segments of a word.
    % Quoted means the substring appears within single quotes,
    % double quotes or was escaped with backlash.
:- type segment
    --->    unquoted(string)
    ;       quoted(string).

:- pred split(string::in, parse_result(list(word))::out) is cc_multi.

:- func word_string(word) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.
:- import_module unit.

split(Input, ParseResult) :-
    parsing_utils.parse(Input, no_skip_whitespace, words_eof, ParseResult).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_, unit, !PS) :-
    semidet_true.

:- pred next_char_np(src::in, char::out, ps::in, ps::out) is semidet.

next_char_np(Src, C, !PS) :-
    % All calls to next_char_np can be changed to next_char_no_progress
    % when compatibility with Mercury 11.07 is dropped.
    parsing_utils.next_char(Src, C, !PS).

:- pred skip_ws(src::in, unit::out, ps::in, ps::out) is semidet.

skip_ws(Src, unit, !PS) :-
    (
        next_char_np(Src, C, !PS),
        ws(C)
    ->
        skip_ws(Src, _, !PS)
    ;
        true
    ).

:- pred ws(char::in) is semidet.

ws(' ').
ws('\t').
ws('\n').
ws('\r').

:- pred words_eof(src::in, list(word)::out, ps::in, ps::out) is semidet.

words_eof(Src, Words, !PS) :-
    skip_ws(Src, _, !PS),
    zero_or_more(word, Src, Words, !PS),
    eof(Src, _, !PS).

:- pred word(src::in, word::out, ps::in, ps::out) is semidet.

word(Src, word(Segments), !PS) :-
    one_or_more(segment, Src, Segments, !PS),
    skip_ws(Src, _, !PS).

:- pred segment(src::in, segment::out, ps::in, ps::out) is semidet.

segment(Src, Segment, !PS) :-
    PS0 = !.PS,
    next_char(Src, C, !PS),
    ( C = ('"') ->
        ( dquote_tail(Src, [], RevCs, !PS) ->
            string.from_rev_char_list(RevCs, String),
            Segment = quoted(String)
        ;
            fail_with_message("unmatched double quote",
                Src, Segment, PS0, !:PS)
        )
    ; C = ('''') ->
        ( squote_tail(Src, [], RevCs, !PS) ->
            string.from_rev_char_list(RevCs, String),
            Segment = quoted(String)
        ;
            fail_with_message("unmatched single quote",
                Src, Segment, PS0, !:PS)
        )
    ; C = ('\\') ->
        ( escape(Src, String, !PS) ->
            Segment = quoted(String)
        ;
            fail_with_message("missing escaped character",
                Src, Segment, PS0, !:PS)
        )
    ; not ws(C) ->
        unquoted(Src, C, String, !PS),
        Segment = unquoted(String)
    ;
        fail
    ).

:- pred dquote_tail(src::in, list(char)::in, list(char)::out, ps::in, ps::out)
    is semidet.

dquote_tail(Src, !RevCs, !PS) :-
    next_char_np(Src, C0, !PS),
    ( C0 = ('"') ->
        true
    ; C0 = ('\\') ->
        next_char_np(Src, C, !PS),
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
    next_char_np(Src, C, !PS),
    ( C = ('''') ->
        true
    ;
        cons(C, !RevCs),
        squote_tail(Src, !RevCs, !PS)
    ).

:- pred escape(src::in, string::out, ps::in, ps::out) is semidet.

escape(Src, String, !PS) :-
    next_char_np(Src, C, !PS),
    String = string.from_char(C).

:- pred unquoted(src::in, char::in, string::out, ps::in, ps::out)
    is semidet.

unquoted(Src, C, String, !PS) :-
    zero_or_more(unquoted_char, Src, Cs, !PS),
    string.from_char_list([C | Cs], String).

:- pred unquoted_char(src::in, char::out, ps::in, ps::out) is semidet.

unquoted_char(Src, C, !PS) :-
    next_char_np(Src, C, !PS),
    not ws(C),
    C \= ('\\'),
    C \= ('"'),
    C \= ('''').

%-----------------------------------------------------------------------------%

word_string(word(Segments)) =
    string.append_list(list.map(segment_string, Segments)).

:- func segment_string(segment) = string.

segment_string(unquoted(S)) = S.
segment_string(quoted(S)) = S.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
