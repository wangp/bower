% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module fold_lines.
:- interface.

:- import_module list.

:- type span
    --->    span(
                mandatory   :: string,
                trailing_ws :: string
            ).

:- pred get_spans_by_whitespace(string::in, list(span)::out) is det.

:- pred fill_lines(int::in, list(span)::in, list(string)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- import_module string_util.

%-----------------------------------------------------------------------------%

% String = "   This is the sentence.  "
% Spans  = [1--2----3--4---5----------]

get_spans_by_whitespace(String, Spans) :-
    get_spans_ws(String, 0, Spans).

:- pred get_spans_ws(string::in, int::in, list(span)::out) is det.

get_spans_ws(String, Pos0, Spans) :-
    until_whitespace(String, Pos0, Pos1),
    skip_whitespace(String, Pos1, Pos2),
    ( Pos2 = Pos0 ->
        Spans = []
    ;
        make_span(String, Pos0, Pos1, Pos2, Span),
        get_spans_ws(String, Pos2, RestSpans),
        Spans = [Span | RestSpans]
    ).

:- pred make_span(string::in, int::in, int::in, int::in, span::out) is det.

make_span(String, Pos0, Pos1, Pos2, Span) :-
    string.unsafe_between(String, Pos0, Pos1, Mandatory),
    string.unsafe_between(String, Pos1, Pos2, Trailer),
    Span = span(Mandatory, Trailer).

%-----------------------------------------------------------------------------%

fill_lines(MaxWidth, Spans, Lines) :-
    take_spans(MaxWidth, Spans, 0, LineSpans, RestSpans),
    make_line(LineSpans, Line),
    (
        RestSpans = [],
        Lines = [Line]
    ;
        RestSpans = [_ | _],
        fill_lines(MaxWidth, RestSpans, RestLines),
        Lines = [Line | RestLines]
    ).

:- pred take_spans(int::in, list(span)::in, int::in, list(span)::out,
    list(span)::out) is det.

take_spans(_MaxWidth, [], _Width0, [], []).
take_spans(MaxWidth, [Span | Spans], Width0, Taken, NotTaken) :-
    Span = span(Mandatory, Trailer),
    Width1 = Width0 + string_wcwidth(Mandatory),
    Width2 = Width1 + string_wcwidth(Trailer),
    (
        Width2 =< MaxWidth
    ->
        take_spans(MaxWidth, Spans, Width2, Taken2, NotTaken),
        Taken = [Span | Taken2]
    ;
        ( Width0 = 0 % first span on a line must be taken
        ; Width1 =< MaxWidth
        )
    ->
        Taken = [Span],
        NotTaken = Spans
    ;
        Taken = [],
        NotTaken = [Span | Spans]
    ).

:- pred make_line(list(span)::in, string::out) is det.

make_line(Spans, Line) :-
    flatten(Spans, Strings),
    string.append_list(Strings, Line).

:- pred flatten(list(span)::in, list(string)::out) is det.

flatten([], []).
flatten([Span | Spans], Strings) :-
    Span = span(Mandatory, Trailer),
    (
        Spans = [],
        Strings = [Mandatory]
    ;
        Spans = [_ | _],
        flatten(Spans, Strings1),
        Strings = [Mandatory, Trailer | Strings1]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
