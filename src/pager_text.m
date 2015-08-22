% Bower - a frontend for the Notmuch email system
% Copyright (C) 2015 Peter Wang

:- module pager_text.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type pager_text
    --->    pager_text(quote_level, string, quote_marker_end, text_type).

:- type quote_level == int.

:- type quote_marker_end == int.

:- type text_type
    --->    plain
    ;       diff(diff_line)
    ;       url(int, int).  % (start, end] columns

:- type diff_line
    --->    diff_common
    ;       diff_add
    ;       diff_rem
    ;       diff_hunk
    ;       diff_index.

:- pred make_text_lines(int::in, string::in, list(pager_text)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module set.
:- import_module string.

:- import_module string_util.
:- import_module uri.

:- type append_text_context
    --->    append_text_context(
                % Current quote level (for wrapped lines).
                atc_cur_quote_level     :: maybe(quote_level),
                % Current line type (for wrapped lines).
                atc_cur_line_type       :: maybe(text_type),
                % Known diffs at these quote levels.
                atc_diff_quote_levels   :: set(quote_level)
            ).

%---------------------------------------------------------------------------%

make_text_lines(Max, String, Lines) :-
    Start = 0,
    LastBreak = 0,
    Cur = 0,
    QuoteLevel = no,
    DiffLine = no,
    DiffQuoteLevels = set.init,
    Context0 = append_text_context(QuoteLevel, DiffLine, DiffQuoteLevels),
    append_text(Max, String, Start, LastBreak, Cur, Context0, _Context,
        cord.init, LinesCord),
    Lines = cord.list(LinesCord).

:- pred append_text(int::in, string::in, int::in, int::in, int::in,
    append_text_context::in, append_text_context::out,
    cord(pager_text)::in, cord(pager_text)::out) is det.

append_text(Max, String, Start, LastBreak, Cur, !Context, !Lines) :-
    ( string.unsafe_index_next(String, Cur, Next, Char) ->
        (
            Char = '\n'
        ->
            append_substring(String, Start, Cur, !Context, !Lines),
            reset_context(!Context),
            append_text(Max, String, Next, Next, Next, !Context, !Lines)
        ;
            char.is_whitespace(Char)
        ->
            append_text(Max, String, Start, Cur, Next, !Context, !Lines)
        ;
            % Wrap long lines.
            % XXX this should actually count with wcwidth
            Next - Start > Max
        ->
            maybe_append_substring(String, Start, LastBreak, !Context, !Lines),
            skip_whitespace(String, LastBreak, NextStart),
            append_text(Max, String, NextStart, NextStart, Next, !Context,
                !Lines)
        ;
            append_text(Max, String, Start, LastBreak, Next, !Context, !Lines)
        )
    ;
        % End of string.
        maybe_append_substring(String, Start, Cur, !Context, !Lines)
    ).

:- pred reset_context(append_text_context::in, append_text_context::out)
    is det.

reset_context(!Context) :-
    !Context ^ atc_cur_quote_level := no,
    !Context ^ atc_cur_line_type := no.

:- pred maybe_append_substring(string::in, int::in, int::in,
    append_text_context::in, append_text_context::out,
    cord(pager_text)::in, cord(pager_text)::out) is det.

maybe_append_substring(String, Start, End, !Context, !Lines) :-
    ( End > Start ->
        append_substring(String, Start, End, !Context, !Lines)
    ;
        true
    ).

:- pred append_substring(string::in, int::in, int::in,
    append_text_context::in, append_text_context::out,
    cord(pager_text)::in, cord(pager_text)::out) is det.

append_substring(String, Start, End, Context0, Context, !Lines) :-
    Context0 = append_text_context(MaybeQuoteLevel0, MaybeLineType0,
        DiffQuoteLevels0),
    string.unsafe_between(String, Start, End, SubString),
    (
        MaybeQuoteLevel0 = yes(QuoteLevel),
        QuoteMarkerEnd = 0
    ;
        MaybeQuoteLevel0 = no,
        detect_quote_level(SubString, 0, QuoteLevel, QuoteMarkerEnd)
    ),
    (
        MaybeLineType0 = yes(diff(DiffLine))
    ->
        % Maintain diff line type across wrapped diff lines.
        LineType = diff(DiffLine),
        DiffQuoteLevels = DiffQuoteLevels0
    ;
        ( contains(DiffQuoteLevels0, QuoteLevel) ->
            (
                QuoteLevel = 0,
                MaybeLineType0 = no,
                detect_diff_end(String, Start)
            ->
                PrevDiff = no,
                delete(QuoteLevel, DiffQuoteLevels0, DiffQuoteLevels1)
            ;
                PrevDiff = yes,
                DiffQuoteLevels1 = DiffQuoteLevels0
            )
        ;
            PrevDiff = no,
            DiffQuoteLevels1 = DiffQuoteLevels0
        ),
        (
            % Only detect diffs at start of line.
            MaybeLineType0 = no,
            detect_diff(String, Start + QuoteMarkerEnd, End, PrevDiff,
                QuoteLevel, DiffLine)
        ->
            LineType = diff(DiffLine),
            insert(QuoteLevel, DiffQuoteLevels1, DiffQuoteLevels)
        ;
            PrevDiff = yes
        ->
            LineType = diff(diff_common),
            DiffQuoteLevels = DiffQuoteLevels0
        ;
            detect_url(SubString, UrlStart, UrlEnd)
        ->
            LineType = url(UrlStart, UrlEnd),
            DiffQuoteLevels = DiffQuoteLevels0
        ;
            LineType = plain,
            DiffQuoteLevels = DiffQuoteLevels0
        )
    ),
    Text = pager_text(QuoteLevel, SubString, QuoteMarkerEnd, LineType),
    % We don't really need cords; a reverse list would do.
    snoc(!.Lines, Text) = !:Lines,
    Context = append_text_context(yes(QuoteLevel), yes(LineType),
        DiffQuoteLevels).

%-----------------------------------------------------------------------------%

:- pred detect_quote_level(string::in, int::in, int::out, int::out) is det.

detect_quote_level(String, Pos0, QuoteLevel, QuoteMarkerEnd) :-
    ( quote_marker(String, Pos0, Pos1) ->
        % Perhaps this should be limited to one whitespace char?
        skip_whitespace(String, Pos1, Pos2),
        detect_quote_level(String, Pos2, QuoteLevel0, QuoteMarkerEnd),
        QuoteLevel = 1 + QuoteLevel0
    ;
        QuoteLevel = 0,
        QuoteMarkerEnd = Pos0
    ).

:- pred quote_marker(string::in, int::in, int::out) is semidet.

quote_marker(String, !Pos) :-
    string.unsafe_index_next(String, !Pos, Char),
    (
        Char = ('>')
    ;
        % Accept a single optional space before the quote character.
        Char = (' '),
        string.unsafe_index_next(String, !Pos, '>')
    ).

:- pred detect_diff(string::in, int::in, int::in, bool::in, quote_level::in,
    diff_line::out) is semidet.

detect_diff(String, I, CurLineEnd, PrevDiff, QuoteLevel, Diff) :-
    string.unsafe_index_next(String, I, J, Char),
    (
        Char = ('+'),
        (
            PrevDiff = yes
        ;
            QuoteLevel > 0,
            skip_nls(String, CurLineEnd, NextLineOffset),
            lookahead_likely_diff(String, NextLineOffset, QuoteLevel)
        ),
        Diff = diff_add
    ;
        Char = ('-'),
        ( J = CurLineEnd ->
            true
        ;
            not dashes_or_sig_separator(String, J, CurLineEnd)
        ),
        (
            PrevDiff = yes
        ;
            QuoteLevel > 0,
            skip_nls(String, CurLineEnd, NextLineOffset),
            lookahead_likely_diff(String, NextLineOffset, QuoteLevel)
        ),
        Diff = diff_rem
    ;
        Char = ('@'),
        unsafe_substring_prefix(String, I, "@@ "),
        Diff = diff_hunk
    ;
        Char = 'd',
        unsafe_substring_prefix(String, I, "diff -"),
        Diff = diff_index
    ;
        Char = 'I',
        unsafe_substring_prefix(String, I, "Index: "),
        Diff = diff_index
    ).

    % Quoted diffs are often missing the standard diff headers.  To help
    % detect diff lines without diff headers, we look ahead one line (only).
    %
:- pred lookahead_likely_diff(string::in, int::in, quote_level::in) is semidet.

lookahead_likely_diff(String, Start, ExpectedQuoteLevel) :-
    detect_quote_level(String, Start, ExpectedQuoteLevel, QuoteMarkerEnd),
    I = QuoteMarkerEnd,
    string.unsafe_index_next(String, I, _, Char),
    (
        Char = ('+')
    ;
        Char = ('-')
    ;
        Char = ('@'),
        unsafe_substring_prefix(String, I, "@@ ")
    ;
        Char = 'd',
        unsafe_substring_prefix(String, I, "diff -")
    ;
        Char = 'I',
        unsafe_substring_prefix(String, I, "Index: ")
    ).

:- pred dashes_or_sig_separator(string::in, int::in, int::in) is semidet.

dashes_or_sig_separator(String, I0, End) :-
    ( I0 < End ->
        string.unsafe_index_next(String, I0, I1, Char),
        (
            Char = ('-'),
            dashes_or_sig_separator(String, I1, End)
        ;
            Char = (' '),
            I1 = End
        )
    ;
        true
    ).

:- pred skip_nls(string::in, int::in, int::out) is det.

skip_nls(String, I0, I) :-
    ( string.unsafe_index_next(String, I0, I1, Char) ->
        ( Char = ('\n') ->
            skip_nls(String, I1, I)
        ;
            I = I0
        )
    ;
        I = I0
    ).

:- pred detect_diff_end(string::in, int::in) is semidet.

detect_diff_end(String, Start) :-
    % At quote level zero diffs are likely posted in full and have a limited
    % set of initial characters.
    string.unsafe_index_next(String, Start, _, Char),
    Char \= ('+'),
    Char \= ('-'),
    Char \= (' '),
    Char \= ('@'),
    Char \= ('d'),
    Char \= ('I'),
    Char \= ('i').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
