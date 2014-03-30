% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module pager.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module screen.
:- import_module scrollable.

%-----------------------------------------------------------------------------%

:- type pager_info.

:- type setup_mode
    --->    include_replies
    ;       toplevel_only.

:- type retain_pager_pos
    --->    new_pager
    ;       retain_pager_pos(pager_info, int).

:- pred setup_pager(setup_mode::in, int::in, list(message)::in,
    pager_info::out, io::di, io::uo) is det.

:- pred setup_pager_for_staging(int::in, string::in, retain_pager_pos::in,
    pager_info::out) is det.

:- type pager_action
    --->    continue
    ;       leave_pager.

:- pred pager_input(int::in, keycode::in, pager_action::out,
    message_update::out, pager_info::in, pager_info::out) is det.

:- pred scroll(int::in, int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- pred scroll_but_stop_at_message(int::in, int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- pred next_message(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred prev_message(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred goto_first_message(pager_info::in, pager_info::out) is det.

:- pred goto_end(int::in, pager_info::in, pager_info::out) is det.

:- pred skip_quoted_text(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred get_top_message(pager_info::in, message::out) is semidet.

:- pred get_top_offset(pager_info::in, int::out) is semidet.

:- pred skip_to_message(message_id::in, pager_info::in, pager_info::out)
    is det.

:- type search_kind
    --->    new_search
    ;       continue_search.

:- pred skip_to_search(int::in, search_kind::in, string::in,
    search_direction::in, message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred highlight_minor(int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- pred highlight_major(int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- type highlighted_thing
    --->    highlighted_part(part, maybe(header_value)) % maybe(subject)
    ;       highlighted_url(string)
    ;       highlighted_fold_marker.

:- pred get_highlighted_thing(pager_info::in, highlighted_thing::out)
    is semidet.

:- pred toggle_content(int::in, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

:- pred draw_pager(screen::in, pager_info::in, io::di, io::uo) is det.

:- pred draw_pager_lines(list(panel)::in, pager_info::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module set.
:- import_module string.
:- import_module version_array.

:- import_module copious_output.
:- import_module prog_config.
:- import_module string_util.
:- import_module uri.

%-----------------------------------------------------------------------------%

:- type pager_info
    --->    pager_info(
                p_tree          :: tree,
                p_id_counter    :: counter,
                p_scrollable    :: scrollable(id_pager_line)
            ).

:- type node_id
    --->    node_id(int).

:- type tree
    --->    leaf(list(pager_line))
    ;       node(
                node_id     :: node_id,
                subtrees    :: list(tree),
                preblank    :: bool
            ).

:- type id_pager_line
    --->    node_id - pager_line.

:- type pager_line
    --->    start_message_header(message, string, header_value)
    ;       header(string, header_value)
    ;       text(quote_level, string, quote_marker_end, text_type)
    ;       part_head(
                part            :: part,
                alternatives    :: list(part)
                % Hidden alternatives for multipart/alternative.
            )
    ;       fold_marker(
                content         :: list(pager_line),
                expanded        :: bool
            )
    ;       message_separator.

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

:- type binding
    --->    leave_pager
    ;       scroll_down
    ;       scroll_up
    ;       page_down
    ;       page_up
    ;       half_page_down
    ;       half_page_up
    ;       home
    ;       end
    ;       next_message
    ;       prev_message
    ;       skip_quoted_text.

:- instance scrollable.line(id_pager_line) where [
    pred(draw_line/6) is draw_id_pager_line
].

%-----------------------------------------------------------------------------%

:- pred allocate_node_id(node_id::out, counter::in, counter::out) is det.

allocate_node_id(node_id(N), !Counter) :-
    counter.allocate(N, !Counter).

:- func dummy_node_id = node_id.

dummy_node_id = node_id(-1).

%-----------------------------------------------------------------------------%

:- func flatten(tree) = list(id_pager_line).

flatten(Tree) = list(Cord) :-
    flatten(dummy_node_id, Tree, Cord, no, _LastBlank).

:- pred flatten(node_id::in, tree::in, cord(id_pager_line)::out,
    bool::in, bool::out) is det.

flatten(ParentId, Tree, Cord, LastBlank0, LastBlank) :-
    (
        Tree = leaf(Lines),
        % This is slightly wasteful as the user cannot do anything on most
        % lines that would require the id.
        Cord = cord.from_list(map(make_id_pager_line(ParentId), Lines)),
        ( list.last(Lines, LastLine) ->
            LastBlank = pred_to_bool(is_blank_line(LastLine))
        ;
            LastBlank = LastBlank0
        )
    ;
        Tree = node(NodeId, SubTrees, PreBlank),
        list.map_foldl(flatten(NodeId), SubTrees, SubCords,
            LastBlank0, LastBlank),
        (
            LastBlank0 = no,
            PreBlank = yes
        ->
            Cord = singleton(NodeId - blank_line)
                ++ cord_list_to_cord(SubCords)
        ;
            Cord = cord_list_to_cord(SubCords)
        )
    ).

:- func make_id_pager_line(node_id, pager_line) = id_pager_line.

make_id_pager_line(NodeId, Line) = NodeId - Line.

:- pred is_blank_line(pager_line::in) is semidet.

is_blank_line(text(_, "", _, _)).
is_blank_line(message_separator).

:- pred replace_node(node_id::in, tree::in, tree::in, tree::out) is det.

replace_node(FindId, NewTree, Tree0, Tree) :-
    (
        Tree0 = leaf(_),
        Tree = Tree0
    ;
        Tree0 = node(NodeId, SubTrees0, PreBlank),
        ( NodeId = FindId ->
            Tree = NewTree
        ;
            list.map(replace_node(FindId, NewTree), SubTrees0, SubTrees),
            Tree = node(NodeId, SubTrees, PreBlank)
        )
    ).

%-----------------------------------------------------------------------------%

setup_pager(Mode, Cols, Messages, Info, !IO) :-
    counter.init(0, Counter0),
    allocate_node_id(NodeId, Counter0, Counter1),
    list.map_foldl2(make_message_tree(Mode, Cols), Messages, Trees,
        Counter1, Counter, !IO),
    Tree = node(NodeId, Trees, no),
    Scrollable = scrollable.init(flatten(Tree)),
    Info = pager_info(Tree, Counter, Scrollable).

:- pred make_message_tree(setup_mode::in, int::in, message::in, tree::out,
    counter::in, counter::out, io::di, io::uo) is det.

make_message_tree(Mode, Cols, Message, Tree, !Counter, !IO) :-
    Headers = Message ^ m_headers,
    counter.allocate(NodeIdInt, !Counter),
    NodeId = node_id(NodeIdInt),

    some [!RevHeaderLines] (
        StartMessage = start_message_header(Message, "Date", Headers ^ h_date),
        !:RevHeaderLines = [StartMessage],
        add_header("From", Headers ^ h_from, !RevHeaderLines),
        add_header("Subject", Headers ^ h_subject, !RevHeaderLines),
        add_header("To", Headers ^ h_to, !RevHeaderLines),
        maybe_add_header("Cc", Headers ^ h_cc, !RevHeaderLines),
        maybe_add_header("Reply-To", Headers ^ h_replyto, !RevHeaderLines),
        cons(blank_line, !RevHeaderLines),
        HeaderTree = leaf(list.reverse(!.RevHeaderLines))
    ),

    Body = Message ^ m_body,
    list.map_foldl3(make_part_tree(Cols), Body, BodyTrees,
        yes, _ElideInitialHeadLine, !Counter, !IO),
    BodyTree = node(NodeId, BodyTrees, no),

    Separators = leaf([
        message_separator,
        message_separator,
        message_separator
    ]),

    (
        Mode = include_replies,
        Replies = Message ^ m_replies,
        list.map_foldl2(make_message_tree(Mode, Cols), Replies, ReplyTrees,
            !Counter, !IO)
    ;
        Mode = toplevel_only,
        ReplyTrees = []
    ),

    SubTrees = [HeaderTree, BodyTree, Separators | ReplyTrees],
    Tree = node(NodeId, SubTrees, no).

:- pred add_header(string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_header(Header, Value, RevLines0, RevLines) :-
    RevLines = [header(Header, Value) | RevLines0].

:- pred maybe_add_header(string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

maybe_add_header(Header, Value, RevLines0, RevLines) :-
    ( empty_header_value(Value) ->
        RevLines = RevLines0
    ;
        add_header(Header, Value, RevLines0, RevLines)
    ).

:- pred make_part_tree(int::in, part::in, tree::out, bool::in, bool::out,
    counter::in, counter::out, io::di, io::uo) is det.

make_part_tree(Cols, Part, Tree, !ElideInitialHeadLine, !Counter, !IO) :-
    make_part_tree_with_alts(Cols, [], Part, Tree, !ElideInitialHeadLine,
        !Counter, !IO).

:- pred make_part_tree_with_alts(int::in, list(part)::in, part::in, tree::out,
    bool::in, bool::out, counter::in, counter::out, io::di, io::uo) is det.

make_part_tree_with_alts(Cols, AltParts, Part, Tree,
        !ElideInitialHeadLine, !Counter, !IO) :-
    Part = part(_MessageId, _PartId, Type, Content, _MaybeFilename,
        _MaybeEncoding, _MaybeLength),
    allocate_node_id(PartNodeId, !Counter),
    (
        Content = text(Text),
        make_text_lines(Cols, Text, Lines),
        fold_quote_blocks(Lines, TextTrees, !Counter),
        (
            !.ElideInitialHeadLine = yes,
            AltParts = []
        ->
            SubTrees = TextTrees
        ;
            HeadLine = part_head(Part, AltParts),
            SubTrees = [leaf([HeadLine]) | TextTrees]
        ),
        Tree = node(PartNodeId, SubTrees, yes),
        !:ElideInitialHeadLine = no
    ;
        Content = subparts(SubParts),
        (
            strcase_equal(Type, "multipart/alternative"),
            select_alternative(SubParts, [FirstSubPart | RestSubParts])
        ->
            make_part_tree_with_alts(Cols, RestSubParts, FirstSubPart, Tree,
                no, _ElideInitialHeadLine, !Counter, !IO)
        ;
            AltParts = [],
            hide_multipart_head_line(Type)
        ->
            list.map_foldl3(make_part_tree(Cols), SubParts, SubPartsTrees,
                !ElideInitialHeadLine, !Counter, !IO),
            Tree = node(PartNodeId, SubPartsTrees, no)
        ;
            list.map_foldl3(make_part_tree(Cols), SubParts, SubPartsTrees,
                yes, _ElideInitialHeadLine, !Counter, !IO),
            HeadLine = part_head(Part, AltParts),
            SubTrees = [leaf([HeadLine]) | SubPartsTrees],
            Tree = node(PartNodeId, SubTrees, yes),
            !:ElideInitialHeadLine = no
        )
    ;
        Content = encapsulated_messages(EncapMessages),
        list.map_foldl2(make_encapsulated_message_tree(Cols),
            EncapMessages, SubTrees0, !Counter, !IO),
        HeadLine = part_head(Part, AltParts),
        SubTrees = [leaf([HeadLine]) | SubTrees0],
        Tree = node(PartNodeId, SubTrees, yes),
        !:ElideInitialHeadLine = no
    ;
        ( Content = unsupported
        ; Content = unsupported_inline
        ),
        make_unsupported_part_tree(Cols, PartNodeId, Part, AltParts, Tree,
            !IO),
        !:ElideInitialHeadLine = no
    ).

:- pred select_alternative(list(part)::in, list(part)::out) is semidet.

select_alternative([X | Xs], Parts) :-
    % Some HTML emails have a first text/plain "alternative" which is
    % completely blank; skip those.
    (
        X ^ pt_content = text(XText),
        string.all_match(is_whitespace, XText)
    ->
        Parts = Xs ++ [X]
    ;
        Parts = [X | Xs]
    ).

:- pred hide_multipart_head_line(string::in) is semidet.

hide_multipart_head_line(PartType) :-
    (
        strcase_equal(PartType, "multipart/mixed")
    ;
        strcase_equal(PartType, "multipart/related")
    ;
        strcase_equal(PartType, "multipart/signed")
    ).

%-----------------------------------------------------------------------------%

:- type append_text_context
    --->    append_text_context(
                % Current quote level (for wrapped lines).
                atc_cur_quote_level     :: maybe(quote_level),
                % Current line type (for wrapped lines).
                atc_cur_line_type       :: maybe(text_type),
                % Known diffs at these quote levels.
                atc_diff_quote_levels   :: set(quote_level)
            ).

:- pred make_text_lines(int::in, string::in, list(pager_line)::out) is det.

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
    cord(pager_line)::in, cord(pager_line)::out) is det.

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
    cord(pager_line)::in, cord(pager_line)::out) is det.

maybe_append_substring(String, Start, End, !Context, !Lines) :-
    ( End > Start ->
        append_substring(String, Start, End, !Context, !Lines)
    ;
        true
    ).

:- pred append_substring(string::in, int::in, int::in,
    append_text_context::in, append_text_context::out,
    cord(pager_line)::in, cord(pager_line)::out) is det.

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
    Text = text(QuoteLevel, SubString, QuoteMarkerEnd, LineType),
    snoc(Text, !Lines),
    Context = append_text_context(yes(QuoteLevel), yes(LineType),
        DiffQuoteLevels).

%-----------------------------------------------------------------------------%

:- pred detect_quote_level(string::in, int::in, int::out, int::out) is det.

detect_quote_level(String, Pos, QuoteLevel, QuoteMarkerEnd) :-
    (
        string.unsafe_index_next(String, Pos, NextPos0, QuoteChar),
        QuoteChar = ('>')
    ->
        % Accept a single optional space after the quote character.
        ( string.unsafe_index_next(String, NextPos0, NextPos1, ' ') ->
            NextPos = NextPos1
        ;
            NextPos = NextPos0
        ),
        detect_quote_level(String, NextPos, QuoteLevel0, QuoteMarkerEnd),
        QuoteLevel = 1 + QuoteLevel0
    ;
        QuoteLevel = 0,
        QuoteMarkerEnd = Pos
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

:- pred fold_quote_blocks(list(pager_line)::in, list(tree)::out,
    counter::in, counter::out) is det.

fold_quote_blocks(Lines0, Trees, !Counter) :-
    (
        Lines0 = [],
        Trees = []
    ;
        Lines0 = [_ | _],
        list.takewhile(is_not_quoted_text, Lines0, NonQuotedLines, Lines1),
        list.takewhile(is_quoted_text, Lines1, QuotedLines, RestLines),
        list.length(QuotedLines, NumQuotedLines),
        (
            NumQuotedLines >= 2 * quoted_lines_threshold,
            list.split_list(NumQuotedLines - quoted_lines_threshold,
                QuotedLines, HiddenLines, ShownLines)
        ->
            allocate_node_id(NodeId, !Counter),
            FoldLine = fold_marker(HiddenLines, no),
            Trees = [
                leaf(NonQuotedLines),
                node(NodeId, [leaf([FoldLine])], no),
                leaf(ShownLines)
                | RestTrees
            ],
            fold_quote_blocks(RestLines, RestTrees, !Counter)
        ;
            Trees = [
                leaf(NonQuotedLines),
                leaf(QuotedLines)
                | RestTrees
            ],
            fold_quote_blocks(RestLines, RestTrees, !Counter)
        )
    ).

:- pred is_not_quoted_text(pager_line::in) is semidet.

is_not_quoted_text(Line) :-
    not is_quoted_text(Line).

:- pred is_quoted_text(pager_line::in) is semidet.

is_quoted_text(Line) :-
    require_complete_switch [Line]
    (
        Line = text(Level, _, _, _),
        Level > 0
    ;
        ( Line = start_message_header(_, _, _)
        ; Line = header(_, _)
        ; Line = part_head(_, _)
        ; Line = fold_marker(_, _)
        ; Line = message_separator
        ),
        fail
    ).

:- func quoted_lines_threshold = int.

quoted_lines_threshold = 3.

%-----------------------------------------------------------------------------%

:- pred make_encapsulated_message_tree(int::in, encapsulated_message::in,
    tree::out, counter::in, counter::out, io::di, io::uo) is det.

make_encapsulated_message_tree(Cols, EncapMessage, Tree, !Counter, !IO) :-
    EncapMessage = encapsulated_message(Headers, Body),
    allocate_node_id(NodeId, !Counter),
    some [!RevLines] (
        !:RevLines = [],
        add_encapsulated_header("Date", Headers ^ h_date, !RevLines),
        add_encapsulated_header("From", Headers ^ h_from, !RevLines),
        add_encapsulated_header("Subject", Headers ^ h_subject, !RevLines),
        add_encapsulated_header("To", Headers ^ h_to, !RevLines),
        add_encapsulated_header("Cc", Headers ^ h_cc, !RevLines),
        add_encapsulated_header("Reply-To", Headers ^ h_replyto, !RevLines),
        cons(blank_line, !RevLines),
        list.reverse(!.RevLines, HeaderLines)
    ),
    list.map_foldl3(make_part_tree(Cols), Body, PartTrees,
        yes, _ElideInitialHeadLine, !Counter, !IO),
    SubTrees = [leaf(HeaderLines) | PartTrees],
    PreBlank = no,
    Tree = node(NodeId, SubTrees, PreBlank).

:- pred add_encapsulated_header(string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_encapsulated_header(Header, Value, RevLines0, RevLines) :-
    (
        Value = header_value(ValueString)
    ;
        Value = decoded_unstructured(ValueString)
    ),
    ( ValueString = "" ->
        RevLines = RevLines0
    ;
        Line = text(0, Header ++ ": " ++ ValueString, 0, plain),
        RevLines = [Line | RevLines0]
    ).

%-----------------------------------------------------------------------------%

:- pred make_unsupported_part_tree(int::in, node_id::in, part::in,
    list(part)::in, tree::out, io::di, io::uo) is det.

make_unsupported_part_tree(Cols, PartNodeId, Part, AltParts, Tree, !IO) :-
    Part = part(MessageId, PartId, Type, Content, _MaybeFilename,
        _MaybeEncoding, _MaybeLength),
    % XXX we should use mailcap, though we don't want to show everything
    ( Content = unsupported_inline ->
        ( strcase_equal(Type, "text/html") ->
            get_maybe_html_dump_command(MaybeFilterCommand, !IO)
        ;
            MaybeFilterCommand = no
        ),
        expand_part(MessageId, PartId, MaybeFilterCommand, MaybeText, !IO)
    ;
        MaybeText = ok("")
    ),
    (
        MaybeText = ok(Text),
        make_text_lines(Cols, Text, TextLines)
    ;
        MaybeText = error(Error),
        make_text_lines(Cols, "(" ++ Error ++ ")", TextLines)
    ),
    HeadLine = part_head(Part, AltParts),
    Lines = [HeadLine | TextLines],
    Tree = node(PartNodeId, [leaf(Lines)], yes).

%-----------------------------------------------------------------------------%

:- func blank_line = pager_line.

blank_line = text(0, "", 0, plain).

%-----------------------------------------------------------------------------%

setup_pager_for_staging(Cols, Text, RetainPagerPos, Info) :-
    make_text_lines(Cols, Text, Lines0),
    Lines = Lines0 ++ [
        message_separator,
        message_separator,
        message_separator
    ],
    counter.init(0, Counter0),
    allocate_node_id(NodeId, Counter0, Counter),
    Tree = node(NodeId, [leaf(Lines)], no),
    Scrollable0 = scrollable.init(flatten(Tree)),
    Info0 = pager_info(Tree, Counter, Scrollable0),
    (
        RetainPagerPos = new_pager,
        Info = Info0
    ;
        RetainPagerPos = retain_pager_pos(OldPager, NumRows),
        % Make an attempt to retain the pager position.
        OldScrollable = OldPager ^ p_scrollable,
        Top = get_top(OldScrollable),
        scroll(NumRows, Top, _, Info0, Info)
    ).

%-----------------------------------------------------------------------------%

pager_input(NumRows, KeyCode, Action, MessageUpdate, !Info) :-
    ( key_binding(KeyCode, Binding) ->
        (
            Binding = leave_pager,
            Action = leave_pager,
            MessageUpdate = clear_message
        ;
            Binding = scroll_down,
            scroll(NumRows, 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = scroll_up,
            scroll(NumRows, -1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = page_down,
            scroll(NumRows, NumRows - 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = page_up,
            scroll(NumRows, -NumRows + 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = half_page_down,
            scroll(NumRows, NumRows//2, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = half_page_up,
            scroll(NumRows, -NumRows//2, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = home,
            scroll_home(MessageUpdate, !Info),
            Action = continue
        ;
            Binding = end,
            scroll_end(NumRows, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = next_message,
            next_message(MessageUpdate, !Info),
            Action = continue
        ;
            Binding = prev_message,
            prev_message(MessageUpdate, !Info),
            Action = continue
        ;
            Binding = skip_quoted_text,
            skip_quoted_text(MessageUpdate, !Info),
            Action = continue
        )
    ;
        Action = continue,
        MessageUpdate = no_change
    ).

:- pred key_binding(keycode::in, binding::out) is semidet.

key_binding(KeyCode, Binding) :-
    (
        KeyCode = char(Char),
        char_binding(Char, Binding)
    ;
        KeyCode = code(Code),
        ( Code = key_down ->
            Binding = scroll_down
        ; Code = key_up ->
            Binding = scroll_up
        ; Code = key_pagedown ->
            Binding = page_down
        ; Code = key_pageup ->
            Binding = page_up
        ; Code = key_home ->
            Binding = home
        ; Code = key_end ->
            Binding = end
        ;
            fail
        )
    ).

:- pred char_binding(char::in, binding::out) is semidet.

char_binding('i', leave_pager).
char_binding('\r', scroll_down).
char_binding('\\', scroll_up).
char_binding('\b', scroll_up).   % XXX doesn't work
char_binding(' ', page_down).
char_binding('b', page_up).
char_binding(']', half_page_down).
char_binding('[', half_page_up).
char_binding('j', next_message).
char_binding('k', prev_message).
char_binding('S', skip_quoted_text).

%-----------------------------------------------------------------------------%

scroll(NumRows, Delta, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    scroll(NumRows, Delta, HitLimit, Scrollable0, Scrollable),
    !Info ^ p_scrollable := Scrollable,
    (
        HitLimit = yes,
        ( Delta < 0 ->
            MessageUpdate = top_limit_message
        ;
            MessageUpdate = bottom_limit_message
        )
    ;
        HitLimit = no,
        MessageUpdate = clear_message
    ).

:- pred scroll_home(message_update::out, pager_info::in, pager_info::out)
    is det.

scroll_home(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = scrollable.get_top(Scrollable0),
    ( Top0 = 0 ->
        MessageUpdate = top_limit_message
    ;
        scrollable.set_top(0, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ).

:- pred scroll_end(int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

scroll_end(NumRows, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = scrollable.get_top(Scrollable0),
    NumLines = scrollable.get_num_lines(Scrollable0),
    Top = int.max(0, NumLines - NumRows),
    ( Top =< Top0 ->
        MessageUpdate = bottom_limit_message
    ;
        scrollable.set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ).

:- func top_limit_message = message_update.

top_limit_message = set_warning("Top of message is shown.").

:- func bottom_limit_message = message_update.

bottom_limit_message = set_warning("Bottom of message is shown.").

%-----------------------------------------------------------------------------%

scroll_but_stop_at_message(NumRows, Delta, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    (
        ( Delta > 0 ->
            Limit = Top0 + Delta,
            search_forward_limit(is_message_start, Scrollable0, Top0 + 1,
                Limit, MessageTop, _)
        ; Delta < 0 ->
            Limit = int.max(0, Top0 + Delta),
            search_reverse_limit(is_message_start, Scrollable0, Top0,
                Limit, MessageTop, _)
        ;
            fail
        )
    ->
        set_top(MessageTop, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ;
        scroll(NumRows, Delta, MessageUpdate, !Info)
    ).

next_message(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    ( search_forward(is_message_start, Scrollable0, Top0 + 1, Top, _) ->
        set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("Already at last message.")
    ).

prev_message(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    ( search_reverse(is_message_start, Scrollable0, Top0 - 1, Top) ->
        set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("Already at first message.")
    ).

goto_first_message(!Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    set_top(0, Scrollable0, Scrollable),
    !Info ^ p_scrollable := Scrollable.

goto_end(NumRows, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    NumLines = scrollable.get_num_lines(Scrollable0),
    ( search_reverse(is_message_start, Scrollable0, NumLines, TopMin) ->
        ( Top0 < TopMin ->
            Top = TopMin
        ;
            Top = max(TopMin, NumLines - NumRows)
        ),
        set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable
    ;
        true
    ).

:- pred is_message_start(id_pager_line::in) is semidet.

is_message_start(_Id - start_message_header(_, _, _)).

skip_quoted_text(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    (
        search_forward(is_quoted_text_or_message_start, Scrollable0,
            Top0 + 1, Top1, _),
        search_forward(is_not_quoted_text_id, Scrollable0, Top1, Top, _)
    ->
        set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable,
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No more quoted text.")
    ).

:- pred is_quoted_text_or_message_start(id_pager_line::in) is semidet.

is_quoted_text_or_message_start(_Id - Line) :-
    is_quoted_text_or_message_start_2(Line).

:- pred is_quoted_text_or_message_start_2(pager_line::in) is semidet.

is_quoted_text_or_message_start_2(Line) :-
    require_complete_switch [Line]
    (
        Line = start_message_header(_, _, _)
    ;
        Line = text(Level, _, _, _),
        Level > 0
    ;
        ( Line = header(_, _)
        ; Line = part_head(_, _)
        ; Line = fold_marker(_, _)
        ; Line = message_separator
        ),
        fail
    ).

:- pred is_not_quoted_text_id(id_pager_line::in) is semidet.

is_not_quoted_text_id(_Id - Line) :-
    is_not_quoted_text(Line).

%-----------------------------------------------------------------------------%

get_top_message(Info, Message) :-
    % XXX we could keep an array for binary search
    Scrollable = Info ^ p_scrollable,
    Top = get_top(Scrollable),
    Lines = get_lines(Scrollable),
    ( Top < version_array.size(Lines) ->
        get_top_message_2(Lines, Top, _, Message)
    ;
        fail
    ).

:- pred get_top_message_2(version_array(id_pager_line)::in, int::in, int::out,
    message::out) is semidet.

get_top_message_2(Lines, I, J, Message) :-
    ( I >= 0 ->
        version_array.lookup(Lines, I) = _Id - Line,
        ( Line = start_message_header(Message0, _, _) ->
            J = I,
            Message = Message0
        ;
            get_top_message_2(Lines, I - 1, J, Message)
        )
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

get_top_offset(Info, Offset) :-
    Scrollable = Info ^ p_scrollable,
    Top = get_top(Scrollable),
    Lines = get_lines(Scrollable),
    ( Top < version_array.size(Lines) ->
        get_top_message_2(Lines, Top, MessageLine, _Message),
        Offset = Top - MessageLine
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

skip_to_message(MessageId, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    ( search_forward(is_message_start(MessageId), Scrollable0, 0, Top, _) ->
        set_top(Top, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable
    ;
        true
    ).

:- pred is_message_start(message_id::in, id_pager_line::in)
    is semidet.

is_message_start(MessageId, _Id - Line) :-
    Line = start_message_header(Message, _, _),
    Message ^ m_id = MessageId.

%-----------------------------------------------------------------------------%

skip_to_search(NumRows, SearchKind, Search, SearchDir, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top0 = get_top(Scrollable0),
    Bot = Top0 + NumRows,
    choose_search_start(Scrollable0, Top0, Bot, SearchKind, SearchDir, Start),
    (
        search(id_pager_line_matches_search(Search), SearchDir, Scrollable0,
            Start, Cursor)
    ->
        (
            % Jump to the message containing the match, if it wasn't already.
            SearchDir = dir_forward,
            search_reverse(is_message_start, Scrollable0, Cursor + 1,
                NewMsgStart),
            search_reverse(is_message_start, Scrollable0, Top0 + 1,
                OldMsgStart),
            NewMsgStart \= OldMsgStart
        ->
            set_top(NewMsgStart, Scrollable0, Scrollable1)
        ;
            Scrollable1 = Scrollable0
        ),
        set_cursor_visible(Cursor, NumRows, Scrollable1, Scrollable),
        MessageUpdate = clear_message
    ;
        set_cursor_none(Scrollable0, Scrollable),
        MessageUpdate = set_warning("Not found.")
    ),
    !Info ^ p_scrollable := Scrollable.

:- pred choose_search_start(scrollable(T)::in, int::in, int::in,
    search_kind::in, search_direction::in, int::out) is det.

choose_search_start(Scrollable, Top, Bot, new_search, SearchDir, Start) :-
    (
        SearchDir = dir_forward,
        Start = Top
    ;
        SearchDir = dir_reverse,
        Start = min(Bot, get_num_lines(Scrollable))
    ).
choose_search_start(Scrollable, Top, Bot, continue_search, SearchDir, Start) :-
    (
        get_cursor(Scrollable, Cursor),
        Cursor >= Top,
        Cursor < Bot
    ->
        (
            SearchDir = dir_forward,
            Start = Cursor + 1
        ;
            SearchDir = dir_reverse,
            Start = Cursor
        )
    ;
        choose_search_start(Scrollable, Top, Bot, new_search, SearchDir, Start)
    ).

:- pred id_pager_line_matches_search(string::in, id_pager_line::in) is semidet.

id_pager_line_matches_search(Search, _Id - Line) :-
    line_matches_search(Search, Line).

:- pred line_matches_search(string::in, pager_line::in) is semidet.

line_matches_search(Search, Line) :-
    require_complete_switch [Line]
    (
        Line = start_message_header(Message, _, Value),
        (
            String = header_value_string(Value),
            strcase_str(String, Search)
        ;
            % XXX this won't match current tags
            Tags = Message ^ m_tags,
            set.member(tag(TagName), Tags),
            strcase_str(TagName, Search)
        )
    ;
        Line = header(_, Value),
        String = header_value_string(Value),
        strcase_str(String, Search)
    ;
        Line = text(_, String, _, _),
        strcase_str(String, Search)
    ;
        Line = part_head(Part, _),
        (
            Part ^ pt_type = Type,
            strcase_str(Type, Search)
        ;
            Part ^ pt_filename = yes(FileName),
            strcase_str(FileName, Search)
        )
    ;
        Line = fold_marker(_, _),
        fail
    ;
        Line = message_separator,
        fail
    ).

%-----------------------------------------------------------------------------%

highlight_minor(NumRows, MessageUpdate, !Info) :-
    ( do_highlight(is_highlightable_minor, NumRows, !Info) ->
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No attachment or URL visible.")
    ).

highlight_major(NumRows, MessageUpdate, !Info) :-
    ( do_highlight(is_highlightable_major, NumRows, !Info) ->
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No attachment or top of message visible.")
    ).

:- pred do_highlight(pred(id_pager_line)::in(pred(in) is semidet), int::in,
    pager_info::in, pager_info::out) is semidet.

do_highlight(Highlightable, NumRows, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Top = get_top(Scrollable0),
    Bot = Top + NumRows,
    (
        get_cursor(Scrollable0, Cur0),
        Cur0 >= Top,
        Cur0 < Bot
    ->
        Start = Cur0 + 1
    ;
        Start = Top
    ),
    ( search_forward_limit(Highlightable, Scrollable0, Start, Bot, Cur, _) ->
        set_cursor(Cur, Scrollable0, Scrollable)
    ; search_forward_limit(Highlightable, Scrollable0, Top, Bot, Cur, _) ->
        set_cursor(Cur, Scrollable0, Scrollable)
    ;
        fail
    ),
    !Info ^ p_scrollable := Scrollable.

:- pred is_highlightable_minor(id_pager_line::in) is semidet.

is_highlightable_minor(_Id - Line) :-
    ( Line = part_head(_, _)
    ; Line = text(_, _, _, url(_, _))
    ; Line = fold_marker(_, _)
    ).

:- pred is_highlightable_major(id_pager_line::in) is semidet.

is_highlightable_major(_Id - Line) :-
    ( Line = start_message_header(_, _, _)
    ; Line = part_head(_, _)
    ).

get_highlighted_thing(Info, Thing) :-
    Scrollable = Info ^ p_scrollable,
    get_cursor_line(Scrollable, _, _NodeId - Line),
    require_complete_switch [Line]
    (
        Line = start_message_header(Message, _, _),
        MessageId = Message ^ m_id,
        Subject = Message ^ m_headers ^ h_subject,
        Part = part(MessageId, 0, "text/plain", unsupported, no, no, no),
        MaybeSubject = yes(Subject),
        Thing = highlighted_part(Part, MaybeSubject)
    ;
        Line = part_head(Part, _),
        MaybeSubject = no,
        Thing = highlighted_part(Part, MaybeSubject)
    ;
        Line = text(_, String, _, url(Start, End)),
        string.between(String, Start, End, Url),
        Thing = highlighted_url(Url)
    ;
        Line = fold_marker(_, _),
        Thing = highlighted_fold_marker
    ;
        Line = header(_, _),
        fail
    ;
        Line = message_separator,
        fail
    ).

%-----------------------------------------------------------------------------%

toggle_content(Cols, MessageUpdate, Info0, Info, !IO) :-
    Scrollable0 = Info0 ^ p_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, IdLine) ->
        toggle_line(Cols, IdLine, MessageUpdate, Info0, Info, !IO)
    ;
        Info = Info0,
        MessageUpdate = clear_message
    ).

:- pred toggle_line(int::in, id_pager_line::in, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

toggle_line(Cols, NodeId - Line, MessageUpdate, Info0, Info, !IO) :-
    (
        Line = part_head(_, _),
        toggle_part(Cols, NodeId, Line, MessageUpdate, Info0, Info, !IO)
    ;
        Line = fold_marker(_, _),
        toggle_folding(NodeId, Line, Info0, Info),
        MessageUpdate = clear_message
    ;
        ( Line = start_message_header(_, _, _)
        ; Line = header(_, _)
        ; Line = text(_, _, _, _)
        ; Line = message_separator
        ),
        Info = Info0,
        MessageUpdate = clear_message
    ).

:- inst part_head
    --->    part_head(ground, ground).

:- pred toggle_part(int::in, node_id::in, pager_line::in(part_head),
    message_update::out, pager_info::in, pager_info::out, io::di, io::uo)
    is det.

toggle_part(Cols, NodeId, Line, MessageUpdate, Info0, Info, !IO) :-
    Line = part_head(Part0, HiddenParts0),
    Info0 = pager_info(Tree0, Counter0, Scrollable0),
    ( cycle(Part0, HiddenParts0, Part, HiddenParts, MessageUpdatePrime) ->
        make_part_tree_with_alts(Cols, HiddenParts, Part, NewNode, no,
            _ElideInitialHeadLine, Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree),
        scrollable.reinit(flatten(Tree), Scrollable0, Scrollable),
        Info = pager_info(Tree, Counter, Scrollable),
        MessageUpdate = MessageUpdatePrime
    ;
        Info = Info0,
        MessageUpdate = set_warning("Part has no alternatives.")
    ).

:- pred cycle(part::in, list(part)::in, part::out, list(part)::out,
    message_update::out) is semidet.

cycle(Part0, HiddenParts0, Part, HiddenParts, MessageUpdate) :-
    HiddenParts0 = [Part | HiddenParts1],
    HiddenParts = HiddenParts1 ++ [Part0],
    Type = Part ^ pt_type,
    MessageUpdate = set_info("Showing " ++ Type ++ " alternative.").

cycle(Part0, HiddenParts0, Part, HiddenParts, MessageUpdate) :-
    HiddenParts0 = [],
    HiddenParts = [],
    Content0 = Part0 ^ pt_content,
    toggle_inline(Content0, Content, Message),
    Part = Part0 ^ pt_content := Content,
    MessageUpdate = set_info(Message).

:- pred toggle_inline(part_content::in, part_content::out, string::out)
    is semidet.

toggle_inline(unsupported, unsupported_inline, "Showing part.").
toggle_inline(unsupported_inline, unsupported, "Part hidden.").

:- inst fold_marker
    --->    fold_marker(ground, ground).

:- pred toggle_folding(node_id::in, pager_line::in(fold_marker),
    pager_info::in, pager_info::out) is det.

toggle_folding(NodeId, Line, Info0, Info) :-
    Line = fold_marker(Content, Expanded0),
    (
        Expanded0 = no,
        FoldLine = fold_marker(Content, yes),
        SubTree = leaf([FoldLine | Content])
    ;
        Expanded0 = yes,
        FoldLine = fold_marker(Content, no),
        SubTree = leaf([FoldLine])
    ),
    NewNode = node(NodeId, [SubTree], no),

    Info0 = pager_info(Tree0, Counter, Scrollable0),
    replace_node(NodeId, NewNode, Tree0, Tree),
    scrollable.reinit(flatten(Tree), Scrollable0, Scrollable),
    Info = pager_info(Tree, Counter, Scrollable).

%-----------------------------------------------------------------------------%

draw_pager(Screen, Info, !IO) :-
    get_main_panels(Screen, MainPanels),
    draw_pager_lines(MainPanels, Info, !IO).

draw_pager_lines(Panels, Info, !IO) :-
    Scrollable = Info ^ p_scrollable,
    scrollable.draw(Panels, Scrollable, !IO).

:- pred draw_id_pager_line(panel::in, id_pager_line::in, int::in, bool::in,
    io::di, io::uo) is det.

draw_id_pager_line(Panel, _Id - Line, _LineNr, IsCursor, !IO) :-
    draw_pager_line(Panel, Line, IsCursor, !IO).

:- pred draw_pager_line(panel::in, pager_line::in, bool::in, io::di, io::uo)
    is det.

draw_pager_line(Panel, Line, IsCursor, !IO) :-
    (
        ( Line = start_message_header(_Message, Header, Value)
        ; Line = header(Header, Value)
        ),
        panel.attr_set(Panel, fg_bg(red, default) + bold, !IO),
        my_addstr(Panel, "| ", !IO),
        my_addstr(Panel, Header, !IO),
        my_addstr(Panel, ": ", !IO),
        (
            IsCursor = yes,
            Attr = reverse
        ;
            IsCursor = no,
            ( Header = "Subject" ->
                Attr = bold
            ;
                Attr = normal
            )
        ),
        panel.attr_set(Panel, Attr, !IO),
        draw_header_value(Panel, Value, !IO)
    ;
        Line = text(QuoteLevel, Text, QuoteMarkerEnd, TextType),
        Attr0 = quote_level_to_attr(QuoteLevel),
        (
            IsCursor = yes,
            Attr1 = reverse
        ;
            IsCursor = no,
            Attr1 = normal
        ),
        (
            TextType = plain,
            panel.attr_set(Panel, Attr0 + Attr1, !IO),
            my_addstr(Panel, Text, !IO)
        ;
            TextType = diff(DiffLine),
            DiffAttr = diff_line_to_attr(DiffLine),
            ( QuoteMarkerEnd = 0 ->
                panel.attr_set(Panel, DiffAttr + Attr1, !IO),
                my_addstr(Panel, Text, !IO)
            ;
                End = string.length(Text),
                panel.attr_set(Panel, Attr0 + Attr1, !IO),
                my_addstr(Panel, string.between(Text, 0, QuoteMarkerEnd), !IO),
                panel.attr_set(Panel, DiffAttr + Attr1, !IO),
                my_addstr(Panel, string.between(Text, QuoteMarkerEnd, End), !IO)
            )
        ;
            TextType = url(UrlStart, UrlEnd),
            End = string.length(Text),
            panel.attr_set(Panel, Attr0 + Attr1, !IO),
            my_addstr(Panel, string.between(Text, 0, UrlStart), !IO),
            panel.attr_set(Panel, fg_bg(magenta, default) + Attr1, !IO),
            my_addstr(Panel, string.between(Text, UrlStart, UrlEnd), !IO),
            panel.attr_set(Panel, Attr0 + Attr1, !IO),
            my_addstr(Panel, string.between(Text, UrlEnd, End), !IO)
        )
    ;
        Line = part_head(Part, HiddenParts),
        Part = part(_MessageId, _Part, ContentType, _Content, MaybeFilename,
            MaybeEncoding, MaybeLength),
        (
            IsCursor = yes,
            Attr = fg_bg(magenta, default) + reverse
        ;
            IsCursor = no,
            Attr = fg_bg(magenta, default) + bold
        ),
        panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, "[-- ", !IO),
        my_addstr(Panel, ContentType, !IO),
        (
            MaybeFilename = yes(Filename),
            my_addstr(Panel, "; ", !IO),
            my_addstr(Panel, Filename, !IO)
        ;
            MaybeFilename = no
        ),
        (
            MaybeLength = yes(Length),
            DecodedLength = decoded_length(MaybeEncoding, Length),
            my_addstr(Panel, format_length(DecodedLength), !IO)
        ;
            MaybeLength = no
        ),
        my_addstr(Panel, " --]", !IO),
        ( toggle_part_message(Part, HiddenParts, AltMessage) ->
            panel.attr_set(Panel, fg_bg(magenta, default), !IO),
            my_addstr(Panel, AltMessage, !IO)
        ;
            true
        )
    ;
        Line = fold_marker(_, _),
        (
            IsCursor = yes,
            Attr = fg_bg(magenta, default) + reverse
        ;
            IsCursor = no,
            Attr = fg_bg(magenta, default)
        ),
        panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, "...", !IO)
    ;
        Line = message_separator,
        panel.attr_set(Panel, fg_bg(blue, default) + bold, !IO),
        my_addstr(Panel, "~", !IO)
    ).

:- func quote_level_to_attr(quote_level) = attr.

quote_level_to_attr(QuoteLevel) = Attr :-
    ( QuoteLevel = 0 ->
        Attr = normal
    ; int.odd(QuoteLevel) ->
        Attr = fg_bg(blue, default) + bold
    ;
        Attr = fg_bg(green, default)
    ).

:- func diff_line_to_attr(diff_line) = attr.

diff_line_to_attr(diff_common) = normal.
diff_line_to_attr(diff_add) = fg_bg(cyan, default) + bold.
diff_line_to_attr(diff_rem) = fg_bg(red, default) + bold.
diff_line_to_attr(diff_hunk) = fg_bg(yellow, default) + bold.
diff_line_to_attr(diff_index) = fg_bg(green, default) + bold.

:- func decoded_length(maybe(string), int) = int.

decoded_length(MaybeEncoding, Length) =
    ( MaybeEncoding = yes("base64") ->
        Length * 3 / 4
    ;
        Length
    ).

:- func format_length(int) = string.

format_length(Size) = String :-
    ( Size = 0 ->
        String = " (0 bytes)"
    ; Size =< 1000000 ->
        Ks = float(Size) / 1000.0,
        String = format(" (%.1f kB)", [f(Ks)])
    ;
        Ms = float(Size) / 1000000.0,
        String = format(" (%.1f MB)", [f(Ms)])
    ).

:- pred toggle_part_message(part::in, list(part)::in, string::out) is semidet.

toggle_part_message(Part, HiddenParts, Message) :-
    (
        HiddenParts = [],
        Content = Part ^ pt_content,
        (
            Content = unsupported,
            Message = "  z to show"
        ;
            Content = unsupported_inline,
            Message = "  z to hide"
        )
    ;
        HiddenParts = [_],
        Message = "  z for alternative"
    ;
        HiddenParts = [_, _ | _],
        Message = "  z for alternatives"
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
