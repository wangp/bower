% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module pager.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module color.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module prog_config.
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

:- pred setup_pager(prog_config::in, setup_mode::in, int::in,
    list(message)::in, pager_info::out, io::di, io::uo) is det.

:- pred setup_pager_for_staging(prog_config::in, int::in, string::in,
    retain_pager_pos::in, pager_info::out) is det.

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

:- type toggle_type
    --->    cycle_alternatives
    ;       toggle_expanded.

:- pred toggle_content(toggle_type::in, int::in, int::in, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

:- pred draw_pager(pager_attrs::in, screen::in, pager_info::in, io::di, io::uo)
    is det.

:- pred draw_pager_lines(pager_attrs::in, list(panel)::in, pager_info::in,
    io::di, io::uo) is det.

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
:- import_module fold_lines.
:- import_module pager_text.
:- import_module quote_arg.
:- import_module string_util.

%-----------------------------------------------------------------------------%

:- type pager_info
    --->    pager_info(
                p_config        :: prog_config,
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
    --->    header(
                start_message   :: maybe(message),
                continue        :: bool,
                name            :: string,
                value           :: string % folded (wrapped)
            )
    ;       text(pager_text)
    ;       part_head(
                part            :: part,
                alternatives    :: list(part),
                % Hidden alternatives for multipart/alternative.
                part_expanded   :: part_expanded
            )
    ;       fold_marker(
                content         :: list(pager_line),
                expanded        :: bool
            )
    ;       message_separator.

:- type part_expanded
    --->    part_expanded
    ;       part_not_expanded.

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

is_blank_line(text(pager_text(_, "", _, _))).
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

setup_pager(Config, Mode, Cols, Messages, Info, !IO) :-
    counter.init(0, Counter0),
    allocate_node_id(NodeId, Counter0, Counter1),
    list.map_foldl2(make_message_tree(Config, Mode, Cols), Messages, Trees,
        Counter1, Counter, !IO),
    Tree = node(NodeId, Trees, no),
    Scrollable = scrollable.init(flatten(Tree)),
    Info = pager_info(Config, Tree, Counter, Scrollable).

:- pred make_message_tree(prog_config::in, setup_mode::in, int::in,
    message::in, tree::out, counter::in, counter::out, io::di, io::uo) is det.

make_message_tree(Config, Mode, Cols, Message, Tree, !Counter, !IO) :-
    Headers = Message ^ m_headers,
    counter.allocate(NodeIdInt, !Counter),
    NodeId = node_id(NodeIdInt),

    some [!RevLines] (
        !:RevLines = [],
        add_header(yes(Message), Cols, "Date", Headers ^ h_date, !RevLines),
        add_header(no, Cols, "From", Headers ^ h_from, !RevLines),
        add_header(no, Cols, "Subject", Headers ^ h_subject, !RevLines),
        add_header(no, Cols, "To", Headers ^ h_to, !RevLines),
        maybe_add_header(Cols, "Cc", Headers ^ h_cc, !RevLines),
        maybe_add_header(Cols, "Reply-To", Headers ^ h_replyto, !RevLines),
        cons(blank_line, !RevLines),
        HeaderTree = leaf(list.reverse(!.RevLines))
    ),

    Body = Message ^ m_body,
    list.map_foldl3(make_part_tree(Config, Cols), Body, BodyTrees,
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
        list.map_foldl2(make_message_tree(Config, Mode, Cols),
            Replies, ReplyTrees, !Counter, !IO)
    ;
        Mode = toplevel_only,
        ReplyTrees = []
    ),

    SubTrees = [HeaderTree, BodyTree, Separators | ReplyTrees],
    Tree = node(NodeId, SubTrees, no).

make_message_tree(Config, Mode, Cols, Message, Tree, !Counter, !IO) :-
    Message = excluded_message(Replies),
    counter.allocate(NodeIdInt, !Counter),
    NodeId = node_id(NodeIdInt),
    (
        Mode = include_replies,
        list.map_foldl2(make_message_tree(Config, Mode, Cols),
            Replies, ReplyTrees, !Counter, !IO)
    ;
        Mode = toplevel_only,
        ReplyTrees = []
    ),
    Tree = node(NodeId, ReplyTrees, no).

:- pred add_header(maybe(message)::in, int::in, string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_header(StartMessage, Cols, Name, Value, !RevLines) :-
    % 2 extra columns for "Name: "
    RemainCols = max(0, Cols - string_wcwidth(Name) - 2),
    get_spans_by_whitespace(header_value_string(Value), Spans),
    fill_lines(RemainCols, Spans, Folded),
    (
        Folded = [],
        FoldedHead = "",
        FoldedTail = []
    ;
        Folded = [FoldedHead | FoldedTail]
    ),
    cons(header(StartMessage, no, Name, FoldedHead), !RevLines),
    foldl(add_header_cont(Name), FoldedTail, !RevLines).

:- pred add_header_cont(string::in, string::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_header_cont(Name, Value, !RevLines) :-
    cons(header(no, yes, Name, Value), !RevLines).

:- pred maybe_add_header(int::in, string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

maybe_add_header(Cols, Header, Value, !RevLines) :-
    ( empty_header_value(Value) ->
        true
    ;
        add_header(no, Cols, Header, Value, !RevLines)
    ).

:- pred make_part_tree(prog_config::in, int::in, part::in, tree::out,
    bool::in, bool::out, counter::in, counter::out, io::di, io::uo) is det.

make_part_tree(Config, Cols, Part, Tree, !ElideInitialHeadLine, !Counter, !IO)
        :-
    make_part_tree_with_alts(Config, Cols, [], Part, default, Tree,
        !ElideInitialHeadLine, !Counter, !IO).

:- pred make_part_tree_with_alts(prog_config::in, int::in, list(part)::in,
    part::in, expand_unsupported::in, tree::out, bool::in, bool::out,
    counter::in, counter::out, io::di, io::uo) is det.

make_part_tree_with_alts(Config, Cols, AltParts, Part, ExpandUnsupported, Tree,
        !ElideInitialHeadLine, !Counter, !IO) :-
    Part = part(_MessageId, _PartId, Type, Content, _MaybeFilename,
        _MaybeEncoding, _MaybeLength),
    allocate_node_id(PartNodeId, !Counter),
    (
        Content = text(Text),
        make_text_lines(Cols, Text, Lines0),
        list.map(wrap_text, Lines0) = Lines,
        fold_quote_blocks(Lines, TextTrees, !Counter),
        (
            !.ElideInitialHeadLine = yes,
            AltParts = []
        ->
            SubTrees = TextTrees
        ;
            HeadLine = part_head(Part, AltParts, part_expanded),
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
            make_part_tree_with_alts(Config, Cols, RestSubParts, FirstSubPart,
                default, Tree, no, _ElideInitialHeadLine, !Counter, !IO)
        ;
            AltParts = [],
            hide_multipart_head_line(Type)
        ->
            list.map_foldl3(make_part_tree(Config, Cols), SubParts,
                SubPartsTrees, !ElideInitialHeadLine, !Counter, !IO),
            Tree = node(PartNodeId, SubPartsTrees, no)
        ;
            list.map_foldl3(make_part_tree(Config, Cols), SubParts,
                SubPartsTrees, yes, _ElideInitialHeadLine, !Counter, !IO),
            HeadLine = part_head(Part, AltParts, part_expanded),
            SubTrees = [leaf([HeadLine]) | SubPartsTrees],
            Tree = node(PartNodeId, SubTrees, yes),
            !:ElideInitialHeadLine = no
        )
    ;
        Content = encapsulated_messages(EncapMessages),
        list.map_foldl2(make_encapsulated_message_tree(Config, Cols),
            EncapMessages, SubTrees0, !Counter, !IO),
        HeadLine = part_head(Part, AltParts, part_expanded),
        SubTrees = [leaf([HeadLine]) | SubTrees0],
        Tree = node(PartNodeId, SubTrees, yes),
        !:ElideInitialHeadLine = no
    ;
        Content = unsupported,
        make_unsupported_part_tree(Config, Cols, PartNodeId, Part,
            ExpandUnsupported, AltParts, Tree, !IO),
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
        Line = text(pager_text(Level, _, _, _)),
        Level > 0
    ;
        ( Line = header(_, _, _, _)
        ; Line = part_head(_, _, _)
        ; Line = fold_marker(_, _)
        ; Line = message_separator
        ),
        fail
    ).

:- func quoted_lines_threshold = int.

quoted_lines_threshold = 3.

%-----------------------------------------------------------------------------%

:- pred make_encapsulated_message_tree(prog_config::in, int::in,
    encapsulated_message::in, tree::out, counter::in, counter::out,
    io::di, io::uo) is det.

make_encapsulated_message_tree(Config, Cols, EncapMessage, Tree, !Counter, !IO)
        :-
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
    list.map_foldl3(make_part_tree(Config, Cols), Body, PartTrees,
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
        Line = text(pager_text(0, Header ++ ": " ++ ValueString, 0, plain)),
        RevLines = [Line | RevLines0]
    ).

%-----------------------------------------------------------------------------%

:- type expand_unsupported
    --->    default
    ;       force(part_expanded).

:- pred make_unsupported_part_tree(prog_config::in, int::in, node_id::in,
    part::in, expand_unsupported::in, list(part)::in, tree::out,
    io::di, io::uo) is det.

make_unsupported_part_tree(Config, Cols, PartNodeId, Part, ExpandUnsupported,
        AltParts, Tree, !IO) :-
    Part = part(MessageId, MaybePartId, Type, _Content, _MaybeFilename,
        _MaybeEncoding, _MaybeLength),
    % XXX we should use mailcap, though we don't want to show everything
    IsHtml = ( strcase_equal(Type, "text/html") -> yes ; no ),
    (
        ExpandUnsupported = default,
        Expanded = expand_unsupported_default(IsHtml)
    ;
        ExpandUnsupported = force(Expanded)
    ),
    (
        Expanded = part_expanded,
        MaybePartId = yes(PartId),
        MaybeFilterCommand = maybe_filter_command(Config, IsHtml),
        expand_part(Config, MessageId, PartId, MaybeFilterCommand,
            MaybeText, !IO),
        (
            MaybeText = ok(Text),
            make_text_lines(Cols, Text, TextLines)
        ;
            MaybeText = error(Error),
            make_text_lines(Cols, "(" ++ Error ++ ")", TextLines)
        )
    ;
        Expanded = part_expanded,
        MaybePartId = no,
        make_text_lines(Cols, "(no part id)", TextLines)
    ;
        Expanded = part_not_expanded,
        TextLines = []
    ),
    HeadLine = part_head(Part, AltParts, Expanded),
    Lines = [HeadLine | wrap_texts(TextLines)],
    Tree = node(PartNodeId, [leaf(Lines)], yes).

:- func expand_unsupported_default(bool) = part_expanded.

expand_unsupported_default(IsHtml) = Expanded :-
    (
        IsHtml = yes,
        Expanded = part_expanded
    ;
        IsHtml = no,
        Expanded = part_not_expanded
    ).

:- func maybe_filter_command(prog_config, bool) = maybe(command_prefix).

maybe_filter_command(Config, IsHtml) = MaybeFilterCommand :-
    (
        IsHtml = yes,
        get_maybe_html_dump_command(Config, MaybeFilterCommand)
    ;
        IsHtml = no,
        MaybeFilterCommand = no
    ).

%-----------------------------------------------------------------------------%

:- func blank_line = pager_line.

blank_line = text(pager_text(0, "", 0, plain)).

:- func wrap_texts(list(pager_text)) = list(pager_line).

wrap_texts(Texts) = map(wrap_text, Texts).

:- func wrap_text(pager_text) = pager_line.

wrap_text(Text) = text(Text).

%-----------------------------------------------------------------------------%

setup_pager_for_staging(Config, Cols, Text, RetainPagerPos, Info) :-
    make_text_lines(Cols, Text, Lines0),
    Lines = wrap_texts(Lines0) ++ [
        message_separator,
        message_separator,
        message_separator
    ],
    counter.init(0, Counter0),
    allocate_node_id(NodeId, Counter0, Counter),
    Tree = node(NodeId, [leaf(Lines)], no),
    Scrollable0 = scrollable.init(flatten(Tree)),
    Info0 = pager_info(Config, Tree, Counter, Scrollable0),
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

is_message_start(_Id - header(yes(_), _, _, _)).

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
        Line = header(yes(_), _, _, _)
    ;
        Line = text(pager_text(Level, _, _, _)),
        Level > 0
    ;
        ( Line = header(no, _, _, _)
        ; Line = part_head(_, _, _)
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
        ( Line = header(yes(Message0), _, _, _) ->
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
    Line = header(yes(Message), _, _, _),
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
        Line = header(StartMessage, _Cont, _, String),
        (
            strcase_str(String, Search)
        ;
            % XXX this won't match current tags
            StartMessage = yes(Message),
            Tags = Message ^ m_tags,
            set.member(tag(TagName), Tags),
            strcase_str(TagName, Search)
        )
    ;
        Line = text(pager_text(_, String, _, _)),
        strcase_str(String, Search)
    ;
        Line = part_head(Part, _, _),
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
    ( Line = part_head(_, _, _)
    ; Line = text(pager_text(_, _, _, url(_, _)))
    ; Line = fold_marker(_, _)
    ).

:- pred is_highlightable_major(id_pager_line::in) is semidet.

is_highlightable_major(_Id - Line) :-
    ( Line = header(yes(_), _, _, _)
    ; Line = part_head(_, _, _)
    ).

get_highlighted_thing(Info, Thing) :-
    Scrollable = Info ^ p_scrollable,
    get_cursor_line(Scrollable, _, _NodeId - Line),
    require_complete_switch [Line]
    (
        Line = header(yes(Message), _, _, _),
        MessageId = Message ^ m_id,
        Subject = Message ^ m_headers ^ h_subject,
        PartId = 0,
        Part = part(MessageId, yes(PartId), "text/plain", unsupported, no, no,
            no),
        MaybeSubject = yes(Subject),
        Thing = highlighted_part(Part, MaybeSubject)
    ;
        Line = part_head(Part, _, _),
        MaybeSubject = no,
        Thing = highlighted_part(Part, MaybeSubject)
    ;
        Line = text(pager_text(_, String, _, url(Start, End))),
        string.between(String, Start, End, Url),
        Thing = highlighted_url(Url)
    ;
        Line = fold_marker(_, _),
        Thing = highlighted_fold_marker
    ;
        Line = header(no, _, _, _),
        fail
    ;
        Line = message_separator,
        fail
    ).

%-----------------------------------------------------------------------------%

toggle_content(ToggleType, NumRows, Cols, MessageUpdate, !Info, !IO) :-
    Scrollable0 = !.Info ^ p_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, IdLine) ->
        toggle_line(ToggleType, NumRows, Cols, IdLine, MessageUpdate,
            !Info, !IO)
    ;
        MessageUpdate = clear_message
    ).

:- pred toggle_line(toggle_type::in, int::in, int::in, id_pager_line::in,
    message_update::out, pager_info::in, pager_info::out, io::di, io::uo)
    is det.

toggle_line(ToggleType, NumRows, Cols, NodeId - Line, MessageUpdate,
        !Info, !IO) :-
    (
        Line = part_head(_, _, _),
        toggle_part(ToggleType, NumRows, Cols, NodeId, Line, MessageUpdate,
            !Info, !IO)
    ;
        Line = fold_marker(_, _),
        toggle_folding(NumRows, NodeId, Line, !Info),
        MessageUpdate = clear_message
    ;
        ( Line = header(_, _, _, _)
        ; Line = text(pager_text(_, _, _, _))
        ; Line = message_separator
        ),
        MessageUpdate = clear_message
    ).

:- inst part_head
    --->    part_head(ground, ground, ground).

:- pred toggle_part(toggle_type::in, int::in, int::in, node_id::in,
    pager_line::in(part_head), message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

toggle_part(ToggleType, NumRows, Cols, NodeId, Line, MessageUpdate,
        Info0, Info, !IO) :-
    Line = part_head(Part0, HiddenParts0, Expanded0),
    (
        ToggleType = cycle_alternatives,
        Expanded0 = part_expanded,
        HiddenParts0 = [Part | HiddenParts1]
    ->
        HiddenParts = HiddenParts1 ++ [Part0],
        Info0 = pager_info(Config, Tree0, Counter0, Scrollable0),
        make_part_tree_with_alts(Config, Cols, HiddenParts, Part,
            force(Expanded0), NewNode, no, _ElideInitialHeadLine,
            Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree),
        scrollable.reinit(flatten(Tree), NumRows, Scrollable0, Scrollable),
        Info = pager_info(Config, Tree, Counter, Scrollable),
        Type = Part ^ pt_type,
        MessageUpdate = set_info("Showing " ++ Type ++ " alternative.")
    ;
        % Fallback.
        toggle_part_expanded(NumRows, Cols, NodeId, Line, MessageUpdate,
            Info0, Info, !IO)
    ).

:- pred toggle_part_expanded(int::in, int::in, node_id::in,
    pager_line::in(part_head), message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

toggle_part_expanded(NumRows, Cols, NodeId, Line0, MessageUpdate, Info0, Info,
        !IO) :-
    Line0 = part_head(Part, HiddenParts, Expanded0),
    (
        Expanded0 = part_expanded,
        Expanded = part_not_expanded,
        MessageUpdate = set_info("Part hidden.")
    ;
        Expanded0 = part_not_expanded,
        Expanded = part_expanded,
        MessageUpdate = set_info("Showing part.")
    ),

    Info0 = pager_info(Config, Tree0, Counter0, Scrollable0),
    (
        Expanded = part_expanded,
        make_part_tree_with_alts(Config, Cols, HiddenParts, Part,
            force(Expanded), NewNode, no, _ElideInitialHeadLine,
            Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree)
    ;
        Expanded = part_not_expanded,
        Line = part_head(Part, HiddenParts, part_not_expanded),
        NewNode = node(NodeId, [leaf([Line])], yes),
        replace_node(NodeId, NewNode, Tree0, Tree),
        Counter = Counter0
    ),
    scrollable.reinit(flatten(Tree), NumRows, Scrollable0, Scrollable),
    Info = pager_info(Config, Tree, Counter, Scrollable).

:- inst fold_marker
    --->    fold_marker(ground, ground).

:- pred toggle_folding(int::in, node_id::in, pager_line::in(fold_marker),
    pager_info::in, pager_info::out) is det.

toggle_folding(NumRows, NodeId, Line, Info0, Info) :-
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

    Info0 = pager_info(Config, Tree0, Counter, Scrollable0),
    replace_node(NodeId, NewNode, Tree0, Tree),
    scrollable.reinit(flatten(Tree), NumRows, Scrollable0, Scrollable),
    Info = pager_info(Config, Tree, Counter, Scrollable).

%-----------------------------------------------------------------------------%

draw_pager(Attrs, Screen, Info, !IO) :-
    get_main_panels(Screen, MainPanels),
    draw_pager_lines(Attrs, MainPanels, Info, !IO).

draw_pager_lines(Attrs, Panels, Info, !IO) :-
    Scrollable = Info ^ p_scrollable,
    scrollable.draw(draw_id_pager_line(Attrs), Panels, Scrollable, !IO).

:- pred draw_id_pager_line(pager_attrs::in, panel::in, id_pager_line::in,
    int::in, bool::in, io::di, io::uo) is det.

draw_id_pager_line(Attrs, Panel, _Id - Line, _LineNr, IsCursor, !IO) :-
    draw_pager_line(Attrs, Panel, Line, IsCursor, !IO).

:- pred draw_pager_line(pager_attrs::in, panel::in, pager_line::in, bool::in,
    io::di, io::uo) is det.

draw_pager_line(Attrs, Panel, Line, IsCursor, !IO) :-
    GAttrs = Attrs ^ p_generic,
    (
        Line = header(_, Continue, Name, Value),
        attr(Panel, GAttrs ^ field_name, !IO),
        (
            Continue = no,
            draw(Panel, Name, !IO),
            draw(Panel, ": ", !IO)
        ;
            Continue = yes,
            getyx(Panel, Y, X, !IO),
            move(Panel, Y, X + string_wcwidth(Name) + 2, !IO)
        ),
        BodyAttr = GAttrs ^ field_body,
        (
            IsCursor = yes,
            Highlight = reverse
        ;
            IsCursor = no,
            ( Name = "Subject" ->
                Highlight = bold
            ;
                Highlight = normal
            )
        ),
        draw(Panel, BodyAttr + Highlight, Value, !IO)
    ;
        Line = text(pager_text(QuoteLevel, Text, QuoteMarkerEnd, TextType)),
        Attr0 = quote_level_to_attr(Attrs, QuoteLevel),
        (
            IsCursor = yes,
            Attr1 = reverse
        ;
            IsCursor = no,
            Attr1 = normal
        ),
        (
            TextType = plain,
            draw(Panel, Attr0 + Attr1, Text, !IO)
        ;
            TextType = diff(DiffLine),
            DiffAttr = diff_line_to_attr(Attrs, DiffLine),
            ( QuoteMarkerEnd = 0 ->
                draw(Panel, DiffAttr + Attr1, Text, !IO)
            ;
                End = string.length(Text),
                draw(Panel, Attr0 + Attr1, Text, 0, QuoteMarkerEnd, !IO),
                draw(Panel, DiffAttr + Attr1, Text, QuoteMarkerEnd, End, !IO)
            )
        ;
            TextType = url(UrlStart, UrlEnd),
            UrlAttr = Attrs ^ p_url,
            End = string.length(Text),
            draw(Panel, Attr0 + Attr1, Text, 0, UrlStart, !IO),
            draw(Panel, UrlAttr + Attr1, Text, UrlStart, UrlEnd, !IO),
            draw(Panel, Attr0 + Attr1, Text, UrlEnd, End, !IO)
        )
    ;
        Line = part_head(Part, HiddenParts, Expanded),
        Part = part(_MessageId, _Part, ContentType, _Content,
            MaybeFilename, MaybeEncoding, MaybeLength),
        (
            IsCursor = yes,
            Attr = Attrs ^ p_part_head + reverse
        ;
            IsCursor = no,
            Attr = Attrs ^ p_part_head
        ),
        draw(Panel, Attr, "[-- ", !IO),
        draw(Panel, ContentType, !IO),
        (
            MaybeFilename = yes(Filename),
            draw(Panel, "; ", !IO),
            draw(Panel, Filename, !IO)
        ;
            MaybeFilename = no
        ),
        (
            MaybeLength = yes(Length),
            DecodedLength = decoded_length(MaybeEncoding, Length),
            draw(Panel, format_length(DecodedLength), !IO)
        ;
            MaybeLength = no
        ),
        draw(Panel, " --]", !IO),
        ( toggle_part_message(Expanded, HiddenParts, AltMessage) ->
            MsgAttr = Attrs ^ p_part_message,
            draw(Panel, MsgAttr, AltMessage, !IO)
        ;
            true
        )
    ;
        Line = fold_marker(_, _),
        (
            IsCursor = yes,
            Attr = Attrs ^ p_fold + reverse
        ;
            IsCursor = no,
            Attr = Attrs ^ p_fold
        ),
        draw(Panel, Attr, "...", !IO)
    ;
        Line = message_separator,
        Attr = Attrs ^ p_separator,
        draw(Panel, Attr, "~", !IO)
    ).

:- func quote_level_to_attr(pager_attrs, quote_level) = attr.

quote_level_to_attr(Attrs, QuoteLevel) = Attr :-
    ( QuoteLevel = 0 ->
        Attr = Attrs ^ p_body
    ; int.odd(QuoteLevel) ->
        Attr = Attrs ^ p_quote_odd
    ;
        Attr = Attrs ^ p_quote_even
    ).

:- func diff_line_to_attr(pager_attrs, diff_line) = attr.

diff_line_to_attr(Attrs, diff_common) = Attrs ^ p_diff_common.
diff_line_to_attr(Attrs, diff_add) = Attrs ^ p_diff_add.
diff_line_to_attr(Attrs, diff_rem) = Attrs ^ p_diff_rem.
diff_line_to_attr(Attrs, diff_hunk) = Attrs ^ p_diff_hunk.
diff_line_to_attr(Attrs, diff_index) = Attrs ^ p_diff_index.

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

:- pred toggle_part_message(part_expanded::in, list(part)::in, string::out)
    is semidet.

toggle_part_message(Expanded, HiddenParts, Message) :-
    (
        Expanded = part_expanded,
        HiddenParts = [_],
        Message = "  z for alternative"
    ;
        Expanded = part_expanded,
        HiddenParts = [_, _ | _],
        Message = "  z for alternatives"
    ;
        Expanded = part_not_expanded,
        HiddenParts = [_ | _],
        Message = "  z to show"
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
