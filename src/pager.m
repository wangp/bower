% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module pager.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module color.
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

:- pred replace_node_under_cursor(prog_config::in, int::in, int::in, part::in,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

:- type toggle_type
    --->    cycle_alternatives
    ;       toggle_expanded.

:- pred toggle_content(prog_config::in, toggle_type::in, int::in, int::in,
    message_update::out, pager_info::in, pager_info::out, io::di, io::uo)
    is det.

:- pred get_percent_visible(pager_info::in, int::in, message_id::in, int::out)
    is semidet.

:- pred draw_pager_lines(screen::in, list(vpanel)::in, pager_attrs::in,
    pager_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module time.
:- import_module version_array.

:- import_module copious_output.
:- import_module fold_lines.
:- import_module list_util.
:- import_module mime_type.
:- import_module pager_text.
:- import_module quote_arg.
:- import_module sanitise.
:- import_module size_util.
:- import_module string_util.
:- import_module time_util.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- type pager_info
    --->    pager_info(
                p_tree          :: tree,
                p_id_counter    :: counter,
                p_scrollable    :: scrollable(id_pager_line),
                p_extents       :: map(message_id, message_extents)
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
    --->    start_of_message_header(
                som_message     :: message,
                som_header_name :: string,
                som_header_value :: string % folded (wrapped)
            )
    ;       subseq_message_header(
                smh_continue    :: bool,
                smh_name        :: string,
                smh_value       :: string % folded (wrapped)
            )
    ;       text(pager_text)
    ;       part_head(
                part            :: part,
                % Hidden alternatives for multipart/alternative.
                alternatives    :: list(part),
                ph_expanded     :: part_expanded,
                importance      :: part_importance
            )
    ;       fold_marker(
                content         :: list(pager_line),
                expanded        :: bool
            )
    ;       signature(
                signature       :: signature
            )
    ;       message_separator.

:- type part_expanded
    --->    part_expanded(part_filtered)
    ;       part_not_expanded.

:- type part_filtered
    --->    part_filtered
    ;       part_not_filtered.

:- type part_importance
    --->    importance_normal
    ;       importance_low.

:- type message_extents
    --->    message_extents(
                start_line      :: int,
                end_excl_line   :: int
            ).

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

is_blank_line(Line) :-
    require_complete_switch [Line]
    (
        Line = text(pager_text(_, "", _, _))
    ;
        Line = message_separator
    ;
        ( Line = start_of_message_header(_, _, _)
        ; Line = subseq_message_header(_, _, _)
        ; Line = part_head(_, _, _, _)
        ; Line = fold_marker(_, _)
        ; Line = signature(_)
        ),
        fail
    ).

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
    Flattened = flatten(Tree),
    Scrollable = scrollable.init(Flattened),
    make_extents(Flattened, Extents),
    Info = pager_info(Tree, Counter, Scrollable, Extents).

:- pred make_message_tree(prog_config::in, setup_mode::in, int::in,
    message::in, tree::out, counter::in, counter::out, io::di, io::uo) is det.

make_message_tree(Config, Mode, Cols, Message, Tree, !Counter, !IO) :-
    allocate_node_id(NodeId, !Counter),
    (
        Message = message(_MessageId, _Timestamp, _Headers, _Tags, _Body,
            Replies),
        make_message_self_trees(Config, Cols, Message, NodeId, SelfTrees,
            !Counter, !IO)
    ;
        Message = excluded_message(_, _, _, _, Replies),
        SelfTrees = []
    ),
    (
        Mode = include_replies,
        list.map_foldl2(make_message_tree(Config, Mode, Cols),
            Replies, ReplyTrees, !Counter, !IO)
    ;
        Mode = toplevel_only,
        ReplyTrees = []
    ),
    Tree = node(NodeId, SelfTrees ++ ReplyTrees, no).

:- pred make_message_self_trees(prog_config::in, int::in, message::in(message),
    node_id::in, list(tree)::out, counter::in, counter::out, io::di, io::uo)
    is det.

make_message_self_trees(Config, Cols, Message, NodeId, Trees, !Counter, !IO) :-
    Message = message(_MessageId, _Timestamp, Headers, _Tags, Body, _Replies),
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

    list.map_foldl3(make_part_tree(Config, Cols), Body, BodyTrees,
        yes, _ElideInitialHeadLine, !Counter, !IO),
    BodyTree = node(NodeId, BodyTrees, no),

    Separators = leaf([
        message_separator,
        message_separator,
        message_separator
    ]),

    Trees = [HeaderTree, BodyTree, Separators].

:- pred add_header(maybe(message)::in, int::in, string::in, header_value::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_header(StartMessage, Cols, Name, Value, !RevLines) :-
    % 2 extra columns for "Name: "
    RemainCols = max(0, Cols - string_wcwidth(Name) - 2),
    ValueStr = header_value_string(Value),
    make_presentable(ValueStr) = presentable_string(DisplayValue),
    get_spans_by_whitespace(DisplayValue, Spans),
    fill_lines(RemainCols, Spans, Folded),
    (
        Folded = [],
        FoldedHead = "",
        FoldedTail = []
    ;
        Folded = [FoldedHead | FoldedTail]
    ),
    (
        StartMessage = yes(Message),
        Line = start_of_message_header(Message, Name, FoldedHead)
    ;
        StartMessage = no,
        Line = subseq_message_header(no, Name, FoldedHead)
    ),
    cons(Line, !RevLines),
    foldl(add_header_cont(Name), FoldedTail, !RevLines).

:- pred add_header_cont(string::in, string::in,
    list(pager_line)::in, list(pager_line)::out) is det.

add_header_cont(Name, Value, !RevLines) :-
    cons(subseq_message_header(yes, Name, Value), !RevLines).

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
    part::in, handle_unsupported::in, tree::out, bool::in, bool::out,
    counter::in, counter::out, io::di, io::uo) is det.

make_part_tree_with_alts(Config, Cols, AltParts, Part, HandleUnsupported, Tree,
        !ElideInitialHeadLine, !Counter, !IO) :-
    Part = part(_MessageId, _PartId, PartType, _MaybeContentDisposition,
        Content, _MaybeFilename, _MaybeContentLength, _MaybeCTE, _IsDecrypted),
    allocate_node_id(PartNodeId, !Counter),
    (
        Content = text(InlineText),
        ( get_text_filter_command(Config, PartType, FilterCommand) ->
            filter_text_part(FilterCommand, InlineText, FilterRes, !IO),
            (
                FilterRes = ok(Text)
            ;
                FilterRes = error(Error),
                Text = "(filter error: " ++ Error ++ ")\n" ++ InlineText
            ),
            Filtered = part_filtered
        ;
            Text = InlineText,
            Filtered = part_not_filtered
        ),
        get_wrap_width(Config, Cols, WrapWidth),
        make_text_lines(WrapWidth, Text, Lines0),
        list.map(wrap_text, Lines0) = Lines,
        fold_quote_blocks(Lines, TextTrees, !Counter),
        (
            !.ElideInitialHeadLine = yes,
            PartType = mime_type.text_plain,
            AltParts = [],
            Filtered = part_not_filtered
        ->
            SubTrees = TextTrees
        ;
            HeadLine = part_head(Part, AltParts, part_expanded(Filtered),
                importance_normal),
            SubTrees = [leaf([HeadLine]) | TextTrees]
        ),
        Tree = node(PartNodeId, SubTrees, yes),
        !:ElideInitialHeadLine = no
    ;
        Content = subparts(Encryption, Signatures, SubParts),
        (
            PartType = mime_type.multipart_alternative,
            select_alternative(SubParts, [FirstSubPart | RestSubParts])
        ->
            % Assuming Signatures = []
            make_part_tree_with_alts(Config, Cols, RestSubParts, FirstSubPart,
                default, Tree, no, _ElideInitialHeadLine, !Counter, !IO),
            !:ElideInitialHeadLine = no
        ;
            get_importance(PartType, AltParts, Encryption, Signatures,
                Importance),
            map(make_signature_line, Signatures, SignatureLines),
            (
                (
                    Encryption = not_encrypted
                ;
                    Encryption = decryption_good
                ),
                (
                    AltParts = [],
                    SignatureLines = [],
                    hide_multipart_head_line(PartType)
                ->
                    list.map_foldl3(make_part_tree(Config, Cols), SubParts,
                        SubPartsTrees, !ElideInitialHeadLine, !Counter, !IO),
                    Tree = node(PartNodeId, SubPartsTrees, no)
                ;
                    list.map_foldl3(make_part_tree(Config, Cols), SubParts,
                        SubPartsTrees, yes, _ElideInitialHeadLine, !Counter, !IO),
                    HeadLine = part_head(Part, AltParts,
                        part_expanded(part_not_filtered), Importance),
                    SubTrees = [leaf([HeadLine | SignatureLines])
                        | SubPartsTrees],
                    Tree = node(PartNodeId, SubTrees, yes),
                    !:ElideInitialHeadLine = no
                )
            ;
                (
                    Encryption = encrypted
                ;
                    Encryption = decryption_bad
                ),
                HeadLine = part_head(Part, AltParts, part_not_expanded,
                    Importance),
                SubTrees = [leaf([HeadLine | SignatureLines])],
                Tree = node(PartNodeId, SubTrees, yes),
                !:ElideInitialHeadLine = no
            )
        )
    ;
        Content = encapsulated_messages(EncapMessages),
        list.map_foldl2(make_encapsulated_message_tree(Config, Cols),
            EncapMessages, SubTrees0, !Counter, !IO),
        HeadLine = part_head(Part, AltParts,
            part_expanded(part_not_filtered), importance_normal),
        SubTrees = [leaf([HeadLine]) | SubTrees0],
        Tree = node(PartNodeId, SubTrees, yes),
        !:ElideInitialHeadLine = no
    ;
        Content = unsupported,
        (
            AltParts = [],
            hide_unsupported_part(PartType)
        ->
            Tree = leaf([])
        ;
            make_unsupported_part_tree(Config, Cols, PartNodeId, Part,
                HandleUnsupported, AltParts, Tree, !IO)
        ),
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

:- pred get_importance(mime_type::in, list(part)::in, encryption::in,
    list(signature)::in, part_importance::out) is det.

get_importance(PartType, AltParts, Encryption, Signatures, Importance) :-
    (
        AltParts = [_ | _],
        Importance = importance_normal
    ;
        AltParts = [],
        (
            ( Encryption = encrypted
            ; Encryption = decryption_bad
            ),
            Importance = importance_normal
        ;
            Encryption = decryption_good,
            Importance = importance_low         % already decrypted
        ;
            Encryption = not_encrypted,
            (
                PartType = mime_type.multipart_signed,
                Signatures = [_ | _]
            ->
                Importance = importance_low     % already verified
            ;
                Importance = importance_normal
            )
        )
    ).

:- pred make_signature_line(signature::in, pager_line::out) is det.

make_signature_line(Signature, signature(Signature)).

:- pred hide_multipart_head_line(mime_type::in) is semidet.

hide_multipart_head_line(PartType) :-
    (
        PartType = mime_type.multipart_mixed
    ;
        PartType = mime_type.multipart_related
    ).

:- pred hide_unsupported_part(mime_type::in) is semidet.

hide_unsupported_part(PartType) :-
    (
        PartType = mime_type.application_pgp_encrypted
    ;
        PartType = mime_type.application_pgp_signature
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
        list_util.take_while(is_not_quoted_text, Lines0, NonQuotedLines, Lines1),
        list_util.take_while(is_quoted_text, Lines1, QuotedLines, RestLines),
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
        ( Line = start_of_message_header(_, _, _)
        ; Line = subseq_message_header(_, _, _)
        ; Line = part_head(_, _, _, _)
        ; Line = fold_marker(_, _)
        ; Line = signature(_)
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
    ValueStr = header_value_string(Value),
    make_presentable(ValueStr) = presentable_string(DisplayValue),
    ( DisplayValue = "" ->
        RevLines = RevLines0
    ;
        Line = text(pager_text(0, Header ++ ": " ++ DisplayValue, 0, plain)),
        RevLines = [Line | RevLines0]
    ).

%-----------------------------------------------------------------------------%

:- type handle_unsupported
    --->    default
    ;       expand_unsupported
    ;       hide_unsupported.

:- pred make_unsupported_part_tree(prog_config::in, int::in, node_id::in,
    part::in, handle_unsupported::in, list(part)::in, tree::out,
    io::di, io::uo) is det.

make_unsupported_part_tree(Config, Cols, PartNodeId, Part, HandleUnsupported,
        AltParts, Tree, !IO) :-
    Part = part(MessageId, MaybePartId, PartType, _MaybeContentDisposition,
        _Content, _MaybeFilename, _MaybeContentLength, _MaybeCTE,
        _IsDecrypted),
    (
        HandleUnsupported = default,
        DoExpand = ( if PartType = mime_type.text_html then yes else no )
    ;
        HandleUnsupported = expand_unsupported,
        DoExpand = yes
    ;
        HandleUnsupported = hide_unsupported,
        DoExpand = no
    ),
    (
        DoExpand = bool.yes,
        MaybePartId = yes(PartId),
        ( get_text_filter_command(Config, PartType, FilterCommand) ->
            MaybeFilterCommand = yes(FilterCommand),
            Filtered = part_filtered
        ;
            MaybeFilterCommand = no,
            Filtered = part_not_filtered
        ),
        expand_part(Config, MessageId, PartId, MaybeFilterCommand,
            MaybeText, !IO),
        (
            MaybeText = ok(Text),
            get_wrap_width(Config, Cols, WrapWidth),
            make_text_lines(WrapWidth, Text, TextLines)
        ;
            MaybeText = error(Error),
            make_text_lines(Cols, "(" ++ Error ++ ")", TextLines)
        ),
        Expanded = part_expanded(Filtered)
    ;
        DoExpand = bool.yes,
        MaybePartId = no,
        make_text_lines(Cols, "(no part id)", TextLines),
        Expanded = part_expanded(part_not_filtered)
    ;
        DoExpand = bool.no,
        Expanded = part_not_expanded,
        TextLines = []
    ),
    HeadLine = part_head(Part, AltParts, Expanded, importance_normal),
    Lines = [HeadLine | wrap_texts(TextLines)],
    Tree = node(PartNodeId, [leaf(Lines)], yes).

%-----------------------------------------------------------------------------%

:- func blank_line = pager_line.

blank_line = text(pager_text(0, "", 0, plain)).

:- func wrap_texts(list(pager_text)) = list(pager_line).

wrap_texts(Texts) = map(wrap_text, Texts).

:- func wrap_text(pager_text) = pager_line.

wrap_text(Text) = text(Text).

%-----------------------------------------------------------------------------%

setup_pager_for_staging(Config, Cols, Text, RetainPagerPos, Info) :-
    get_wrap_width(Config, Cols, WrapWidth),
    make_text_lines(WrapWidth, Text, Lines0),
    Lines = wrap_texts(Lines0) ++ [
        message_separator,
        message_separator,
        message_separator
    ],
    counter.init(0, Counter0),
    allocate_node_id(NodeId, Counter0, Counter),
    Tree = node(NodeId, [leaf(Lines)], no),
    Flattened = flatten(Tree),
    Scrollable0 = scrollable.init(Flattened),
    make_extents(Flattened, Extents0),
    Info0 = pager_info(Tree, Counter, Scrollable0, Extents0),
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
        ( Code = curs.key_down ->
            Binding = scroll_down
        ; Code = curs.key_up ->
            Binding = scroll_up
        ; Code = curs.key_pagedown ->
            Binding = page_down
        ; Code = curs.key_pageup ->
            Binding = page_up
        ; Code = curs.key_home ->
            Binding = home
        ; Code = curs.key_end ->
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

is_message_start(IdLine) :-
    is_start_of_message(IdLine, _Message).

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
    require_complete_switch [Line]
    (
        Line = start_of_message_header(_, _, _)
    ;
        Line = text(pager_text(Level, _, _, _)),
        Level > 0
    ;
        ( Line = subseq_message_header(_, _, _)
        ; Line = part_head(_, _, _, _)
        ; Line = fold_marker(_, _)
        ; Line = signature(_)
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
        scan_back_for_message_start(Lines, Top, _, Message)
    ;
        fail
    ).

get_top_offset(Info, Offset) :-
    Scrollable = Info ^ p_scrollable,
    Top = get_top(Scrollable),
    Lines = get_lines(Scrollable),
    ( Top < version_array.size(Lines) ->
        scan_back_for_message_start(Lines, Top, MessageLine, _Message),
        Offset = Top - MessageLine
    ;
        fail
    ).

:- pred scan_back_for_message_start(version_array(id_pager_line)::in,
    int::in, int::out, message::out) is semidet.

scan_back_for_message_start(Lines, I, J, Message) :-
    ( I >= 0 ->
        version_array.lookup(Lines, I) = IdLine,
        ( is_start_of_message(IdLine, Message0) ->
            J = I,
            Message = Message0
        ;
            scan_back_for_message_start(Lines, I - 1, J, Message)
        )
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

skip_to_message(MessageId, !Info) :-
    Scrollable0 = !.Info ^ p_scrollable,
    Extents0 = !.Info ^ p_extents,
    ( map.search(Extents0, MessageId, message_extents(Start, _EndExcl)) ->
        set_top(Start, Scrollable0, Scrollable),
        !Info ^ p_scrollable := Scrollable
    ;
        true
    ).

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
        Line = start_of_message_header(Message, _Name, Value),
        (
            strcase_str(Value, Search)
        ;
            % XXX this won't match current tags
            Tags = Message ^ m_tags,
            set.member(tag(TagName), Tags),
            strcase_str(TagName, Search)
        )
    ;
        Line = subseq_message_header(_Continue, _Name, Value),
        strcase_str(Value, Search)
    ;
        Line = text(pager_text(_, String, _, _)),
        strcase_str(String, Search)
    ;
        Line = part_head(Part, _, _, _),
        (
            Part ^ pt_content_type = PartType,
            strcase_str(to_string(PartType), Search)
        ;
            Part ^ pt_filename = yes(filename(FileName)),
            strcase_str(FileName, Search)
        )
    ;
        Line = fold_marker(_, _),
        fail
    ;
        Line = signature(signature(_Status, _Errors)),
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
    require_complete_switch [Line]
    (
        Line = part_head(_, _, _, importance_normal)
    ;
        Line = text(pager_text(_, _, _, url(_, _)))
    ;
        Line = fold_marker(_, _)
    ;
        ( Line = start_of_message_header(_, _, _)
        ; Line = subseq_message_header(_, _, _)
        ; Line = signature(_)
        ; Line = message_separator
        ),
        fail
    ).

:- pred is_highlightable_major(id_pager_line::in) is semidet.

is_highlightable_major(_Id - Line) :-
    require_complete_switch [Line]
    (
        Line = start_of_message_header(_, _, _)
    ;
        Line = part_head(_, _, _, _)
    ;
        ( Line = subseq_message_header(_, _, _)
        ; Line = text(_)
        ; Line = fold_marker(_, _)
        ; Line = signature(_)
        ; Line = message_separator
        ),
        fail
    ).

get_highlighted_thing(Info, Thing) :-
    Scrollable = Info ^ p_scrollable,
    get_cursor_line(Scrollable, _, _NodeId - Line),
    require_complete_switch [Line]
    (
        Line = start_of_message_header(Message, _, _),
        MessageId = Message ^ m_id,
        Subject = Message ^ m_headers ^ h_subject,
        PartId = 0,
        % Hmm.
        Part = part(MessageId, yes(PartId), mime_type.text_plain,
            no, unsupported, no, no, no, not_decrypted),
        MaybeSubject = yes(Subject),
        Thing = highlighted_part(Part, MaybeSubject)
    ;
        Line = subseq_message_header(_, _, _),
        fail
    ;
        Line = part_head(Part, _, _, _),
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
        Line = signature(_),
        fail
    ;
        Line = message_separator,
        fail
    ).

%-----------------------------------------------------------------------------%

replace_node_under_cursor(Config, NumRows, Cols, Part, Info0, Info, !IO) :-
    Info0 = pager_info(Tree0, Counter0, Scrollable0, _Extents0),
    ( get_cursor_line(Scrollable0, _, NodeId - _Line) ->
        make_part_tree(Config, Cols, Part, NewNode, no, _ElideInitialHeadLine,
            Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree),
        Flattened = flatten(Tree),
        scrollable.reinit(Flattened, NumRows, Scrollable0, Scrollable),
        make_extents(Flattened, Extents),
        Info = pager_info(Tree, Counter, Scrollable, Extents)
    ;
        Info = Info0
    ).

%-----------------------------------------------------------------------------%

toggle_content(Config, ToggleType, NumRows, Cols, MessageUpdate, !Info, !IO) :-
    Scrollable0 = !.Info ^ p_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, IdLine) ->
        toggle_line(Config, ToggleType, NumRows, Cols, IdLine, MessageUpdate,
            !Info, !IO)
    ;
        MessageUpdate = clear_message
    ).

:- pred toggle_line(prog_config::in, toggle_type::in, int::in, int::in,
    id_pager_line::in, message_update::out, pager_info::in, pager_info::out,
    io::di, io::uo) is det.

toggle_line(Config, ToggleType, NumRows, Cols, NodeId - Line, MessageUpdate,
        !Info, !IO) :-
    (
        Line = part_head(_, _, _, _),
        toggle_part(Config, ToggleType, NumRows, Cols, NodeId, Line,
            MessageUpdate, !Info, !IO)
    ;
        Line = fold_marker(_, _),
        toggle_folding(NumRows, NodeId, Line, !Info),
        MessageUpdate = clear_message
    ;
        ( Line = start_of_message_header(_, _, _)
        ; Line = subseq_message_header(_, _, _)
        ; Line = text(pager_text(_, _, _, _))
        ; Line = signature(_)
        ; Line = message_separator
        ),
        MessageUpdate = clear_message
    ).

:- inst part_head
    --->    part_head(ground, ground, ground, ground).

:- pred toggle_part(prog_config::in, toggle_type::in, int::in, int::in,
    node_id::in, pager_line::in(part_head), message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

toggle_part(Config, ToggleType, NumRows, Cols, NodeId, Line, MessageUpdate,
        Info0, Info, !IO) :-
    Line = part_head(Part0, HiddenParts0, Expanded0, _Importance0),
    (
        ToggleType = cycle_alternatives,
        Expanded0 = part_expanded(_),
        HiddenParts0 = [Part | HiddenParts1]
    ->
        HiddenParts = HiddenParts1 ++ [Part0],
        Info0 = pager_info(Tree0, Counter0, Scrollable0, _Extents0),
        make_part_tree_with_alts(Config, Cols, HiddenParts, Part,
            expand_unsupported, NewNode, no, _ElideInitialHeadLine,
            Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree),
        Flattened = flatten(Tree),
        scrollable.reinit(Flattened, NumRows, Scrollable0, Scrollable),
        make_extents(Flattened, Extents),
        Info = pager_info(Tree, Counter, Scrollable, Extents),
        Type = mime_type.to_string(Part ^ pt_content_type),
        MessageUpdate = set_info("Showing " ++ Type ++ " alternative.")
    ;
        % Fallback.
        toggle_part_expanded(Config, NumRows, Cols, NodeId, Line,
            MessageUpdate, Info0, Info, !IO)
    ).

:- pred toggle_part_expanded(prog_config::in, int::in, int::in, node_id::in,
    pager_line::in(part_head), message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

toggle_part_expanded(Config, NumRows, Cols, NodeId, Line0, MessageUpdate,
        Info0, Info, !IO) :-
    Line0 = part_head(Part, HiddenParts, WasExpanded, Importance),
    Info0 = pager_info(Tree0, Counter0, Scrollable0, _Extents0),
    (
        WasExpanded = part_not_expanded,
        MessageUpdate = set_info("Showing part."),
        make_part_tree_with_alts(Config, Cols, HiddenParts, Part,
            expand_unsupported, NewNode, no, _ElideInitialHeadLine,
            Counter0, Counter, !IO),
        replace_node(NodeId, NewNode, Tree0, Tree)
    ;
        WasExpanded = part_expanded(_),
        MessageUpdate = set_info("Part hidden."),
        Line = part_head(Part, HiddenParts, part_not_expanded, Importance),
        NewNode = node(NodeId, [leaf([Line])], yes),
        replace_node(NodeId, NewNode, Tree0, Tree),
        Counter = Counter0
    ),
    Flattened = flatten(Tree),
    scrollable.reinit(Flattened, NumRows, Scrollable0, Scrollable),
    make_extents(Flattened, Extents),
    Info = pager_info(Tree, Counter, Scrollable, Extents).

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

    Info0 = pager_info(Tree0, Counter, Scrollable0, _Extents0),
    replace_node(NodeId, NewNode, Tree0, Tree),
    Flattened = flatten(Tree),
    scrollable.reinit(Flattened, NumRows, Scrollable0, Scrollable),
    make_extents(Flattened, Extents),
    Info = pager_info(Tree, Counter, Scrollable, Extents).

%-----------------------------------------------------------------------------%

:- pred make_extents(list(id_pager_line)::in,
    map(message_id, message_extents)::out) is det.

make_extents(Lines, Extents) :-
    make_extents_loop(Lines, 0, map.init, Extents).

:- pred make_extents_loop(list(id_pager_line)::in, int::in,
    map(message_id, message_extents)::in,
    map(message_id, message_extents)::out) is det.

make_extents_loop(Lines, LineNr0, !Extents) :-
    (
        Lines = []
    ;
        Lines = [HeadLine | TailLines],
        (
            is_start_of_message(HeadLine, Message),
            MessageId = Message ^ m_id
        ->
            Start = LineNr0,
            skip_message_lines(TailLines, NextLines, Start + 1, EndExcl),
            map.set(MessageId, message_extents(Start, EndExcl), !Extents),
            make_extents_loop(NextLines, EndExcl, !Extents)
        ;
            make_extents_loop(TailLines, LineNr0 + 1, !Extents)
        )
    ).

:- pred skip_message_lines(list(id_pager_line)::in, list(id_pager_line)::out,
    int::in, int::out) is det.

skip_message_lines(Lines0, Lines, LineNr0, LineNr) :-
    (
        Lines0 = [],
        Lines = [],
        LineNr = LineNr0
    ;
        Lines0 = [HeadLine | TailLines],
        ( is_part_of_message(HeadLine) ->
            skip_message_lines(TailLines, Lines, LineNr0 + 1, LineNr)
        ;
            Lines = Lines0,
            LineNr = LineNr0
        )
    ).

:- pred is_start_of_message(id_pager_line::in, message::out) is semidet.

is_start_of_message(_NodeId - Line, Message) :-
    require_complete_switch [Line]
    (
        Line = start_of_message_header(Message, _Name, _Value)
    ;
        ( Line = subseq_message_header(_, _, _)
        ; Line = text(_)
        ; Line = part_head(_, _, _, _)
        ; Line = fold_marker(_, _)
        ; Line = signature(_)
        ; Line = message_separator
        ),
        fail
    ).

:- pred is_part_of_message(id_pager_line::in) is semidet.

is_part_of_message(_NodeId - Line) :-
    require_complete_switch [Line]
    (
        Line = start_of_message_header(_, _, _),
        fail
    ;
        Line = subseq_message_header(_, _, _)
    ;
        Line = text(_)
    ;
        Line = part_head(_, _, _, _)
    ;
        Line = fold_marker(_, _)
    ;
        Line = signature(_)
    ;
        Line = message_separator,
        fail
    ).

%-----------------------------------------------------------------------------%

get_percent_visible(Info, NumPagerRows, MessageId, Percent) :-
    require_det
    (
        Scrollable = Info ^ p_scrollable,
        Top = get_top(Scrollable),
        Ref = Top + NumPagerRows,
        Extents = Info ^ p_extents
    ),
    map.search(Extents, MessageId, message_extents(Start, EndExcl)),
    require_det
    (
        F = float(Ref - Start) / float(EndExcl - Start),
        % floor instead of round, otherwise we see 100% before end of message.
        I = floor_to_int(100.0 * F),
        Percent = max(0, min(I, 100))
    ).

%-----------------------------------------------------------------------------%

draw_pager_lines(Screen, Panels, Attrs, Info, !IO) :-
    Scrollable = Info ^ p_scrollable,
    scrollable.draw(draw_id_pager_line(Attrs), Screen, Panels, Scrollable,
        !IO).

:- pred draw_id_pager_line(pager_attrs::in, screen::in, vpanel::in,
    id_pager_line::in, int::in, bool::in, io::di, io::uo) is det.

draw_id_pager_line(Attrs, Screen, Panel, _Id - Line, _LineNr, IsCursor, !IO) :-
    draw_pager_line(Attrs, Screen, Panel, Line, IsCursor, !IO).

:- pred draw_pager_line(pager_attrs::in, screen::in, vpanel::in,
    pager_line::in, bool::in, io::di, io::uo) is det.

draw_pager_line(Attrs, Screen, Panel, Line, IsCursor, !IO) :-
    GAttrs = Attrs ^ p_generic,
    (
        (
            Line = start_of_message_header(_Message, Name, Value),
            Continue = no
        ;
            Line = subseq_message_header(Continue, Name, Value)
        ),
        attr(Screen, Panel, GAttrs ^ field_name, !IO),
        (
            Continue = no,
            draw(Screen, Panel, Name, !IO),
            draw(Screen, Panel, ": ", !IO)
        ;
            Continue = yes,
            getyx(Screen, Panel, Y, X, !IO),
            move(Screen, Panel, Y, X + string_wcwidth(Name) + 2, !IO)
        ),
        BodyAttr = GAttrs ^ field_body,
        (
            IsCursor = yes,
            Highlight = curs.reverse
        ;
            IsCursor = no,
            ( Name = "Subject" ->
                Highlight = curs.bold
            ;
                Highlight = curs.normal
            )
        ),
        draw(Screen, Panel, curs.(BodyAttr + Highlight), Value, !IO)
    ;
        Line = text(pager_text(QuoteLevel, Text, QuoteMarkerEnd, TextType)),
        Attr0 = quote_level_to_attr(Attrs, QuoteLevel),
        (
            IsCursor = yes,
            Attr1 = curs.reverse
        ;
            IsCursor = no,
            Attr1 = curs.normal
        ),
        (
            TextType = plain,
            draw(Screen, Panel, curs.(Attr0 + Attr1), Text, !IO)
        ;
            TextType = diff(DiffLine),
            DiffAttr = diff_line_to_attr(Attrs, DiffLine),
            ( QuoteMarkerEnd = 0 ->
                draw(Screen, Panel, curs.(DiffAttr + Attr1), Text, !IO)
            ;
                End = string.length(Text),
                draw(Screen, Panel, curs.(Attr0 + Attr1),
                    Text, 0, QuoteMarkerEnd, !IO),
                draw(Screen, Panel, curs.(DiffAttr + Attr1),
                    Text, QuoteMarkerEnd, End, !IO)
            )
        ;
            TextType = url(UrlStart, UrlEnd),
            UrlAttr = Attrs ^ p_url,
            End = string.length(Text),
            draw(Screen, Panel, curs.(Attr0 + Attr1),
                Text, 0, UrlStart, !IO),
            draw(Screen, Panel, curs.(UrlAttr + Attr1),
                Text, UrlStart, UrlEnd, !IO),
            draw(Screen, Panel, curs.(Attr0 + Attr1),
                Text, UrlEnd, End, !IO)
        )
    ;
        Line = part_head(Part, HiddenParts, Expanded, Importance),
        Part = part(_MessageId, _Part, ContentType, _MaybeContentDisposition,
            _Content, MaybeFilename, MaybeContentLength, MaybeCTE,
            _IsDecrypted),
        (
            Importance = importance_normal,
            Attr0 = Attrs ^ p_part_head
        ;
            Importance = importance_low,
            Attr0 = Attrs ^ p_part_head_low
        ),
        (
            IsCursor = yes,
            Attr = curs.(Attr0 + curs.reverse)
        ;
            IsCursor = no,
            Attr = Attr0
        ),
        draw(Screen, Panel, Attr, "[-- ", !IO),
        draw(Screen, Panel, mime_type.to_string(ContentType), !IO),
        (
            MaybeFilename = yes(filename(Filename)),
            draw(Screen, Panel, "; ", !IO),
            draw(Screen, Panel, Filename, !IO)
        ;
            MaybeFilename = no
        ),
        /*
        (
            MaybeContentDisposition = yes(content_disposition(Disposition)),
            draw(Screen, Panel, "; ", !IO),
            draw(Screen, Panel, Disposition, !IO)
        ;
            MaybeContentDisposition = no
        ),
        (
            MaybeContentCharset = yes(content_charset(Charset)),
            draw(Screen, Panel, "; charset=", !IO),
            draw(Screen, Panel, Charset, !IO)
        ;
            MaybeContentCharset = no
        ),
        */
        (
            MaybeContentLength = yes(content_length(Length)),
            ( estimate_decoded_length(MaybeCTE, Length, DecodedLength0) ->
                DecodedLength = DecodedLength0
            ;
                DecodedLength = Length
            ),
            draw(Screen, Panel, " (", !IO),
            draw(Screen, Panel, format_approx_length(DecodedLength), !IO),
            draw(Screen, Panel, ")", !IO)
        ;
            MaybeContentLength = no
        ),
        draw(Screen, Panel, " --]", !IO),
        ( make_part_message(Part, HiddenParts, Expanded, Message) ->
            attr(Screen, Panel, Attrs ^ p_part_message, !IO),
            draw2(Screen, Panel, "  ", Message, !IO)
        ;
            true
        )
    ;
        Line = fold_marker(_, _),
        (
            IsCursor = yes,
            Attr = curs.(Attrs ^ p_fold + curs.reverse)
        ;
            IsCursor = no,
            Attr = Attrs ^ p_fold
        ),
        draw(Screen, Panel, Attr, "...", !IO)
    ;
        Line = signature(signature(Status, Errors)),
        BodyAttr = GAttrs ^ field_body,
        GoodAttr = GAttrs ^ good_key,
        BadAttr = GAttrs ^ bad_key,
        (
            Status = none,
            draw(Screen, Panel, BadAttr, " ─• No signature ", !IO)
        ;
            Status = good(MaybeFingerprint, _MaybeCreated, MaybeExpires,
                MaybeUserId),
            draw(Screen, Panel, GoodAttr, " ─• Good signature ", !IO),
            (
                MaybeUserId = yes(UserId),
                draw(Screen, Panel, BodyAttr, lstrip(UserId), !IO),
                (
                    MaybeFingerprint = yes(Fingerprint),
                    draw(Screen, Panel, BodyAttr, ", fpr ", !IO),
                    draw(Screen, Panel, BodyAttr, Fingerprint, !IO)
                ;
                    MaybeFingerprint = no
                )
            ;
                MaybeUserId = no,
                MaybeFingerprint = yes(Fingerprint),
                draw(Screen, Panel, BodyAttr, "fingerprint ", !IO),
                draw(Screen, Panel, BodyAttr, Fingerprint, !IO)
            ;
                MaybeUserId = no,
                MaybeFingerprint = no,
                draw(Screen, Panel, BodyAttr, "(no user id)", !IO)
            ),
            % Not really tested; don't know when it occurs.
            (
                MaybeExpires = yes(Expires),
                localtime(Expires, TM, !IO),
                draw(Screen, Panel, BodyAttr, "; expires ", !IO),
                draw(Screen, Panel, BodyAttr, asctime(TM), !IO)
            ;
                MaybeExpires = no
            )
        ;
            Status = not_good(BadStatus, MaybeKeyId),
            (
                BadStatus = bad,
                BadMessage = " ─• Bad signature "
            ;
                BadStatus = error,
                BadMessage = " ─• Error verifying signature "
            ;
                BadStatus = unknown,
                BadMessage = " ─• Problem verifying signature "
            ),
            draw(Screen, Panel, BadAttr, BadMessage, !IO),
            (
                MaybeKeyId = yes(KeyId),
                draw(Screen, Panel, BodyAttr, "key id ", !IO),
                draw(Screen, Panel, BodyAttr, KeyId, !IO)
            ;
                MaybeKeyId = no,
                draw(Screen, Panel, BodyAttr, "(no key id)", !IO)
            )
        ),
        ( Errors > 0 ->
            draw(Screen, Panel, BodyAttr, format(" (errors: %d)", [i(Errors)]),
                !IO)
        ;
            true
        )
    ;
        Line = message_separator,
        Attr = Attrs ^ p_separator,
        draw(Screen, Panel, Attr, "~", !IO)
    ).

:- func quote_level_to_attr(pager_attrs, quote_level) = curs.attr.

quote_level_to_attr(Attrs, QuoteLevel) = Attr :-
    ( QuoteLevel = 0 ->
        Attr = Attrs ^ p_body
    ; int.odd(QuoteLevel) ->
        Attr = Attrs ^ p_quote_odd
    ;
        Attr = Attrs ^ p_quote_even
    ).

:- func diff_line_to_attr(pager_attrs, diff_line) = curs.attr.

diff_line_to_attr(Attrs, diff_common) = Attrs ^ p_diff_common.
diff_line_to_attr(Attrs, diff_add) = Attrs ^ p_diff_add.
diff_line_to_attr(Attrs, diff_rem) = Attrs ^ p_diff_rem.
diff_line_to_attr(Attrs, diff_hunk) = Attrs ^ p_diff_hunk.
diff_line_to_attr(Attrs, diff_index) = Attrs ^ p_diff_index.

:- pred make_part_message(part::in, list(part)::in, part_expanded::in,
    string::out) is semidet.

make_part_message(Part, HiddenParts, Expanded, Message) :-
    ( if make_part_message_2(Part, HiddenParts, Expanded, MessageB) then
        ( if Expanded = part_expanded(part_filtered) then
            Message = "filtered, " ++ MessageB
        else
            Message = MessageB
        )
    else
        ( if Expanded = part_expanded(part_filtered) then
            Message = "filtered"
        else
            fail
        )
    ).

:- pred make_part_message_2(part::in, list(part)::in, part_expanded::in,
    string::out) is semidet.

make_part_message_2(Part, HiddenParts, Expanded, Message) :-
    Content = Part ^ pt_content,
    require_complete_switch [Content]
    (
        Content = subparts(EncStatus, Signatures, _)
    ;
        ( Content = text(_)
        ; Content = encapsulated_messages(_)
        ; Content = unsupported
        ),
        EncStatus = not_encrypted,
        Signatures = []
    ),
    require_complete_switch [EncStatus]
    (
        ( EncStatus = encrypted
        ; EncStatus = decryption_bad
        ),
        Message = "z to decrypt"
    ;
        EncStatus = decryption_good,
        Message = "decrypted"
    ;
        EncStatus = not_encrypted,
        (
            Expanded = part_expanded(_Filtered),
            (
                HiddenParts = [],
                Signatures = [],
                Part ^ pt_content_type = mime_type.multipart_signed,
                Message = "y to verify"
            ;
                HiddenParts = [_],
                Message = "z for alternative"
            ;
                HiddenParts = [_, _ | _],
                Message = "z for alternatives"
            )
        ;
            Expanded = part_not_expanded,
            Message = "z to show"
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
