%-----------------------------------------------------------------------------%

:- module thread_pager.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module time.

:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type thread_pager_info.

:- pred setup_thread_pager(tm::in, int::in, int::in, list(message)::in,
    thread_pager_info::out, int::out) is det.

:- type thread_pager_action
    --->    continue
    ;       leave.

:- pred thread_pager_input(char::in, thread_pager_action::out,
    message_update::out, thread_pager_info::in, thread_pager_info::out) is det.

:- pred draw_thread_pager(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.

:- import_module curs.
:- import_module curs.panel.
:- import_module pager.
:- import_module scrollable.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type thread_pager_info
    --->    thread_pager_info(
                tp_scrollable       :: scrollable(thread_line),
                tp_num_thread_rows  :: int,
                tp_pager            :: pager_info,
                tp_num_pager_rows   :: int
            ).

:- type thread_line
    --->    thread_line(
                tp_message  :: message,
                tp_unread   :: unread,
                tp_replied  :: replied,
                tp_flagged  :: flagged,
                tp_graphics :: list(graphic),
                tp_reldate  :: string
            ).

:- type graphic
    --->    blank
    ;       vert
    ;       tee
    ;       ell.

:- type unread
    --->    unread
    ;       read.

:- type replied
    --->    replied
    ;       not_replied.

:- type flagged
    --->    flagged
    ;       unflagged.

:- instance scrollable.line(thread_line) where [
    pred(draw_line/5) is draw_thread_line
].

%-----------------------------------------------------------------------------%

setup_thread_pager(Nowish, Rows, Cols, Messages, ThreadPagerInfo,
        NumThreadLines) :-
    append_messages(Nowish, [], [], Messages, cord.init, ThreadCord),
    ThreadLines = list(ThreadCord),
    Scrollable = scrollable.init_with_cursor(ThreadLines, 0),
    setup_pager(Cols, Messages, PagerInfo),

    NumThreadLines = get_num_lines(Scrollable),
    NumThreadRows = int.min(max_thread_lines, NumThreadLines),
    SepLine = 1,
    NumPagerRows = int.max(0, Rows - NumThreadRows - SepLine),
    ThreadPagerInfo1 = thread_pager_info(Scrollable, NumThreadRows,
        PagerInfo, NumPagerRows),
    skip_to_unread(_MessageUpdate, ThreadPagerInfo1, ThreadPagerInfo).

:- func max_thread_lines = int.

max_thread_lines = 8.

:- pred append_messages(tm::in, list(graphic)::in, list(graphic)::in,
    list(message)::in, cord(thread_line)::in, cord(thread_line)::out) is det.

append_messages(_Nowish, _Above, _Below, [], !Cord).
append_messages(Nowish, Above0, Below0, [Message | Messages], !Cord) :-
    (
        Messages = [],
        make_thread_line(Nowish, Message, Above0 ++ [ell], Line),
        snoc(Line, !Cord),
        MessagesCord = cord.empty,
        Below1 = Below0
    ;
        Messages = [_ | _],
        make_thread_line(Nowish, Message, Above0 ++ [tee], Line),
        snoc(Line, !Cord),
        append_messages(Nowish, Above0, Below0, Messages, cord.init, MessagesCord),
        ( get_first(MessagesCord, FollowingLine) ->
            Below1 = FollowingLine ^ tp_graphics
        ;
            unexpected($module, $pred, "empty cord")
        )
    ),
    ( not_blank_at_column(Below1, length(Above0)) ->
        Above1 = Above0 ++ [vert]
    ;
        Above1 = Above0 ++ [blank]
    ),
    append_messages(Nowish, Above1, Below1, Message ^ m_replies, !Cord),
    !:Cord = !.Cord ++ MessagesCord.

:- pred not_blank_at_column(list(graphic)::in, int::in) is semidet.

not_blank_at_column(Graphics, Col) :-
    list.index0(Graphics, Col, Graphic),
    Graphic \= blank.

:- pred make_thread_line(tm::in, message::in, list(graphic)::in,
    thread_line::out) is det.

make_thread_line(Nowish, Message, Graphics, Line) :-
    Timestamp = Message ^ m_timestamp,
    Tags = Message ^ m_tags,
    timestamp_to_tm(Timestamp, TM),
    Shorter = no,
    make_reldate(Nowish, TM, Shorter, RelDate),
    Line0 = thread_line(Message, read, not_replied, unflagged, Graphics,
        RelDate),
    list.foldl(apply_tag, Tags, Line0, Line).

:- pred apply_tag(string::in, thread_line::in, thread_line::out) is det.

apply_tag(Tag, !Line) :-
    ( Tag = "unread" ->
        !Line ^ tp_unread := unread
    ; Tag = "replied" ->
        !Line ^ tp_replied := replied
    ; Tag = "flagged" ->
        !Line ^ tp_flagged := flagged
    ;
        true
    ).

%-----------------------------------------------------------------------------%

thread_pager_input(Char, Action, MessageUpdate, !Info) :-
    NumPagerRows = !.Info ^ tp_num_pager_rows,
    ( Char = 'j' ->
        next_message(MessageUpdate, !Info),
        Action = continue
    ; Char = 'k' ->
        prev_message(MessageUpdate, !Info),
        Action = continue
    ; Char = '\r' ->
        scroll(1, MessageUpdate, !Info),
        Action = continue
    ; Char = ('\\') ->
        scroll(-1, MessageUpdate, !Info),
        Action = continue
    ; Char = ']' ->
        Delta = int.min(15, NumPagerRows - 1),
        scroll(Delta, MessageUpdate, !Info),
        Action = continue
    ; Char = '[' ->
        Delta = int.min(15, NumPagerRows - 1),
        scroll(-Delta, MessageUpdate, !Info),
        Action = continue
    ; Char = ' ' ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll(Delta, MessageUpdate, !Info),
        Action = continue
    ; Char = 'b' ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll(-Delta, MessageUpdate, !Info),
        Action = continue
    ; Char = 'S' ->
        skip_quoted_text(MessageUpdate, !Info),
        Action = continue
    ; Char = '\t' ->
        skip_to_unread(MessageUpdate, !Info),
        Action = continue
    ;
        ( Char = 'i'
        ; Char = 'q'
        )
    ->
        Action = leave,
        MessageUpdate = clear_message
    ;
        Action = continue,
        MessageUpdate = no_change
    ).

:- pred next_message(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

next_message(MessageUpdate, !Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    pager.next_message(MessageUpdate, PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred prev_message(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

prev_message(MessageUpdate, !Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    pager.prev_message(MessageUpdate, PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred scroll(int::in, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

scroll(Delta, MessageUpdate, !Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    NumPagerRows = !.Info ^ tp_num_pager_rows,
    pager.scroll(NumPagerRows, Delta, MessageUpdate, PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred skip_quoted_text(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

skip_quoted_text(MessageUpdate, !Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    pager.skip_quoted_text(MessageUpdate, PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred sync_thread_to_pager(thread_pager_info::in, thread_pager_info::out)
    is det.

sync_thread_to_pager(!Info) :-
    PagerInfo = !.Info ^ tp_pager,
    Scrollable0 = !.Info ^ tp_scrollable,
    NumThreadRows = !.Info ^ tp_num_thread_rows,
    (
        % XXX inefficient
        get_top_message_id(PagerInfo, MessageId),
        search_forward(is_message(MessageId), Scrollable0, 0, Cursor, _)
    ->
        set_cursor(Cursor, NumThreadRows, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred skip_to_unread(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

skip_to_unread(MessageUpdate, !Info) :-
    !.Info = thread_pager_info(Scrollable0, NumThreadRows, PagerInfo0,
        NumPagerRows),
    (
        get_cursor(Scrollable0, Cursor0),
        search_forward(is_unread_line, Scrollable0, Cursor0 + 1, Cursor,
            ThreadLine)
    ->
        set_cursor(Cursor, NumThreadRows, Scrollable0, Scrollable),
        MessageId = ThreadLine ^ tp_message ^ m_id,
        skip_to_message(MessageId, PagerInfo0, PagerInfo),
        !:Info = thread_pager_info(Scrollable, NumThreadRows, PagerInfo,
            NumPagerRows),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No more unread messages.")
    ).

:- pred is_message(message_id::in, thread_line::in) is semidet.

is_message(MessageId, Line) :-
    Line ^ tp_message ^ m_id = MessageId.

:- pred is_unread_line(thread_line::in) is semidet.

is_unread_line(Line) :-
    Line ^ tp_unread = unread.

%-----------------------------------------------------------------------------%

draw_thread_pager(Screen, Info, !IO) :-
    Scrollable = Info ^ tp_scrollable,
    PagerInfo = Info ^ tp_pager,
    split_panels(Screen, Info, ThreadPanels, SepPanel, PagerPanels),
    scrollable.draw(ThreadPanels, Scrollable, !IO),
    draw_sep(Screen ^ cols, SepPanel, !IO),
    draw_pager_lines(PagerPanels, PagerInfo, !IO).

:- pred draw_sep(int::in, maybe(panel)::in, io::di, io::uo) is det.

draw_sep(Cols, MaybeSepPanel, !IO) :-
    (
        MaybeSepPanel = yes(Panel),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, fg_bg(white, blue), !IO),
        hline(Panel, char.to_int('-'), Cols, !IO)
    ;
        MaybeSepPanel = no
    ).

:- pred draw_thread_line(panel::in, thread_line::in, bool::in,
    io::di, io::uo) is det.

draw_thread_line(Panel, Line, IsCursor, !IO) :-
    Line = thread_line(Message, Unread, Replied, Flagged, Graphics, RelDate),
    From = Message ^ m_from,
    (
        IsCursor = yes,
        panel.attr_set(Panel, fg_bg(yellow, red) + bold, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, fg_bg(blue, black) + bold, !IO)
    ),
    my_addstr_fixed(Panel, 13, RelDate, ' ', !IO),
    cond_attr_set(Panel, normal, IsCursor, !IO),
    (
        Replied = replied,
        my_addstr(Panel, "r", !IO)
    ;
        Replied = not_replied,
        my_addstr(Panel, " ", !IO)
    ),
    (
        Flagged = flagged,
        cond_attr_set(Panel, fg_bg(red, black) + bold, IsCursor, !IO),
        my_addstr(Panel, "! ", !IO)
    ;
        Flagged = unflagged,
        my_addstr(Panel, "  ", !IO)
    ),
    cond_attr_set(Panel, fg_bg(magenta, black), IsCursor, !IO),
    list.foldl(draw_graphic(Panel), Graphics, !IO),
    my_addstr(Panel, "> ", !IO),
    (
        Unread = unread,
        cond_attr_set(Panel, bold, IsCursor, !IO)
    ;
        Unread = read,
        cond_attr_set(Panel, normal, IsCursor, !IO)
    ),
    my_addstr(Panel, From, !IO).
    % XXX should indicate changes of subject

:- pred draw_graphic(panel::in, graphic::in, io::di, io::uo) is det.

draw_graphic(Panel, Graphic, !IO) :-
    my_addstr(Panel, graphic_to_char(Graphic), !IO).

:- func graphic_to_char(graphic) = string.

graphic_to_char(blank) = " ".
graphic_to_char(vert) = "│".
graphic_to_char(tee) = "├".
graphic_to_char(ell) = "└".

:- pred cond_attr_set(panel::in, attr::in, bool::in, io::di, io::uo) is det.

cond_attr_set(Panel, Attr, IsCursor, !IO) :-
    (
        IsCursor = no,
        panel.attr_set(Panel, Attr, !IO)
    ;
        IsCursor = yes
    ).

:- pred split_panels(screen::in, thread_pager_info::in,
    list(panel)::out, maybe(panel)::out, list(panel)::out) is det.

split_panels(Screen, Info, ThreadPanels, MaybeSepPanel, PagerPanels) :-
    NumThreadRows = Info ^ tp_num_thread_rows,
    NumPagerRows = Info ^ tp_num_pager_rows,
    Panels0 = Screen ^ main_panels,
    list.split_upto(NumThreadRows, Panels0, ThreadPanels, Panels1),
    (
        Panels1 = [SepPanel | Panels2],
        MaybeSepPanel = yes(SepPanel),
        list.take_upto(NumPagerRows, Panels2, PagerPanels)
    ;
        Panels1 = [],
        MaybeSepPanel = no,
        PagerPanels = []
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
