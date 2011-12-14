%-----------------------------------------------------------------------------%

:- module thread_pager.
:- interface.

:- import_module bool.
:- import_module io.

:- import_module data.
:- import_module screen.
:- import_module text_entry.

%-----------------------------------------------------------------------------%

:- pred open_thread_pager(screen::in, thread_id::in, bool::out,
    history::in, history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module time.
:- import_module version_array.

:- import_module callout.
:- import_module compose.
:- import_module copious_output.
:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module pager.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module scrollable.
:- import_module string_util.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type thread_pager_info
    --->    thread_pager_info(
                tp_scrollable       :: scrollable(thread_line),
                tp_num_thread_rows  :: int,
                tp_pager            :: pager_info,
                tp_num_pager_rows   :: int,
                tp_search           :: maybe(string),
                tp_search_history   :: history,
                tp_need_refresh     :: bool
            ).

:- type thread_line
    --->    thread_line(
                tp_message      :: message,
                tp_clean_from   :: string,
                tp_unread       :: pair(unread),
                tp_replied      :: replied,
                tp_deleted      :: pair(deleted),
                tp_flagged      :: pair(flagged),
                tp_graphics     :: list(graphic),
                tp_reldate      :: string,
                tp_subject      :: maybe(string)
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

:- type deleted
    --->    deleted
    ;       not_deleted.

:- type flagged
    --->    flagged
    ;       unflagged.

:- type thread_pager_action
    --->    continue
    ;       resize
    ;       start_reply(message, reply_kind)
    ;       prompt_save_part(part)
    ;       prompt_open_part(part)
    ;       prompt_search
    ;       leave(
                map(set(tag_delta), list(message_id))
                % Group messages by the tag changes to be applied.
            ).

:- instance scrollable.line(thread_line) where [
    pred(draw_line/5) is draw_thread_line
].

%-----------------------------------------------------------------------------%

open_thread_pager(Screen, ThreadId, NeedRefresh, SearchHistory0, SearchHistory,
        !IO) :-
    run_notmuch([
        "show", "--format=json", "--", thread_id_to_search_term(ThreadId)
    ], parse_messages_list, Result, !IO),
    (
        Result = ok(Messages0),
        list.filter_map(filter_unwanted_messages, Messages0, Messages1),
        list.map_foldl(expand_copious_output, Messages1, Messages, !IO)
    ;
        Result = error(Error),
        unexpected($module, $pred, io.error_message(Error))
    ),
    time(Time, !IO),
    Nowish = localtime(Time),
    get_rows_cols(Screen, Rows, Cols),
    setup_thread_pager(Nowish, Rows - 2, Cols, Messages, SearchHistory0,
        ThreadPagerInfo0, Count),
    string.format("Showing %d messages.", [i(Count)], Msg),
    update_message(Screen, set_info(Msg), !IO),
    thread_pager_loop(Screen, ThreadPagerInfo0, ThreadPagerInfo, !IO),
    NeedRefresh = ThreadPagerInfo ^ tp_need_refresh,
    SearchHistory = ThreadPagerInfo ^ tp_search_history.

:- pred filter_unwanted_messages(message::in, message::out) is semidet.

filter_unwanted_messages(!Message) :-
    Tags = !.Message ^ m_tags,
    not list.contains(Tags, "draft"),

    Replies0 = !.Message ^ m_replies,
    list.filter_map(filter_unwanted_messages, Replies0, Replies),
    !Message ^ m_replies := Replies.

:- pred setup_thread_pager(tm::in, int::in, int::in, list(message)::in,
    history::in, thread_pager_info::out, int::out) is det.

setup_thread_pager(Nowish, Rows, Cols, Messages, SearchHistory,
        ThreadPagerInfo, NumThreadLines) :-
    append_messages(Nowish, [], [], Messages, "", cord.init, ThreadCord),
    ThreadLines = list(ThreadCord),
    Scrollable = scrollable.init_with_cursor(ThreadLines),
    NumThreadLines = get_num_lines(Scrollable),
    setup_pager(Cols, Messages, PagerInfo),

    compute_num_rows(Rows, Scrollable, NumThreadRows,
        NumPagerRows),
    ThreadPagerInfo1 = thread_pager_info(Scrollable, NumThreadRows,
        PagerInfo, NumPagerRows, no, SearchHistory, no),
    (
        get_cursor_line(Scrollable, _, FirstLine),
        is_unread_line(FirstLine)
    ->
        ThreadPagerInfo = ThreadPagerInfo1
    ;
        skip_to_unread(_MessageUpdate, ThreadPagerInfo1, ThreadPagerInfo)
    ).

:- pred resize_thread_pager(int::in, int::in,
    thread_pager_info::in, thread_pager_info::out) is det.

resize_thread_pager(Rows, _Cols, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    compute_num_rows(Rows, Scrollable0, NumThreadRows, NumPagerRows),
    ( get_cursor(Scrollable0, Cursor) ->
        set_cursor_centred(Cursor, NumThreadRows, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ),
    !Info ^ tp_num_thread_rows := NumThreadRows,
    !Info ^ tp_num_pager_rows := NumPagerRows.

:- pred compute_num_rows(int::in, scrollable(thread_line)::in,
    int::out, int::out) is det.

compute_num_rows(Rows, Scrollable, NumThreadRows, NumPagerRows) :-
    NumThreadLines = get_num_lines(Scrollable),
    SepLine = 1,
    ExtraLines = SepLine + 2,
    Y0 = int.max(1, (Rows - ExtraLines) // 3),
    Y1 = int.min(Y0, NumThreadLines),
    Y2 = int.min(Y1, max_thread_lines),
    NumThreadRows = Y2,
    NumPagerRows = int.max(0, Rows - NumThreadRows - SepLine).

:- func max_thread_lines = int.

max_thread_lines = 8.

:- pred append_messages(tm::in, list(graphic)::in, list(graphic)::in,
    list(message)::in, string::in,
    cord(thread_line)::in, cord(thread_line)::out) is det.

append_messages(_Nowish, _Above, _Below, [], _PrevSubject, !Cord).
append_messages(Nowish, Above0, Below0, [Message | Messages], PrevSubject,
        !Cord) :-
    (
        Messages = [],
        make_thread_line(Nowish, Message, Above0 ++ [ell], PrevSubject, Line),
        snoc(Line, !Cord),
        MessagesCord = cord.empty,
        Below1 = Below0
    ;
        Messages = [_ | _],
        make_thread_line(Nowish, Message, Above0 ++ [tee], PrevSubject, Line),
        snoc(Line, !Cord),
        get_last_subject(Message, LastSubject),
        append_messages(Nowish, Above0, Below0, Messages, LastSubject,
            cord.init, MessagesCord),
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
    Replies = Message ^ m_replies,
    Subject = Message ^ m_headers ^ h_subject,
    append_messages(Nowish, Above1, Below1, Replies, Subject, !Cord),
    !:Cord = !.Cord ++ MessagesCord.

:- pred get_last_subject(message::in, string::out) is det.

get_last_subject(Message, LastSubject) :-
    Replies = Message ^ m_replies,
    ( list.last(Replies, LastReply) ->
        get_last_subject(LastReply, LastSubject)
    ;
        LastSubject = Message ^ m_headers ^ h_subject
    ).

:- pred not_blank_at_column(list(graphic)::in, int::in) is semidet.

not_blank_at_column(Graphics, Col) :-
    list.index0(Graphics, Col, Graphic),
    Graphic \= blank.

:- pred make_thread_line(tm::in, message::in, list(graphic)::in, string::in,
    thread_line::out) is det.

make_thread_line(Nowish, Message, Graphics, PrevSubject, Line) :-
    Timestamp = Message ^ m_timestamp,
    Tags = Message ^ m_tags,
    From = clean_email_address(Message ^ m_headers ^ h_from),
    Subject = Message ^ m_headers ^ h_subject,
    timestamp_to_tm(Timestamp, TM),
    Shorter = no,
    make_reldate(Nowish, TM, Shorter, RelDate),
    ( canonicalise_subject(Subject) = canonicalise_subject(PrevSubject) ->
        MaybeSubject = no
    ;
        MaybeSubject = yes(Subject)
    ),
    Line0 = thread_line(Message, From, read - read, not_replied,
        not_deleted - not_deleted, unflagged - unflagged,
        Graphics, RelDate, MaybeSubject),
    list.foldl(apply_tag, Tags, Line0, Line).

:- func clean_email_address(string) = string.

clean_email_address(Orig) = Clean :-
    (
        strrchr(Orig, '<', Index),
        string.unsafe_prev_index(Orig, Index, SpaceIndex, ' ')
    ->
        string.unsafe_between(Orig, 0, SpaceIndex, Clean)
    ;
        Clean = Orig
    ).

:- pred apply_tag(string::in, thread_line::in, thread_line::out) is det.

apply_tag(Tag, !Line) :-
    ( Tag = "unread" ->
        !Line ^ tp_unread := unread - unread
    ; Tag = "replied" ->
        !Line ^ tp_replied := replied
    ; Tag = "deleted" ->
        !Line ^ tp_deleted := deleted - deleted
    ; Tag = "flagged" ->
        !Line ^ tp_flagged := flagged - flagged
    ;
        true
    ).

:- func canonicalise_subject(string) = list(string).

canonicalise_subject(S) = Words :-
    list.negated_filter(is_reply_marker, string.words(S), Words).

:- pred is_reply_marker(string::in) is semidet.

is_reply_marker("Re:").
is_reply_marker("RE:").
is_reply_marker("R:").
is_reply_marker("Aw:").
is_reply_marker("AW:").
is_reply_marker("Vs:").
is_reply_marker("VS:").
is_reply_marker("Sv:").
is_reply_marker("SV:").

%-----------------------------------------------------------------------------%

:- pred thread_pager_loop(screen::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

:- pragma inline(thread_pager_loop/5). % force direct tail recursion

thread_pager_loop(Screen, !Info, !IO) :-
    draw_thread_pager(Screen, !.Info, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_keycode(Key, !IO),
    thread_pager_loop_2(Screen, Key, !Info, !IO).

:- pred thread_pager_loop_2(screen::in, keycode::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

thread_pager_loop_2(Screen, Key, !Info, !IO) :-
    thread_pager_input(Key, Action, MessageUpdate, !Info),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = resize,
        create_screen(NewScreen, !IO),
        get_rows_cols(NewScreen, Rows, Cols),
        resize_thread_pager(Rows, Cols, !Info),
        thread_pager_loop(NewScreen, !Info, !IO)
    ;
        Action = start_reply(Message, ReplyKind),
        start_reply(Screen, Message, ReplyKind, !IO),
        % Don't really *need* to refresh if the reply is aborted.
        !Info ^ tp_need_refresh := yes,
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = prompt_save_part(Part),
        prompt_save_part(Screen, Part, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = prompt_open_part(Part),
        prompt_open_part(Screen, Part, MaybeNextKey, !IO),
        (
            MaybeNextKey = yes(NextKey),
            thread_pager_loop_2(Screen, NextKey, !Info, !IO)
        ;
            MaybeNextKey = no,
            thread_pager_loop(Screen, !Info, !IO)
        )
    ;
        Action = prompt_search,
        prompt_search(Screen, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = leave(TagGroups),
        ( map.is_empty(TagGroups) ->
            true
        ;
            map.foldl2(apply_tag_delta, TagGroups, yes, Res, !IO),
            (
                Res = yes,
                update_message(Screen, clear_message, !IO)
            ;
                Res = no,
                Msg = "Encountered problems while applying tags.",
                update_message(Screen, set_warning(Msg), !IO)
            ),
            !Info ^ tp_need_refresh := yes
        )
    ).

:- pred apply_tag_delta(set(tag_delta)::in, list(message_id)::in,
    bool::in, bool::out, io::di, io::uo) is det.

apply_tag_delta(TagDeltaSet, MessageIds, !AccRes, !IO) :-
    set.to_sorted_list(TagDeltaSet, TagDeltas),
    tag_messages(TagDeltas, MessageIds, Res, !IO),
    (
        Res = ok
    ;
        Res = error(_),
        !:AccRes = no
    ).

%-----------------------------------------------------------------------------%

:- pred thread_pager_input(keycode::in, thread_pager_action::out,
    message_update::out, thread_pager_info::in, thread_pager_info::out) is det.

thread_pager_input(Key, Action, MessageUpdate, !Info) :-
    NumPagerRows = !.Info ^ tp_num_pager_rows,
    (
        ( Key = char('j')
        ; Key = code(key_down)
        )
    ->
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('J')
    ->
        set_current_line_read(!Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = char('k')
        ; Key = code(key_up)
        )
    ->
        prev_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('K')
    ->
        set_current_line_read(!Info),
        prev_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('\r')
    ->
        scroll(1, MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('\\')
    ->
        scroll(-1, MessageUpdate, !Info),
        Action = continue
    ;
        Key = char(']')
    ->
        Delta = int.min(15, NumPagerRows - 1),
        scroll_but_stop_at_message(Delta, MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('[')
    ->
        Delta = int.min(15, NumPagerRows - 1),
        scroll_but_stop_at_message(-Delta, MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = char(' ')
        ; Key = code(key_pagedown)
        )
    ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll_but_stop_at_message(Delta, MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = char('b')
        ; Key = code(key_pageup)
        )
    ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll_but_stop_at_message(-Delta, MessageUpdate, !Info),
        Action = continue
    ;
        Key = code(key_home)
    ->
        goto_first_message(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        Key = code(key_end)
    ->
        goto_end(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        Key = char('S')
    ->
        skip_quoted_text(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('\t')
    ->
        skip_to_unread(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('\x12\') % ^R
    ->
        mark_all_read(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        Key = char('N')
    ->
        toggle_unread(!Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('d')
    ->
        toggle_deleted(deleted, !Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('u')
    ->
        toggle_deleted(not_deleted, !Info),
        Action = continue,
        MessageUpdate = clear_message
    ;
        Key = char('F')
    ->
        toggle_flagged(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        Key = char('v')
    ->
        highlight_part(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('s')
    ->
        save_part(Action, MessageUpdate, !Info)
    ;
        Key = char('o')
    ->
        open_part(Action, MessageUpdate, !Info)
    ;
        Key = char('/')
    ->
        Action = prompt_search,
        MessageUpdate = clear_message
    ;
        Key = char('n')
    ->
        skip_to_search(continue_search, MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = char('i')
        ; Key = char('q')
        )
    ->
        get_tag_delta_groups(!.Info, TagGroups),
        Action = leave(TagGroups),
        MessageUpdate = clear_message
    ;
        Key = char('I')
    ->
        mark_all_read(!Info),
        get_tag_delta_groups(!.Info, TagGroups),
        Action = leave(TagGroups),
        MessageUpdate = clear_message
    ;
        Key = char('r')
    ->
        reply(!.Info, direct_reply, Action, MessageUpdate)
    ;
        Key = char('g')
    ->
        reply(!.Info, group_reply, Action, MessageUpdate)
    ;
        Key = char('L')
    ->
        reply(!.Info, list_reply, Action, MessageUpdate)
    ;
        Key = code(key_resize)
    ->
        Action = resize,
        MessageUpdate = no_change
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

:- pred scroll_but_stop_at_message(int::in, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

scroll_but_stop_at_message(Delta, MessageUpdate, !Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    NumPagerRows = !.Info ^ tp_num_pager_rows,
    pager.scroll_but_stop_at_message(NumPagerRows, Delta, MessageUpdate,
        PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred goto_first_message(thread_pager_info::in, thread_pager_info::out)
    is det.

goto_first_message(!Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    pager.goto_first_message(PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred goto_end(thread_pager_info::in, thread_pager_info::out) is det.

goto_end(!Info) :-
    PagerInfo0 = !.Info ^ tp_pager,
    NumRows = !.Info ^ tp_num_pager_rows,
    pager.goto_end(NumRows, PagerInfo0, PagerInfo),
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
        get_top_message(PagerInfo, Message),
        MessageId = Message ^ m_id,
        search_forward(is_message(MessageId), Scrollable0, 0, Cursor, _)
    ->
        set_cursor_centred(Cursor, NumThreadRows, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred skip_to_unread(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

skip_to_unread(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    NumThreadRows = !.Info ^ tp_num_thread_rows,
    PagerInfo0 = !.Info ^ tp_pager,
    (
        get_cursor(Scrollable0, Cursor0),
        search_forward(is_unread_line, Scrollable0, Cursor0 + 1, Cursor,
            ThreadLine)
    ->
        set_cursor_centred(Cursor, NumThreadRows, Scrollable0, Scrollable),
        MessageId = ThreadLine ^ tp_message ^ m_id,
        skip_to_message(MessageId, PagerInfo0, PagerInfo),
        !Info ^ tp_scrollable := Scrollable,
        !Info ^ tp_pager := PagerInfo,
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No more unread messages.")
    ).

:- pred is_message(message_id::in, thread_line::in) is semidet.

is_message(MessageId, Line) :-
    Line ^ tp_message ^ m_id = MessageId.

:- pred is_unread_line(thread_line::in) is semidet.

is_unread_line(Line) :-
    Line ^ tp_unread = _ - unread.

:- pred set_current_line_read(thread_pager_info::in, thread_pager_info::out)
    is det.

set_current_line_read(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        set_line_read(Line0, Line),
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred mark_all_read(thread_pager_info::in, thread_pager_info::out) is det.

mark_all_read(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    scrollable.map_lines(set_line_read, Scrollable0, Scrollable),
    !Info ^ tp_scrollable := Scrollable.

:- pred set_line_read(thread_line::in, thread_line::out) is det.

set_line_read(Line0, Line) :-
    Line0 ^ tp_unread = OrigUnread - _,
    Line = Line0 ^ tp_unread := OrigUnread - read.

:- pred toggle_unread(thread_pager_info::in, thread_pager_info::out) is det.

toggle_unread(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Line0 ^ tp_unread = OrigUnread - Unread0,
        (
            Unread0 = unread,
            Unread = read
        ;
            Unread0 = read,
            Unread = unread
        ),
        Line = Line0 ^ tp_unread := OrigUnread - Unread,
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred toggle_flagged(thread_pager_info::in, thread_pager_info::out) is det.

toggle_flagged(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Line0 ^ tp_flagged = OrigFlag - Flag0,
        (
            Flag0 = flagged,
            Flag = unflagged
        ;
            Flag0 = unflagged,
            Flag = flagged
        ),
        Line = Line0 ^ tp_flagged := OrigFlag - Flag,
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred toggle_deleted(deleted::in,
    thread_pager_info::in, thread_pager_info::out) is det.

toggle_deleted(Deleted, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Line0 ^ tp_deleted = OrigDeleted - _,
        Line = Line0 ^ tp_deleted := OrigDeleted - Deleted,
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred highlight_part(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

highlight_part(MessageUpdate, !Info) :-
    Pager0 = !.Info ^ tp_pager,
    NumRows = !.Info ^ tp_num_pager_rows,
    highlight_part(NumRows, MessageUpdate, Pager0, Pager),
    !Info ^ tp_pager := Pager.

:- pred save_part(thread_pager_action::out, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

save_part(Action, MessageUpdate, !Info) :-
    Pager = !.Info ^ tp_pager,
    ( get_highlighted_part(Pager, Part) ->
        Action = prompt_save_part(Part),
        MessageUpdate = clear_message
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_save_part(screen::in, part::in, io::di, io::uo)
    is det.

prompt_save_part(Screen, Part, !IO) :-
    MessageId = Part ^ pt_msgid,
    PartId = Part ^ pt_part,
    MaybeInitial = Part ^ pt_filename,
    (
        MaybeInitial = yes(Initial)
    ;
        MaybeInitial = no,
        MessageId = message_id(IdStr),
        Initial = string.format("%s.part_%d", [s(IdStr), i(PartId)])
    ),
    text_entry_initial(Screen, "Save to file: ", init_history, Initial,
        complete_path, Return, !IO),
    (
        Return = yes(FileName),
        FileName \= ""
    ->
        FollowSymLinks = no,
        io.file_type(FollowSymLinks, FileName, ResType, !IO),
        (
            ResType = ok(_),
            % XXX prompt to overwrite
            Error = FileName ++ " already exists.",
            MessageUpdate = set_warning(Error)
        ;
            ResType = error(_),
            % This assumes the file doesn't exist.
            do_save_part(MessageId, PartId, FileName, Res, !IO),
            (
                Res = ok,
                ( PartId = 0 ->
                    MessageUpdate = set_info("Message saved.")
                ;
                    MessageUpdate = set_info("Attachment saved.")
                )
            ;
                Res = error(Error),
                ErrorMessage = io.error_message(Error),
                MessageUpdate = set_warning(ErrorMessage)
            )
        )
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_save_part(message_id::in, int::in, string::in,
    io.res::out, io::di, io::uo) is det.

do_save_part(MessageId, Part, FileName, Res, !IO) :-
    Args = [
        "show", "--format=raw", "--part=" ++ from_int(Part),
        message_id_to_search_term(MessageId)
    ],
    args_to_quoted_command(Args, no, redirect_output(FileName), Command),
    get_notmuch_prefix(Notmuch, !IO),
    io.call_system(Notmuch ++ Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("notmuch show returned exit status %d",
                [i(ExitStatus)], Msg),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred open_part(thread_pager_action::out, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

open_part(Action, MessageUpdate, !Info) :-
    Pager = !.Info ^ tp_pager,
    ( get_highlighted_part(Pager, Part) ->
        Action = prompt_open_part(Part),
        MessageUpdate = clear_message
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_open_part(screen::in, part::in, maybe(keycode)::out,
    io::di, io::uo) is det.

prompt_open_part(Screen, Part, MaybeNextKey, !IO) :-
    Initial = "xdg-open",
    text_entry_initial(Screen, "Open with command: ", init_history, Initial,
        complete_path, Return, !IO),
    (
        Return = yes(Command),
        Command \= ""
    ->
        io.make_temp(FileName, !IO),
        MessageId = Part ^ pt_msgid,
        PartId = Part ^ pt_part,
        do_save_part(MessageId, PartId, FileName, Res, !IO),
        (
            Res = ok,
            args_to_quoted_command([Command, FileName], CommandString),
            CallMessage = set_info("Calling " ++ Command ++ "..."),
            update_message_immed(Screen, CallMessage, !IO),
            io.call_system(CommandString, CallRes, !IO),
            (
                CallRes = ok(ExitStatus),
                ( ExitStatus = 0 ->
                    ContMessage = set_info(
                        "Press any key to continue (deletes temporary file)"),
                    update_message_immed(Screen, ContMessage, !IO),
                    get_keycode(Key, !IO),
                    MaybeNextKey = yes(Key),
                    MessageUpdate = clear_message
                ;
                    string.format("%s returned with exit status %d",
                        [s(Command), i(ExitStatus)], Msg),
                    MessageUpdate = set_warning(Msg),
                    MaybeNextKey = no
                )
            ;
                CallRes = error(Error),
                Msg = "Error: " ++ io.error_message(Error),
                MessageUpdate = set_warning(Msg),
                MaybeNextKey = no
            )
        ;
            Res = error(Error),
            string.format("Error saving to %s: %s",
                [s(FileName), s(io.error_message(Error))], Msg),
            MessageUpdate = set_warning(Msg),
            MaybeNextKey = no
        ),
        io.remove_file(FileName, _, !IO)
    ;
        MessageUpdate = clear_message,
        MaybeNextKey = no
    ),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred prompt_search(screen::in, thread_pager_info::in,
    thread_pager_info::out, io::di, io::uo) is det.

prompt_search(Screen, !Info, !IO) :-
    History0 = !.Info ^ tp_search_history,
    text_entry(Screen, "Search for: ", History0, complete_none, Return, !IO),
    (
        Return = yes(Search),
        ( Search = "" ->
            !Info ^ tp_search := no
        ;
            add_history_nodup(Search, History0, History),
            !Info ^ tp_search := yes(Search),
            !Info ^ tp_search_history := History,
            skip_to_search(new_search, MessageUpdate, !Info),
            update_message(Screen, MessageUpdate, !IO)
        )
    ;
        Return = no
    ).

:- pred skip_to_search(search_kind::in, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

skip_to_search(SearchKind, MessageUpdate, !Info) :-
    MaybeSearch = !.Info ^ tp_search,
    (
        MaybeSearch = yes(Search),
        Pager0 = !.Info ^ tp_pager,
        NumRows = !.Info ^ tp_num_pager_rows,
        pager.skip_to_search(NumRows, SearchKind, Search, MessageUpdate,
            Pager0, Pager),
        !Info ^ tp_pager := Pager,
        sync_thread_to_pager(!Info)
    ;
        MaybeSearch = no,
        MessageUpdate = set_warning("No search string.")
    ).

%-----------------------------------------------------------------------------%

:- pred get_tag_delta_groups(thread_pager_info::in,
    map(set(tag_delta), list(message_id))::out) is det.

get_tag_delta_groups(Info, TagGroups) :-
    Scrollable = Info ^ tp_scrollable,
    Lines = get_lines(Scrollable),
    version_array.foldl(get_changed_status_messages_2, Lines,
        map.init, TagGroups).

:- pred get_changed_status_messages_2(thread_line::in,
    map(set(tag_delta), list(message_id))::in,
    map(set(tag_delta), list(message_id))::out) is det.

get_changed_status_messages_2(Line, !TagGroups) :-
    TagSet = get_tag_delta_set(Line),
    ( set.non_empty(TagSet) ->
        MessageId = Line ^ tp_message ^ m_id,
        ( map.search(!.TagGroups, TagSet, Messages0) ->
            map.det_update(TagSet, [MessageId | Messages0], !TagGroups)
        ;
            map.det_insert(TagSet, [MessageId], !TagGroups)
        )
    ;
        true
    ).

:- func get_tag_delta_set(thread_line) = set(tag_delta).

get_tag_delta_set(Line) = TagSet :-
    Line ^ tp_unread = Unread0 - Unread,
    Line ^ tp_flagged = Flag0 - Flag,
    Line ^ tp_deleted = Deleted0 - Deleted,
    some [!TagSet] (
        !:TagSet = set.init,
        (
            Unread = read,
            Unread0 = unread,
            set.insert("-unread", !TagSet)
        ;
            Unread = unread,
            Unread0 = read,
            set.insert("+unread", !TagSet)
        ;
            Unread = read,
            Unread0 = read
        ;
            Unread = unread,
            Unread0 = unread
        ),
        (
            Flag = flagged,
            Flag0 = unflagged,
            set.insert("+flagged", !TagSet)
        ;
            Flag = unflagged,
            Flag0 = flagged,
            set.insert("-flagged", !TagSet)
        ;
            Flag = unflagged,
            Flag0 = unflagged
        ;
            Flag = flagged,
            Flag0 = flagged
        ),
        (
            Deleted = deleted,
            Deleted0 = not_deleted,
            set.insert("+deleted", !TagSet)
        ;
            Deleted = not_deleted,
            Deleted0 = deleted,
            set.insert("-deleted", !TagSet)
        ;
            Deleted = not_deleted,
            Deleted0 = not_deleted
        ;
            Deleted = deleted,
            Deleted0 = deleted
        ),
        TagSet = !.TagSet
    ).

%-----------------------------------------------------------------------------%

:- pred reply(thread_pager_info::in, reply_kind::in, thread_pager_action::out,
    message_update::out) is det.

reply(Info, ReplyKind, Action, MessageUpdate) :-
    PagerInfo = Info ^ tp_pager,
    ( get_top_message(PagerInfo, Message) ->
        MessageUpdate = clear_message,
        Action = start_reply(Message, ReplyKind)
    ;
        MessageUpdate = set_warning("Nothing to reply to."),
        Action = continue
    ).

%-----------------------------------------------------------------------------%

:- pred draw_thread_pager(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

draw_thread_pager(Screen, Info, !IO) :-
    Scrollable = Info ^ tp_scrollable,
    PagerInfo = Info ^ tp_pager,
    split_panels(Screen, Info, ThreadPanels, SepPanel, PagerPanels),
    scrollable.draw(ThreadPanels, Scrollable, !IO),
    get_cols(Screen, Cols),
    draw_sep(Cols, SepPanel, !IO),
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
    Line = thread_line(_Message, From, UnreadPair, Replied, DeletedPair,
        FlaggedPair, Graphics, RelDate, MaybeSubject),
    UnreadPair = _ - Unread,
    DeletedPair = _ - Deleted,
    FlaggedPair = _ - Flagged,
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
        Unread = unread,
        my_addstr(Panel, "n", !IO)
    ;
        Unread = read,
        my_addstr(Panel, " ", !IO)
    ),
    (
        Replied = replied,
        my_addstr(Panel, "r", !IO)
    ;
        Replied = not_replied,
        my_addstr(Panel, " ", !IO)
    ),
    (
        Deleted = deleted,
        my_addstr(Panel, "d", !IO)
    ;
        Deleted = not_deleted,
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
    my_addstr(Panel, From, !IO),
    (
        MaybeSubject = yes(Subject),
        my_addstr(Panel, ". ", !IO),
        cond_attr_set(Panel, fg_bg(green, black), IsCursor, !IO),
        my_addstr(Panel, Subject, !IO)
    ;
        MaybeSubject = no
    ).

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
    get_main_panels(Screen, Panels0),
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
