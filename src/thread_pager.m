% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module thread_pager.
:- interface.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module crypto.
:- import_module data.
:- import_module prog_config.
:- import_module screen.
:- import_module tags.
:- import_module view_common.

%-----------------------------------------------------------------------------%

:- type thread_pager_effects
    --->    thread_pager_effects(
                % Set of tags for the thread.
                thread_tags     :: set(tag),

                % Tag changes to be applied to messages.
                tag_changes     :: map(set(tag_delta), list(message_id)),

                % Number of messages added (by sending).
                added_messages  :: int
            ).

:- pred open_thread_pager(prog_config::in, crypto::in, screen::in,
    thread_id::in, maybe(string)::in,
    screen_transition(thread_pager_effects)::out,
    common_history::in, common_history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.
:- import_module time.
:- import_module version_array.

:- import_module addressbook.
:- import_module callout.
:- import_module color.
:- import_module compose.
:- import_module curs.
:- import_module curs.panel.
:- import_module pager.
:- import_module path_expand.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module recall.
:- import_module resend.
:- import_module scrollable.
:- import_module shell_word.
:- import_module string_util.
:- import_module sys_util.
:- import_module text_entry.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type thread_pager_info
    --->    thread_pager_info(
                tp_config           :: prog_config,
                tp_crypto           :: crypto,
                tp_thread_id        :: thread_id,
                tp_messages         :: list(message),
                tp_ordering         :: ordering,
                tp_scrollable       :: scrollable(thread_line),
                tp_num_thread_rows  :: int,
                tp_pager            :: pager_info,
                tp_num_pager_rows   :: int,
                tp_search           :: maybe(string),
                tp_search_dir       :: search_direction,
                tp_common_history   :: common_history,
                tp_added_messages   :: int
            ).

:- type ordering
    --->    ordering_threaded
    ;       ordering_flat.

:- type thread_line
    --->    thread_line(
                tp_message      :: message,
                tp_parent       :: maybe(message_id),
                tp_clean_from   :: string,
                tp_prev_tags    :: set(tag),
                tp_curr_tags    :: set(tag),
                tp_std_tags     :: standard_tags,   % cached from tp_curr_tags
                tp_nonstd_tags_width :: int,
                tp_selected     :: selected,
                tp_graphics     :: maybe(list(graphic)),
                tp_reldate      :: string,
                tp_subject      :: maybe(header_value)
            ).

:- type selected
    --->    not_selected
    ;       selected.

:- type graphic
    --->    blank
    ;       vert
    ;       tee
    ;       ell.

:- type message_tag_deltas
    --->    message_tag_deltas(
                mtd_add_tags    :: set(tag),
                mtd_remove_tags :: set(tag)
            ).

:- type thread_pager_action
    --->    continue
    ;       resize
    ;       start_reply(message, reply_kind)
    ;       prompt_resend(message_id)
    ;       start_recall
    ;       edit_as_template(message)
    ;       prompt_tag(string)
    ;       bulk_tag(keep_selection)
    ;       prompt_save_part(part, maybe(header_value))
    ;       prompt_open_part(part)
    ;       prompt_open_url(string)
    ;       prompt_search(search_direction)
    ;       toggle_content(toggle_type)
    ;       toggle_ordering
    ;       addressbook_add
    ;       refresh_results
    ;       leave.

:- type keep_selection
    --->    clear_selection
    ;       keep_selection.

:- type arbitrary_tag_changes
    --->    no
    ;       yes(
                add_tags    :: set(tag),
                remove_tags :: set(tag)
            ).

:- type message_flat
    --->    message_flat(
                mf_message      :: message,
                mf_maybe_parent :: maybe(message_id)
            ).

%-----------------------------------------------------------------------------%

open_thread_pager(Config, Crypto, Screen, ThreadId, MaybeSearch, Transition,
        CommonHistory0, CommonHistory, !IO) :-
    create_thread_pager(Config, Crypto, Screen, ThreadId, ordering_threaded,
        no, CommonHistory0, Info0, ResCount, !IO),
    Info1 = Info0 ^ tp_search := MaybeSearch,
    (
        ResCount = ok(Count),
        string.format("Showing %d messages.", [i(Count)], Msg),
        MessageUpdate = set_info(Msg)
    ;
        ResCount = error(Error),
        MessageUpdate = set_warning(Error)
    ),
    update_message(Screen, MessageUpdate, !IO),
    thread_pager_loop(Screen, Info1, Info, !IO),
    get_effects(Info, Effects),
    Transition = screen_transition(Effects, no_change),
    CommonHistory = Info ^ tp_common_history.

:- pred reopen_thread_pager(screen::in, bool::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

reopen_thread_pager(Screen, KeepCached, !Info, !IO) :-
    Ordering = !.Info ^ tp_ordering,
    reopen_thread_pager_with_ordering(Screen, KeepCached, Ordering,
        !Info, !IO).

:- pred reopen_thread_pager_with_ordering(screen::in, bool::in, ordering::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

reopen_thread_pager_with_ordering(Screen, KeepCached, Ordering, Info0, Info,
        !IO) :-
    Info0 = thread_pager_info(Config0, Crypto0, ThreadId0, Cached0, _Ordering0,
        Scrollable0, _NumThreadRows, Pager0, _NumPagerRows,
        Search, SearchDir, CommonHistory, AddedMessages),
    (
        KeepCached = yes,
        Cached0 = [_ | _]
    ->
        MaybeCached = yes(Cached0)
    ;
        MaybeCached = no
    ),
    create_thread_pager(Config0, Crypto0, Screen, ThreadId0, Ordering,
        MaybeCached, CommonHistory, Info1, ResCount, !IO),
    (
        ResCount = ok(Count),
        string.format("Showing %d messages.", [i(Count)], Msg),
        MessageUpdate = set_info(Msg)
    ;
        ResCount = error(Error),
        MessageUpdate = set_warning(Error)
    ),

    Info1 = thread_pager_info(Config, Crypto, ThreadId, Cached, _Ordering,
        Scrollable1, NumThreadRows, Pager1, NumPagerRows,
        _Search1, _SearchDir1, _CommonHistory1, _AddedMessages1),

    % Reapply tag changes from previous state.
    ThreadLines0 = get_lines_list(Scrollable0),
    ThreadLines1 = get_lines_list(Scrollable1),
    list.foldl(create_tag_delta_map, ThreadLines0, map.init, DeltaMap),
    list.map(restore_tag_deltas(DeltaMap), ThreadLines1, ThreadLines),
    Scrollable = scrollable.init(ThreadLines),

    % Restore cursor and pager position.
    (
        get_cursor_line(Scrollable0, _Cursor, CursorLine0),
        Message0 = CursorLine0 ^ tp_message,
        Message0 = message(MessageId0, _, _, _, _, _)
    ->
        pager.skip_to_message(MessageId0, Pager1, Pager2),
        ( get_top_offset(Pager0, TopOffset) ->
            pager.scroll_but_stop_at_message(NumPagerRows, TopOffset, _,
                Pager2, Pager)
        ;
            Pager = Pager2
        )
    ;
        Pager = Pager1
    ),

    Info2 = thread_pager_info(Config, Crypto, ThreadId, Cached, Ordering,
        Scrollable, NumThreadRows, Pager, NumPagerRows,
        Search, SearchDir, CommonHistory, AddedMessages),
    sync_thread_to_pager(Info2, Info),

    update_message(Screen, MessageUpdate, !IO).

:- pred create_thread_pager(prog_config::in, crypto::in, screen::in,
    thread_id::in, ordering::in, maybe(list(message))::in, common_history::in,
    thread_pager_info::out, maybe_error(int)::out, io::di, io::uo) is det.

create_thread_pager(Config, Crypto, Screen, ThreadId, Ordering, MaybeCached,
        CommonHistory, Info, ResCount, !IO) :-
    (
        MaybeCached = yes(MessagesCached),
        ParseResult = ok(MessagesCached)
    ;
        MaybeCached = no,
        run_notmuch(Config, [
            "show", "--format=json", "--entire-thread=false",
            "--", thread_id_to_search_term(ThreadId)
        ], parse_messages_list, ParseResult, !IO)
    ),
    (
        ParseResult = ok(Messages)
    ;
        ParseResult = error(_),
        Messages = []
    ),
    time(Time, !IO),
    Nowish = localtime(Time),
    get_rows_cols(Screen, Rows, Cols),
    setup_thread_pager(Config, Crypto, ThreadId, Ordering, Nowish, Rows - 2,
        Cols, Messages, CommonHistory, Info, Count, !IO),
    (
        ParseResult = ok(_),
        ResCount = ok(Count)
    ;
        ParseResult = error(Error),
        ResCount = error("Error parsing notmuch response: " ++ Error)
    ).

:- pred setup_thread_pager(prog_config::in, crypto::in, thread_id::in,
    ordering::in, tm::in, int::in, int::in, list(message)::in,
    common_history::in, thread_pager_info::out, int::out, io::di, io::uo)
    is det.

setup_thread_pager(Config, Crypto, ThreadId, Ordering, Nowish, Rows, Cols,
        Messages, CommonHistory, ThreadPagerInfo, NumThreadLines, !IO) :-
    (
        Ordering = ordering_threaded,
        append_threaded_messages(Nowish, Messages, ThreadLines),
        setup_pager(Config, include_replies, Cols, Messages, PagerInfo0, !IO)
    ;
        Ordering = ordering_flat,
        append_flat_messages(Nowish, Messages, ThreadLines,
            SortedFlatMessages),
        setup_pager(Config, toplevel_only, Cols, SortedFlatMessages,
            PagerInfo0, !IO)
    ),
    Scrollable0 = scrollable.init_with_cursor(ThreadLines),
    NumThreadLines = get_num_lines(Scrollable0),
    compute_num_rows(Rows, Scrollable0, NumThreadRows, NumPagerRows),
    (
        ThreadLines = [],
        Scrollable = Scrollable0,
        PagerInfo = PagerInfo0
    ;
        ThreadLines = [FirstLine | RestLines],
        ( list.find_first_match(is_unread_line, ThreadLines, UnreadLine) ->
            CursorLine = UnreadLine
        ;
            list.foldl(get_latest_line, RestLines, FirstLine, LatestLine),
            CursorLine = LatestLine
        ),
        Message = CursorLine ^ tp_message,
        (
            Message = message(MessageId, _, _, _, _, _),
            goto_message(MessageId, NumThreadRows, Scrollable0, Scrollable),
            pager.skip_to_message(MessageId, PagerInfo0, PagerInfo)
        ;
            Message = excluded_message(_),
            Scrollable = Scrollable0,
            PagerInfo = PagerInfo0
        )
    ),
    AddedMessages = 0,
    ThreadPagerInfo = thread_pager_info(Config, Crypto, ThreadId, Messages,
        Ordering, Scrollable, NumThreadRows, PagerInfo, NumPagerRows, no,
        dir_forward, CommonHistory, AddedMessages).

:- pred get_latest_line(thread_line::in, thread_line::in, thread_line::out)
    is det.

get_latest_line(LineA, LineB, Line) :-
    TimestampA = get_timestamp_fallback(LineA ^ tp_message),
    TimestampB = get_timestamp_fallback(LineB ^ tp_message),
    ( TimestampA >= TimestampB ->
        Line = LineA
    ;
        Line = LineB
    ).

:- pred resize_thread_pager(screen::in,
    thread_pager_info::in, thread_pager_info::out) is det.

resize_thread_pager(Screen, !Info) :-
    get_rows_cols(Screen, Rows, _Cols),
    Scrollable0 = !.Info ^ tp_scrollable,
    compute_num_rows(Rows - 2, Scrollable0, NumThreadRows, NumPagerRows),
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

:- pred append_threaded_messages(tm::in, list(message)::in,
    list(thread_line)::out) is det.

append_threaded_messages(Nowish, Messages, ThreadLines) :-
    PrevSubject = decoded_unstructured(""),
    append_threaded_messages(Nowish, [], [], no, Messages, PrevSubject,
        cord.init, ThreadCord),
    ThreadLines = list(ThreadCord).

:- pred append_threaded_messages(tm::in, list(graphic)::in, list(graphic)::in,
    maybe(message_id)::in, list(message)::in, header_value::in,
    cord(thread_line)::in, cord(thread_line)::out) is det.

append_threaded_messages(_Nowish, _Above, _Below, _MaybeParentId,
        [], _PrevSubject, !Cord).
append_threaded_messages(Nowish, Above0, Below0, MaybeParentId,
        [Message | Messages], PrevSubject, !Cord) :-
    (
        Messages = [],
        Graphics = Above0 ++ [ell],
        make_thread_line(Nowish, Message, MaybeParentId, yes(Graphics),
            PrevSubject, Line),
        snoc(Line, !Cord),
        MessagesCord = cord.empty,
        Below1 = Below0
    ;
        Messages = [_ | _],
        Graphics = Above0 ++ [tee],
        make_thread_line(Nowish, Message, MaybeParentId, yes(Graphics),
            PrevSubject, Line),
        snoc(Line, !Cord),
        get_last_subject(Message, LastSubject),
        append_threaded_messages(Nowish, Above0, Below0, MaybeParentId,
            Messages, LastSubject, cord.init, MessagesCord),
        ( get_first(MessagesCord, FollowingLine) ->
            MaybeGraphics = FollowingLine ^ tp_graphics,
            (
                MaybeGraphics = yes(Below1)
            ;
                MaybeGraphics = no,
                Below1 = []
            )
        ;
            unexpected($module, $pred, "empty cord")
        )
    ),
    ( not_blank_at_column(Below1, length(Above0)) ->
        Above1 = Above0 ++ [vert]
    ;
        Above1 = Above0 ++ [blank]
    ),
    MaybeMessageId = get_maybe_message_id(Message),
    Subject = get_subject_fallback(Message),
    Replies = get_replies(Message),
    append_threaded_messages(Nowish, Above1, Below1, MaybeMessageId, Replies,
        Subject, !Cord),
    !:Cord = !.Cord ++ MessagesCord.

:- pred get_last_subject(message::in, header_value::out) is det.

get_last_subject(Message, LastSubject) :-
    Replies = get_replies(Message),
    ( list.last(Replies, LastReply) ->
        get_last_subject(LastReply, LastSubject)
    ;
        LastSubject = get_subject_fallback(Message)
    ).

:- pred not_blank_at_column(list(graphic)::in, int::in) is semidet.

not_blank_at_column(Graphics, Col) :-
    list.index0(Graphics, Col, Graphic),
    Graphic \= blank.

:- pred append_flat_messages(tm::in, list(message)::in,
    list(thread_line)::out, list(message)::out) is det.

append_flat_messages(Nowish, Messages, ThreadLines, SortedFlatMessages) :-
    flatten_messages(no, Messages, [], MessagesFlat0),
    list.sort(compare_by_timestamp, MessagesFlat0, MessagesFlat),
    list.foldl2(append_flat_message(Nowish), MessagesFlat,
        decoded_unstructured(""), _PrevSubject, [], RevThreadLines),
    list.reverse(RevThreadLines, ThreadLines),
    SortedFlatMessages = list.map(mf_message, MessagesFlat).

:- pred flatten_messages(maybe(message_id)::in, list(message)::in,
    list(message_flat)::in, list(message_flat)::out) is det.

flatten_messages(_MaybeParentId, [], !Acc).
flatten_messages(MaybeParentId, [Message | Messages], !Acc) :-
    MaybeMessageId = get_maybe_message_id(Message),
    Replies = get_replies(Message),
    list.cons(message_flat(Message, MaybeParentId), !Acc),
    flatten_messages(MaybeMessageId, Replies, !Acc),
    flatten_messages(MaybeParentId, Messages, !Acc).

:- pred compare_by_timestamp(message_flat::in, message_flat::in,
    comparison_result::out) is det.

compare_by_timestamp(A, B, Rel) :-
    TimestampA = get_timestamp_fallback(A ^ mf_message),
    TimestampB = get_timestamp_fallback(B ^ mf_message),
    compare(Rel, TimestampA, TimestampB).

:- pred append_flat_message(tm::in, message_flat::in, header_value::in,
    header_value::out, list(thread_line)::in, list(thread_line)::out) is det.

append_flat_message(Nowish, MessageFlat, PrevSubject, Subject, !RevAcc) :-
    MessageFlat = message_flat(Message, MaybeParentId),
    make_thread_line(Nowish, Message, MaybeParentId, no, PrevSubject, Line),
    Subject = get_subject_fallback(Message),
    cons(Line, !RevAcc).

:- func mf_message(message_flat) = message. % accessor

:- pred make_thread_line(tm::in, message::in, maybe(message_id)::in,
    maybe(list(graphic))::in, header_value::in, thread_line::out) is det.

make_thread_line(Nowish, Message, MaybeParentId, MaybeGraphics, PrevSubject,
        Line) :-
    (
        Message = message(_Id, Timestamp, Headers, Tags, _Body, _Replies),
        From = Headers ^ h_from,
        CleanFrom = clean_email_address(header_value_string(From)),
        Subject = Headers ^ h_subject
    ;
        Message = excluded_message(_Replies),
        Timestamp = get_timestamp_fallback(Message),
        CleanFrom = "",
        Subject = get_subject_fallback(Message),
        Tags = set.init
    ),
    get_standard_tags(Tags, StdTags, NonstdTagsWidth),
    timestamp_to_tm(Timestamp, TM),
    make_reldate(Nowish, TM, no, RelDate),
    ( canonicalise_subject(Subject) = canonicalise_subject(PrevSubject) ->
        MaybeSubject = no
    ;
        MaybeSubject = yes(Subject)
    ),
    Line = thread_line(Message, MaybeParentId, CleanFrom, Tags, Tags, StdTags,
        NonstdTagsWidth, not_selected, MaybeGraphics, RelDate, MaybeSubject).

:- func clean_email_address(string) = string.

clean_email_address(Orig) = Clean :-
    % Maybe should do proper address parsing now.
    (
        strrchr(Orig, '<', Index),
        string.unsafe_prev_index(Orig, Index, SpaceIndex, ' ')
    ->
        string.unsafe_between(Orig, 0, SpaceIndex, Clean)
    ;
        Clean = Orig
    ).

:- func canonicalise_subject(header_value) = list(string).

canonicalise_subject(Subject) = Words :-
    String = header_value_string(Subject),
    list.negated_filter(is_reply_marker, string.words(String), Words).

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

:- pred create_tag_delta_map(thread_line::in,
    map(message_id, message_tag_deltas)::in,
    map(message_id, message_tag_deltas)::out) is det.

create_tag_delta_map(ThreadLine, !DeltaMap) :-
    Message = ThreadLine ^ tp_message,
    (
        Message = message(MessageId, _, _, _, _, _),
        PrevTags = ThreadLine ^ tp_prev_tags,
        CurrTags = ThreadLine ^ tp_curr_tags,
        set.difference(CurrTags, PrevTags, AddTags),
        set.difference(PrevTags, CurrTags, RemoveTags),
        (
            set.is_empty(AddTags),
            set.is_empty(RemoveTags)
        ->
            true
        ;
            Deltas = message_tag_deltas(AddTags, RemoveTags),
            map.det_insert(MessageId, Deltas, !DeltaMap)
        )
    ;
        Message = excluded_message(_)
    ).

:- pred restore_tag_deltas(map(message_id, message_tag_deltas)::in,
    thread_line::in, thread_line::out) is det.

restore_tag_deltas(DeltaMap, ThreadLine0, ThreadLine) :-
    ThreadLine0 = thread_line(Message, ParentId, From, PrevTags, CurrTags0,
        _StdTags0, _NonstdTagsWidth0, Selected, ModeGraphics, RelDate,
        MaybeSubject),
    (
        Message = message(MessageId, _, _, _, _, _),
        ( map.search(DeltaMap, MessageId, Deltas) ->
            Deltas = message_tag_deltas(AddTags, RemoveTags),
            set.difference(CurrTags0, RemoveTags, CurrTags1),
            set.union(CurrTags1, AddTags, CurrTags),
            get_standard_tags(CurrTags, StdTags, NonstdTagsWidth),
            ThreadLine = thread_line(Message, ParentId, From, PrevTags, CurrTags,
                StdTags, NonstdTagsWidth, Selected, ModeGraphics, RelDate,
                MaybeSubject)
        ;
            ThreadLine = ThreadLine0
        )
    ;
        Message = excluded_message(_),
        ThreadLine = ThreadLine0
    ).

%-----------------------------------------------------------------------------%

:- pred handle_screen_transition(screen::in, screen::out,
    screen_transition(T)::in, T::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_screen_transition(Screen0, Screen, Transition, T, !Info, !IO) :-
    Transition = screen_transition(T, MessageUpdate),
    fast_forward_screen(Screen0, Screen, Resized, !IO),
    (
        Resized = yes,
        resize_thread_pager(Screen, !Info)
    ;
        Resized = no
    ),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred thread_pager_loop(screen::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

:- pragma inline(thread_pager_loop/5). % force direct tail recursion

thread_pager_loop(Screen, !Info, !IO) :-
    draw_thread_pager(Screen, !.Info, !IO),
    draw_status_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_keycode_blocking(Key, !IO),
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
        replace_screen_for_resize(Screen, NewScreen, !IO),
        resize_thread_pager(NewScreen, !Info),
        thread_pager_loop(NewScreen, !Info, !IO)
    ;
        Action = start_reply(Message, ReplyKind),
        (
            Message = message(_, _, _, _, _, _),
            Config = !.Info ^ tp_config,
            Crypto = !.Info ^ tp_crypto,
            start_reply(Config, Crypto, Screen, Message, ReplyKind, Transition,
                !IO),
            handle_screen_transition(Screen, NewScreen, Transition, Sent,
                !Info, !IO),
            (
                Sent = sent,
                AddedMessages0 = !.Info ^ tp_added_messages,
                !Info ^ tp_added_messages := AddedMessages0 + 1,
                % XXX would be nice to move cursor to the sent message
                reopen_thread_pager(NewScreen, no, !Info, !IO)
            ;
                Sent = not_sent
            ),
            thread_pager_loop(NewScreen, !Info, !IO)
        ;
            Message = excluded_message(_),
            thread_pager_loop(Screen, !Info, !IO)
        )
    ;
        Action = prompt_resend(MessageId),
        handle_resend(Screen, MessageId, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = start_recall,
        ThreadId = !.Info ^ tp_thread_id,
        handle_recall(Screen, NewScreen, ThreadId, Sent, !Info, !IO),
        (
            Sent = sent,
            AddedMessages0 = !.Info ^ tp_added_messages,
            !Info ^ tp_added_messages := AddedMessages0 + 1,
            reopen_thread_pager(NewScreen, no, !Info, !IO)
        ;
            Sent = not_sent
        ),
        thread_pager_loop(NewScreen, !Info, !IO)
    ;
        Action = edit_as_template(Message),
        handle_edit_as_template(Screen, NewScreen, Message, Sent, !Info, !IO),
        (
            Sent = sent,
            AddedMessages0 = !.Info ^ tp_added_messages,
            !Info ^ tp_added_messages := AddedMessages0 + 1,
            reopen_thread_pager(NewScreen, no, !Info, !IO)
        ;
            Sent = not_sent
        ),
        thread_pager_loop(NewScreen, !Info, !IO)
    ;
        Action = prompt_tag(Initial),
        prompt_tag(Screen, Initial, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = bulk_tag(KeepSelection),
        bulk_tag(Screen, Done, !Info, !IO),
        (
            Done = yes,
            KeepSelection = clear_selection
        ->
            unselect_all(_MessageUpdate, !Info)
        ;
            true
        ),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = prompt_save_part(Part, MaybeSubject),
        prompt_save_part(Screen, Part, MaybeSubject, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = prompt_open_part(Part),
        prompt_open_part(Screen, Part, MaybeNextKey, !Info, !IO),
        (
            MaybeNextKey = yes(NextKey),
            thread_pager_loop_2(Screen, NextKey, !Info, !IO)
        ;
            MaybeNextKey = no,
            thread_pager_loop(Screen, !Info, !IO)
        )
    ;
        Action = prompt_open_url(Url),
        prompt_open_url(Screen, Url, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = prompt_search(SearchDir),
        prompt_search(Screen, SearchDir, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = toggle_content(ToggleType),
        toggle_content(Screen, ToggleType, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = toggle_ordering,
        toggle_ordering(!.Info, Ordering),
        reopen_thread_pager_with_ordering(Screen, yes, Ordering, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = addressbook_add,
        addressbook_add(Screen, !.Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = refresh_results,
        reopen_thread_pager(Screen, no, !Info, !IO),
        thread_pager_loop(Screen, !Info, !IO)
    ;
        Action = leave
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
        ( Key = code(key_home)
        ; Key = char('g')
        )
    ->
        goto_first_message(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        ( Key = code(key_end)
        ; Key = char('G')
        )
    ->
        goto_end(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        Key = char('p')
    ->
        goto_parent_message(MessageUpdate, !Info),
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
        mark_preceding_read(!Info),
        next_message(MessageUpdate, !Info),
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
        Key = char('+')
    ->
        Action = prompt_tag("+"),
        MessageUpdate = clear_message
    ;
        Key = char('-')
    ->
        Action = prompt_tag("-"),
        MessageUpdate = clear_message
    ;
        Key = char('t')
    ->
        toggle_select(!Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('T')
    ->
        unselect_all(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('''')
    ->
        Action = bulk_tag(clear_selection),
        MessageUpdate = clear_message
    ;
        Key = char('"')
    ->
        Action = bulk_tag(keep_selection),
        MessageUpdate = clear_message
    ;
        Key = char('v')
    ->
        highlight_minor(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('V')
    ->
        highlight_major(MessageUpdate, !Info),
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
        Key = char('z')
    ->
        Action = toggle_content(cycle_alternatives),
        MessageUpdate = clear_message
    ;
        Key = char('Z')
    ->
        Action = toggle_content(toggle_expanded),
        MessageUpdate = clear_message
    ;
        Key = char('/')
    ->
        Action = prompt_search(dir_forward),
        MessageUpdate = clear_message
    ;
        Key = char('?')
    ->
        Action = prompt_search(dir_reverse),
        MessageUpdate = clear_message
    ;
        Key = char('n')
    ->
        skip_to_search(continue_search, MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('O')
    ->
        Action = toggle_ordering,
        MessageUpdate = no_change
    ;
        Key = char('=')
    ->
        Action = refresh_results,
        MessageUpdate = clear_message
    ;
        ( Key = char('i')
        ; Key = char('q')
        )
    ->
        Action = leave,
        MessageUpdate = clear_message
    ;
        Key = char('I')
    ->
        mark_all_read(!Info),
        Action = leave,
        MessageUpdate = clear_message
    ;
        Key = char('r')
    ->
        reply(!.Info, direct_reply, Action, MessageUpdate)
    ;
        Key = char('e')
    ->
        reply(!.Info, group_reply, Action, MessageUpdate)
    ;
        Key = char('L')
    ->
        reply(!.Info, list_reply, Action, MessageUpdate)
    ;
        Key = char('B')
    ->
        resend(!.Info, Action, MessageUpdate)
    ;
        Key = char('R')
    ->
        Action = start_recall,
        MessageUpdate = no_change
    ;
        Key = char('E')
    ->
        edit_as_template(!.Info, Action, MessageUpdate)
    ;
        Key = char('a')
    ->
        Action = addressbook_add,
        MessageUpdate = no_change
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

:- pred goto_parent_message(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

goto_parent_message(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    NumThreadRows = !.Info ^ tp_num_thread_rows,
    PagerInfo0 = !.Info ^ tp_pager,
    ( get_cursor_line(Scrollable0, Cursor0, ThreadLine) ->
        (
            ThreadLine ^ tp_parent = yes(ParentId),
            search_reverse(is_message(ParentId), Scrollable0, Cursor0, Cursor)
        ->
            set_cursor_centred(Cursor, NumThreadRows, Scrollable0, Scrollable),
            skip_to_message(ParentId, PagerInfo0, PagerInfo),
            !Info ^ tp_scrollable := Scrollable,
            !Info ^ tp_pager := PagerInfo,
            MessageUpdate = clear_message
        ;
            MessageUpdate = set_warning("Message has no parent.")
        )
    ;
        MessageUpdate = clear_message
    ).

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
        MessageId = Message ^ m_id
    ->
        goto_message(MessageId, NumThreadRows, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred goto_message(message_id::in, int::in,
    scrollable(thread_line)::in, scrollable(thread_line)::out) is det.

goto_message(MessageId, NumThreadRows, !Scrollable) :-
    ( search_forward(is_message(MessageId), !.Scrollable, 0, Cursor, _) ->
        set_cursor_centred(Cursor, NumThreadRows, !Scrollable)
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
        ( MessageId = ThreadLine ^ tp_message ^ m_id ->
            skip_to_message(MessageId, PagerInfo0, PagerInfo),
            !Info ^ tp_scrollable := Scrollable,
            !Info ^ tp_pager := PagerInfo
        ;
            true
        ),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No more unread messages.")
    ).

:- pred is_message(message_id::in, thread_line::in) is semidet.

is_message(MessageId, Line) :-
    Line ^ tp_message ^ m_id = MessageId.

:- pred is_unread_line(thread_line::in) is semidet.

is_unread_line(Line) :-
    Line ^ tp_std_tags ^ unread = unread.

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

:- pred mark_preceding_read(thread_pager_info::in, thread_pager_info::out)
    is det.

mark_preceding_read(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor(Scrollable0, Cursor) ->
        mark_preceding_read_2(Cursor, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred mark_preceding_read_2(int::in,
    scrollable(thread_line)::in, scrollable(thread_line)::out) is det.

mark_preceding_read_2(LineNum, !Scrollable) :-
    (
        get_line(!.Scrollable, LineNum, Line0),
        is_unread_line(Line0)
    ->
        set_line_read(Line0, Line),
        set_line(LineNum, Line, !Scrollable),
        mark_preceding_read_2(LineNum - 1, !Scrollable)
    ;
        true
    ).

:- pred mark_all_read(thread_pager_info::in, thread_pager_info::out) is det.

mark_all_read(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    scrollable.map_lines(set_line_read, Scrollable0, Scrollable),
    !Info ^ tp_scrollable := Scrollable.

:- pred set_line_read(thread_line::in, thread_line::out) is det.

set_line_read(!Line) :-
    Tags0 = !.Line ^ tp_curr_tags,
    set.delete(tag("unread"), Tags0, Tags),
    !Line ^ tp_curr_tags := Tags,
    !Line ^ tp_std_tags ^ unread := read.

:- pred set_line_unread(thread_line::in, thread_line::out) is det.

set_line_unread(!Line) :-
    Tags0 = !.Line ^ tp_curr_tags,
    set.insert(tag("unread"), Tags0, Tags),
    !Line ^ tp_curr_tags := Tags,
    !Line ^ tp_std_tags ^ unread := unread.

:- pred toggle_unread(thread_pager_info::in, thread_pager_info::out) is det.

toggle_unread(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Unread0 = Line0 ^ tp_std_tags ^ unread,
        (
            Unread0 = unread,
            set_line_read(Line0, Line)
        ;
            Unread0 = read,
            set_line_unread(Line0, Line)
        ),
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred toggle_flagged(thread_pager_info::in, thread_pager_info::out) is det.

toggle_flagged(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        TagSet0 = Line0 ^ tp_curr_tags,
        Flagged0 = Line0 ^ tp_std_tags ^ flagged,
        (
            Flagged0 = flagged,
            set.delete(tag("flagged"), TagSet0, TagSet),
            Flagged = unflagged
        ;
            Flagged0 = unflagged,
            set.insert(tag("flagged"), TagSet0, TagSet),
            Flagged = flagged
        ),
        Line1 = Line0 ^ tp_curr_tags := TagSet,
        Line = Line1 ^ tp_std_tags ^ flagged := Flagged,
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
        TagSet0 = Line0 ^ tp_curr_tags,
        (
            Deleted = not_deleted,
            set.delete(tag("deleted"), TagSet0, TagSet)
        ;
            Deleted = deleted,
            set.insert(tag("deleted"), TagSet0, TagSet)
        ),
        Line1 = Line0 ^ tp_curr_tags := TagSet,
        Line = Line1 ^ tp_std_tags ^ deleted := Deleted,
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred prompt_tag(screen::in, string::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

prompt_tag(Screen, Initial, !Info, !IO) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        gather_initial_tags(CursorLine0, no, _AndTagSet, set.init, BothTagSet),
        set.map(tag_to_string, BothTagSet, BothStringSet),
        Config = !.Info ^ tp_config,
        Completion = complete_tags_smart(Config, BothStringSet, BothStringSet),
        prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
            !Info, !IO),
        (
            TagChanges = yes(AddTags, RemoveTags),
            CursorLine0 = thread_line(Message, ParentId, From,
                PrevTags, CurrTags0, _StdTags0, _NonstdTagsWidth0,
                Selected, ModeGraphics, RelDate, MaybeSubject),
            % Notmuch performs tag removals before addition.
            set.difference(CurrTags0, RemoveTags, CurrTags1),
            set.union(CurrTags1, AddTags, CurrTags),
            get_standard_tags(CurrTags, StdTags, NonstdTagsWidth),
            CursorLine = thread_line(Message, ParentId, From,
                PrevTags, CurrTags, StdTags, NonstdTagsWidth,
                Selected, ModeGraphics, RelDate, MaybeSubject),
            set_cursor_line(CursorLine, Scrollable0, Scrollable),
            !Info ^ tp_scrollable := Scrollable
        ;
            TagChanges = no
        )
    ;
        true
    ).

:- pred prompt_arbitrary_tag_changes(screen::in, string::in,
    completion_type::in, arbitrary_tag_changes::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
        !Info, !IO) :-
    History0 = !.Info ^ tp_common_history ^ ch_tag_history,
    FirstTime = no,
    text_entry_full(Screen, "Change tags: ", History0, Initial,
        Completion, FirstTime, Return, !IO),
    (
        Return = yes(String),
        (
            Words = string.words(String),
            Words = [_ | _]
        ->
            add_history_nodup(String, History0, History),
            !Info ^ tp_common_history ^ ch_tag_history := History,
            ( validate_tag_deltas(Words, _TagDeltas, AddTags, RemoveTags) ->
                TagChanges = yes(AddTags, RemoveTags)
            ;
                TagChanges = no,
                update_message(Screen,
                    set_warning("Tags must be of the form +tag or -tag."),
                    !IO)
            )
        ;
            TagChanges = no
        )
    ;
        Return = no,
        TagChanges = no
    ).

%-----------------------------------------------------------------------------%

:- pred toggle_select(thread_pager_info::in, thread_pager_info::out) is det.

toggle_select(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        Selected0 = CursorLine0 ^ tp_selected,
        (
            Selected0 = selected,
            Selected = not_selected
        ;
            Selected0 = not_selected,
            Selected = selected
        ),
        CursorLine = CursorLine0 ^ tp_selected := Selected,
        set_cursor_line(CursorLine, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred unselect_all(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

unselect_all(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    map_lines(unselect_line, Scrollable0, Scrollable),
    !Info ^ tp_scrollable := Scrollable,
    MessageUpdate = set_info("Unselected all messages.").

:- pred unselect_line(thread_line::in, thread_line::out) is det.

unselect_line(!Line) :-
    !Line ^ tp_selected := not_selected.

%-----------------------------------------------------------------------------%

:- pred bulk_tag(screen::in, bool::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

bulk_tag(Screen, Done, !Info, !IO) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    ( any_selected_line(Lines0) ->
        Prompt = "Action: (d)elete, (u)ndelete, (N) toggle unread, " ++
            "(') mark read, (+/-) change tags",
        update_message_immed(Screen, set_prompt(Prompt), !IO),
        get_keycode_blocking(KeyCode, !IO),
        ( KeyCode = char('-') ->
            Config = !.Info ^ tp_config,
            init_bulk_tag_completion(Config, Lines0, Completion),
            bulk_arbitrary_tag_changes(Screen, "-", Completion, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('+') ->
            Config = !.Info ^ tp_config,
            init_bulk_tag_completion(Config, Lines0, Completion),
            bulk_arbitrary_tag_changes(Screen, "+", Completion, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('d') ->
            AddTags = set.make_singleton_set(tag("deleted")),
            RemoveTags = set.init,
            bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO),
            Done = yes
        ; KeyCode = char('u') ->
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("deleted")),
            bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO),
            Done = yes
        ; KeyCode = char('N') ->
            bulk_toggle_unread(MessageUpdate, Done, !Info, !IO)
        ; KeyCode = char('''') ->
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("unread")),
            bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO),
            Done = yes
        ;
            MessageUpdate = set_info("No changes."),
            Done = no
        )
    ;
        MessageUpdate = set_warning("No messages selected."),
        Done = no
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred any_selected_line(list(thread_line)::in) is semidet.

any_selected_line(Lines) :-
    list.member(Line, Lines),
    Line ^ tp_selected = selected.

:- pred init_bulk_tag_completion(prog_config::in, list(thread_line)::in,
    completion_type::out) is det.

init_bulk_tag_completion(Config, Lines, Completion) :-
    list.foldl2(gather_bulk_initial_tags, Lines,
        no, MaybeAndTagSet, set.init, OrTagSet),
    (
        MaybeAndTagSet = yes(AndTagSet),
        set.map(tag_to_string, AndTagSet, AndStringSet)
    ;
        MaybeAndTagSet = no,
        set.init(AndStringSet)
    ),
    set.map(tag_to_string, OrTagSet, OrStringSet),
    Completion = complete_tags_smart(Config, AndStringSet, OrStringSet).

:- pred gather_bulk_initial_tags(thread_line::in,
    maybe(set(tag))::in, maybe(set(tag))::out, set(tag)::in, set(tag)::out)
    is det.

gather_bulk_initial_tags(Line, MaybeAndTagSet0, MaybeAndTagSet, !OrTagSet) :-
    Selected = Line ^ tp_selected,
    (
        Selected = selected,
        gather_initial_tags(Line, MaybeAndTagSet0, AndTagSet, !OrTagSet),
        MaybeAndTagSet = yes(AndTagSet)
    ;
        Selected = not_selected,
        MaybeAndTagSet = MaybeAndTagSet0
    ).

:- pred gather_initial_tags(thread_line::in, maybe(set(tag))::in, set(tag)::out,
    set(tag)::in, set(tag)::out) is det.

gather_initial_tags(Line, MaybeAndTagSet0, AndTagSet, !OrTagSet) :-
    Line = thread_line(_Message, _MaybeParentId, _From, _PrevTags, CurrTags0,
        StdTags, _NonstdTagsWidth0, _Selected, _MaybeGraphics, _RelDate,
        _MaybeSubject),
    apply_standard_tag_state(StdTags, CurrTags0, TagSet),
    (
        MaybeAndTagSet0 = no,
        AndTagSet = TagSet
    ;
        MaybeAndTagSet0 = yes(AndTagSet0),
        set.intersect(TagSet, AndTagSet0, AndTagSet)
    ),
    set.union(TagSet, !OrTagSet).

:- pred bulk_arbitrary_tag_changes(screen::in, string::in, completion_type::in,
    message_update::out, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

bulk_arbitrary_tag_changes(Screen, Initial, Completion, MessageUpdate, !Info,
        !IO) :-
    prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
        !Info, !IO),
    (
        TagChanges = yes(AddTags, RemoveTags),
        bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO)
    ;
        TagChanges = no,
        MessageUpdate = clear_message
    ).

:- pred bulk_tag_changes(set(tag)::in, set(tag)::in, message_update::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    list.map_foldl(update_selected_line_for_tag_changes(AddTags, RemoveTags),
        Lines0, Lines, [], SelectedIds),
    (
        SelectedIds = [_ | _],
        set_lines_list(Lines, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable,
        list.length(SelectedIds, NumMessages),
        string.format("Modified tags in %d messages.", [i(NumMessages)],
            Message),
        MessageUpdate = set_info(Message)
    ;
        SelectedIds = [],
        MessageUpdate = set_info("No changes.")
    ).

:- pred update_selected_line_for_tag_changes(set(tag)::in, set(tag)::in,
    thread_line::in, thread_line::out,
    list(message_id)::in, list(message_id)::out) is det.

update_selected_line_for_tag_changes(AddTags, RemoveTags, Line0, Line,
        !MessageIds) :-
    Line0 = thread_line(Message, MaybeParentId, From, PrevTags, CurrTags0,
        _StdTags0, _NonstdTagsWidth0, Selected, MaybeGraphics, RelDate,
        MaybeSubject),
    (
        MessageId = Message ^ m_id,
        Selected = selected,
        % Notmuch performs tag removals before addition.
        TagSet0 = CurrTags0,
        set.difference(TagSet0, RemoveTags, TagSet1),
        set.union(TagSet1, AddTags, TagSet),
        TagSet \= TagSet0
    ->
        CurrTags = TagSet,
        get_standard_tags(CurrTags, StdTags, NonstdTagsWidth),
        Line = thread_line(Message, MaybeParentId, From, PrevTags, CurrTags,
            StdTags, NonstdTagsWidth, Selected, MaybeGraphics, RelDate,
            MaybeSubject),
        list.cons(MessageId, !MessageIds)
    ;
        Line = Line0
    ).

:- pred bulk_toggle_unread(message_update::out, bool::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

bulk_toggle_unread(MessageUpdate, Done, !Info, !IO) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    ( common_unread_state(Lines0, no, yes(CommonUnreadState)) ->
        (
            CommonUnreadState = read,
            AddTags = set.make_singleton_set(tag("unread")),
            RemoveTags = set.init
        ;
            CommonUnreadState = unread,
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("unread"))
        ),
        bulk_tag_changes(AddTags, RemoveTags, MessageUpdate, !Info, !IO),
        Done = yes
    ;
        Message = "Selected messages differ in unread state.",
        MessageUpdate = set_info(Message),
        Done = no
    ).

:- pred common_unread_state(list(thread_line)::in,
    maybe(unread)::in, maybe(unread)::out) is semidet.

common_unread_state([], State, State).
common_unread_state([H | T], State0, State) :-
    Selected = H ^ tp_selected,
    (
        Selected = selected,
        State0 = no,
        State1 = yes(H ^ tp_std_tags ^ unread),
        common_unread_state(T, State1, State)
    ;
        Selected = selected,
        State0 = yes(H ^ tp_std_tags ^ unread),
        common_unread_state(T, State0, State)
    ;
        Selected = not_selected,
        common_unread_state(T, State0, State)
    ).

%-----------------------------------------------------------------------------%

:- pred highlight_minor(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

highlight_minor(MessageUpdate, !Info) :-
    Pager0 = !.Info ^ tp_pager,
    NumRows = !.Info ^ tp_num_pager_rows,
    highlight_minor(NumRows, MessageUpdate, Pager0, Pager),
    !Info ^ tp_pager := Pager.

:- pred highlight_major(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

highlight_major(MessageUpdate, !Info) :-
    Pager0 = !.Info ^ tp_pager,
    NumRows = !.Info ^ tp_num_pager_rows,
    highlight_major(NumRows, MessageUpdate, Pager0, Pager),
    !Info ^ tp_pager := Pager.

%-----------------------------------------------------------------------------%

:- pred save_part(thread_pager_action::out, message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

save_part(Action, MessageUpdate, !Info) :-
    Pager = !.Info ^ tp_pager,
    ( get_highlighted_thing(Pager, highlighted_part(Part, MaybeSubject)) ->
        Action = prompt_save_part(Part, MaybeSubject),
        MessageUpdate = clear_message
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_save_part(screen::in, part::in, maybe(header_value)::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

prompt_save_part(Screen, Part, MaybeSubject, !Info, !IO) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    MaybePartFilename = Part ^ pt_filename,
    (
        MaybePartFilename = yes(PartFilename)
    ;
        MaybePartFilename = no,
        MaybeSubject = yes(Subject),
        make_filename_from_subject(Subject, PartFilename)
    ;
        MaybePartFilename = no,
        MaybeSubject = no,
        MessageId = message_id(IdStr),
        (
            MaybePartId = yes(PartId),
            PartFilename = string.format("%s.part_%d", [s(IdStr), i(PartId)])
        ;
            MaybePartId = no,
            PartFilename = IdStr ++ ".part"
        )
    ),
    History0 = !.Info ^ tp_common_history ^ ch_save_history,
    make_save_part_initial_prompt(History0, PartFilename, Initial),
    get_home_dir(Home, !IO),
    text_entry_initial(Screen, "Save to file: ", History0, Initial,
        complete_path(Home), Return, !IO),
    (
        Return = yes(FileName0),
        FileName0 \= ""
    ->
        add_history_nodup(FileName0, History0, History),
        !Info ^ tp_common_history ^ ch_save_history := History,
        expand_tilde_home(Home, FileName0, FileName),
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
            Config = !.Info ^ tp_config,
            do_save_part(Config, MessageId, MaybePartId, FileName, Res, !IO),
            (
                Res = ok,
                ( MaybePartId = yes(0) ->
                    MessageUpdate = set_info("Message saved.")
                ;
                    MessageUpdate = set_info("Attachment saved.")
                )
            ;
                Res = error(Error),
                MessageUpdate = set_warning(Error)
            )
        )
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred make_filename_from_subject(header_value::in, string::out) is det.

make_filename_from_subject(Subject, Filename) :-
    SubjectString = header_value_string(Subject),
    string.to_char_list(SubjectString, CharList0),
    list.filter_map(replace_subject_char, CharList0, CharList),
    string.from_char_list(CharList, Filename).

:- pred replace_subject_char(char::in, char::out) is semidet.

replace_subject_char(C0, C) :-
    (
        ( char.is_alnum_or_underscore(C0)
        ; C0 = ('+')
        ; C0 = ('-')
        ; C0 = ('.')
        ; char.to_int(C0) >= 0x80
        )
    ->
        C = C0
    ;
        ( C0 = (' ')
        ; C0 = ('/')
        ; C0 = ('\\')
        ; C0 = (':')
        ),
        C = ('-')
    ).

:- pred make_save_part_initial_prompt(history::in, string::in, string::out)
    is det.

make_save_part_initial_prompt(History, PartFilename, Initial) :-
    choose_text_initial(History, "", PrevFilename),
    dir.dirname(PrevFilename, PrevDirName),
    ( PrevDirName = "." ->
        Initial = PartFilename
    ;
        Initial = PrevDirName / PartFilename
    ).

:- pred do_save_part(prog_config::in, message_id::in, maybe(int)::in,
    string::in, maybe_error::out, io::di, io::uo) is det.

do_save_part(Config, MessageId, MaybePartId, FileName, Res, !IO) :-
    (
        MaybePartId = yes(PartId),
        get_notmuch_command(Config, Notmuch),
        make_quoted_command(Notmuch, [
            "show", "--format=raw", "--part=" ++ from_int(PartId),
            message_id_to_search_term(MessageId)
        ], no_redirect, redirect_output(FileName), Command),
        io.call_system(Command, CallRes, !IO)
    ;
        MaybePartId = no,
        CallRes = error(io.make_io_error("no part id"))
    ),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("notmuch show returned exit status %d",
                [i(ExitStatus)], Msg),
            Res = error(Msg)
        )
    ;
        CallRes = error(Error),
        Res = error(io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%

:- pred open_part(thread_pager_action::out,
    message_update::out, thread_pager_info::in, thread_pager_info::out) is det.

open_part(Action, MessageUpdate, !Info) :-
    Pager = !.Info ^ tp_pager,
    ( get_highlighted_thing(Pager, Thing) ->
        (
            Thing = highlighted_part(Part, _MaybeFilename),
            Action = prompt_open_part(Part)
        ;
            Thing = highlighted_url(Url),
            Action = prompt_open_url(Url)
        ;
            Thing = highlighted_fold_marker,
            % This is a bit ugly as we will end up looking up the line again.
            Action = toggle_content(toggle_expanded)
        ),
        MessageUpdate = clear_message
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_open_part(screen::in, part::in, maybe(keycode)::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

prompt_open_part(Screen, Part, MaybeNextKey, !Info, !IO) :-
    History0 = !.Info ^ tp_common_history ^ ch_open_part_history,
    text_entry(Screen, "Open with command: ", History0, complete_none,
        Return, !IO),
    (
        Return = yes(Command1),
        Command1 \= ""
    ->
        add_history_nodup(Command1, History0, History),
        !Info ^ tp_common_history ^ ch_open_part_history := History,
        Config = !.Info ^ tp_config,
        do_open_part(Config, Screen, Part, Command1, MessageUpdate,
            MaybeNextKey, !IO)
    ;
        MessageUpdate = clear_message,
        MaybeNextKey = no
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_open_part(prog_config::in, screen::in, part::in, string::in,
    message_update::out, maybe(keycode)::out, io::di, io::uo) is det.

do_open_part(Config, Screen, Part, Command, MessageUpdate, MaybeNextKey,
        !IO) :-
    promise_equivalent_solutions [MessageUpdate, MaybeNextKey, !:IO] (
        shell_word.split(Command, ParseResult),
        (
            ParseResult = ok([]),
            % Should not happen.
            MessageUpdate = clear_message,
            MaybeNextKey = no
        ;
            ParseResult = ok(CommandWords),
            CommandWords = [_ | _],
            do_open_part_2(Config, Screen, Part, CommandWords, MessageUpdate,
                MaybeNextKey, !IO)
        ;
            (
                ParseResult = error(yes(Error), _Line, Column)
            ;
                ParseResult = error(no, _Line, Column),
                Error = "parse error"
            ),
            Msg = string.format("%d: %s", [i(Column), s(Error)]),
            MessageUpdate = set_warning(Msg),
            MaybeNextKey = no
        )
    ).

:- pred do_open_part_2(prog_config::in, screen::in, part::in,
    list(word)::in(non_empty_list), message_update::out, maybe(keycode)::out,
    io::di, io::uo) is det.

do_open_part_2(Config, Screen, Part, CommandWords, MessageUpdate, MaybeNextKey,
        !IO) :-
    Part = part(MessageId, MaybePartId, _Type, _Content, MaybePartFileName,
        _MaybeEncoding, _MaybeLength),
    (
        MaybePartFileName = yes(PartFilename),
        get_extension(PartFilename, Ext)
    ->
        make_temp_suffix(Ext, FileName, !IO)
    ;
        io.make_temp(FileName, !IO)
    ),
    do_save_part(Config, MessageId, MaybePartId, FileName, Res, !IO),
    (
        Res = ok,
        call_open_command(Screen, CommandWords, FileName, MaybeError, !IO),
        (
            MaybeError = ok,
            ContMessage = set_info(
                "Press any key to continue (deletes temporary file)"),
            update_message_immed(Screen, ContMessage, !IO),
            get_keycode_blocking(Key, !IO),
            MaybeNextKey = yes(Key),
            MessageUpdate = clear_message
        ;
            MaybeError = error(Msg),
            MessageUpdate = set_warning(Msg),
            MaybeNextKey = no
        )
    ;
        Res = error(Error),
        string.format("Error saving to %s: %s", [s(FileName), s(Error)], Msg),
        MessageUpdate = set_warning(Msg),
        MaybeNextKey = no
    ),
    io.remove_file(FileName, _, !IO).

:- pred call_open_command(screen::in, list(word)::in(non_empty_list),
    string::in, maybe_error::out, io::di, io::uo) is det.

call_open_command(Screen, CommandWords, Arg, MaybeError, !IO) :-
    make_open_command(CommandWords, Arg, CommandToShow, CommandToRun, Bg),
    CallMessage = set_info("Calling " ++ CommandToShow ++ "..."),
    update_message_immed(Screen, CallMessage, !IO),
    (
        Bg = run_in_background,
        io.call_system(CommandToRun, CallRes, !IO)
    ;
        Bg = run_in_foreground,
        curs.def_prog_mode(!IO),
        curs.stop(!IO),
        io.call_system(CommandToRun, CallRes, !IO),
        curs.reset_prog_mode(!IO),
        curs.refresh(!IO)
    ),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            MaybeError = ok
        ;
            string.format("%s returned with exit status %d",
                [s(CommandToShow), i(ExitStatus)], Msg),
            MaybeError = error(Msg)
        )
    ;
        CallRes = error(Error),
        MaybeError = error("Error: " ++ io.error_message(Error))
    ).

:- pred make_open_command(list(word)::in(non_empty_list), string::in,
    string::out, string::out, run_in_background::out) is det.

make_open_command(CommandWords0, Arg, CommandToShow, CommandToRun, Bg) :-
    remove_bg_operator(CommandWords0, CommandWords, Bg),
    WordStrings = list.map(word_string, CommandWords),
    CommandToShow = string.join_list(" ", WordStrings),
    CommandPrefix = command_prefix(
        shell_quoted(string.join_list(" ", list.map(quote_arg, WordStrings))),
        ( detect_ssh(CommandWords) -> quote_twice ; quote_once )
    ),
    (
        Bg = run_in_background,
        make_quoted_command(CommandPrefix, [Arg],
            redirect_input("/dev/null"), redirect_output("/dev/null"),
            redirect_stderr("/dev/null"), run_in_background,
            CommandToRun)
    ;
        Bg = run_in_foreground,
        make_quoted_command(CommandPrefix, [Arg], no_redirect, no_redirect,
            CommandToRun)
    ).

:- pred remove_bg_operator(list(word)::in(non_empty_list), list(word)::out,
    run_in_background::out) is det.

remove_bg_operator(Words0, Words, Bg) :-
    (
        list.split_last(Words0, ButLast, Last0),
        remove_bg_operator_2(Last0, Last)
    ->
        (
            Last = word([]),
            Words = ButLast
        ;
            Last = word([_ | _]),
            Words = ButLast ++ [Last]
        ),
        Bg = run_in_background
    ;
        Words = Words0,
        Bg = run_in_foreground
    ).

:- pred remove_bg_operator_2(word::in, word::out) is semidet.

remove_bg_operator_2(word(Segments0), word(Segments)) :-
    list.split_last(Segments0, ButLast, Last0),
    Last0 = unquoted(LastString0),
    string.remove_suffix(LastString0, "&", LastString),
    ( LastString = "" ->
        Segments = ButLast
    ;
        Segments = ButLast ++ [unquoted(LastString)]
    ).

%-----------------------------------------------------------------------------%

:- pred prompt_open_url(screen::in, string::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

prompt_open_url(Screen, Url, !Info, !IO) :-
    History0 = !.Info ^ tp_common_history ^ ch_open_url_history,
    % No completion for command inputs yet.
    text_entry(Screen, "Open URL with command: ", History0, complete_none,
        Return, !IO),
    (
        Return = yes(Command1),
        Command1 \= ""
    ->
        add_history_nodup(Command1, History0, History),
        !Info ^ tp_common_history ^ ch_open_url_history := History,
        do_open_url(Screen, Command1, Url, MessageUpdate, !IO)
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_open_url(screen::in, string::in, string::in, message_update::out,
    io::di, io::uo) is det.

do_open_url(Screen, Command, Url, MessageUpdate, !IO) :-
    promise_equivalent_solutions [MessageUpdate, !:IO] (
        shell_word.split(Command, ParseResult),
        (
            ParseResult = ok([]),
            % Should not happen.
            MessageUpdate = clear_message
        ;
            ParseResult = ok(CommandWords),
            CommandWords = [_ | _],
            call_open_command(Screen, CommandWords, Url, MaybeError, !IO),
            (
                MaybeError = ok,
                MessageUpdate = no_change
            ;
                MaybeError = error(Msg),
                MessageUpdate = set_warning(Msg)
            )
        ;
            (
                ParseResult = error(yes(Error), _Line, Column)
            ;
                ParseResult = error(no, _Line, Column),
                Error = "parse error"
            ),
            Msg = string.format("%d: %s", [i(Column), s(Error)]),
            MessageUpdate = set_warning(Msg)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred prompt_search(screen::in, search_direction::in, thread_pager_info::in,
    thread_pager_info::out, io::di, io::uo) is det.

prompt_search(Screen, SearchDir, !Info, !IO) :-
    History0 = !.Info ^ tp_common_history ^ ch_internal_search_history,
    text_entry(Screen, "Search for: ", History0, complete_none, Return, !IO),
    (
        Return = yes(Search),
        ( Search = "" ->
            !Info ^ tp_search := no
        ;
            add_history_nodup(Search, History0, History),
            !Info ^ tp_search := yes(Search),
            !Info ^ tp_search_dir := SearchDir,
            !Info ^ tp_common_history ^ ch_internal_search_history := History,
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
        SearchDir = !.Info ^ tp_search_dir,
        pager.skip_to_search(NumRows, SearchKind, Search, SearchDir,
            MessageUpdate, Pager0, Pager),
        !Info ^ tp_pager := Pager,
        sync_thread_to_pager(!Info)
    ;
        MaybeSearch = no,
        MessageUpdate = set_warning("No search string.")
    ).

%-----------------------------------------------------------------------------%

:- pred toggle_content(screen::in, toggle_type::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

toggle_content(Screen, ToggleType, !Info, !IO) :-
    NumRows = !.Info ^ tp_num_pager_rows,
    get_cols(Screen, Cols),
    Pager0 = !.Info ^ tp_pager,
    pager.toggle_content(ToggleType, NumRows, Cols, MessageUpdate,
        Pager0, Pager, !IO),
    !Info ^ tp_pager := Pager,
    sync_thread_to_pager(!Info),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred toggle_ordering(thread_pager_info::in, ordering::out) is det.

toggle_ordering(Info, Ordering) :-
    Ordering0 = Info ^ tp_ordering,
    (
        Ordering0 = ordering_flat,
        Ordering = ordering_threaded
    ;
        Ordering0 = ordering_threaded,
        Ordering = ordering_flat
    ).

%-----------------------------------------------------------------------------%

:- pred get_effects(thread_pager_info::in, thread_pager_effects::out) is det.

get_effects(Info, Effects) :-
    Scrollable = Info ^ tp_scrollable,
    Lines = get_lines(Scrollable),
    version_array.foldl(get_overall_tags, Lines,
        set.init, TagSet),
    version_array.foldl(get_changed_status_messages, Lines,
        map.init, TagGroups),
    AddedMessages = Info ^ tp_added_messages,
    Effects = thread_pager_effects(TagSet, TagGroups, AddedMessages).

:- pred get_overall_tags(thread_line::in, set(tag)::in, set(tag)::out) is det.

get_overall_tags(Line, !TagSet) :-
    CurrTags = Line ^ tp_curr_tags,
    set.union(CurrTags, !TagSet).

:- pred get_changed_status_messages(thread_line::in,
    map(set(tag_delta), list(message_id))::in,
    map(set(tag_delta), list(message_id))::out) is det.

get_changed_status_messages(Line, !TagGroups) :-
    TagSet = get_tag_delta_set(Line),
    (
        set.non_empty(TagSet),
        MessageId = Line ^ tp_message ^ m_id
    ->
        ( map.search(!.TagGroups, TagSet, Messages0) ->
            map.det_update(TagSet, [MessageId | Messages0], !TagGroups)
        ;
            map.det_insert(TagSet, [MessageId], !TagGroups)
        )
    ;
        true
    ).

:- func get_tag_delta_set(thread_line) = set(tag_delta).

get_tag_delta_set(Line) = TagDeltaSet :-
    PrevTags = Line ^ tp_prev_tags,
    CurrTags = Line ^ tp_curr_tags,
    set.difference(CurrTags, PrevTags, AddTags),
    set.difference(PrevTags, CurrTags, RemoveTags),
    set.map(make_tag_delta("+"), AddTags) = AddTagDeltas,
    set.map(make_tag_delta("-"), RemoveTags) = RemoveTagDeltas,
    set.union(AddTagDeltas, RemoveTagDeltas, TagDeltaSet).

:- func make_tag_delta(string, tag) = tag_delta.

make_tag_delta(Op, tag(Tag)) = tag_delta(Op ++ Tag).

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

:- pred resend(thread_pager_info::in, thread_pager_action::out,
    message_update::out) is det.

resend(Info, Action, MessageUpdate) :-
    PagerInfo = Info ^ tp_pager,
    (
        get_top_message(PagerInfo, Message),
        MessageId = Message ^ m_id
    ->
        MessageUpdate = clear_message,
        Action = prompt_resend(MessageId)
    ;
        MessageUpdate = set_warning("No message to resend."),
        Action = continue
    ).

:- pred handle_resend(screen::in, message_id::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_resend(Screen, MessageId, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    History0 = !.Info ^ tp_common_history,
    ToHistory0 = History0 ^ ch_to_history,
    handle_resend(Config, Screen, MessageId, MessageUpdate, ToHistory0,
        ToHistory, !IO),
    update_message(Screen, MessageUpdate, !IO),
    History = History0 ^ ch_to_history := ToHistory,
    !Info ^ tp_common_history := History.
    % XXX increment tp_added_messages?

%-----------------------------------------------------------------------------%

:- pred handle_recall(screen::in, screen::out, thread_id::in, sent::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_recall(!Screen, ThreadId, Sent, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    Crypto = !.Info ^ tp_crypto,
    select_recall(Config, !.Screen, yes(ThreadId), TransitionA, !IO),
    handle_screen_transition(!Screen, TransitionA, MaybeSelected, !Info, !IO),
    (
        MaybeSelected = yes(Message),
        (
            Message = message(_, _, _, _, _, _),
            continue_from_message(Config, Crypto, !.Screen, postponed_message,
                Message, TransitionB, !IO),
            handle_screen_transition(!Screen, TransitionB, Sent, !Info, !IO)
        ;
            Message = excluded_message(_),
            Sent = not_sent
        )
    ;
        MaybeSelected = no,
        Sent = not_sent
    ).

%-----------------------------------------------------------------------------%

:- pred edit_as_template(thread_pager_info::in, thread_pager_action::out,
    message_update::out) is det.

edit_as_template(Info, Action, MessageUpdate) :-
    PagerInfo = Info ^ tp_pager,
    ( get_top_message(PagerInfo, Message) ->
        MessageUpdate = clear_message,
        Action = edit_as_template(Message)
    ;
        MessageUpdate = set_warning("No message to edit."),
        Action = continue
    ).

:- pred handle_edit_as_template(screen::in, screen::out, message::in,
    sent::out, thread_pager_info::in, thread_pager_info::out, io::di, io::uo)
    is det.

handle_edit_as_template(!Screen, Message, Sent, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    Crypto = !.Info ^ tp_crypto,
    (
        Message = message(_, _, _, _, _, _),
        continue_from_message(Config, Crypto, !.Screen, arbitrary_message,
            Message, Transition, !IO),
        handle_screen_transition(!Screen, Transition, Sent, !Info, !IO)
    ;
        Message = excluded_message(_),
        Sent = not_sent
    ).

%-----------------------------------------------------------------------------%

:- pred addressbook_add(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

addressbook_add(Screen, Info, !IO) :-
    Config = Info ^ tp_config,
    Scrollable = Info ^ tp_scrollable,
    (
        get_cursor_line(Scrollable, _Cursor, Line),
        From = Line ^ tp_message ^ m_headers ^ h_from
    ->
        Address0 = header_value_string(From)
    ;
        Address0 = ""
    ),
    prompt_addressbook_add(Config, Screen, Address0, !IO).

%-----------------------------------------------------------------------------%

:- pred draw_thread_pager(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

draw_thread_pager(Screen, Info, !IO) :-
    Scrollable = Info ^ tp_scrollable,
    PagerInfo = Info ^ tp_pager,
    Config = Info ^ tp_config,
    Attrs = thread_attrs(Config),
    PagerAttrs = pager_attrs(Config),

    split_panels(Screen, Info, ThreadPanels, SepPanel, PagerPanels),
    scrollable.draw(draw_thread_line(Attrs), ThreadPanels, Scrollable, !IO),
    get_cols(Screen, Cols),
    draw_sep(Attrs, Cols, SepPanel, !IO),
    draw_pager_lines(PagerAttrs, PagerPanels, PagerInfo, !IO).

:- pred draw_sep(thread_attrs::in, int::in, maybe(panel)::in, io::di, io::uo)
    is det.

draw_sep(Attrs, Cols, MaybeSepPanel, !IO) :-
    (
        MaybeSepPanel = yes(Panel),
        panel.erase(Panel, !IO),
        attr(Panel, Attrs ^ t_status ^ bar, !IO),
        hline(Panel, char.to_int('-'), Cols, !IO)
    ;
        MaybeSepPanel = no
    ).

:- pred draw_thread_line(thread_attrs::in, panel::in, thread_line::in, int::in,
    bool::in, io::di, io::uo) is det.

draw_thread_line(TAttrs, Panel, Line, _LineNr, IsCursor, !IO) :-
    Line = thread_line(_Message, _ParentId, From, _PrevTags, CurrTags,
        StdTags, NonstdTagsWidth, Selected, MaybeGraphics, RelDate,
        MaybeSubject),
    Attrs = TAttrs ^ t_generic,
    (
        IsCursor = yes,
        RelDateAttr = Attrs ^ current
    ;
        IsCursor = no,
        RelDateAttr = Attrs ^ relative_date
    ),
    draw_fixed(Panel, RelDateAttr, 13, RelDate, ' ', !IO),

    (
        Selected = selected,
        mattr_draw(Panel, unless(IsCursor, Attrs ^ selected), "*", !IO)
    ;
        Selected = not_selected,
        draw(Panel, " ", !IO)
    ),
    mattr(Panel, unless(IsCursor, Attrs ^ standard_tag), !IO),

    StdTags = standard_tags(Unread, Replied, Deleted, Flagged),
    (
        Unread = unread,
        draw(Panel, "n", !IO)
    ;
        Unread = read,
        draw(Panel, " ", !IO)
    ),
    (
        Replied = replied,
        draw(Panel, "r", !IO)
    ;
        Replied = not_replied,
        draw(Panel, " ", !IO)
    ),
    (
        Deleted = deleted,
        draw(Panel, "d", !IO)
    ;
        Deleted = not_deleted,
        draw(Panel, " ", !IO)
    ),
    (
        Flagged = flagged,
        mattr_draw(Panel, unless(IsCursor, Attrs ^ flagged), "! ", !IO)
    ;
        Flagged = unflagged,
        draw(Panel, "  ", !IO)
    ),

    mattr(Panel, unless(IsCursor, TAttrs ^ t_tree), !IO),
    (
        MaybeGraphics = yes(Graphics),
        list.foldl(draw_graphic(Panel), Graphics, !IO)
    ;
        MaybeGraphics = no
    ),
    draw(Panel, "• ", !IO),

    (
        Unread = unread,
        Highlight = bold
    ;
        Unread = read,
        Highlight = normal
    ),
    mattr_draw(Panel, unless(IsCursor, Attrs ^ author + Highlight), From,
        !IO),

    (
        MaybeSubject = yes(Subject),
        draw(Panel, ". ", !IO),
        panel.getyx(Panel, Row, SubjectX0, !IO),
        mattr_draw(Panel, unless(IsCursor, Attrs ^ subject),
            header_value_string(Subject), !IO),
        panel.getyx(Panel, _, SubjectX, !IO),
        panel.getmaxyx(Panel, _, MaxX, !IO)
    ;
        MaybeSubject = no,
        panel.getmaxyx(Panel, Row, MaxX, !IO),
        SubjectX0 = MaxX,
        SubjectX = MaxX
    ),

    % Draw non-standard tags, overlapping up to half of the subject.
    ( NonstdTagsWidth > 0 ->
        (
            MaybeSubject = yes(_),
            MaxX - SubjectX < NonstdTagsWidth,
            SubjectMidX = (MaxX + SubjectX0)/2,
            MoveX = max(SubjectMidX, MaxX - NonstdTagsWidth),
            MoveX < SubjectX
        ->
            panel.move(Panel, Row, MoveX, !IO)
        ;
            true
        ),
        attr_set(Panel, Attrs ^ other_tag, !IO),
        set.fold(draw_display_tag(Panel), CurrTags, !IO)
    ;
        true
    ).

:- pred draw_display_tag(panel::in, tag::in, io::di, io::uo) is det.

draw_display_tag(Panel, Tag, !IO) :-
    ( display_tag(Tag) ->
        Tag = tag(TagName),
        draw2(Panel, " ", TagName, !IO)
    ;
        true
    ).

:- pred draw_graphic(panel::in, graphic::in, io::di, io::uo) is det.

draw_graphic(Panel, Graphic, !IO) :-
    draw(Panel, graphic_to_char(Graphic), !IO).

:- func graphic_to_char(graphic) = string.

graphic_to_char(blank) = " ".
graphic_to_char(vert) = "│".
graphic_to_char(tee) = "├".
graphic_to_char(ell) = "└".

:- func unless(bool, attr) = maybe(attr).

unless(no, X) = yes(X).
unless(yes, _) = no.

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
