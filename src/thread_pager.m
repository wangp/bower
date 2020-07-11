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
                % Set of tags for the thread (non-excluded messages only).
                thread_tags     :: set(tag),

                % Tag changes to be applied to messages.
                tag_changes     :: map(set(tag_delta), list(message_id)),

                % Number of messages added (by sending).
                added_messages  :: int
            ).

:- pred open_thread_pager(prog_config::in, crypto::in, screen::in,
    thread_id::in, set(tag)::in, list(string)::in, maybe(string)::in,
    screen_transition(thread_pager_effects)::out,
    common_history::in, common_history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module dir.
:- import_module float.
:- import_module int.
:- import_module parsing_utils.
:- import_module string.
:- import_module time.
:- import_module version_array.
:- use_module require.

:- import_module addressbook.
:- import_module async.
:- import_module callout.
:- import_module color.
:- import_module compose.
:- import_module cord_util.
:- import_module make_temp.
:- import_module mime_type.
:- import_module pager.
:- import_module path_expand.
:- import_module pipe_to.
:- import_module poll_notify.
:- import_module quote_arg.
:- import_module recall.
:- import_module resend.
:- import_module sanitise.
:- import_module scrollable.
:- import_module shell_word.
:- import_module string_util.
:- import_module text_entry.
:- import_module time_util.
:- import_module view_async.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- type thread_pager_info
    --->    thread_pager_info(
                tp_config           :: prog_config,
                tp_crypto           :: crypto,
                tp_thread_id        :: thread_id,
                tp_include_tags     :: set(tag),
                tp_messages         :: list(message),
                tp_ordering         :: ordering,

                tp_scrollable       :: scrollable(thread_line),
                tp_num_thread_rows  :: int,
                tp_pager            :: pager_info,
                tp_num_pager_rows   :: int,

                tp_refresh_time     :: timestamp,
                tp_next_poll_time   :: maybe(timestamp),
                tp_thread_poll_count:: int,
                tp_index_poll_string :: string,
                tp_index_poll_count :: int,

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
                tp_clean_from   :: presentable_string,
                tp_prev_tags    :: set(tag),
                tp_curr_tags    :: set(tag),
                tp_std_tags     :: standard_tags, % cached from tp_curr_tags
                tp_nonstd_tags_width :: int,      % cached from tp_curr_tags
                tp_selected     :: selected,
                tp_graphics     :: maybe(list(graphic)),
                tp_reldate      :: string,
                tp_subject      :: maybe(presentable_string)
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
    ;       continue_no_draw
    ;       resize
    ;       start_reply(message, reply_kind)
    ;       start_forward(message)
    ;       prompt_resend(message_id)
    ;       start_recall
    ;       edit_as_template(message)
    ;       prompt_tag(string)
    ;       bulk_tag(keep_selection)
    ;       prompt_save_part(part, maybe(header_value))
    ;       prompt_open_part(part)
    ;       prompt_open_url(string)
    ;       prompt_search(search_direction)
    ;       decrypt_part
    ;       verify_part
    ;       toggle_content(toggle_type)
    ;       toggle_ordering
    ;       addressbook_add
    ;       pipe_ids
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

open_thread_pager(Config, Crypto, Screen, ThreadId, IncludeTags,
        IndexPollTerms, MaybeSearch, Transition, CommonHistory0, CommonHistory,
        !IO) :-
    current_timestamp(RefreshTime, !IO),
    get_thread_messages(Config, ThreadId, IncludeTags, ParseResult, Messages,
        !IO),

    Ordering = ordering_threaded,
    create_pager_and_thread_lines(Config, Screen, Messages, Ordering,
        Scrollable, NumThreadRows, PagerInfo, NumPagerRows, !IO),
    NumMessages = get_num_lines(Scrollable),

    NextPollTime = next_poll_time(Config, RefreshTime),
    ThreadPollCount = 0,
    IndexPollString = join_list(" ", IndexPollTerms ++
        ["AND", timestamp_to_int_string(RefreshTime) ++ ".."]),
    IndexPollCount = 0,
    AddedMessages0 = 0,

    Info0 = thread_pager_info(Config, Crypto, ThreadId, IncludeTags, Messages,
        Ordering, Scrollable, NumThreadRows, PagerInfo, NumPagerRows,
        RefreshTime, NextPollTime, ThreadPollCount,
        IndexPollString, IndexPollCount,
        MaybeSearch, dir_forward, CommonHistory0, AddedMessages0),

    (
        ParseResult = ok,
        MessageUpdate = set_info(showing_num_messages(NumMessages))
    ;
        ParseResult = error(Error),
        MessageUpdate = set_warning(Error)
    ),
    update_message(Screen, MessageUpdate, !IO),

    thread_pager_loop(Screen, redraw, Info0, Info, !IO),
    flush_async_with_progress(Screen, !IO),

    get_effects(Info, Effects),
    Transition = screen_transition(Effects, no_change),
    CommonHistory = Info ^ tp_common_history.

:- pred reopen_thread_pager(screen::in, bool::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

reopen_thread_pager(Screen, KeepCached, !Info, !IO) :-
    !.Info = thread_pager_info(Config, _Crypto, ThreadId, IncludeTags,
        Messages0, Ordering,
        Scrollable0, _NumThreadRows0, Pager0, _NumPagerRows0,
        _RefreshTime0, _NextPollTime0, _ThreadPollCount0,
        _IndexPollString, _IndexPollCount,
        _Search, _SearchDir, _CommonHistory, _AddedMessages),

    (
        KeepCached = yes,
        Messages0 = [_ | _]
    ->
        Messages = Messages0,
        ParseResult = ok,
        ShowMessageUpdate = bool.no
    ;
        current_timestamp(RefreshTime, !IO),
        get_thread_messages(Config, ThreadId, IncludeTags, ParseResult,
            Messages, !IO),
        ShowMessageUpdate = bool.yes,
        !Info ^ tp_refresh_time := RefreshTime,
        !Info ^ tp_next_poll_time := next_poll_time(Config, RefreshTime),
        !Info ^ tp_thread_poll_count := 0
        % Do not reset tp_index_poll_count as it does not depend on
        % RefreshTime.
    ),

    create_pager_and_thread_lines(Config, Screen, Messages, Ordering,
        Scrollable1, NumThreadRows, Pager1, NumPagerRows, !IO),

    % Reapply tag changes from previous state.
    ThreadLines0 = get_lines_list(Scrollable0),
    ThreadLines1 = get_lines_list(Scrollable1),
    list.foldl(create_tag_delta_map, ThreadLines0, map.init, DeltaMap),
    list.map(restore_tag_deltas(DeltaMap), ThreadLines1, ThreadLines),
    Scrollable = scrollable.init(ThreadLines),
    NumMessages = get_num_lines(Scrollable),

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

    !Info ^ tp_scrollable := Scrollable,
    !Info ^ tp_num_thread_rows := NumThreadRows,
    !Info ^ tp_pager := Pager,
    !Info ^ tp_num_pager_rows := NumPagerRows,

    sync_thread_to_pager(!Info),

    (
        ShowMessageUpdate = yes,
        (
            ParseResult = ok,
            MessageUpdate = set_info(showing_num_messages(NumMessages))
        ;
            ParseResult = error(Error),
            MessageUpdate = set_warning(Error)
        ),
        update_message(Screen, MessageUpdate, !IO)
    ;
        ShowMessageUpdate = no
    ).

:- func showing_num_messages(int) = string.

showing_num_messages(Count) =
    string.format("Showing %d messages.", [i(Count)]).

%-----------------------------------------------------------------------------%

:- pred get_thread_messages(prog_config::in, thread_id::in, set(tag)::in,
    maybe_error::out, list(message)::out, io::di, io::uo) is det.

get_thread_messages(Config, ThreadId, IncludeTags, Res, Messages, !IO) :-
    get_decrypt_by_default(Config, DecryptByDefault),
    get_verify_by_default(Config, VerifyByDefault),
    (
        DecryptByDefault = yes,
        % Avoid likely error messages about missing keys.
        RedirectStderr = redirect_stderr("/dev/null"),
        SuspendCurs = soft_suspend_curses
    ;
        DecryptByDefault = no,
        RedirectStderr = no_redirect,
        SuspendCurs = no_suspend_curses
    ),

    get_exclude_tags(Config, ExcludeTags0),
    set.difference(ExcludeTags0, IncludeTags, ExcludeTags),

    % Pass --exclude=false so that we can decide for ourselves if a message
    % should be excluded.
    run_notmuch(Config,
        [
            "show", "--format=json", "--entire-thread=true", "--exclude=false",
            decrypt_arg_bool(DecryptByDefault),
            verify_arg(VerifyByDefault),
            "--", thread_id_to_search_term(ThreadId)
        ],
        RedirectStderr, SuspendCurs,
        parse_thread_set(yes(ExcludeTags)), ParseResult, !IO),
    (
        ParseResult = ok(Messages),
        Res = ok
    ;
        ParseResult = error(Error),
        Res = error("Error parsing notmuch response: " ++ Error),
        Messages = []
    ).

:- func decrypt_arg(maybe_decrypted) = string.

decrypt_arg(is_decrypted) = decrypt_arg_bool(yes).
decrypt_arg(not_decrypted) = decrypt_arg_bool(no).

:- func decrypt_arg_bool(bool) = string.

decrypt_arg_bool(yes) = "--decrypt".
decrypt_arg_bool(no) = "--decrypt=false".

:- func verify_arg(bool) = string.

verify_arg(yes) = "--verify".
verify_arg(no) = "--verify=false".

%-----------------------------------------------------------------------------%

:- pred create_pager_and_thread_lines(prog_config::in, screen::in,
    list(message)::in, ordering::in,
    scrollable(thread_line)::out, int::out, pager_info::out, int::out,
    io::di, io::uo) is det.

create_pager_and_thread_lines(Config, Screen, Messages, Ordering,
        Scrollable, NumThreadRows, PagerInfo, NumPagerRows, !IO) :-
    get_rows_cols(Screen, Rows0, Cols, !IO),
    Rows = Rows0 - 2,
    current_timestamp(NowTime, !IO),
    localtime(NowTime, Nowish, !IO),
    (
        Ordering = ordering_threaded,
        append_threaded_messages(Nowish, Messages, ThreadLines, !IO),
        setup_pager(Config, include_replies, Cols, Messages, PagerInfo0, !IO)
    ;
        Ordering = ordering_flat,
        append_flat_messages(Nowish, Messages, ThreadLines,
            SortedFlatMessages, !IO),
        setup_pager(Config, toplevel_only, Cols, SortedFlatMessages,
            PagerInfo0, !IO)
    ),
    Scrollable0 = scrollable.init_with_cursor(ThreadLines),
    compute_num_rows(Rows, Scrollable0, NumThreadRows, NumPagerRows),
    list.filter(not_excluded_thread_line, ThreadLines, NonExcludedThreadLines),
    (
        NonExcludedThreadLines = [],
        Scrollable = Scrollable0,
        PagerInfo = PagerInfo0
    ;
        NonExcludedThreadLines = [FirstLine | RestLines],
        (
            list.find_first_match(is_unread_and_not_excluded,
                NonExcludedThreadLines, UnreadLine)
        ->
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
            Message = excluded_message(_, _, _, _, _),
            Scrollable = Scrollable0,
            PagerInfo = PagerInfo0
        )
    ).

:- pred not_excluded_thread_line(thread_line::in) is semidet.

not_excluded_thread_line(Line) :-
    non_excluded_message(Line ^ tp_message).

:- pred get_latest_line(thread_line::in, thread_line::in, thread_line::out)
    is det.

get_latest_line(LineA, LineB, Line) :-
    TimestampA = get_timestamp_or_zero(LineA ^ tp_message),
    TimestampB = get_timestamp_or_zero(LineB ^ tp_message),
    ( TimestampA - TimestampB >= 0.0 ->
        Line = LineA
    ;
        Line = LineB
    ).

:- pred resize_thread_pager(screen::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

resize_thread_pager(Screen, !Info, !IO) :-
    get_rows_cols(Screen, Rows, _Cols, !IO),
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
    list(thread_line)::out, io::di, io::uo) is det.

append_threaded_messages(Nowish, Messages, ThreadLines, !IO) :-
    append_threaded_messages(Nowish, [], [], no, Messages, no,
        cord.init, ThreadCord, !IO),
    ThreadLines = list(ThreadCord).

:- pred append_threaded_messages(tm::in, list(graphic)::in, list(graphic)::in,
    maybe(message_id)::in, list(message)::in, maybe(header_value)::in,
    cord(thread_line)::in, cord(thread_line)::out, io::di, io::uo) is det.

append_threaded_messages(_Nowish, _Above, _Below, _MaybeParentId,
        [], _MaybePrevSubject, !Cord, !IO).
append_threaded_messages(Nowish, Above0, Below0, MaybeParentId,
        [Message | Messages], MaybePrevSubject, !Cord, !IO) :-
    (
        Messages = [],
        Graphics = Above0 ++ [ell],
        make_thread_line(Nowish, Message, MaybeParentId, yes(Graphics),
            MaybePrevSubject, Line, !IO),
        cord_util.snoc(Line, !Cord),
        MessagesCord = cord.empty,
        Below1 = Below0
    ;
        Messages = [_ | _],
        Graphics = Above0 ++ [tee],
        make_thread_line(Nowish, Message, MaybeParentId, yes(Graphics),
            MaybePrevSubject, Line, !IO),
        cord_util.snoc(Line, !Cord),
        get_maybe_last_subject(Message, LastSubject),
        append_threaded_messages(Nowish, Above0, Below0, MaybeParentId,
            Messages, LastSubject, cord.init, MessagesCord, !IO),
        ( get_first(MessagesCord, FollowingLine) ->
            MaybeGraphics = FollowingLine ^ tp_graphics,
            (
                MaybeGraphics = yes(Below1)
            ;
                MaybeGraphics = no,
                Below1 = []
            )
        ;
            require.unexpected($module, $pred, "empty cord")
        )
    ),
    ( not_blank_at_column(Below1, length(Above0)) ->
        Above1 = Above0 ++ [vert]
    ;
        Above1 = Above0 ++ [blank]
    ),
    MaybeMessageId = get_maybe_message_id(Message),
    MaybeSubject = get_maybe_subject(Message),
    Replies = get_replies(Message),
    append_threaded_messages(Nowish, Above1, Below1, MaybeMessageId, Replies,
        MaybeSubject, !Cord, !IO),
    !:Cord = !.Cord ++ MessagesCord.

:- pred get_maybe_last_subject(message::in, maybe(header_value)::out) is det.

get_maybe_last_subject(Message, MaybeLastSubject) :-
    Replies = get_replies(Message),
    ( list.last(Replies, LastReply) ->
        get_maybe_last_subject(LastReply, MaybeLastSubject)
    ;
        MaybeLastSubject = get_maybe_subject(Message)
    ).

:- pred not_blank_at_column(list(graphic)::in, int::in) is semidet.

not_blank_at_column(Graphics, Col) :-
    list.index0(Graphics, Col, Graphic),
    Graphic \= blank.

:- pred append_flat_messages(tm::in, list(message)::in,
    list(thread_line)::out, list(message)::out, io::di, io::uo) is det.

append_flat_messages(Nowish, Messages, ThreadLines, SortedFlatMessages, !IO) :-
    flatten_messages(no, Messages, [], MessagesFlat0),
    list.sort(compare_by_timestamp, MessagesFlat0, MessagesFlat),
    list.foldl3(append_flat_message(Nowish), MessagesFlat,
        no, _MaybePrevSubject, [], RevThreadLines, !IO),
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
    TimestampA = get_timestamp_or_zero(A ^ mf_message),
    TimestampB = get_timestamp_or_zero(B ^ mf_message),
    compare(Rel, TimestampA, TimestampB).

:- pred append_flat_message(tm::in, message_flat::in, maybe(header_value)::in,
    maybe(header_value)::out, list(thread_line)::in, list(thread_line)::out,
    io::di, io::uo) is det.

append_flat_message(Nowish, MessageFlat, MaybePrevSubject, MaybeSubject,
        !RevAcc, !IO) :-
    MessageFlat = message_flat(Message, MaybeParentId),
    make_thread_line(Nowish, Message, MaybeParentId, no, MaybePrevSubject,
        Line, !IO),
    MaybeSubject = get_maybe_subject(Message),
    cons(Line, !RevAcc).

:- func mf_message(message_flat) = message. % accessor

:- pred make_thread_line(tm::in, message::in, maybe(message_id)::in,
    maybe(list(graphic))::in, maybe(header_value)::in, thread_line::out,
    io::di, io::uo) is det.

make_thread_line(Nowish, Message, MaybeParentId, MaybeGraphics,
        MaybePrevSubject, Line, !IO) :-
    (
        Message = message(_Id, Timestamp0, Headers0, Tags0, _Body, _Replies),
        MaybeTimestamp = yes(Timestamp0),
        MaybeHeaders = yes(Headers0),
        MaybeTags = yes(Tags0)
    ;
        Message = excluded_message(_MaybeId, MaybeTimestamp, MaybeHeaders,
            MaybeTags, _Replies)
    ),
    (
        MaybeTimestamp = yes(Timestamp),
        localtime(Timestamp, TM, !IO),
        make_reldate(Nowish, TM, no, RelDate)
    ;
        MaybeTimestamp = no,
        RelDate = "unknown"
    ),
    (
        MaybeHeaders = yes(Headers),
        From = Headers ^ h_from,
        CleanFrom = clean_email_address(header_value_string(From)),
        CurSubject = Headers ^ h_subject,
        (
            MaybePrevSubject = yes(PrevSubject),
            canonicalise_subject(CurSubject) = canonicalise_subject(PrevSubject)
        ->
            MaybeSubject = no
        ;
            CurSubjectStr = header_value_string(CurSubject),
            MaybeSubject = yes(make_presentable(CurSubjectStr))
        )
    ;
        MaybeHeaders = no,
        CleanFrom = "",
        MaybeSubject = no
    ),
    (
        MaybeTags = yes(Tags)
    ;
        MaybeTags = no,
        Tags = set.init
    ),
    get_standard_tags(Tags, StdTags, NonstdTagsWidth),
    Line = thread_line(Message, MaybeParentId, make_presentable(CleanFrom),
        Tags, Tags, StdTags, NonstdTagsWidth,
        not_selected, MaybeGraphics, RelDate, MaybeSubject).

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
        Message = excluded_message(_, _, _, _, _)
    ).

:- pred restore_tag_deltas(map(message_id, message_tag_deltas)::in,
    thread_line::in, thread_line::out) is det.

restore_tag_deltas(DeltaMap, ThreadLine0, ThreadLine) :-
    Message = ThreadLine0 ^ tp_message,
    (
        Message = message(MessageId, _, _, _, _, _),
        ( map.search(DeltaMap, MessageId, Deltas) ->
            Deltas = message_tag_deltas(AddTags, RemoveTags),
            CurrTags0 = ThreadLine0 ^ tp_curr_tags,
            set.difference(CurrTags0, RemoveTags, CurrTags1),
            set.union(CurrTags1, AddTags, CurrTags),
            set_tags(CurrTags, ThreadLine0, ThreadLine)
        ;
            ThreadLine = ThreadLine0
        )
    ;
        Message = excluded_message(_, _, _, _, _),
        ThreadLine = ThreadLine0
    ).

%-----------------------------------------------------------------------------%

:- pred handle_screen_transition(screen::in, screen_transition(T)::in, T::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_screen_transition(Screen, Transition, T, !Info, !IO) :-
    Transition = screen_transition(T, MessageUpdate),
    resize_thread_pager(Screen, !Info, !IO),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- type on_entry
    --->    redraw
    ;       no_draw
    ;       no_draw_have_key(keycode).

:- pred thread_pager_loop(screen::in, on_entry::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

thread_pager_loop(Screen, OnEntry, !Info, !IO) :-
    (
        OnEntry = redraw,
        draw_thread_pager(Screen, !.Info, !IO),
        update_panels(Screen, !IO)
    ;
        OnEntry = no_draw
    ;
        OnEntry = no_draw_have_key(_)
    ),

    (
        ( OnEntry = redraw
        ; OnEntry = no_draw
        ),
        poll_async_with_progress(Screen, handle_poll_result, !Info, !IO),
        get_keycode_async_aware(!.Info ^ tp_next_poll_time, Key, !IO)
    ;
        OnEntry = no_draw_have_key(Key)
    ),
    thread_pager_input(Key, Action, MessageUpdate, !Info),
    update_message(Screen, MessageUpdate, !IO),

    (
        Action = continue,
        maybe_sched_poll(!Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = continue_no_draw,
        maybe_sched_poll(!Info, !IO),
        thread_pager_loop(Screen, no_draw, !Info, !IO)
    ;
        Action = resize,
        recreate_screen_for_resize(Screen, !IO),
        resize_thread_pager(Screen, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = start_reply(Message, ReplyKind),
        (
            Message = message(MessageId, _, _, _, _, _),
            flush_async_with_progress(Screen, !IO),
            Config = !.Info ^ tp_config,
            Crypto = !.Info ^ tp_crypto,
            Pager = !.Info ^ tp_pager,
            get_part_visibility_map(Pager, MessageId, PartVisibilityMap),
            start_reply(Config, Crypto, Screen, Message, ReplyKind,
                PartVisibilityMap, Transition, !IO),
            handle_screen_transition(Screen, Transition, Sent, !Info, !IO),
            (
                Sent = sent,
                AddedMessages0 = !.Info ^ tp_added_messages,
                !Info ^ tp_added_messages := AddedMessages0 + 1,
                % XXX would be nice to move cursor to the sent message
                reopen_thread_pager(Screen, no, !Info, !IO)
            ;
                Sent = not_sent
            )
        ;
            Message = excluded_message(_, _, _, _, _)
        ),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = start_forward(Message),
        (
            Message = message(MessageId, _, _, _, _, _),
            flush_async_with_progress(Screen, !IO),
            Config = !.Info ^ tp_config,
            Crypto = !.Info ^ tp_crypto,
            Pager = !.Info ^ tp_pager,
            get_part_visibility_map(Pager, MessageId, PartVisibilityMap),
            start_forward(Config, Crypto, Screen, Message, PartVisibilityMap,
                Transition, !IO),
            handle_screen_transition(Screen, Transition, _Sent, !Info, !IO)
        ;
            Message = excluded_message(_, _, _, _, _)
        ),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = prompt_resend(MessageId),
        flush_async_with_progress(Screen, !IO),
        handle_resend(Screen, MessageId, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = start_recall,
        flush_async_with_progress(Screen, !IO),
        ThreadId = !.Info ^ tp_thread_id,
        handle_recall(Screen, ThreadId, Sent, !Info, !IO),
        (
            Sent = sent,
            AddedMessages0 = !.Info ^ tp_added_messages,
            !Info ^ tp_added_messages := AddedMessages0 + 1,
            reopen_thread_pager(Screen, no, !Info, !IO)
        ;
            Sent = not_sent
        ),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = edit_as_template(Message),
        flush_async_with_progress(Screen, !IO),
        handle_edit_as_template(Screen, Message, Sent, !Info, !IO),
        (
            Sent = sent,
            AddedMessages0 = !.Info ^ tp_added_messages,
            !Info ^ tp_added_messages := AddedMessages0 + 1,
            reopen_thread_pager(Screen, no, !Info, !IO)
        ;
            Sent = not_sent
        ),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = prompt_tag(Initial),
        prompt_tag(Screen, Initial, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
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
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = prompt_save_part(Part, MaybeSubject),
        prompt_save_part(Screen, Part, MaybeSubject, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = prompt_open_part(Part),
        prompt_open_part(Screen, Part, MaybeNextKey, !Info, !IO),
        (
            MaybeNextKey = yes(NextKey),
            thread_pager_loop(Screen, no_draw_have_key(NextKey), !Info, !IO)
        ;
            MaybeNextKey = no,
            thread_pager_loop(Screen, redraw, !Info, !IO)
        )
    ;
        Action = prompt_open_url(Url),
        prompt_open_url(Screen, Url, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = prompt_search(SearchDir),
        prompt_search(Screen, SearchDir, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = decrypt_part,
        decrypt_part(Screen, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = verify_part,
        verify_part(Screen, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = toggle_content(ToggleType),
        toggle_content(Screen, ToggleType, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = toggle_ordering,
        toggle_ordering(!Info),
        reopen_thread_pager(Screen, yes, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = addressbook_add,
        addressbook_add(Screen, !.Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = pipe_ids,
        pipe_ids(Screen, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
    ;
        Action = refresh_results,
        flush_async_with_progress(Screen, !IO),
        reopen_thread_pager(Screen, no, !Info, !IO),
        thread_pager_loop(Screen, redraw, !Info, !IO)
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
        ; Key = code(curs.key_down)
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
        ; Key = code(curs.key_up)
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
        ; Key = code(curs.key_pagedown)
        )
    ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll_but_stop_at_message(Delta, MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = char('b')
        ; Key = code(curs.key_pageup)
        )
    ->
        Delta = int.max(0, NumPagerRows - 1),
        scroll_but_stop_at_message(-Delta, MessageUpdate, !Info),
        Action = continue
    ;
        ( Key = code(curs.key_home)
        ; Key = char('g')
        )
    ->
        goto_first_message(!Info),
        MessageUpdate = clear_message,
        Action = continue
    ;
        ( Key = code(curs.key_end)
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
        ( Key = char('\t')
        ; Key = char(',')
        )
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
        Key = char('a')
    ->
        toggle_archive(!Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('d')
    ->
        change_deleted(deleted, !Info),
        next_message(MessageUpdate, !Info),
        Action = continue
    ;
        Key = char('u')
    ->
        change_deleted(not_deleted, !Info),
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
        choose_toggle_action(!.Info, toggle_content(cycle_alternatives),
            Action),
        MessageUpdate = clear_message
    ;
        Key = char('Z')
    ->
        choose_toggle_action(!.Info, toggle_content(toggle_expanded),
            Action),
        MessageUpdate = clear_message
    ;
        Key = char('y')
    ->
        Action = verify_part,
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
        Key = char('A')
    ->
        mark_all_archived(!Info),
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
        Key = char('W')
    ->
        forward(!.Info, Action, MessageUpdate)
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
        Key = char('@')
    ->
        Action = addressbook_add,
        MessageUpdate = no_change
    ;
        Key = char('|')
    ->
        Action = pipe_ids,
        MessageUpdate = no_change
    ;
        ( Key = code(curs.key_resize) ->
            Action = resize
        ; Key = timeout_or_error ->
            Action = continue_no_draw
        ;
            Action = continue
        ),
        MessageUpdate = no_change
    ).

%-----------------------------------------------------------------------------%

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
    NumPagerRows = !.Info ^ tp_num_pager_rows,
    pager.goto_end(NumPagerRows, PagerInfo0, PagerInfo),
    !Info ^ tp_pager := PagerInfo,
    sync_thread_to_pager(!Info).

:- pred goto_parent_message(message_update::out,
    thread_pager_info::in, thread_pager_info::out) is det.

goto_parent_message(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    NumThreadRows = !.Info ^ tp_num_thread_rows,
    PagerInfo0 = !.Info ^ tp_pager,
    (
        get_cursor_line(Scrollable0, Cursor0, ThreadLine0),
        ThreadLine0 ^ tp_parent = yes(ParentId),
        find_non_excluded_ancestor(Scrollable0, ParentId, Cursor0, Cursor,
            AncestorId)
    ->
        set_cursor_centred(Cursor, NumThreadRows, Scrollable0, Scrollable),
        skip_to_message(AncestorId, PagerInfo0, PagerInfo),
        !Info ^ tp_scrollable := Scrollable,
        !Info ^ tp_pager := PagerInfo,
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("Message has no parent.")
    ).

:- pred find_non_excluded_ancestor(scrollable(thread_line)::in, message_id::in,
    int::in, int::out, message_id::out) is semidet.

find_non_excluded_ancestor(Scrollable, ParentId, !Cursor, AncestorId) :-
    search_reverse(thread_line_has_message_id(ParentId), Scrollable, !Cursor),
    get_line(Scrollable, !.Cursor, ThreadLine),
    ( non_excluded_message(ThreadLine ^ tp_message) ->
        AncestorId = ParentId
    ;
        ThreadLine ^ tp_parent = yes(GP),
        find_non_excluded_ancestor(Scrollable, GP, !Cursor, AncestorId)
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
    (
        search_forward(thread_line_has_message_id(MessageId),
            !.Scrollable, 0, Cursor, _)
    ->
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
        search_forward(is_unread_and_not_excluded, Scrollable0,
            Cursor0 + 1, Cursor, ThreadLine)
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

:- pred thread_line_has_message_id(message_id::in, thread_line::in) is semidet.

thread_line_has_message_id(MessageId, Line) :-
    Message = Line ^ tp_message,
    require_complete_switch [Message]
    (
        Message = message(MessageId, _, _, _, _, _)
    ;
        Message = excluded_message(yes(MessageId), _, _, _, _)
    ).

:- pred is_unread_and_not_excluded(thread_line::in) is semidet.

is_unread_and_not_excluded(Line) :-
    non_excluded_message(Line ^ tp_message),
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
        is_unread_and_not_excluded(Line0)
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
    remove_tag(tag("unread"), !Line).

:- pred set_line_unread(thread_line::in, thread_line::out) is det.

set_line_unread(!Line) :-
    add_tag(tag("unread"), !Line).

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

:- pred toggle_archive(thread_pager_info::in, thread_pager_info::out) is det.

toggle_archive(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Tags0 = Line0 ^ tp_curr_tags,
        (
            ( set.contains(Tags0, tag("inbox"))
            ; set.contains(Tags0, tag("unread"))
            )
        ->
            set_line_archived(Line0, Line)
        ;
            set_line_unarchived(Line0, Line)
        ),
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred mark_all_archived(thread_pager_info::in, thread_pager_info::out) is det.

mark_all_archived(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    scrollable.map_lines(set_line_archived, Scrollable0, Scrollable),
    !Info ^ tp_scrollable := Scrollable.

:- pred set_line_archived(thread_line::in, thread_line::out) is det.

set_line_archived(!Line) :-
    % Although this doesn't skip excluded messages, it ends up having no
    % effect.
    Tags0 = !.Line ^ tp_curr_tags,
    set.delete_list([tag("inbox"), tag("unread")], Tags0, Tags),
    set_tags(Tags, !Line).

:- pred set_line_unarchived(thread_line::in, thread_line::out) is det.

set_line_unarchived(!Line) :-
    add_tag(tag("inbox"), !Line).

:- pred toggle_flagged(thread_pager_info::in, thread_pager_info::out) is det.

toggle_flagged(!Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        Flagged0 = Line0 ^ tp_std_tags ^ flagged,
        (
            Flagged0 = flagged,
            remove_tag(tag("flagged"), Line0, Line)
        ;
            Flagged0 = unflagged,
            add_tag(tag("flagged"), Line0, Line)
        ),
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred change_deleted(deleted::in,
    thread_pager_info::in, thread_pager_info::out) is det.

change_deleted(Deleted, !Info) :-
    Scrollable0 = !.Info ^ tp_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor, Line0) ->
        (
            Deleted = not_deleted,
            remove_tag(tag("deleted"), Line0, Line)
        ;
            Deleted = deleted,
            add_tag(tag("deleted"), Line0, Line)
        ),
        set_cursor_line(Line, Scrollable0, Scrollable),
        !Info ^ tp_scrollable := Scrollable
    ;
        true
    ).

:- pred add_tag(tag::in, thread_line::in, thread_line::out) is det.

add_tag(Tag, !Line) :-
    TagSet0 = !.Line ^ tp_curr_tags,
    set.insert(Tag, TagSet0, TagSet),
    set_tags(TagSet, !Line).

:- pred remove_tag(tag::in, thread_line::in, thread_line::out) is det.

remove_tag(Tag, !Line) :-
    TagSet0 = !.Line ^ tp_curr_tags,
    ( set.remove(Tag, TagSet0, TagSet) ->
        set_tags(TagSet, !Line)
    ;
        true
    ).

:- pred set_tags(set(tag)::in, thread_line::in, thread_line::out) is det.

set_tags(TagSet, !Line) :-
    get_standard_tags(TagSet, StdTags, NonstdTagsWidth),
    !Line ^ tp_curr_tags := TagSet,
    !Line ^ tp_std_tags := StdTags,
    !Line ^ tp_nonstd_tags_width := NonstdTagsWidth.

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
            % Notmuch performs tag removals before addition.
            CurrTags0 = CursorLine0 ^ tp_curr_tags,
            set.difference(CurrTags0, RemoveTags, CurrTags1),
            set.union(CurrTags1, AddTags, CurrTags),
            set_tags(CurrTags, CursorLine0, CursorLine),

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
        get_keycode_blocking_handle_resize(Screen, set_prompt(Prompt), KeyCode,
            !Info, !IO),
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
    TagSet = Line ^ tp_curr_tags,
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
    Message = Line0 ^ tp_message,
    TagSet0 = Line0 ^ tp_curr_tags,
    Selected = Line0 ^ tp_selected,
    (
        MessageId = Message ^ m_id,
        Selected = selected,
        % Notmuch performs tag removals before addition.
        set.difference(TagSet0, RemoveTags, TagSet1),
        set.union(TagSet1, AddTags, TagSet),
        TagSet \= TagSet0
    ->
        set_tags(TagSet, Line0, Line),
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
    Part = part(MessageId, MaybePartId, _Type, _MaybeContentCharset,
        _MaybeContentDisposition, _Content, MaybePartFilename,
        _MaybeContentLength, _MaybeCTE, IsDecrypted),
    (
        MaybePartFilename = yes(filename(PartFilename))
    ;
        MaybePartFilename = no,
        MaybeSubject = yes(Subject),
        make_filename_from_subject(Subject, PartFilename)
    ;
        MaybePartFilename = no,
        MaybeSubject = no,
        MessageId = message_id(IdStr),
        (
            MaybePartId = yes(part_id(PartIdInt)),
            PartFilename = string.format("%s.part_%d", [s(IdStr), i(PartIdInt)])
        ;
            MaybePartId = yes(part_id_string(PartIdStr)),
            PartFilename = string.format("%s.part_%s", [s(IdStr), s(PartIdStr)])
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
            do_save_part(Config, MessageId, MaybePartId, IsDecrypted, FileName,
                Res, !IO),
            (
                Res = ok,
                ( MaybePartId = yes(part_id(0)) ->
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

:- pred do_save_part(prog_config::in, message_id::in, maybe(part_id)::in,
    maybe_decrypted::in, string::in, maybe_error::out, io::di, io::uo) is det.

do_save_part(Config, MessageId, MaybePartId, IsDecrypted, FileName, Res, !IO)
        :-
    (
        MaybePartId = yes(PartId),
        get_notmuch_command(Config, Notmuch),
        make_quoted_command(Notmuch, [
            "show", "--format=raw", decrypt_arg(IsDecrypted),
            part_id_to_part_option(PartId),
            "--", message_id_to_search_term(MessageId)
        ], no_redirect, redirect_output(FileName), Command),
        % Decryption may invoke pinentry-curses.
        curs.soft_suspend(io.call_system(Command), CallRes, !IO)
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
            MaybeNextKey, !Info, !IO)
    ;
        MessageUpdate = clear_message,
        MaybeNextKey = no
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_open_part(prog_config::in, screen::in, part::in, string::in,
    message_update::out, maybe(keycode)::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

do_open_part(Config, Screen, Part, Command, MessageUpdate, MaybeNextKey,
        !Info, !IO) :-
    promise_equivalent_solutions [MessageUpdate, MaybeNextKey, !:Info, !:IO] (
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
                MaybeNextKey, !Info, !IO)
        ;
            (
                ParseResult = error(yes(Error), _Line, Column),
                Message = string.format("parse error at column %d: %s",
                    [i(Column), s(Error)])
            ;
                ParseResult = error(no, _Line, Column),
                Message = string.format("parse error at column %d",
                    [i(Column)])
            ),
            MessageUpdate = set_warning(Message),
            MaybeNextKey = no
        )
    ).

:- pred do_open_part_2(prog_config::in, screen::in, part::in,
    list(word)::in(non_empty_list), message_update::out, maybe(keycode)::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

do_open_part_2(Config, Screen, Part, CommandWords, MessageUpdate, MaybeNextKey,
        !Info, !IO) :-
    Part = part(MessageId, MaybePartId, _ContentType, _MaybeContentCharset,
        _MaybeContentDisposition, _Content, MaybePartFileName,
        _MaybeContentLength, _MaybeCTE, IsDecrypted),
    (
        MaybePartFileName = yes(filename(PartFilename)),
        get_extension(PartFilename, Ext)
    ->
        make_temp_suffix(Ext, Res0, !IO)
    ;
        make_temp_suffix("", Res0, !IO)
    ),
    (
        Res0 = ok(FileName),
        do_save_part(Config, MessageId, MaybePartId, IsDecrypted, FileName,
            Res, !IO),
        (
            Res = ok,
            call_open_command(Screen, CommandWords, FileName, MaybeError, !IO),
            (
                MaybeError = ok,
                Message = "Press any key to continue (deletes temporary file)",
                get_keycode_blocking_handle_resize(Screen, set_info(Message),
                    Key, !Info, !IO),
                MaybeNextKey = yes(Key),
                MessageUpdate = clear_message
            ;
                MaybeError = error(Msg),
                MessageUpdate = set_warning(Msg),
                MaybeNextKey = no
            )
        ;
            Res = error(Error),
            string.format("Error saving to %s: %s", [s(FileName), s(Error)],
                Msg),
            MessageUpdate = set_warning(Msg),
            MaybeNextKey = no
        ),
        io.remove_file(FileName, _, !IO)
    ;
        Res0 = error(Error),
        string.format("Error opening temporary file: %s", [s(Error)], Msg),
        MessageUpdate = set_warning(Msg),
        MaybeNextKey = no
    ).

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
        curs.suspend(io.call_system(CommandToRun), CallRes, !IO)
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
                ParseResult = error(yes(Error), _Line, Column),
                Message = string.format("parse error at column %d: %s",
                    [i(Column), s(Error)])
            ;
                ParseResult = error(no, _Line, Column),
                Message = string.format("parse error at column %d",
                    [i(Column)])
            ),
            MessageUpdate = set_warning(Message)
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

:- pred choose_toggle_action(thread_pager_info::in, thread_pager_action::in,
    thread_pager_action::out) is det.

choose_toggle_action(Info, Action0, Action) :-
    Pager = Info ^ tp_pager,
    ( get_highlighted_thing(Pager, Thing) ->
        (
            Thing = highlighted_part(Part, _),
            Content = Part ^ pt_content,
            (
                ( Content = text(_)
                ; Content = subparts(not_encrypted, _, _)
                ; Content = encapsulated_message(_)
                ; Content = unsupported
                ),
                Action = Action0
            ;
                Content = subparts(decryption_good, _, _),
                Action = Action0
            ;
                Content = subparts(encrypted, _, _),
                Action = decrypt_part
            ;
                Content = subparts(decryption_bad, _, _),
                Action = decrypt_part
            )
        ;
            Thing = highlighted_url(_),
            Action = continue
        ;
            Thing = highlighted_fold_marker,
            Action = Action0
        )
    ;
        Action = continue
    ).

:- pred decrypt_part(screen::in, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

decrypt_part(Screen, !Info, !IO) :-
    Pager = !.Info ^ tp_pager,
    (
        get_highlighted_thing(Pager, Thing),
        Thing = highlighted_part(Part, _)
    ->
        MessageId = Part ^ pt_msgid,
        MaybePartId = Part ^ pt_part,
        (
            MaybePartId = yes(PartId),
            do_decrypt_part(Screen, MessageId, PartId, MessageUpdate,
                !Info, !IO)
        ;
            MaybePartId = no,
            MessageUpdate = set_warning("Missing part id.")
        )
    ;
        MessageUpdate = set_warning("No part selected.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_decrypt_part(screen::in, message_id::in, part_id::in,
    message_update::out, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

do_decrypt_part(Screen, MessageId, PartId, MessageUpdate, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    run_notmuch(Config,
        [
            "show", "--format=json", "--decrypt",
            part_id_to_part_option(PartId),
            "--", message_id_to_search_term(MessageId)
        ],
        redirect_stderr("/dev/null"),
        soft_suspend_curses, % Decryption may invoke pinentry-curses.
        parse_part(MessageId, not_decrypted), ParseResult, !IO),
    (
        ParseResult = ok(Part),
        Content = Part ^ pt_content,
        (
            Content = subparts(EncStatus, _SigStatus, _SubParts)
        ;
            ( Content = text(_)
            ; Content = encapsulated_message(_)
            ; Content = unsupported
            ),
            % Should not happen.
            EncStatus = not_encrypted
        ),
        (
            EncStatus = not_encrypted,
            MessageUpdate = set_info("Part not encrypted.")
        ;
            EncStatus = encrypted,
            MessageUpdate = set_warning("Part encrypted.")
        ;
            EncStatus = decryption_good,
            MessageUpdate = set_info("Part decrypted.")
        ;
            EncStatus = decryption_bad,
            MessageUpdate = set_warning("Decryption failed.")
        ),
        Pager0 = !.Info ^ tp_pager,
        NumRows = !.Info ^ tp_num_pager_rows,
        get_cols(Screen, Cols, !IO),
        replace_node_under_cursor(Config, NumRows, Cols, Part, Pager0, Pager,
            !IO),
        !Info ^ tp_pager := Pager
    ;
        ParseResult = error(Error),
        MessageUpdate = set_warning("Error: " ++ Error)
    ).

%-----------------------------------------------------------------------------%

:- pred verify_part(screen::in, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

verify_part(Screen, !Info, !IO) :-
    Pager = !.Info ^ tp_pager,
    (
        get_highlighted_thing(Pager, Thing),
        Thing = highlighted_part(Part, _)
    ->
        ( Part ^ pt_content_type = mime_type.multipart_signed ->
            do_verify_part(Screen, Part, MessageUpdate, !Info, !IO)
        ;
            MessageUpdate =
                set_warning("Please select multipart/signed part to verify.")
        )
    ;
        MessageUpdate = set_warning("No part selected.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_verify_part(screen::in, part::in, message_update::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

do_verify_part(Screen, Part0, MessageUpdate, !Info, !IO) :-
    Part0 = part(MessageId, MaybePartId, Type, _MaybeContentDisposition0,
        _Content0, _MaybeFilename0, _MaybeContentCharset, _MaybeContentLength,
        _MaybeCTE, IsDecrypted),
    (
        MaybePartId = yes(PartId),
        Config = !.Info ^ tp_config,
        % Why doesn't notmuch allow --body=false? We could avoid downloading
        % the content again.
        run_notmuch(Config,
            [
                "show", "--format=json",
                "--verify", decrypt_arg(IsDecrypted), % seems unlikely
                part_id_to_part_option(PartId),
                "--", message_id_to_search_term(MessageId)
            ],
            redirect_stderr("/dev/null"),
            soft_suspend_curses, % Decryption may invoke pinentry-curses.
            parse_part(MessageId, not_decrypted), ParseResult, !IO),
        (
            ParseResult = ok(Part1),
            (
                Part1 = part(MessageId, MaybePartId, Type, MaybeContentCharset,
                    MaybeContentDisposition, Content, MaybeFilename,
                    MaybeContentLength, MaybeCTE, _IsDecrypted1)
            ->
                Part = part(MessageId, MaybePartId, Type, MaybeContentCharset,
                    MaybeContentDisposition, Content, MaybeFilename,
                    MaybeContentLength, MaybeCTE, IsDecrypted),

                Pager0 = !.Info ^ tp_pager,
                NumRows = !.Info ^ tp_num_pager_rows,
                get_cols(Screen, Cols, !IO),
                % Regenerating the part tree is overkill...
                replace_node_under_cursor(Config, NumRows, Cols, Part,
                    Pager0, Pager, !IO),
                !Info ^ tp_pager := Pager,

                post_verify_message_update(Content, MessageUpdate)
            ;
                MessageUpdate = set_warning("notmuch return unexpected part.")
            )
        ;
            ParseResult = error(Error),
            MessageUpdate = set_warning("Error: " ++ Error)
        )
    ;
        MaybePartId = no,
        MessageUpdate = set_warning("Missing part id.")
    ).

:- pred post_verify_message_update(part_content::in, message_update::out)
    is det.

post_verify_message_update(Content, MessageUpdate) :-
    (
        Content = subparts(_, Signatures, _),
        ( all_true(good_signature, Signatures) ->
            (
                Signatures = [],
                MessageUpdate = set_info("No signature.")
            ;
                Signatures = [_],
                MessageUpdate = set_info("Verified signature.")
            ;
                Signatures = [_, _ | _],
                MessageUpdate = set_info("Verified signatures.")
            )
        ;
            MessageUpdate = set_warning("Failed to verify signature.")
        )
    ;
        ( Content = text(_)
        ; Content = encapsulated_message(_)
        ; Content = unsupported
        ),
        MessageUpdate = set_warning("Unexpected content.")
    ).

:- pred good_signature(signature::in) is semidet.

good_signature(Signature) :-
    Signature = signature(Status, MaybeSigErrors),
    require_complete_switch [MaybeSigErrors]
    (
        MaybeSigErrors = no
    ;
        MaybeSigErrors = yes(sig_errors_v3(NumErrors)),
        NumErrors = 0
    ;
        MaybeSigErrors = yes(sig_errors_v4(SigErrors)),
        SigErrors = []
    ),
    require_complete_switch [Status]
    (
        Status = good(_, _, _, _)
    ;
        Status = none,
        fail
    ;
        Status = not_good(_, _),
        fail
    ).

%-----------------------------------------------------------------------------%

:- pred toggle_content(screen::in, toggle_type::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

toggle_content(Screen, ToggleType, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    NumRows = !.Info ^ tp_num_pager_rows,
    get_cols(Screen, Cols, !IO),
    Pager0 = !.Info ^ tp_pager,
    pager.toggle_content(Config, ToggleType, NumRows, Cols, MessageUpdate,
        Pager0, Pager, !IO),
    !Info ^ tp_pager := Pager,
    sync_thread_to_pager(!Info),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred toggle_ordering(thread_pager_info::in, thread_pager_info::out) is det.

toggle_ordering(!Info) :-
    Ordering0 = !.Info ^ tp_ordering,
    (
        Ordering0 = ordering_flat,
        Ordering = ordering_threaded
    ;
        Ordering0 = ordering_threaded,
        Ordering = ordering_flat
    ),
    !Info ^ tp_ordering := Ordering.

%-----------------------------------------------------------------------------%

:- pred get_effects(thread_pager_info::in, thread_pager_effects::out) is det.

get_effects(Info, Effects) :-
    Scrollable = Info ^ tp_scrollable,
    Lines = get_lines(Scrollable),
    version_array.foldl(get_non_excluded_message_tags, Lines,
        set.init, TagSet),
    version_array.foldl(get_non_excluded_message_tag_delta_groups, Lines,
        map.init, TagDeltaGroups),
    AddedMessages = Info ^ tp_added_messages,
    Effects = thread_pager_effects(TagSet, TagDeltaGroups, AddedMessages).

:- pred get_non_excluded_message_tags(thread_line::in,
    set(tag)::in, set(tag)::out) is det.

get_non_excluded_message_tags(Line, !TagSet) :-
    Line = thread_line(Message, _MaybeParentId, _From, _PrevTags, CurrTags,
        _StdTags, _NonstdTagsWidth, _Selected, _Graphics, _RelDate,
        _MaybeSubject),
    (
        Message = message(_, _, _, _, _, _),
        set.union(CurrTags, !TagSet)
    ;
        Message = excluded_message(_, _, _, _, _)
    ).

:- pred get_non_excluded_message_tag_delta_groups(thread_line::in,
    map(set(tag_delta), list(message_id))::in,
    map(set(tag_delta), list(message_id))::out) is det.

get_non_excluded_message_tag_delta_groups(Line, !TagDeltaGroups) :-
    Line = thread_line(Message, _MaybeParentId, _From, PrevTags, CurrTags,
        _StdTags, _NonstdTagsWidth, _Selected, _Graphics, _RelDate,
        _MaybeSubject),
    (
        Message = message(MessageId, _, _, _, _, _),
        TagDeltaSet = get_tag_delta_set(PrevTags, CurrTags),
        ( set.is_empty(TagDeltaSet) ->
            true
        ;
            ( map.search(!.TagDeltaGroups, TagDeltaSet, Messages0) ->
                map.det_update(TagDeltaSet, [MessageId | Messages0],
                    !TagDeltaGroups)
            ;
                map.det_insert(TagDeltaSet, [MessageId], !TagDeltaGroups)
            )
        )
    ;
        Message = excluded_message(_, _, _, _, _)
    ).

:- func get_tag_delta_set(set(tag), set(tag)) = set(tag_delta).

get_tag_delta_set(PrevTags, CurrTags) = TagDeltaSet :-
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

:- pred forward(thread_pager_info::in, thread_pager_action::out,
    message_update::out) is det.

forward(Info, Action, MessageUpdate) :-
    PagerInfo = Info ^ tp_pager,
    ( get_top_message(PagerInfo, Message) ->
        MessageUpdate = clear_message,
        Action = start_forward(Message)
    ;
        MessageUpdate = set_warning("No message to forward."),
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

:- pred handle_recall(screen::in, thread_id::in, sent::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_recall(Screen, ThreadId, Sent, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    Crypto = !.Info ^ tp_crypto,
    select_recall(Config, Screen, yes(ThreadId), TransitionA, !IO),
    handle_screen_transition(Screen, TransitionA, MaybeSelected, !Info, !IO),
    (
        MaybeSelected = yes(Message),
        (
            Message = message(_, _, _, _, _, _),
            PartVisibilityMap = map.init,
            continue_from_message(Config, Crypto, Screen, postponed_message,
                Message, PartVisibilityMap, TransitionB, !IO),
            handle_screen_transition(Screen, TransitionB, Sent, !Info, !IO)
        ;
            Message = excluded_message(_, _, _, _, _),
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

:- pred handle_edit_as_template(screen::in, message::in, sent::out,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_edit_as_template(Screen, Message, Sent, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    Crypto = !.Info ^ tp_crypto,
    Pager = !.Info ^ tp_pager,
    (
        Message = message(MessageId, _, _, _, _, _),
        get_part_visibility_map(Pager, MessageId, PartVisibilityMap),
        continue_from_message(Config, Crypto, Screen, arbitrary_message,
            Message, PartVisibilityMap, Transition, !IO),
        handle_screen_transition(Screen, Transition, Sent, !Info, !IO)
    ;
        Message = excluded_message(_, _, _, _, _),
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

:- pred pipe_ids(screen::in, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

pipe_ids(Screen, !Info, !IO) :-
    PromptWhat0 = "Pipe: (t) current thread ID",
    get_selected_or_current_message_ids(!.Info, HaveSelectedMessages,
        MessageIds),
    (
        HaveSelectedMessages = no,
        PromptWhat = PromptWhat0 ++ ", (m) current message ID"
    ;
        HaveSelectedMessages = yes,
        PromptWhat = PromptWhat0 ++ ", (m) selected message IDs"
    ),
    get_keycode_blocking_handle_resize(Screen, set_prompt(PromptWhat), KeyCode,
        !Info, !IO),
    ( KeyCode = char('t') ->
        PromptCommand = "Pipe thread ID: ",
        ThreadId = !.Info ^ tp_thread_id,
        IdStrings = [thread_id_to_search_term(ThreadId)],
        pipe_ids_2(Screen, PromptCommand, IdStrings, MessageUpdate, !Info, !IO)
    ; KeyCode = char('m') ->
        (
            MessageIds = [],
            MessageUpdate = set_warning("No message.")
        ;
            (
                MessageIds = [_],
                PromptCommand = "Pipe message ID: "
            ;
                MessageIds = [_, _ | _],
                PromptCommand = "Pipe message IDs: "
            ),
            IdStrings = map(message_id_to_search_term, MessageIds),
            pipe_ids_2(Screen, PromptCommand, IdStrings, MessageUpdate,
                !Info, !IO)
        )
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred pipe_ids_2(screen::in, string::in, list(string)::in,
    message_update::out, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

pipe_ids_2(Screen, PromptCommand, Strings, MessageUpdate, !Info, !IO) :-
    History0 = !.Info ^ tp_common_history ^ ch_pipe_id_history,
    prompt_and_pipe_to_command(Screen, PromptCommand, Strings, MessageUpdate,
        History0, History, !IO),
    !Info ^ tp_common_history ^ ch_pipe_id_history := History.

:- pred get_selected_or_current_message_ids(thread_pager_info::in, bool::out,
    list(message_id)::out) is det.

get_selected_or_current_message_ids(Info, Selected, MessageIds) :-
    Scrollable = Info ^ tp_scrollable,
    Lines = get_lines_list(Scrollable),
    (
        list.filter_map(selected_line_message_id, Lines, SelectedMessageIds),
        SelectedMessageIds \= []
    ->
        Selected = yes,
        MessageIds = SelectedMessageIds
    ;
        Selected = no,
        (
            get_cursor_line(Scrollable, _Cursor, CursorLine),
            thread_line_message_id(CursorLine, MessageId)
        ->
            MessageIds = [MessageId]
        ;
            MessageIds = []
        )
    ).

:- pred selected_line_message_id(thread_line::in, message_id::out) is semidet.

selected_line_message_id(Line, MessageId) :-
    Line ^ tp_selected = selected,
    thread_line_message_id(Line, MessageId).

:- pred thread_line_message_id(thread_line::in, message_id::out) is semidet.

thread_line_message_id(Line, MessageId) :-
    Message = Line ^ tp_message,
    require_complete_switch [Message]
    (
        Message = message(MessageId, _, _, _, _, _)
    ;
        Message = excluded_message(yes(MessageId), _, _, _, _)
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_sched_poll(thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

maybe_sched_poll(!Info, !IO) :-
    MaybeNextPollTime = !.Info ^ tp_next_poll_time,
    (
        MaybeNextPollTime = no
    ;
        MaybeNextPollTime = yes(NextPollTime),
        current_timestamp(Time, !IO),
        DeltaSecs = NextPollTime - Time,
        ( DeltaSecs =< 0.0 ->
            sched_poll(Time, !Info, !IO)
        ;
            true
        )
    ).

:- pred sched_poll(timestamp::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

sched_poll(Time, !Info, !IO) :-
    Config = !.Info ^ tp_config,
    get_notmuch_command(Config, Notmuch),
    ThreadId = !.Info ^ tp_thread_id,
    RefreshTime = !.Info ^ tp_refresh_time,
    IndexPollString = !.Info ^ tp_index_poll_string,
    StdinContents = string.append_list([
        % query 1
        thread_id_to_search_term(ThreadId), " ",
        timestamp_to_int_string(RefreshTime), "..",
        "\n",
        % query 2
        IndexPollString, " AND NOT ", thread_id_to_search_term(ThreadId),
        "\n"
    ]),
    Op = async_lowprio_command(Notmuch, ["count", "--batch"],
        yes(StdinContents)),
    push_lowprio_async(Op, _Pushed, !IO),
    !Info ^ tp_next_poll_time := next_poll_time(Config, Time).

:- pred handle_poll_result(screen::in, string::in,
    thread_pager_info::in, thread_pager_info::out, io::di, io::uo) is det.

handle_poll_result(Screen, CountOutput, !Info, !IO) :-
    (
        string.split_at_char('\n', CountOutput) = [LineA, LineB, ""],
        string.to_int(LineA, ThreadCount),
        string.to_int(LineB, IndexCount)
    ->
        ThreadCount0 = !.Info ^ tp_thread_poll_count,
        IndexCount0 = !.Info ^ tp_index_poll_count,
        (
            ThreadCount = ThreadCount0,
            IndexCount = IndexCount0
        ->
            true
        ;
            !Info ^ tp_thread_poll_count := ThreadCount,
            !Info ^ tp_index_poll_count := IndexCount,

            % Redraw the bar immediately.
            draw_thread_pager_bar(Screen, !.Info, !IO),
            update_panels(Screen, !IO),

            % Run notify command if any.
            ( if ThreadCount > 0 ; IndexCount > 0 then
                Config = !.Info ^ tp_config,
                maybe_poll_notify(Config,
                    count_messages_since_refresh(ThreadCount, IndexCount),
                    MessageUpdate, !IO),
                update_message(Screen, MessageUpdate, !IO)
            else
                true
            )
        )
    ;
        update_message_immed(Screen,
            set_warning("notmuch count returned unexpected result"), !IO)
    ).

:- func next_poll_time(prog_config, timestamp) = maybe(timestamp).

next_poll_time(Config, Time) = NextPollTime :-
    get_poll_period_secs(Config, Maybe),
    (
        Maybe = yes(PollSecs),
        NextPollTime = yes(Time + float(PollSecs))
    ;
        Maybe = no,
        NextPollTime = no
    ).

%-----------------------------------------------------------------------------%

:- pred get_keycode_blocking_handle_resize(screen::in, message_update::in,
    keycode::out, thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

get_keycode_blocking_handle_resize(Screen, Message, Key, !Info, !IO) :-
    update_message_immed(Screen, Message, !IO),
    get_keycode_blocking(Key0, !IO),
    ( Key0 = code(curs.key_resize) ->
        recreate_screen_for_resize(Screen, !IO),
        resize_thread_pager(Screen, !Info, !IO),
        draw_thread_pager(Screen, !.Info, !IO),
        update_panels(Screen, !IO),
        get_keycode_blocking_handle_resize(Screen, Message, Key, !Info, !IO)
    ;
        Key = Key0
    ).

%-----------------------------------------------------------------------------%

:- pred draw_thread_pager(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

draw_thread_pager(Screen, Info, !IO) :-
    Scrollable = Info ^ tp_scrollable,
    PagerInfo = Info ^ tp_pager,
    Config = Info ^ tp_config,
    Attrs = thread_attrs(Config),
    PagerAttrs = pager_attrs(Config),

    get_main_panels(Screen, MainPanels, !IO),
    split_panels(Info, MainPanels, ThreadPanels, SepPanel, PagerPanels),
    scrollable.draw(draw_thread_line(Attrs), Screen, ThreadPanels, Scrollable,
        !IO),
    get_cols(Screen, Cols, !IO),
    draw_sep(Screen, SepPanel, Attrs, Cols, !IO),
    draw_pager_lines(Screen, PagerPanels, PagerAttrs, PagerInfo, !IO),
    draw_thread_pager_bar(Screen, Info, !IO).

:- pred draw_sep(screen::in, maybe(vpanel)::in, thread_attrs::in, int::in,
    io::di, io::uo) is det.

draw_sep(Screen, MaybeSepPanel, Attrs, Cols, !IO) :-
    (
        MaybeSepPanel = yes(Panel),
        erase(Screen, Panel, !IO),
        attr(Screen, Panel, Attrs ^ t_status ^ bar, !IO),
        hline(Screen, Panel, '-', Cols, !IO)
    ;
        MaybeSepPanel = no
    ).

:- pred draw_thread_line(thread_attrs::in, screen::in, vpanel::in,
    thread_line::in, int::in, bool::in, io::di, io::uo) is det.

draw_thread_line(TAttrs, Screen, Panel, Line, _LineNr, IsCursor, !IO) :-
    Line = thread_line(Message, _ParentId, presentable_string(From),
        _PrevTags, CurrTags, StdTags, NonstdTagsWidth,
        Selected, MaybeGraphics, RelDate, MaybeSubject),
    Attrs = TAttrs ^ t_generic,
    (
        IsCursor = yes,
        RelDateAttr = Attrs ^ current
    ;
        IsCursor = no,
        RelDateAttr = Attrs ^ relative_date
    ),
    draw_fixed(Screen, Panel, RelDateAttr, 13, RelDate, ' ', !IO),

    (
        Selected = selected,
        mattr_draw(Screen, Panel, unless(IsCursor, Attrs ^ selected), "*", !IO)
    ;
        Selected = not_selected,
        draw(Screen, Panel, " ", !IO)
    ),
    mattr(Screen, Panel, unless(IsCursor, Attrs ^ standard_tag), !IO),

    StdTags = standard_tags(Unread, Replied, Deleted, Flagged),
    (
        Unread = unread,
        draw(Screen, Panel, "n", !IO)
    ;
        Unread = read,
        draw(Screen, Panel, " ", !IO)
    ),
    (
        Replied = replied,
        draw(Screen, Panel, "r", !IO)
    ;
        Replied = not_replied,
        draw(Screen, Panel, " ", !IO)
    ),
    (
        Deleted = deleted,
        draw(Screen, Panel, "d", !IO)
    ;
        Deleted = not_deleted,
        draw(Screen, Panel, " ", !IO)
    ),
    (
        Flagged = flagged,
        mattr_draw(Screen, Panel, unless(IsCursor, Attrs ^ flagged), "! ", !IO)
    ;
        Flagged = unflagged,
        draw(Screen, Panel, "  ", !IO)
    ),

    mattr(Screen, Panel, unless(IsCursor, TAttrs ^ t_tree), !IO),
    (
        MaybeGraphics = yes(Graphics),
        list.foldl(draw_graphic(Screen, Panel), Graphics, !IO)
    ;
        MaybeGraphics = no
    ),
    draw(Screen, Panel, "• ", !IO),

    (
        Message = message(_, _, _, _, _, _)
    ;
        Message = excluded_message(_, _, _, _, _),
        draw(Screen, Panel, "(excluded) ", !IO)
    ),

    getyx(Screen, Panel, Row, FromStartX, !IO),
    (
        Unread = unread,
        Highlight = curs.bold
    ;
        Unread = read,
        Highlight = curs.normal
    ),
    mattr_draw(Screen, Panel,
        unless(IsCursor, curs.(Attrs ^ author + Highlight)),
        From, !IO),
    (
        MaybeSubject = yes(presentable_string(Subject)),
        draw(Screen, Panel, ". ", !IO),
        mattr_draw(Screen, Panel, unless(IsCursor, Attrs ^ subject),
            Subject, !IO)
    ;
        MaybeSubject = no
    ),
    getyx(Screen, Panel, _, SubjectEndX, !IO),
    getmaxyx(Screen, Panel, _, MaxX, !IO),

    % Draw non-standard tags, overlapping from/subject text if necessary.
    ( NonstdTagsWidth > 0 ->
        (
            NonstdTagsWidth > MaxX - SubjectEndX,
            MoveX = max(FromStartX, MaxX - NonstdTagsWidth),
            MoveX < SubjectEndX
        ->
            move(Screen, Panel, Row, MoveX, !IO)
        ;
            true
        ),
        attr(Screen, Panel, Attrs ^ other_tag, !IO),
        set.fold(draw_display_tag(Screen, Panel), CurrTags, !IO)
    ;
        true
    ).

:- pred draw_display_tag(screen::in, vpanel::in, tag::in, io::di, io::uo)
    is det.

draw_display_tag(Screen, Panel, Tag, !IO) :-
    ( display_tag(Tag) ->
        Tag = tag(TagName),
        draw2(Screen, Panel, " ", TagName, !IO)
    ;
        true
    ).

:- pred draw_graphic(screen::in, vpanel::in, graphic::in, io::di, io::uo)
    is det.

draw_graphic(Screen, Panel, Graphic, !IO) :-
    draw(Screen, Panel, graphic_to_char(Graphic), !IO).

:- func graphic_to_char(graphic) = string.

graphic_to_char(blank) = " ".
graphic_to_char(vert) = "│".
graphic_to_char(tee) = "├".
graphic_to_char(ell) = "└".

:- func unless(bool, curs.attr) = maybe(curs.attr).

unless(no, X) = yes(X).
unless(yes, _) = no.

:- pred draw_thread_pager_bar(screen::in, thread_pager_info::in,
    io::di, io::uo) is det.

draw_thread_pager_bar(Screen, Info, !IO) :-
    ThreadCount = Info ^ tp_thread_poll_count,
    IndexCount = Info ^ tp_index_poll_count,
    ( ThreadCount = 0, IndexCount = 0 ->
        MaybeText = no
    ;
        MaybeText = yes(count_messages_since_refresh(ThreadCount, IndexCount))
    ),

    Scrollable = Info ^ tp_scrollable,
    PagerInfo = Info ^ tp_pager,
    NumPagerRows = Info ^ tp_num_pager_rows,
    (
        get_cursor_line(Scrollable, _Cursor, ThreadLine),
        Message = ThreadLine ^ tp_message,
        MessageId = Message ^ m_id,
        get_percent_visible(PagerInfo, NumPagerRows, MessageId, Percent)
    ->
        MaybeProgress = yes(string.format("%3d%%", [i(Percent)]))
    ;
        MaybeProgress = no
    ),

    draw_status_bar(Screen, MaybeText, MaybeProgress, !IO).

:- func count_messages_since_refresh(int, int) = string.

count_messages_since_refresh(ThreadCount, IndexCount) =
    ( IndexCount = 0 ->
        string.format("%+d thread messages since refresh", [i(ThreadCount)])
    ; ThreadCount = 0 ->
        string.format("%+d messages in index", [i(IndexCount)])
    ;
        string.format("%+d thread messages since refresh, +%d messages in index",
            [i(ThreadCount), i(IndexCount)])
    ).

:- pred split_panels(thread_pager_info::in, list(vpanel)::in,
    list(vpanel)::out, maybe(vpanel)::out, list(vpanel)::out) is det.

split_panels(Info, MainPanels, ThreadPanels, MaybeSepPanel, PagerPanels) :-
    NumThreadRows = Info ^ tp_num_thread_rows,
    NumPagerRows = Info ^ tp_num_pager_rows,
    list.split_upto(NumThreadRows, MainPanels, ThreadPanels, Panels1),
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
