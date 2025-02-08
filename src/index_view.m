% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module index_view.
:- interface.

:- import_module io.

:- import_module crypto.
:- import_module notmuch_config.
:- import_module prog_config.
:- import_module screen.
:- import_module view_common.

%-----------------------------------------------------------------------------%

:- pred open_index(prog_config::in, notmuch_config::in, crypto::in, screen::in,
    string::in, common_history::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module string.
:- import_module time.
:- use_module require.

:- import_module addressbook.
:- import_module async.
:- import_module callout.
:- import_module char_util.
:- import_module color.
:- import_module compose.
:- import_module cord_util.
:- import_module data.
:- import_module pipe_to.
:- import_module poll_notify.
:- import_module recall.
:- import_module sanitise.
:- import_module scrollable.
:- import_module search_term.
:- import_module signal.
:- import_module string_util.
:- import_module tags.
:- import_module text_entry.
:- import_module thread_pager.
:- import_module time_util.
:- import_module view_async.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- type index_info
    --->    index_info(
                i_config            :: prog_config,
                i_crypto            :: crypto,
                i_scrollable        :: scrollable(index_line),
                % XXX clarify "notmuch search" vs search string
                i_search_terms      :: string,
                i_search_tokens     :: list(token),
                i_search_time       :: timestamp,
                i_last_active_time  :: timestamp,
                i_next_poll_time    :: maybe(timestamp),
                i_poll_count        :: int,
                i_internal_search   :: maybe(string),
                i_internal_search_dir :: search_direction,
                i_show_authors      :: show_authors,
                i_common_history    :: common_history
            ).

:- type index_line
    --->    index_line(
                i_id        :: thread_id,
                i_selected  :: selected,
                i_date      :: string,
                i_authors   :: presentable_string,
                i_subject   :: presentable_string,
                i_tags      :: set(tag),
                i_std_tags  :: standard_tags, % cached from i_tags
                i_nonstd_tags_width :: int,   % cached from i_nonstd_tags_width
                i_matched   :: int,
                i_total     :: int,
                i_unmatched_ids :: list(message_id)
            ).

:- type selected
    --->    not_selected
    ;       selected.

:- type show_authors
    --->    show_authors
    ;       hide_authors.

:- type binding
    --->    scroll_down
    ;       scroll_up
    ;       home
    ;       end
    ;       page_up
    ;       page_down
    ;       half_page_up
    ;       half_page_down
    ;       skip_to_unread
    ;       open_thread_pager(obscure_mode_bool)
    ;       enter_limit
    ;       enter_limit_tilde
    ;       limit_alias_char(char)
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       start_reply(reply_kind)
    ;       addressbook_add
    ;       prompt_internal_search(search_direction)
    ;       skip_to_internal_search(rel_search_direction)
    ;       toggle_unread
    ;       toggle_archive
    ;       toggle_flagged
    ;       set_deleted
    ;       unset_deleted
    ;       mark_spam
    ;       prompt_tag(string)
    ;       toggle_select
    ;       select_all
    ;       unselect_all
    ;       bulk_tag(keep_selection)
    ;       pipe_thread_id
    ;       toggle_show_authors
    ;       quit.

:- type action
    --->    continue            % active
    ;       continue_no_draw    % active
    ;       continue_inactive
    ;       resize
    ;       open_thread_pager(
                thread_id,
                set(tag),           % include tags
                list(message_id),   % unmatched message ids
                obscure_mode_bool
            )
    ;       enter_limit(maybe(string))
    ;       limit_alias_char(char)
    ;       refresh_all(refresh_type)
    ;       start_compose
    ;       start_recall
    ;       start_reply(reply_kind)
    ;       addressbook_add
    ;       prompt_internal_search(search_direction)
    ;       toggle_unread
    ;       toggle_archive
    ;       toggle_flagged
    ;       set_deleted
    ;       unset_deleted
    ;       mark_spam
    ;       prompt_tag(string)
    ;       bulk_tag(keep_selection)
    ;       pipe_thread_id
    ;       quit.

:- type rel_search_direction
    --->    prevailing_dir
    ;       opposite_dir.

:- type keep_selection
    --->    clear_selection
    ;       keep_selection.

:- type try_reply_result
    --->    ok(compose.sent)
    ;       unable_to_choose
    ;       error.

:- type arbitrary_tag_changes
    --->    no
    ;       yes(
                tag_deltas  :: list(tag_delta),
                add_tags    :: set(tag),
                remove_tags :: set(tag)
            ).

:- type refresh_verbosity
    --->    verbose
    ;       quiet.

:- type refresh_type
    --->    manual_refresh
    ;       auto_refresh.

%-----------------------------------------------------------------------------%

open_index(Config, NotmuchConfig, Crypto, Screen, LimitString,
        !.CommonHistory, !IO) :-
    current_timestamp(Time, !IO),
    ( LimitString = "" ->
        SearchTokens = [],
        Threads = []
    ;
        parse_search_string_and_expand(Config, yes(NotmuchConfig), LimitString,
            ParseRes, !IO),
        (
            ParseRes = ok(SearchTokens),
            search_terms_with_progress(Config, Screen, manual_refresh, no,
                SearchTokens, MaybeThreads, !IO)
        ;
            ParseRes = error(Error),
            SearchTokens = [],
            MaybeThreads = no,
            update_message(Screen, set_warning(Error), !IO)
        ),
        (
            MaybeThreads = yes(Threads)
        ;
            MaybeThreads = no,
            Threads = []
        ),
        LimitHistory0 = !.CommonHistory ^ ch_limit_history,
        add_history_nodup(LimitString, LimitHistory0, LimitHistory),
        !CommonHistory ^ ch_limit_history := LimitHistory
    ),
    set.init(SelectedThreadIds),
    setup_index_scrollable(Time, SelectedThreadIds, Threads, Scrollable, !IO),
    SearchTime = Time,
    LastActiveTime = Time,
    NextPollTime = next_poll_time(Config, Time),
    PollCount = 0,
    MaybeSearch = no,
    IndexInfo = index_info(Config, Crypto, Scrollable, LimitString,
        SearchTokens, SearchTime, LastActiveTime, NextPollTime, PollCount,
        MaybeSearch, dir_forward, show_authors, !.CommonHistory),
    index_loop(Screen, redraw, update_activity, IndexInfo, !IO).

:- pred search_new_limit_string(screen::in, string::in, maybe(string)::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

search_new_limit_string(Screen, LimitString, MaybeDesc, !IndexInfo, !IO) :-
    Config = !.IndexInfo ^ i_config,
    current_timestamp(Time, !IO),
    parse_search_string_and_expand(Config, no, LimitString, ParseRes, !IO),
    (
        ParseRes = ok(Tokens),
        search_terms_with_progress(Config, Screen, manual_refresh, MaybeDesc,
            Tokens, MaybeThreads, !IO),
        (
            MaybeThreads = yes(Threads),
            set.init(SelectedThreadIds),
            setup_index_scrollable(Time, SelectedThreadIds, Threads,
                Scrollable, !IO),
            !IndexInfo ^ i_scrollable := Scrollable,
            !IndexInfo ^ i_search_terms := LimitString,
            !IndexInfo ^ i_search_tokens := Tokens,
            !IndexInfo ^ i_search_time := Time,
            !IndexInfo ^ i_next_poll_time := next_poll_time(Config, Time),
            !IndexInfo ^ i_poll_count := 0
        ;
            MaybeThreads = no
        )
    ;
        ParseRes = error(Error),
        update_message(Screen, set_warning(Error), !IO)
    ).

:- pred search_terms_with_progress(prog_config::in, screen::in,
    refresh_type::in, maybe(string)::in, list(token)::in,
    maybe(list(thread))::out, io::di, io::uo) is det.

search_terms_with_progress(Config, Screen, RefreshType, MaybeDesc, Tokens,
        MaybeThreads, !IO) :-
    flush_async_with_progress(Screen, !IO),
    (
        RefreshType = manual_refresh,
        MessagePrefix = "Searching"
    ;
        RefreshType = auto_refresh,
        MessagePrefix = "Auto-refreshing"
    ),
    (
        MaybeDesc = no,
        Message = MessagePrefix ++ "..."
    ;
        MaybeDesc = yes(Desc),
        Message = MessagePrefix ++ " " ++ Desc ++ "..."
    ),
    update_message_immed(Screen, set_info(Message), !IO),
    search_terms_quiet(Config, RefreshType, Tokens, MaybeThreads,
        MessageUpdate, !IO),
    update_message(Screen, MessageUpdate, !IO).

:- pred search_terms_quiet(prog_config::in, refresh_type::in, list(token)::in,
    maybe(list(thread))::out, message_update::out, io::di, io::uo) is det.

search_terms_quiet(Config, RefreshType, Tokens, MaybeThreads, MessageUpdate,
        !IO) :-
    tokens_to_search_terms(Tokens, Terms),
    check_apply_limit(Tokens, ApplyLimit),
    get_default_max_threads(Config, DefaultMaxThreads),
    (
        ApplyLimit = yes,
        DefaultMaxThreads = max_threads(MaxThreads)
    ->
        LimitOption = ["--limit=" ++ from_int(MaxThreads)]
    ;
        LimitOption = []
    ),
    ignore_sigint(yes, !IO),
    run_notmuch(Config,
        ["search", "--format=json", "--exclude=all" | LimitOption]
        ++ ["--", Terms],
        no_suspend_curses,
        parse_search_summary, ResThreads, !IO),
    ignore_sigint(no, !IO),
    (
        ResThreads = ok(Threads),
        MaybeThreads = yes(Threads),
        NumThreads = list.length(Threads),
        (
            ApplyLimit = yes,
            DefaultMaxThreads = max_threads(NumThreads)
        ->
            string.format("Found %d threads (capped). Use ~A to disable cap.",
                [i(NumThreads)], Message0)
        ;
            string.format("Found %d threads.", [i(NumThreads)], Message0)
        ),
        (
            RefreshType = manual_refresh,
            Message = Message0
        ;
            RefreshType = auto_refresh,
            Message = Message0 ++ " [auto-refresh]"
        ),
        MessageUpdate = set_info(Message)
    ;
        ResThreads = error(Error),
        MaybeThreads = no,
        Message = "Error: " ++ Error,
        MessageUpdate = set_warning(Message)
    ).

:- pred setup_index_scrollable(timestamp::in, set(thread_id)::in,
    list(thread)::in, scrollable(index_line)::out, io::di, io::uo) is det.

setup_index_scrollable(Time, SelectedThreadIds, Threads, Scrollable, !IO) :-
    localtime(Time, Nowish, !IO),
    list.foldl2(add_thread(Nowish, SelectedThreadIds), Threads,
        cord.init, LinesCord, !IO),
    Lines = list(LinesCord),
    (
        Lines = [],
        Scrollable = scrollable.init(Lines)
    ;
        Lines = [_ | _],
        Scrollable = scrollable.init_with_cursor(Lines)
    ).

:- pred add_thread(tm::in, set(thread_id)::in, thread::in,
    cord(index_line)::in, cord(index_line)::out, io::di, io::uo) is det.

add_thread(Nowish, SelectedThreadIds, Thread, !Lines, !IO) :-
    thread_to_index_line(Nowish, SelectedThreadIds, Thread, Line, !IO),
    cord_util.snoc(Line, !Lines).

:- pred thread_to_index_line(tm::in, set(thread_id)::in, thread::in,
    index_line::out, io::di, io::uo) is det.

thread_to_index_line(Nowish, SelectedThreadIds, Thread, Line, !IO) :-
    Thread = thread(ThreadId, Timestamp, Authors, Subject, Tags,
        Matched, Total, UnmatchedMessageIds),
    localtime(Timestamp, TM, !IO),
    Shorter = yes,
    make_reldate(Nowish, TM, Shorter, Date),
    get_standard_tags(Tags, StdTags, DisplayTagsWidth),
    ( set.contains(SelectedThreadIds, ThreadId) ->
        Selected = selected
    ;
        Selected = not_selected
    ),
    Line = index_line(ThreadId, Selected, Date, make_presentable(Authors),
        make_presentable(Subject), Tags, StdTags, DisplayTagsWidth,
        Matched, Total, UnmatchedMessageIds).

%-----------------------------------------------------------------------------%

:- type on_entry
    --->    redraw
    ;       no_draw.

:- type maybe_update_activity
    --->    update_activity
    ;       no_update_activity.

:- pred index_loop(screen::in, on_entry::in, maybe_update_activity::in,
    index_info::in, io::di, io::uo) is det.

index_loop(Screen, OnEntry, MaybeUpdateActivity, !.IndexInfo, !IO) :-
    (
        OnEntry = redraw,
        draw_index_view(Screen, !.IndexInfo, !IO),
        update_panels(Screen, !IO)
    ;
        OnEntry = no_draw
    ),

    poll_async_with_progress(Screen, handle_poll_result, !IndexInfo, !IO),

    current_timestamp(Time0, !IO),
    (
        MaybeUpdateActivity = update_activity,
        !IndexInfo ^ i_last_active_time := Time0
    ;
        MaybeUpdateActivity = no_update_activity
    ),
    get_next_input_deadline(!.IndexInfo, MaybeDeadline),
    get_keycode_async_aware(MaybeDeadline, KeyCode, !IO),
    ( KeyCode = timeout_or_error ->
        % XXX can we distinguish timeout from error?
        current_timestamp(Time1, !IO),
        should_auto_refresh(!.IndexInfo, Time1, ShouldAutoRefresh),
        (
            ShouldAutoRefresh = yes,
            Action = refresh_all(auto_refresh)
        ;
            ShouldAutoRefresh = no,
            Action = continue_inactive
        )
    ;
        get_main_rows(Screen, NumMainRows, !IO),
        index_view_input(NumMainRows, KeyCode, MessageUpdate, Action,
            !IndexInfo),
        update_message(Screen, MessageUpdate, !IO)
    ),

    (
        Action = continue,
        maybe_sched_poll(!IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = continue_no_draw,
        maybe_sched_poll(!IndexInfo, !IO),
        index_loop(Screen, no_draw, update_activity, !.IndexInfo, !IO)
    ;
        Action = continue_inactive,
        maybe_sched_poll(!IndexInfo, !IO),
        index_loop(Screen, no_draw, no_update_activity, !.IndexInfo, !IO)
    ;
        Action = resize,
        recreate_screen_for_resize(Screen, !IO),
        recreate_index_view(Screen, !IndexInfo, !IO),
        % The window may be resized inadvertently, so it should probably not
        % prevent auto-refresh.
        index_loop(Screen, redraw, no_update_activity, !.IndexInfo, !IO)
    ;
        Action = open_thread_pager(ThreadId, IncludeTags, UnmatchedMessageIds,
            ObscureMode),
        flush_async_with_progress(Screen, !IO),
        Config = !.IndexInfo ^ i_config,
        Crypto = !.IndexInfo ^ i_crypto,
        Tokens = !.IndexInfo ^ i_search_tokens,
        index_poll_terms(Tokens, IndexPollTerms, !IO),
        CommonHistory0 = !.IndexInfo ^ i_common_history,
        % Use the last search string that was entered in any view, not
        % necessarily the active search string in the index view.
        SearchHistory = CommonHistory0 ^ ch_internal_search_history,
        ( history_latest(SearchHistory, LastSearchString) ->
            MaybeSearch = yes(LastSearchString)
        ;
            MaybeSearch = no
        ),
        open_thread_pager(Config, Crypto, Screen, ThreadId, IncludeTags,
            list_to_set(UnmatchedMessageIds), IndexPollTerms, ObscureMode,
            MaybeSearch, Transition, CommonHistory0, CommonHistory, !IO),
        handle_screen_transition(Screen, Transition, TagUpdates,
            !IndexInfo, !IO),
        effect_thread_pager_changes(TagUpdates, !IndexInfo, !IO),
        !IndexInfo ^ i_common_history := CommonHistory,
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = enter_limit(MaybeInitial),
        Config = !.IndexInfo ^ i_config,
        History0 = !.IndexInfo ^ i_common_history ^ ch_limit_history,
        (
            MaybeInitial = yes(Initial),
            FirstTime = no
        ;
            MaybeInitial = no,
            choose_text_initial(History0, "", Initial),
            FirstTime = yes
        ),
        Completion = complete_limit(Config, search_alias_section),
        text_entry_full(Screen, "Limit to messages matching: ", History0,
            Initial, Completion, FirstTime, Return, !IO),
        (
            Return = yes(LimitString),
            add_history_nodup(LimitString, History0, History),
            !IndexInfo ^ i_common_history ^ ch_limit_history := History,
            search_new_limit_string(Screen, LimitString, no, !IndexInfo, !IO)
        ;
            Return = no,
            update_message(Screen, clear_message, !IO)
        ),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = limit_alias_char(Char),
        LimitString = "~" ++ string.from_char(Char),
        search_new_limit_string(Screen, LimitString, yes(LimitString),
            !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = refresh_all(RefreshType),
        refresh_all(Screen, verbose, RefreshType, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = start_compose,
        flush_async_with_progress(Screen, !IO),
        Config = !.IndexInfo ^ i_config,
        Crypto = !.IndexInfo ^ i_crypto,
        History0 = !.IndexInfo ^ i_common_history,
        start_compose(Config, Crypto, Screen, no, Transition,
            History0, History, !IO),
        !IndexInfo ^ i_common_history := History,
        handle_screen_transition(Screen, Transition, Sent, !IndexInfo, !IO),
        (
            Sent = sent,
            refresh_all(Screen, quiet, auto_refresh, !IndexInfo, !IO)
        ;
            Sent = not_sent
        ),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = start_reply(ReplyKind),
        flush_async_with_progress(Screen, !IO),
        start_reply(Screen, ReplyKind, MaybeRefresh, !IndexInfo, !IO),
        (
            MaybeRefresh = yes(ThreadId),
            refresh_index_line(Screen, ThreadId, !IndexInfo, !IO)
        ;
            MaybeRefresh = no
        ),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = start_recall,
        flush_async_with_progress(Screen, !IO),
        handle_recall(Screen, Sent, !IndexInfo, !IO),
        (
            Sent = sent,
            refresh_all(Screen, quiet, auto_refresh, !IndexInfo, !IO)
        ;
            Sent = not_sent
        ),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = addressbook_add,
        addressbook_add(Screen, !.IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = prompt_internal_search(SearchDir),
        prompt_internal_search(Screen, SearchDir, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = toggle_unread,
        modify_tag_cursor_line(toggle_unread, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = toggle_archive,
        modify_tag_cursor_line(toggle_archive, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = toggle_flagged,
        modify_tag_cursor_line(toggle_flagged, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = set_deleted,
        modify_tag_cursor_line(set_deleted, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = unset_deleted,
        modify_tag_cursor_line(unset_deleted, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = mark_spam,
        modify_tag_cursor_line(mark_spam, Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = prompt_tag(Initial),
        prompt_tag(Screen, Initial, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = bulk_tag(KeepSelection),
        bulk_tag(Screen, Done, !IndexInfo, !IO),
        (
            Done = yes,
            KeepSelection = clear_selection
        ->
            unselect_all(_MessageUpdate, !IndexInfo)
        ;
            true
        ),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = pipe_thread_id,
        pipe_thread_id(Screen, !IndexInfo, !IO),
        index_loop(Screen, redraw, update_activity, !.IndexInfo, !IO)
    ;
        Action = quit,
        flush_async_with_progress(Screen, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred get_next_input_deadline(index_info::in, maybe(timestamp)::out) is det.

get_next_input_deadline(Info, MaybeDeadline) :-
    Config = Info ^ i_config,
    MaybeNextPollTime = Info ^ i_next_poll_time,
    PollCount = Info ^ i_poll_count,
    (
        PollCount > 0,
        get_auto_refresh_inactive_secs(Config, yes(AutoRefreshInactiveSecs))
    ->
        LastActiveTime = Info ^ i_last_active_time,
        AutoRefreshTime = LastActiveTime + float(AutoRefreshInactiveSecs),
        earlier_timestamp(MaybeNextPollTime, AutoRefreshTime, Deadline),
        MaybeDeadline = yes(Deadline)
    ;
        MaybeDeadline = MaybeNextPollTime
    ).

:- pred earlier_timestamp(maybe(timestamp)::in, timestamp::in, timestamp::out)
    is det.

earlier_timestamp(MaybeT0, T1, T) :-
    (
        MaybeT0 = yes(T0),
        T0 =< T1
    ->
        T = T0
    ;
        T = T1
    ).

:- pred should_auto_refresh(index_info::in, timestamp::in, bool::out) is det.

should_auto_refresh(Info, CurrTime, ShouldAutoRefresh) :-
    Config = Info ^ i_config,
    PollCount = Info ^ i_poll_count,
    LastActiveTime = Info ^ i_last_active_time,
    (
        PollCount > 0,
        get_auto_refresh_inactive_secs(Config, yes(AutoRefreshInactiveSecs)),
        AutoRefreshTime = LastActiveTime + float(AutoRefreshInactiveSecs),
        AutoRefreshTime =< CurrTime
    ->
        ShouldAutoRefresh = yes
    ;
        ShouldAutoRefresh = no
    ).

%-----------------------------------------------------------------------------%

:- pred index_view_input(int::in, keycode::in, message_update::out,
    action::out, index_info::in, index_info::out) is det.

index_view_input(NumRows, KeyCode, MessageUpdate, Action, !IndexInfo) :-
    ( key_binding(KeyCode, Binding) ->
        (
            Binding = scroll_down,
            move_cursor(NumRows, 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = scroll_up,
            move_cursor(NumRows, -1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = page_down,
            move_cursor(NumRows, NumRows - 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = page_up,
            move_cursor(NumRows, -NumRows + 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = half_page_down,
            Delta = int.max(15, NumRows / 2),
            move_cursor(NumRows, Delta, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = half_page_up,
            Delta = int.max(15, NumRows / 2),
            move_cursor(NumRows, -Delta, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = home,
            Scrollable0 = !.IndexInfo ^ i_scrollable,
            NumLines = get_num_lines(Scrollable0),
            move_cursor(NumRows, -NumLines, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = end,
            Scrollable0 = !.IndexInfo ^ i_scrollable,
            NumLines = get_num_lines(Scrollable0),
            move_cursor(NumRows, NumLines, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = skip_to_unread,
            skip_to_unread(NumRows, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = open_thread_pager(ObscureMode),
            open_thread_pager(!.IndexInfo, ObscureMode, Action),
            MessageUpdate = clear_message
        ;
            Binding = enter_limit,
            MessageUpdate = no_change,
            Action = enter_limit(no)
        ;
            Binding = enter_limit_tilde,
            MessageUpdate = no_change,
            Action = enter_limit(yes("~"))
        ;
            Binding = limit_alias_char(Char),
            MessageUpdate = no_change,
            Action = limit_alias_char(Char)
        ;
            Binding = refresh_all,
            MessageUpdate = no_change,
            Action = refresh_all(manual_refresh)
        ;
            Binding = start_compose,
            MessageUpdate = no_change,
            Action = start_compose
        ;
            Binding = start_recall,
            MessageUpdate = no_change,
            Action = start_recall
        ;
            Binding = start_reply(ReplyKind),
            MessageUpdate = no_change,
            Action = start_reply(ReplyKind)
        ;
            Binding = addressbook_add,
            MessageUpdate = no_change,
            Action = addressbook_add
        ;
            Binding = prompt_internal_search(SearchDir),
            MessageUpdate = no_change,
            Action = prompt_internal_search(SearchDir)
        ;
            Binding = skip_to_internal_search(RelSearchDir),
            skip_to_internal_search(NumRows, RelSearchDir, MessageUpdate,
                !IndexInfo),
            Action = continue
        ;
            Binding = toggle_unread,
            MessageUpdate = no_change,
            Action = toggle_unread
        ;
            Binding = toggle_archive,
            MessageUpdate = no_change,
            Action = toggle_archive
        ;
            Binding = toggle_flagged,
            MessageUpdate = no_change,
            Action = toggle_flagged
        ;
            Binding = set_deleted,
            MessageUpdate = no_change,
            Action = set_deleted
        ;
            Binding = unset_deleted,
            MessageUpdate = no_change,
            Action = unset_deleted
        ;
            Binding = mark_spam,
            MessageUpdate = no_change,
            Action = mark_spam
        ;
            Binding = prompt_tag(Initial),
            MessageUpdate = no_change,
            Action = prompt_tag(Initial)
        ;
            Binding = toggle_select,
            toggle_select(NumRows, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = select_all,
            select_all(MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = unselect_all,
            unselect_all(MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = bulk_tag(KeepSelection),
            MessageUpdate = no_change,
            Action = bulk_tag(KeepSelection)
        ;
            Binding = pipe_thread_id,
            MessageUpdate = no_change,
            Action = pipe_thread_id
        ;
            Binding = toggle_show_authors,
            toggle_show_authors(MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = quit,
            MessageUpdate = no_change,
            Action = quit
        )
    ;
        ( KeyCode = code(curs.key_resize) ->
            Action = resize
        ;
            % KeyCode = timeout_or_error assumed to be handled earlier.
            Action = continue_no_draw
        ),
        MessageUpdate = no_change
    ).

:- pred key_binding(keycode::in, binding::out) is semidet.

key_binding(char(Char), Binding) :-
    key_binding_char(Char, Binding).
key_binding(code(Code), Binding) :-
    ( Code = curs.key_up ->
        Binding = scroll_up
    ; Code = curs.key_down ->
        Binding = scroll_down
    ; Code = curs.key_home ->
        Binding = home
    ; Code = curs.key_end ->
        Binding = end
    ; Code = curs.key_pageup ->
        Binding = page_up
    ; Code = curs.key_pagedown ->
        Binding = page_down
    ;
        fail
    ).
key_binding(meta(Char), Binding) :-
    ( Char = ('\r') ->
        Binding = open_thread_pager(obscure_unmatched_messages)
    ;
        % Prevent Alt-Backspace, etc.
        is_printable(Char),
        Binding = limit_alias_char(Char)
    ).

:- pred key_binding_char(char::in, binding::out) is semidet.

key_binding_char('=', refresh_all).
key_binding_char('j', scroll_down).
key_binding_char('k', scroll_up).
key_binding_char('g', home).
key_binding_char('G', end).
key_binding_char('[', half_page_up).
key_binding_char(']', half_page_down).
key_binding_char('\t', skip_to_unread).
key_binding_char(',', skip_to_unread).
key_binding_char('\r', open_thread_pager(do_not_obscure)).
key_binding_char('l', enter_limit).
key_binding_char('~', enter_limit_tilde).
key_binding_char('m', start_compose).
key_binding_char('r', start_reply(direct_reply)).
key_binding_char('e', start_reply(group_reply)).
key_binding_char('L', start_reply(list_reply)).
key_binding_char('R', start_recall).
key_binding_char('@', addressbook_add).
key_binding_char('/', prompt_internal_search(dir_forward)).
key_binding_char('?', prompt_internal_search(dir_reverse)).
key_binding_char('n', skip_to_internal_search(prevailing_dir)).
key_binding_char('N', skip_to_internal_search(opposite_dir)).
key_binding_char('U', toggle_unread).
key_binding_char('a', toggle_archive).
key_binding_char('F', toggle_flagged).
key_binding_char('d', set_deleted).
key_binding_char('u', unset_deleted).
key_binding_char('$', mark_spam).
key_binding_char('+', prompt_tag("+")).
key_binding_char('-', prompt_tag("-")).
key_binding_char('t', toggle_select).
key_binding_char('\x01\', select_all). % ^A
key_binding_char('T', unselect_all).
key_binding_char('''', bulk_tag(clear_selection)).
key_binding_char('"', bulk_tag(keep_selection)).
key_binding_char('|', pipe_thread_id).
key_binding_char('z', toggle_show_authors).
key_binding_char('q', quit).

:- pred move_cursor(int::in, int::in, message_update::out,
    index_info::in, index_info::out) is det.

move_cursor(MainRows, Delta, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    move_cursor(MainRows, Delta, HitLimit, Scrollable0, Scrollable),
    !Info ^ i_scrollable := Scrollable,
    (
        HitLimit = no,
        MessageUpdate = clear_message
    ;
        HitLimit = yes,
        ( Delta > 0 ->
            MessageUpdate = set_warning("You are on the last message.")
        ;
            MessageUpdate = set_warning("You are on the first message.")
        )
    ).

:- pred skip_to_unread(int::in, message_update::out,
    index_info::in, index_info::out) is det.

skip_to_unread(NumRows, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor(Scrollable0, Cursor0) ->
        (
            search_forward(is_unread_line, Scrollable0, Cursor0 + 1, Cursor, _)
        ->
            set_cursor_centred(Cursor, NumRows, Scrollable0, Scrollable),
            MessageUpdate = clear_message
        ;
            search_forward(is_unread_line, Scrollable0, 0, Cursor, _),
            Cursor < Cursor0
        ->
            set_cursor_centred(Cursor, NumRows, Scrollable0, Scrollable),
            MessageUpdate = set_info("Search wrapped to top.")
        ;
            Scrollable = Scrollable0,
            MessageUpdate = set_warning("No unread messages.")
        )
    ;
        require.unexpected($module, $pred, "no cursor")
    ),
    !Info ^ i_scrollable := Scrollable.

:- pred is_unread_line(index_line::in) is semidet.

is_unread_line(Line) :-
    Line ^ i_std_tags ^ unread = unread.

:- pred open_thread_pager(index_info::in, obscure_mode_bool::in, action::out)
    is det.

open_thread_pager(Info, ObscureMode, Action) :-
    Scrollable = Info ^ i_scrollable,
    ( get_cursor_line(Scrollable, _, CursorLine) ->
        ThreadId = CursorLine ^ i_id,
        IncludeTags = CursorLine ^ i_tags,
        UnmatchedMessageIds = CursorLine ^ i_unmatched_ids,
        Action = open_thread_pager(ThreadId, IncludeTags, UnmatchedMessageIds,
            ObscureMode)
    ;
        Action = continue
    ).

%-----------------------------------------------------------------------------%

:- pred effect_thread_pager_changes(thread_pager_effects::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

effect_thread_pager_changes(Effects, !Info, !IO) :-
    Effects = thread_pager_effects(TagSet, TagGroups, AddedMessages),
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _, CursorLine0) ->
        set_tags(TagSet, CursorLine0, CursorLine1),
        % Increment the total number of messages.  Too bad we don't know if the
        % matched number should be increased as well.
        Total1 = CursorLine1 ^ i_total,
        CursorLine = CursorLine1 ^ i_total := Total1 + AddedMessages,
        set_cursor_line(CursorLine, Scrollable0, Scrollable),
        !Info ^ i_scrollable := Scrollable,
        Config = !.Info ^ i_config,
        map.foldl(async_tag_messages(Config), TagGroups, !IO)
    ;
        require.unexpected($module, $pred, "cursor not on expected line")
    ).

%-----------------------------------------------------------------------------%

:- pred start_reply(screen::in, reply_kind::in, maybe(thread_id)::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

start_reply(Screen, ReplyKind, MaybeRefresh, !Info, !IO) :-
    Scrollable = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable, _, CursorLine) ->
        ThreadId = CursorLine ^ i_id,
        try_reply(Screen, ThreadId, no, ReplyKind, TryResA, !Info, !IO),
        (
            TryResA = ok(sent),
            MaybeRefresh = yes(ThreadId)
        ;
            TryResA = ok(not_sent),
            MaybeRefresh = no
        ;
            TryResA = unable_to_choose,
            try_reply(Screen, ThreadId, yes, ReplyKind, TryResB, !Info, !IO),
            (
                TryResB = ok(sent),
                MaybeRefresh = yes(ThreadId)
            ;
                TryResB = ok(not_sent),
                MaybeRefresh = no
            ;
                TryResB = unable_to_choose,
                Msg = "Unable to choose message to reply to.",
                update_message(Screen, set_warning(Msg), !IO),
                MaybeRefresh = no
            ;
                TryResB = error,
                MaybeRefresh = no
            )
        ;
            TryResA = error,
            MaybeRefresh = no
        )
    ;
        update_message(Screen, set_warning("No thread."), !IO),
        MaybeRefresh = no
    ).

:- pred try_reply(screen::in, thread_id::in, bool::in, reply_kind::in,
    try_reply_result::out, index_info::in, index_info::out, io::di, io::uo)
    is det.

try_reply(Screen, ThreadId, RequireUnread, ReplyKind, Res, !Info, !IO) :-
    (
        RequireUnread = yes,
        Args0 = ["tag:unread"]
    ;
        RequireUnread = no,
        Args0 = []
    ),
    Args = [
        "search", "--format=json", "--output=messages", "--exclude=all",
        "--", thread_id_to_search_term(ThreadId),
        "-tag:sent",
        "-tag:replied",
        "-tag:deleted",
        "-tag:draft"
        | Args0
    ],
    Config = !.Info ^ i_config,
    Crypto = !.Info ^ i_crypto,
    run_notmuch(Config, Args, no_suspend_curses,
        parse_search_messages, ListRes, !IO),
    (
        ListRes = ok(MessageIds),
        ( MessageIds = [MessageId] ->
            History0 = !.Info ^ i_common_history,
            start_reply_to_message_id(Config, Crypto, Screen, MessageId,
                ReplyKind, Transition, History0, History, !IO),
            !Info ^ i_common_history := History,
            handle_screen_transition(Screen, Transition, Sent, !Info, !IO),
            Res = ok(Sent)
        ;
            Res = unable_to_choose
        )
    ;
        ListRes = error(Error),
        update_message(Screen, set_warning(Error), !IO),
        Res = error
    ).

%-----------------------------------------------------------------------------%

:- pred handle_recall(screen::in, sent::out, index_info::in, index_info::out,
    io::di, io::uo) is det.

handle_recall(Screen, Sent, !IndexInfo, !IO) :-
    Config = !.IndexInfo ^ i_config,
    Crypto = !.IndexInfo ^ i_crypto,
    History0 = !.IndexInfo ^ i_common_history,
    select_recall(Config, Screen, no, TransitionA, !IO),
    handle_screen_transition(Screen, TransitionA, MaybeSelected,
        !IndexInfo, !IO),
    (
        MaybeSelected = yes(Message),
        (
            Message = message(_, _, _, CurrTags, _, _),
            PartVisibilityMap = map.init,
            continue_from_message(Config, Crypto, Screen, postponed_message,
                Message, PartVisibilityMap, CurrTags, TransitionB,
                History0, History, !IO),
            !IndexInfo ^ i_common_history := History,
            handle_screen_transition(Screen, TransitionB, Sent, !IndexInfo,
                !IO)
        ;
            Message = excluded_message(_, _, _, _, _),
            Sent = not_sent
        )
    ;
        MaybeSelected = no,
        Sent = not_sent
    ).

%-----------------------------------------------------------------------------%

:- pred addressbook_add(screen::in, index_info::in, io::di, io::uo) is det.

addressbook_add(Screen, Info, !IO) :-
    Config = Info ^ i_config,
    Scrollable = Info ^ i_scrollable,
    ( get_cursor_line(Scrollable, _Cursor, Line) ->
        ThreadId = Line ^ i_id,
        Args = [
            "search", "--format=json", "--output=messages", "--exclude=all",
            "--", thread_id_to_search_term(ThreadId)
        ],
        run_notmuch(Config, Args, no_suspend_curses,
            parse_search_messages, ListRes, !IO),
        ( ListRes = ok([MessageId | _]) ->
            run_notmuch(Config,
                [
                    "show", "--format=json", "--part=0", "--",
                    message_id_to_search_term(MessageId)
                ],
                no_suspend_curses,
                parse_message, MessageRes, !IO),
            (
                MessageRes = ok(Message),
                From = Message ^ m_headers ^ h_from
            ->
                Address0 = header_value_string(From)
            ;
                Address0 = ""
            )
        ;
            Address0 = ""
        )
    ;
        Address0 = ""
    ),
    prompt_addressbook_add(Config, Screen, Address0, !IO).

%-----------------------------------------------------------------------------%

:- pred prompt_internal_search(screen::in, search_direction::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

prompt_internal_search(Screen, SearchDir, !Info, !IO) :-
    History0 = !.Info ^ i_common_history ^ ch_internal_search_history,
    text_entry(Screen, "Search for: ", History0, complete_none, Return, !IO),
    (
        Return = yes(Search),
        ( Search = "" ->
            !Info ^ i_internal_search := no
        ;
            add_history_nodup(Search, History0, History),
            !Info ^ i_internal_search := yes(Search),
            !Info ^ i_internal_search_dir := SearchDir,
            !Info ^ i_common_history ^ ch_internal_search_history := History,
            get_main_rows(Screen, NumRows, !IO),
            skip_to_internal_search(NumRows, prevailing_dir, MessageUpdate,
                !Info),
            update_message(Screen, MessageUpdate, !IO)
        )
    ;
        Return = no
    ).

:- pred skip_to_internal_search(int::in, rel_search_direction::in,
    message_update::out, index_info::in, index_info::out) is det.

skip_to_internal_search(NumRows, RelSearchDir, MessageUpdate, !Info) :-
    MaybeSearch = !.Info ^ i_internal_search,
    (
        MaybeSearch = yes(Search),
        SearchDir0 = !.Info ^ i_internal_search_dir,
        (
            RelSearchDir = prevailing_dir,
            SearchDir = SearchDir0
        ;
            RelSearchDir = opposite_dir,
            SearchDir = opposite_search_direction(SearchDir0)
        ),
        Scrollable0 = !.Info ^ i_scrollable,
        ( get_cursor(Scrollable0, Cursor0) ->
            (
                SearchDir = dir_forward,
                Start = Cursor0 + 1,
                WrapStart = 0,
                WrapMessage = "Search wrapped to top."
            ;
                SearchDir = dir_reverse,
                Start = Cursor0,
                WrapStart = get_num_lines(Scrollable0),
                WrapMessage = "Search wrapped to bottom."
            ),
            skip_to_internal_search_2(Search, SearchDir, Start, WrapStart,
                WrapMessage, NumRows, MessageUpdate, Scrollable0, Scrollable),
            !Info ^ i_scrollable := Scrollable
        ;
            MessageUpdate = no_change
        )
    ;
        MaybeSearch = no,
        MessageUpdate = set_warning("No search string.")
    ).

:- pred skip_to_internal_search_2(string::in, search_direction::in,
    int::in, int::in, string::in, int::in, message_update::out,
    scrollable(index_line)::in, scrollable(index_line)::out) is det.

skip_to_internal_search_2(Search, SearchDir, Start, WrapStart, WrapMessage,
        NumRows, MessageUpdate, Scrollable0, Scrollable) :-
    (
        scrollable.search(line_matches_internal_search(Search),
            SearchDir, Scrollable0, Start, Cursor)
    ->
        set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
        MessageUpdate = clear_message
    ;
        scrollable.search(line_matches_internal_search(Search),
            SearchDir, Scrollable0, WrapStart, Cursor)
    ->
        set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
        MessageUpdate = set_info(WrapMessage)
    ;
        Scrollable = Scrollable0,
        MessageUpdate = set_warning("Not found.")
    ).

:- pred line_matches_internal_search(string::in, index_line::in) is semidet.

line_matches_internal_search(Search, Line) :-
    Line = index_line(_Id, _Selected, _Date, presentable_string(Authors),
        presentable_string(Subject), Tags, _StdTags, _TagsWidth,
        _Matched, _Total, _UnmatchedMessageIds),
    (
        strcase_str(Authors, Search)
    ;
        strcase_str(Subject, Search)
    ;
        set.member(tag(TagName), Tags),
        strcase_str(TagName, Search)
    ).

%-----------------------------------------------------------------------------%

:- pred modify_tag_cursor_line(pred(index_line, index_line, list(tag_delta)),
    screen, index_info, index_info, io, io).
:- mode modify_tag_cursor_line(in(pred(in, out, out) is det),
    in, in, out, di, uo) is det.
:- mode modify_tag_cursor_line(in(pred(in, out, out) is semidet),
    in, in, out, di, uo) is det.

modify_tag_cursor_line(ModifyPred, Screen, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        ThreadId = CursorLine0 ^ i_id,
        ( ModifyPred(CursorLine0, CursorLine, TagDeltas) ->
            Config = !.Info ^ i_config,
            async_tag_threads(Config, TagDeltas, [ThreadId], !IO),
            set_cursor_line(CursorLine, Scrollable0, Scrollable),
            !Info ^ i_scrollable := Scrollable,
            get_main_rows(Screen, NumRows, !IO),
            move_cursor(NumRows, 1, _MessageUpdate, !Info),
            MessageUpdate = clear_message
        ;
            MessageUpdate = set_warning("Refusing to tag multiple messages.")
        )
    ;
        MessageUpdate = set_warning("No thread.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred toggle_unread(index_line::in, index_line::out, list(tag_delta)::out)
    is det.

toggle_unread(Line0, Line, [TagDelta]) :-
    Unread0 = Line0 ^ i_std_tags ^ unread,
    (
        Unread0 = unread,
        remove_tag(tag("unread"), Line0, Line),
        TagDelta = tag_delta("-unread")
    ;
        Unread0 = read,
        add_tag(tag("unread"), Line0, Line),
        TagDelta = tag_delta("+unread")
    ).

:- pred toggle_archive(index_line::in, index_line::out, list(tag_delta)::out)
    is det.

toggle_archive(Line0, Line, TagDeltas) :-
    TagSet0 = Line0 ^ i_tags,
    (
        ( set.contains(TagSet0, tag("inbox"))
        ; set.contains(TagSet0, tag("unread"))
        )
    ->
        set.delete_list([tag("inbox"), tag("unread")], TagSet0, TagSet),
        TagDeltas = [tag_delta("-inbox"), tag_delta("-unread")]
    ;
        set.insert(tag("inbox"), TagSet0, TagSet),
        TagDeltas = [tag_delta("+inbox")]
    ),
    set_tags(TagSet, Line0, Line).

:- pred toggle_flagged(index_line::in, index_line::out, list(tag_delta)::out)
    is semidet.

toggle_flagged(Line0, Line, [TagDelta]) :-
    Flagged0 = Line0 ^ i_std_tags ^ flagged,
    (
        Flagged0 = flagged,
        TagDelta = tag_delta("-flagged"),
        remove_tag(tag("flagged"), Line0, Line)
    ;
        Flagged0 = unflagged,
        TagDelta = tag_delta("+flagged"),
        % Refuse to flag multiple messages.
        NumMessages = Line0 ^ i_total,
        NumMessages = 1,
        add_tag(tag("flagged"), Line0, Line)
    ).

:- pred set_deleted(index_line::in, index_line::out, list(tag_delta)::out)
    is det.

set_deleted(!Line, [TagDelta]) :-
    add_tag(tag("deleted"), !Line),
    TagDelta = tag_delta("+deleted").

:- pred unset_deleted(index_line::in, index_line::out, list(tag_delta)::out)
    is det.

unset_deleted(!Line, [TagDelta]) :-
    remove_tag(tag("deleted"), !Line),
    TagDelta = tag_delta("-deleted").

:- pred mark_spam(index_line::in, index_line::out, list(tag_delta)::out)
    is det.

mark_spam(!Line, TagDeltas) :-
    add_tag(tag("spam"), !Line),
    remove_tag(tag("unread"), !Line),
    TagDeltas = [tag_delta("+spam"), tag_delta("-unread")].

:- pred add_tag(tag::in, index_line::in, index_line::out) is det.

add_tag(Tag, !Line) :-
    TagSet0 = !.Line ^ i_tags,
    set.insert(Tag, TagSet0, TagSet),
    set_tags(TagSet, !Line).

:- pred remove_tag(tag::in, index_line::in, index_line::out) is det.

remove_tag(Tag, !Line) :-
    TagSet0 = !.Line ^ i_tags,
    ( set.remove(Tag, TagSet0, TagSet) ->
        set_tags(TagSet, !Line)
    ;
        true
    ).

:- pred set_tags(set(tag)::in, index_line::in, index_line::out) is det.

set_tags(TagSet, !Line) :-
    get_standard_tags(TagSet, StdTags, NonstdTagsWidth),
    !Line ^ i_tags := TagSet,
    !Line ^ i_std_tags := StdTags,
    !Line ^ i_nonstd_tags_width := NonstdTagsWidth.

%-----------------------------------------------------------------------------%

:- pred prompt_tag(screen::in, string::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

prompt_tag(Screen, Initial, !Info, !IO) :-
    Config = !.Info ^ i_config,
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        gather_initial_tags(CursorLine0, no, _AndTagSet, set.init, TagSet),
        set.map(tag_to_string, TagSet, BothStringSet),
        Completion = complete_tags_smart(Config, BothStringSet, BothStringSet),
        prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
            !Info, !IO),
        (
            TagChanges = yes(TagDeltas, AddTags, RemoveTags),
            apply_tag_changes(CursorLine0, TagDeltas, AddTags, RemoveTags,
                !Info, !IO)
        ;
            TagChanges = no
        )
    ;
        update_message(Screen, set_warning("No thread."), !IO)
    ).

:- pred prompt_arbitrary_tag_changes(screen::in, string::in,
    completion_type::in, arbitrary_tag_changes::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
        !Info, !IO) :-
    History0 = !.Info ^ i_common_history ^ ch_tag_history,
    FirstTime = no,
    text_entry_full(Screen, "Change tags: ", History0, Initial, Completion,
        FirstTime, Return, !IO),
    (
        Return = yes(String),
        (
            Words = string.words(String),
            Words = [_ | _]
        ->
            add_history_nodup(String, History0, History),
            !Info ^ i_common_history ^ ch_tag_history := History,
            ( validate_tag_deltas(Words, TagDeltas, AddTags, RemoveTags) ->
                TagChanges = yes(TagDeltas, AddTags, RemoveTags)
            ;
                TagChanges = no,
                update_message(Screen,
                    set_warning("Tags must be of the form +tag or -tag."), !IO)
            )
        ;
            TagChanges = no
        )
    ;
        Return = no,
        TagChanges = no
    ).

:- pred apply_tag_changes(index_line::in, list(tag_delta)::in,
    set(tag)::in, set(tag)::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

apply_tag_changes(CursorLine0, TagDeltas, AddTags, RemoveTags, !Info, !IO) :-
    Config = !.Info ^ i_config,
    ThreadId = CursorLine0 ^ i_id,
    async_tag_threads(Config, TagDeltas, [ThreadId], !IO),
    % Notmuch performs tag removals before addition.
    TagSet0 = CursorLine0 ^ i_tags,
    set.difference(TagSet0, RemoveTags, TagSet1),
    set.union(TagSet1, AddTags, TagSet),
    set_tags(TagSet, CursorLine0, CursorLine),

    Scrollable0 = !.Info ^ i_scrollable,
    set_cursor_line(CursorLine, Scrollable0, Scrollable),
    !Info ^ i_scrollable := Scrollable.

%-----------------------------------------------------------------------------%

:- pred toggle_select(int::in, message_update::out,
    index_info::in, index_info::out) is det.

toggle_select(NumRows, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        Selected0 = CursorLine0 ^ i_selected,
        (
            Selected0 = selected,
            Selected = not_selected
        ;
            Selected0 = not_selected,
            Selected = selected
        ),
        CursorLine = CursorLine0 ^ i_selected := Selected,
        set_cursor_line(CursorLine, Scrollable0, Scrollable),
        !Info ^ i_scrollable := Scrollable,
        move_cursor(NumRows, 1, MessageUpdate, !Info)
    ;
        MessageUpdate = set_warning("No thread.")
    ).

:- pred select_all(message_update::out, index_info::in, index_info::out)
    is det.

select_all(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    map_lines(select_line, Scrollable0, Scrollable),
    !Info ^ i_scrollable := Scrollable,
    MessageUpdate = set_info("Selected all threads.").

:- pred select_line(index_line::in, index_line::out) is det.

select_line(!Line) :-
    !Line ^ i_selected := selected.

:- pred unselect_all(message_update::out, index_info::in, index_info::out)
    is det.

unselect_all(MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    map_lines(unselect_line, Scrollable0, Scrollable),
    !Info ^ i_scrollable := Scrollable,
    MessageUpdate = set_info("Unselected all threads.").

:- pred unselect_line(index_line::in, index_line::out) is det.

unselect_line(!Line) :-
    !Line ^ i_selected := not_selected.

%-----------------------------------------------------------------------------%

:- pred bulk_tag(screen::in, bool::out, index_info::in, index_info::out,
    io::di, io::uo) is det.

bulk_tag(Screen, Done, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    ( any_selected_line(Lines0) ->
        Prompt = "Bulk: (d)elete, (u)ndelete, (U) unread, (') read, " ++
            "($) spam, (+/-) tags",
        get_keycode_blocking_handle_resize(Screen, set_prompt(Prompt), KeyCode,
            !Info, !IO),
        ( KeyCode = char('-') ->
            Config = !.Info ^ i_config,
            init_bulk_tag_completion(Config, Lines0, Completion),
            bulk_arbitrary_tag_changes(Screen, "-", Completion, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('+') ->
            Config = !.Info ^ i_config,
            init_bulk_tag_completion(Config, Lines0, Completion),
            bulk_arbitrary_tag_changes(Screen, "+", Completion, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('d') ->
            TagDeltas = [tag_delta("+deleted")],
            AddTags = set.make_singleton_set(tag("deleted")),
            RemoveTags = set.init,
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('u') ->
            TagDeltas = [tag_delta("-deleted")],
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("deleted")),
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('U') ->
            bulk_toggle_unread(MessageUpdate, Done, !Info, !IO)
        ; KeyCode = char('''') ->
            TagDeltas = [tag_delta("-unread")],
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("unread")),
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO),
            Done = yes
        ; KeyCode = char('$') ->
            TagDeltas = [tag_delta("+spam"), tag_delta("-unread")],
            AddTags = set.make_singleton_set(tag("spam")),
            RemoveTags = set.make_singleton_set(tag("unread")),
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO),
            Done = yes
        ;
            MessageUpdate = set_info("No changes."),
            Done = no
        )
    ;
        MessageUpdate = set_warning("No threads selected."),
        Done = no
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred any_selected_line(list(index_line)::in) is semidet.

any_selected_line(Lines) :-
    list.member(Line, Lines),
    Line ^ i_selected = selected.

:- pred init_bulk_tag_completion(prog_config::in, list(index_line)::in,
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

:- pred gather_bulk_initial_tags(index_line::in,
    maybe(set(tag))::in, maybe(set(tag))::out, set(tag)::in, set(tag)::out)
    is det.

gather_bulk_initial_tags(Line, MaybeAndTagSet0, MaybeAndTagSet, !OrTagSet) :-
    Selected = Line ^ i_selected,
    (
        Selected = selected,
        gather_initial_tags(Line, MaybeAndTagSet0, AndTagSet, !OrTagSet),
        MaybeAndTagSet = yes(AndTagSet)
    ;
        Selected = not_selected,
        MaybeAndTagSet = MaybeAndTagSet0
    ).

:- pred gather_initial_tags(index_line::in, maybe(set(tag))::in, set(tag)::out,
    set(tag)::in, set(tag)::out) is det.

gather_initial_tags(Line, MaybeAndTagSet0, AndTagSet, !OrTagSet) :-
    TagSet = Line ^ i_tags,
    (
        MaybeAndTagSet0 = no,
        AndTagSet = TagSet
    ;
        MaybeAndTagSet0 = yes(AndTagSet0),
        set.intersect(TagSet, AndTagSet0, AndTagSet)
    ),
    set.union(TagSet, !OrTagSet).

:- pred bulk_arbitrary_tag_changes(screen::in, string::in, completion_type::in,
    message_update::out, index_info::in, index_info::out, io::di, io::uo)
    is det.

bulk_arbitrary_tag_changes(Screen, Initial, Completion, MessageUpdate,
        !Info, !IO) :-
    prompt_arbitrary_tag_changes(Screen, Initial, Completion, TagChanges,
        !Info, !IO),
    (
        TagChanges = yes(TagDeltas, AddTags, RemoveTags),
        bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
            !Info, !IO)
    ;
        TagChanges = no,
        MessageUpdate = clear_message
    ).

:- pred bulk_tag_changes(list(tag_delta)::in, set(tag)::in, set(tag)::in,
    message_update::out, index_info::in, index_info::out, io::di, io::uo)
    is det.

bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    list.map_foldl(
        update_selected_line_for_tag_changes(AddTags, RemoveTags),
        Lines0, Lines, [], SelectedThreadIds),
    (
        SelectedThreadIds = [_ | _],
        Config = !.Info ^ i_config,
        async_tag_threads(Config, TagDeltas, SelectedThreadIds, !IO),
        set_lines_list(Lines, Scrollable0, Scrollable),
        !Info ^ i_scrollable := Scrollable,
        list.length(SelectedThreadIds, NumThreads),
        string.format("Modified tags in %d threads.", [i(NumThreads)],
            Message),
        MessageUpdate = set_info(Message)
    ;
        SelectedThreadIds = [],
        MessageUpdate = set_info("No changes.")
    ).

:- pred update_selected_line_for_tag_changes(set(tag)::in, set(tag)::in,
    index_line::in, index_line::out, list(thread_id)::in, list(thread_id)::out)
    is det.

update_selected_line_for_tag_changes(AddTags, RemoveTags, Line0, Line,
        !ThreadIds) :-
    ThreadId = Line0 ^ i_id,
    TagSet0 = Line0 ^ i_tags,
    Selected = Line0 ^ i_selected,
    (
        Selected = selected,
        % Notmuch performs tag removals before addition.
        set.difference(TagSet0, RemoveTags, TagSet1),
        set.union(TagSet1, AddTags, TagSet),
        TagSet \= TagSet0
    ->
        set_tags(TagSet, Line0, Line),
        list.cons(ThreadId, !ThreadIds)
    ;
        Line = Line0
    ).

:- pred bulk_toggle_unread(message_update::out, bool::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

bulk_toggle_unread(MessageUpdate, Done, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    ( common_unread_state(Lines0, no, yes(CommonUnreadState)) ->
        (
            CommonUnreadState = read,
            TagDelta = tag_delta("+unread"),
            AddTags = set.make_singleton_set(tag("unread")),
            RemoveTags = set.init
        ;
            CommonUnreadState = unread,
            TagDelta = tag_delta("-unread"),
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("unread"))
        ),
        bulk_tag_changes([TagDelta], AddTags, RemoveTags, MessageUpdate,
            !Info, !IO),
        Done = yes
    ;
        Message = "Selected threads differ in unread state.",
        MessageUpdate = set_info(Message),
        Done = no
    ).

:- pred common_unread_state(list(index_line)::in,
    maybe(unread)::in, maybe(unread)::out) is semidet.

common_unread_state([], State, State).
common_unread_state([H | T], State0, State) :-
    Selected = H ^ i_selected,
    (
        Selected = selected,
        State0 = no,
        State1 = yes(H ^ i_std_tags ^ unread),
        common_unread_state(T, State1, State)
    ;
        Selected = selected,
        State0 = yes(H ^ i_std_tags ^ unread),
        common_unread_state(T, State0, State)
    ;
        Selected = not_selected,
        common_unread_state(T, State0, State)
    ).

%-----------------------------------------------------------------------------%

:- pred async_tag_threads(prog_config::in, list(tag_delta)::in,
    list(thread_id)::in, io::di, io::uo) is det.

async_tag_threads(Config, TagDeltas, ThreadIds, !IO) :-
    get_notmuch_command(Config, Notmuch),
    TagDeltaStrings = list.map(tag_delta_to_string, TagDeltas),
    SearchTerms = list.map(thread_id_to_search_term, ThreadIds),
    Args = list.condense([
        ["tag"], TagDeltaStrings, ["--"], SearchTerms
    ]),
    Op = async_shell_command(Notmuch, Args, async_tag_attempts),
    push_async(Op, !IO).

:- pred async_tag_messages(prog_config::in, set(tag_delta)::in,
    list(message_id)::in, io::di, io::uo) is det.

async_tag_messages(Config, TagDeltaSet, MessageIds, !IO) :-
    get_notmuch_command(Config, Notmuch),
    set.to_sorted_list(TagDeltaSet, TagDeltas),
    TagDeltaStrings = list.map(tag_delta_to_string, TagDeltas),
    SearchTerms = list.map(message_id_to_search_term, MessageIds),
    Args = list.condense([
        ["tag"], TagDeltaStrings, ["--"], SearchTerms
    ]),
    Op = async_shell_command(Notmuch, Args, async_tag_attempts),
    push_async(Op, !IO).

:- func async_tag_attempts = int.

async_tag_attempts = 3.

%-----------------------------------------------------------------------------%

:- pred pipe_thread_id(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

pipe_thread_id(Screen, !Info, !IO) :-
    get_selected_or_current_thread_ids(!.Info, HaveSelectedThreads, ThreadIds),
    (
        HaveSelectedThreads = no,
        PromptCommand = "Pipe current thread ID: "
    ;
        HaveSelectedThreads = yes,
        PromptCommand = "Pipe selected thread IDs: "
    ),
    (
        ThreadIds = [],
        MessageUpdate = set_warning("No thread.")
    ;
        ThreadIds = [_ | _],
        IdStrings = map(thread_id_to_search_term, ThreadIds),
        pipe_thread_id_2(Screen, PromptCommand, IdStrings, MessageUpdate,
            !Info, !IO)
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred pipe_thread_id_2(screen::in, string::in, list(string)::in,
    message_update::out, index_info::in, index_info::out, io::di, io::uo)
    is det.

pipe_thread_id_2(Screen, PromptCommand, Strings, MessageUpdate, !Info, !IO) :-
    History0 = !.Info ^ i_common_history ^ ch_pipe_id_history,
    prompt_and_pipe_to_command(Screen, PromptCommand, Strings, MessageUpdate,
        History0, History, !IO),
    !Info ^ i_common_history ^ ch_pipe_id_history := History.

:- pred get_selected_or_current_thread_ids(index_info::in, bool::out,
    list(thread_id)::out) is det.

get_selected_or_current_thread_ids(Info, Selected, ThreadIds) :-
    Scrollable = Info ^ i_scrollable,
    Lines = get_lines_list(Scrollable),
    (
        list.filter_map(selected_line_thread_id, Lines, SelectedThreadIds),
        SelectedThreadIds \= []
    ->
        Selected = yes,
        ThreadIds = SelectedThreadIds
    ;
        Selected = no,
        ( get_cursor_line(Scrollable, _Cursor, CursorLine) ->
            ThreadId = CursorLine ^ i_id,
            ThreadIds = [ThreadId]
        ;
            ThreadIds = []
        )
    ).

:- pred selected_line_thread_id(index_line::in, thread_id::out) is semidet.

selected_line_thread_id(Line, ThreadId) :-
    Line ^ i_selected = selected,
    ThreadId = Line ^ i_id.

%-----------------------------------------------------------------------------%

:- pred toggle_show_authors(message_update::out,
    index_info::in, index_info::out) is det.

toggle_show_authors(MessageUpdate, !Info) :-
    ShowAuthors0 = !.Info ^ i_show_authors,
    (
        ShowAuthors0 = show_authors,
        MessageUpdate = set_info("Hiding authors."),
        ShowAuthors = hide_authors
    ;
        ShowAuthors0 = hide_authors,
        MessageUpdate = set_info("Showing authors."),
        ShowAuthors = show_authors
    ),
    !Info ^ i_show_authors := ShowAuthors.

%-----------------------------------------------------------------------------%

:- pred refresh_all(screen::in, refresh_verbosity::in, refresh_type::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

refresh_all(Screen, Verbose, RefreshType, !Info, !IO) :-
    flush_async_with_progress(Screen, !IO),
    current_timestamp(Time, !IO),
    % The user might have changed search aliases and is trying to force a
    % refresh, so expand the search terms from the beginning.
    Config = !.Info ^ i_config,
    Terms = !.Info ^ i_search_terms,
    parse_search_string_and_expand(Config, no, Terms, ParseRes, !IO),
    (
        ParseRes = ok(Tokens),
        (
            Verbose = verbose,
            search_terms_with_progress(Config, Screen, RefreshType, no, Tokens,
                MaybeThreads, !IO)
        ;
            Verbose = quiet,
            search_terms_quiet(Config, RefreshType, Tokens, MaybeThreads,
                _MessageUpdate, !IO)
        ),
        (
            MaybeThreads = yes(Threads),
            refresh_all_2(Screen, Time, Tokens, Threads, !Info, !IO)
        ;
            MaybeThreads = no
        )
    ;
        ParseRes = error(Error),
        update_message(Screen, set_warning(Error), !IO)
    ).

:- pred refresh_all_2(screen::in, timestamp::in, list(token)::in,
    list(thread)::in, index_info::in, index_info::out, io::di, io::uo) is det.

refresh_all_2(Screen, Time, Tokens, Threads, !Info, !IO) :-
    some [!Scrollable] (
        Scrollable0 = !.Info ^ i_scrollable,
        Top0 = get_top(Scrollable0),
        get_selected_thread_ids(Scrollable0, SelectedThreadIds),
        setup_index_scrollable(Time, SelectedThreadIds, Threads, !:Scrollable,
            !IO),
        ( Top0 < get_num_lines(!.Scrollable) ->
            set_top(Top0, !Scrollable)
        ;
            true
        ),
        ( get_cursor_line(Scrollable0, _Cursor0, Line0) ->
            ThreadId0 = Line0 ^ i_id,
            (
                search_forward(line_matches_thread_id(ThreadId0),
                    !.Scrollable, 0, Cursor, _)
            ->
                get_main_rows(Screen, NumRows, !IO),
                set_cursor_visible(Cursor, NumRows, !Scrollable)
            ;
                true
            )
        ;
            true
        ),
        !Info ^ i_scrollable := !.Scrollable,
        !Info ^ i_search_tokens := Tokens,
        !Info ^ i_search_time := Time,
        !Info ^ i_next_poll_time := next_poll_time(!.Info ^ i_config, Time),
        !Info ^ i_poll_count := 0
    ).

:- pred get_selected_thread_ids(scrollable(index_line)::in,
    set(thread_id)::out) is det.

get_selected_thread_ids(Scrollable, SelectedThreadIds) :-
    Lines = get_lines_list(Scrollable),
    list.foldl(add_thread_id_if_selected, Lines, [], SelectedThreadIdsList),
    SelectedThreadIds = set.from_list(SelectedThreadIdsList).

:- pred add_thread_id_if_selected(index_line::in,
    list(thread_id)::in, list(thread_id)::out) is det.

add_thread_id_if_selected(Line, !SelectedThreadIds) :-
    MaybeSelected = Line ^ i_selected,
    (
        MaybeSelected = not_selected
    ;
        MaybeSelected = selected,
        ThreadId = Line ^ i_id,
        !:SelectedThreadIds = [ThreadId | !.SelectedThreadIds]
    ).

:- pred line_matches_thread_id(thread_id::in, index_line::in) is semidet.

line_matches_thread_id(ThreadId, Line) :-
    Line ^ i_id = ThreadId.

%-----------------------------------------------------------------------------%

:- pred refresh_index_line(screen::in, thread_id::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

refresh_index_line(Screen, ThreadId, !IndexInfo, !IO) :-
    Config = !.IndexInfo ^ i_config,
    Term = thread_id_to_search_term(ThreadId),
    run_notmuch(Config,
        ["search", "--format=json", "--exclude=all", "--", Term],
        no_suspend_curses,
        parse_search_summary, Result, !IO),
    (
        Result = ok([Thread]),
        current_timestamp(Time, !IO),
        localtime(Time, Nowish, !IO),
        replace_index_cursor_line(Nowish, Thread, !IndexInfo, !IO)
    ;
        ( Result = ok([])
        ; Result = ok([_, _ | _])
        ; Result = error(_)
        ),
        update_message(Screen, set_warning("Error refreshing index."), !IO)
    ).

:- pred replace_index_cursor_line(tm::in, thread::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

replace_index_cursor_line(Nowish, Thread, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, LineNum, Line0) ->
        MaybeSelected = Line0 ^ i_selected,
        (
            MaybeSelected = selected,
            ThreadId = Line0 ^ i_id,
            set.singleton_set(ThreadId, SelectedThreadIds)
        ;
            MaybeSelected = not_selected,
            set.init(SelectedThreadIds)
        ),
        thread_to_index_line(Nowish, SelectedThreadIds, Thread, Line, !IO),
        set_line(LineNum, Line, Scrollable0, Scrollable),
        !Info ^ i_scrollable := Scrollable
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred recreate_index_view(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

recreate_index_view(Screen, !IndexInfo, !IO) :-
    % Keep cursor visible.
    Scrollable0 = !.IndexInfo ^ i_scrollable,
    ( get_cursor(Scrollable0, Cursor) ->
        get_main_rows(Screen, NumRows, !IO),
        set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
        !IndexInfo ^ i_scrollable := Scrollable
    ;
        true
    ).

:- pred handle_screen_transition(screen::in, screen_transition(T)::in, T::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

handle_screen_transition(Screen, Transition, T, !Info, !IO) :-
    Transition = screen_transition(T, MessageUpdate),
    recreate_index_view(Screen, !Info, !IO),    % in case of resize
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_sched_poll(index_info::in, index_info::out, io::di, io::uo)
    is det.

maybe_sched_poll(!Info, !IO) :-
    MaybeNextPollTime = !.Info ^ i_next_poll_time,
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

:- pred sched_poll(timestamp::in, index_info::in, index_info::out, io::di, io::uo)
    is det.

sched_poll(Time, !Info, !IO) :-
    Config = !.Info ^ i_config,
    get_notmuch_command(Config, Notmuch),
    Tokens = !.Info ^ i_search_tokens,
    SearchTime = !.Info ^ i_search_time,
    index_poll_terms(Tokens, IndexPollTerms, !IO),
    % Could use notmuch count --batch
    Args =
        ["count", "--"] ++
        IndexPollTerms ++
        ["AND", timestamp_to_int_string(SearchTime) ++ ".."],
    Op = async_lowprio_command(Notmuch, Args, no),
    push_lowprio_async(Op, _Pushed, !IO),
    !Info ^ i_next_poll_time := next_poll_time(Config, Time).

:- pred index_poll_terms(list(token), list(string), io, io).
:- mode index_poll_terms(in, out, di, uo) is det.

index_poll_terms(Tokens, IndexPollTerms, !IO) :-
    tokens_to_search_terms(Tokens, SearchTerms),
    IndexPollTerms = ["(", SearchTerms, ")", "AND", "tag:unread"].

:- pred handle_poll_result(screen::in, string::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

handle_poll_result(Screen, CountOutput, !Info, !IO) :-
    ( string.to_int(rstrip(CountOutput), Count) ->
        Count0 = !.Info ^ i_poll_count,
        ( Count0 = Count ->
            true
        ;
            !Info ^ i_poll_count := Count,
            % Redraw the bar immediately.
            draw_index_bar(Screen, !.Info, !IO),
            update_panels(Screen, !IO),

            % Run notify command if any.
            ( Count > 0 ->
                Config = !.Info ^ i_config,
                maybe_poll_notify(Config, count_messages_since_refresh(Count),
                    MessageUpdate, !IO),
                update_message(Screen, MessageUpdate, !IO)
            ;
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
        Maybe = yes(PollPeriodSecs),
        NextPollTime = yes(Time + float(PollPeriodSecs))
    ;
        Maybe = no,
        NextPollTime = no
    ).

%-----------------------------------------------------------------------------%

:- pred get_keycode_blocking_handle_resize(screen::in, message_update::in,
    keycode::out, index_info::in, index_info::out, io::di, io::uo) is det.

get_keycode_blocking_handle_resize(Screen, Message, Key, !Info, !IO) :-
    update_message_immed(Screen, Message, !IO),
    get_keycode_blocking(Key0, !IO),
    ( Key0 = code(curs.key_resize) ->
        recreate_screen_for_resize(Screen, !IO),
        recreate_index_view(Screen, !Info, !IO),
        draw_index_view(Screen, !.Info, !IO),
        update_panels(Screen, !IO),
        get_keycode_blocking_handle_resize(Screen, Message, Key, !Info, !IO)
    ;
        Key = Key0
    ).

%-----------------------------------------------------------------------------%

:- pred draw_index_view(screen::in, index_info::in, io::di, io::uo) is det.

draw_index_view(Screen, Info, !IO) :-
    Config = Info ^ i_config,
    Attrs = index_attrs(Config),
    ShowAuthors = Info ^ i_show_authors,
    (
        ShowAuthors = show_authors,
        get_cols(Screen, Cols, !IO),
        get_author_width(Cols, AuthorWidth)
    ;
        ShowAuthors = hide_authors,
        AuthorWidth = 0
    ),
    get_main_panels(Screen, MainPanels, !IO),
    Scrollable = Info ^ i_scrollable,
    scrollable.draw(draw_index_line(Attrs, AuthorWidth), Screen, MainPanels,
        Scrollable, !IO),
    draw_index_bar(Screen, Info, !IO).

:- pred get_author_width(int::in, int::out) is det.

get_author_width(Cols, AuthorWidth) :-
    Rem = Cols - 16 - 40,
    ( Rem < 4 ->
        AuthorWidth = 0
    ;
        AuthorWidth = min(Rem, 20)
    ).

:- pred draw_index_line(index_attrs::in, int::in, screen::in, vpanel::in,
    index_line::in, int::in, bool::in, io::di, io::uo) is det.

draw_index_line(IAttrs, AuthorWidth, Screen, Panel, Line, _LineNr, IsCursor,
        !IO) :-
    Line = index_line(_Id, Selected, Date, presentable_string(Authors),
        presentable_string(Subject), Tags, StdTags, NonstdTagsWidth,
        Matched, Total, _UnmatchedMessageIds),
    Attrs = IAttrs ^ i_generic,
    (
        IsCursor = yes,
        DateAttr = Attrs ^ current
    ;
        IsCursor = no,
        DateAttr = Attrs ^ relative_date
    ),
    draw_fixed(Screen, Panel, DateAttr, 10, Date, ' ', !IO),

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
        Base = curs.bold,
        draw(Screen, Panel, "n", !IO)
    ;
        Unread = read,
        Base = curs.normal,
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

    ( AuthorWidth > 0 ->
        mattr_draw_fixed(Screen, Panel,
            unless(IsCursor, curs.(Attrs ^ author + Base)),
            AuthorWidth, Authors, ' ', !IO)
    ;
        true
    ),

    ( Matched = Total ->
        CountStr = format(" %3d", [i(Total)]),
        TotalStr = "     "
    ;
        CountStr = format(" %3d", [i(Matched)]),
        TotalStr = format("/%-3d ", [i(Total)])
    ),
    mattr_draw(Screen, Panel, unless(IsCursor, IAttrs ^ i_count), CountStr,
        !IO),
    mattr_draw(Screen, Panel, unless(IsCursor, IAttrs ^ i_total), TotalStr,
        !IO),

    getyx(Screen, Panel, Row, SubjectX0, !IO),
    mattr_draw(Screen, Panel, unless(IsCursor, Attrs ^ subject), Subject, !IO),

    % Draw non-standard tags, overlapping up to half of the subject.
    ( NonstdTagsWidth > 0 ->
        getyx(Screen, Panel, _, SubjectX, !IO),
        getmaxyx(Screen, Panel, _, MaxX, !IO),
        (
            MaxX - SubjectX < NonstdTagsWidth,
            SubjectMidX = (MaxX + SubjectX0)/2,
            MoveX = max(SubjectMidX, MaxX - NonstdTagsWidth),
            MoveX < SubjectX
        ->
            move(Screen, Panel, Row, MoveX, !IO)
        ;
            true
        ),
        attr(Screen, Panel, Attrs ^ other_tag, !IO),
        set.fold(draw_display_tag(Screen, Panel), Tags, !IO)
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

:- pred draw_index_bar(screen::in, index_info::in, io::di, io::uo) is det.

draw_index_bar(Screen, Info, !IO) :-
    Count = Info ^ i_poll_count,
    ( Count = 0 ->
        ( semidet_fail ->
            MaybeText = yes(debug_timestamps_text(Info))
        ;
            MaybeText = no
        )
    ;
        MaybeText = yes(count_messages_since_refresh(Count))
    ),
    draw_status_bar(Screen, MaybeText, no, !IO).

:- func count_messages_since_refresh(int) = string.

count_messages_since_refresh(Count) =
    string.format("%+d messages since refresh", [i(Count)]).

:- func debug_timestamps_text(index_info) = string.

debug_timestamps_text(Info) = Text :-
    SearchTime = Info ^ i_search_time,
    SearchTimeStr = timestamp_to_int_string(SearchTime),

    LastActiveTime = Info ^ i_last_active_time,
    LastActiveTimeStr = timestamp_to_int_string(LastActiveTime),

    MaybeNextPollTime = Info ^ i_next_poll_time,
    (
        MaybeNextPollTime = yes(NextPollTime),
        NextPollTimeStr = timestamp_to_int_string(NextPollTime)
    ;
        MaybeNextPollTime = no,
        NextPollTimeStr = "-"
    ),

    PollCount = Info ^ i_poll_count,

    Text = string.format(
        "search_time: %s, last_active_time: %s, " ++
        "next_poll_time: %s, poll_count: %d",
        [s(SearchTimeStr), s(LastActiveTimeStr),
        s(NextPollTimeStr), i(PollCount)]).

:- func unless(bool, curs.attr) = maybe(curs.attr).

unless(no, X) = yes(X).
unless(yes, _) = no.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
