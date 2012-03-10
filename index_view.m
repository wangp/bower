% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module index_view.
:- interface.

:- import_module io.

:- import_module screen.

%-----------------------------------------------------------------------------%

:- pred open_index(screen::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module time.

:- import_module callout.
:- import_module compose.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module maildir.
:- import_module recall.
:- import_module scrollable.
:- import_module search_term.
:- import_module string_util.
:- import_module tags.
:- import_module text_entry.
:- import_module thread_pager.
:- import_module time_util.
:- import_module view_common.

%-----------------------------------------------------------------------------%

:- type index_info
    --->    index_info(
                i_scrollable        :: scrollable(index_line),
                i_search_terms      :: string,
                i_search_tokens     :: list(token),
                i_search_time       :: time_t,
                i_poll_time         :: time_t,
                i_poll_count        :: int,
                i_internal_search   :: maybe(string),
                i_common_history    :: common_history,
                i_compose_history   :: compose_history
            ).

:- type index_line
    --->    index_line(
                i_id        :: thread_id,
                i_selected  :: selected,
                i_unread    :: unread,
                i_replied   :: replied,
                i_deleted   :: deleted,
                i_flagged   :: flagged,
                i_date      :: string,
                i_authors   :: string,
                i_subject   :: string,
                i_tags      :: set(tag),
                i_nonstd_tags_width :: int,
                i_matched   :: int,
                i_total     :: int
            ).

:- type selected
    --->    not_selected
    ;       selected.

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
    ;       enter
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_internal_search
    ;       skip_to_internal_search
    ;       toggle_unread
    ;       toggle_flagged
    ;       set_deleted
    ;       unset_deleted
    ;       prompt_tag(string)
    ;       toggle_select
    ;       unselect_all
    ;       bulk_tag
    ;       quit.

:- type action
    --->    continue
    ;       resize
    ;       open_pager(thread_id)
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_internal_search
    ;       toggle_unread
    ;       toggle_flagged
    ;       set_deleted
    ;       unset_deleted
    ;       prompt_tag(string)
    ;       bulk_tag
    ;       quit.

:- type arbitrary_tag_changes
    --->    no
    ;       yes(
                tag_deltas  :: list(tag_delta),
                add_tags    :: set(tag),
                remove_tags :: set(tag)
            ).

:- instance scrollable.line(index_line) where [
    pred(draw_line/6) is draw_index_line
].

%-----------------------------------------------------------------------------%

open_index(Screen, SearchString, !IO) :-
    time(Time, !IO),
    ( SearchString = "" ->
        SearchTokens = [],
        Threads = [],
        LimitHistory = init_history
    ;
        predigest_search_string(SearchString, SearchTokens, !IO),
        search_terms_with_progress(Screen, SearchTokens, Threads, !IO),
        add_history_nodup(SearchString, init_history, LimitHistory)
    ),
    setup_index_scrollable(Time, Threads, Scrollable),
    SearchTime = Time,
    PollTime = Time,
    PollCount = 0,
    MaybeSearch = no,
    CommonHistory = common_history(LimitHistory, init_history, init_history,
        init_history, init_history),
    IndexInfo = index_info(Scrollable, SearchString, SearchTokens, SearchTime,
        PollTime, PollCount, MaybeSearch, CommonHistory, init_compose_history),
    index_loop(Screen, IndexInfo, !IO).

:- pred search_terms_with_progress(screen::in, list(token)::in,
    list(thread)::out, io::di, io::uo) is det.

search_terms_with_progress(Screen, Tokens, Threads, !IO) :-
    update_message_immed(Screen, set_info("Searching..."), !IO),
    tokens_to_search_terms(Tokens, Terms, ApplyCap, !IO),
    (
        ApplyCap = yes,
        LimitOption = ["--limit=" ++ from_int(default_max_threads)]
    ;
        ApplyCap = no,
        LimitOption = []
    ),
    run_notmuch(["search", "--format=json" | LimitOption] ++ ["--", Terms],
        parse_threads_list, ResThreads, !IO),
    (
        ResThreads = ok(Threads),
        NumThreads = list.length(Threads),
        (
            ApplyCap = yes,
            NumThreads = default_max_threads
        ->
            string.format("Found %d threads (capped). Use ~A to disable cap.",
                [i(NumThreads)], Message)
        ;
            string.format("Found %d threads.", [i(NumThreads)], Message)
        ),
        MessageUpdate = set_info(Message)
    ;
        ResThreads = error(Error),
        Message = "Error: " ++ io.error_message(Error),
        MessageUpdate = set_warning(Message),
        Threads = []
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred setup_index_scrollable(time_t::in, list(thread)::in,
    scrollable(index_line)::out) is det.

setup_index_scrollable(Time, Threads, Scrollable) :-
    Nowish = localtime(Time),
    list.foldl(add_thread(Nowish), Threads, cord.init, LinesCord),
    Lines = list(LinesCord),
    (
        Lines = [],
        Scrollable = scrollable.init(Lines)
    ;
        Lines = [_ | _],
        Scrollable = scrollable.init_with_cursor(Lines)
    ).

:- pred add_thread(tm::in, thread::in,
    cord(index_line)::in, cord(index_line)::out) is det.

add_thread(Nowish, Thread, !Lines) :-
    thread_to_index_line(Nowish, Thread, Line),
    snoc(Line, !Lines).

:- pred thread_to_index_line(tm::in, thread::in, index_line::out) is det.

thread_to_index_line(Nowish, Thread, Line) :-
    Thread = thread(Id, Timestamp, Authors, Subject, Tags, Matched, Total),
    timestamp_to_tm(Timestamp, TM),
    Shorter = yes,
    make_reldate(Nowish, TM, Shorter, Date),
    get_standard_tag_state(Tags, Unread, Replied, Deleted, Flagged),
    get_nonstandard_tags_width(Tags, NonstdTagsWidth),
    Line = index_line(Id, not_selected, Unread, Replied, Deleted, Flagged,
        Date, Authors, Subject, Tags, NonstdTagsWidth, Matched, Total).

:- func default_max_threads = int.

default_max_threads = 300.

%-----------------------------------------------------------------------------%

:- pred index_loop(screen::in, index_info::in, io::di, io::uo) is det.

index_loop(Screen, !.IndexInfo, !IO) :-
    draw_index_view(Screen, !.IndexInfo, !IO),
    draw_index_bar(Screen, !.IndexInfo, !IO),
    panel.update_panels(!IO),
    get_keycode(KeyCode, !IO),
    index_view_input(Screen, KeyCode, MessageUpdate, Action, !IndexInfo),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        maybe_poll(!IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = resize,
        recreate_screen(Screen, NewScreen, !IndexInfo, !IO),
        index_loop(NewScreen, !.IndexInfo, !IO)
    ;
        Action = open_pager(ThreadId),
        CommonHistory0 = !.IndexInfo ^ i_common_history,
        open_thread_pager(Screen, ThreadId, NeedRefresh,
            CommonHistory0, CommonHistory, !IO),
        % In case of resize.
        recreate_screen(Screen, NewScreen, !IndexInfo, !IO),
        !IndexInfo ^ i_common_history := CommonHistory,
        (
            NeedRefresh = yes,
            refresh_index_line(NewScreen, ThreadId, !IndexInfo, !IO)
        ;
            NeedRefresh = no
        ),
        index_loop(NewScreen, !.IndexInfo, !IO)
    ;
        Action = enter_limit,
        History0 = !.IndexInfo ^ i_common_history ^ ch_limit_history,
        Completion = complete_tags(["tag:", "is:"]),
        text_entry(Screen, "Limit to messages matching: ", History0,
            Completion, Return, !IO),
        (
            Return = yes(LimitString),
            time(Time, !IO),
            predigest_search_string(LimitString, Tokens, !IO),
            search_terms_with_progress(Screen, Tokens, Threads, !IO),
            setup_index_scrollable(Time, Threads, Scrollable),
            add_history_nodup(LimitString, History0, History),
            !IndexInfo ^ i_scrollable := Scrollable,
            !IndexInfo ^ i_search_terms := LimitString,
            !IndexInfo ^ i_search_tokens := Tokens,
            !IndexInfo ^ i_search_time := Time,
            !IndexInfo ^ i_poll_time := Time,
            !IndexInfo ^ i_poll_count := 0,
            !IndexInfo ^ i_common_history ^ ch_limit_history := History,
            index_loop(Screen, !.IndexInfo, !IO)
        ;
            Return = no,
            update_message(Screen, clear_message, !IO),
            index_loop(Screen, !.IndexInfo, !IO)
        )
    ;
        Action = refresh_all,
        refresh_all(Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = start_compose,
        ComposeHistory0 = !.IndexInfo ^ i_compose_history,
        start_compose(Screen, ComposeHistory0, ComposeHistory, !IO),
        !IndexInfo ^ i_compose_history := ComposeHistory,
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = start_recall,
        select_recall(Screen, MaybeSelected, !IO),
        (
            MaybeSelected = yes(Message),
            continue_postponed(Screen, Message, !IO)
        ;
            MaybeSelected = no
        ),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = prompt_internal_search,
        prompt_internal_search(Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = toggle_unread,
        modify_tag_cursor_line(toggle_unread, Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = toggle_flagged,
        modify_tag_cursor_line(toggle_flagged, Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = set_deleted,
        modify_tag_cursor_line(set_deleted, Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = unset_deleted,
        modify_tag_cursor_line(unset_deleted, Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = prompt_tag(Initial),
        prompt_tag(Screen, Initial, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = bulk_tag,
        bulk_tag(Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = quit
    ).

%-----------------------------------------------------------------------------%

:- pred index_view_input(screen::in, keycode::in, message_update::out,
    action::out, index_info::in, index_info::out) is det.

index_view_input(Screen, KeyCode, MessageUpdate, Action, !IndexInfo) :-
    ( key_binding(KeyCode, Binding) ->
        (
            Binding = scroll_down,
            move_cursor(Screen, 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = scroll_up,
            move_cursor(Screen, -1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = page_down,
            get_main_rows(Screen, NumRows),
            move_cursor(Screen, NumRows - 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = page_up,
            get_main_rows(Screen, NumRows),
            move_cursor(Screen, -NumRows + 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = half_page_down,
            get_main_rows(Screen, NumRows),
            Delta = int.max(15, NumRows / 2),
            move_cursor(Screen, Delta, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = half_page_up,
            get_main_rows(Screen, NumRows),
            Delta = int.max(15, NumRows / 2),
            move_cursor(Screen, -Delta, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = home,
            Scrollable0 = !.IndexInfo ^ i_scrollable,
            NumLines = get_num_lines(Scrollable0),
            move_cursor(Screen, -NumLines, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = end,
            Scrollable0 = !.IndexInfo ^ i_scrollable,
            NumLines = get_num_lines(Scrollable0),
            move_cursor(Screen, NumLines, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = skip_to_unread,
            skip_to_unread(Screen, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = enter,
            enter(!.IndexInfo, Action),
            MessageUpdate = clear_message
        ;
            Binding = enter_limit,
            MessageUpdate = no_change,
            Action = enter_limit
        ;
            Binding = refresh_all,
            MessageUpdate = no_change,
            Action = refresh_all
        ;
            Binding = start_compose,
            MessageUpdate = no_change,
            Action = start_compose
        ;
            Binding = start_recall,
            MessageUpdate = no_change,
            Action = start_recall
        ;
            Binding = prompt_internal_search,
            MessageUpdate = no_change,
            Action = prompt_internal_search
        ;
            Binding = skip_to_internal_search,
            skip_to_internal_search(Screen, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = toggle_unread,
            MessageUpdate = no_change,
            Action = toggle_unread
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
            Binding = prompt_tag(Initial),
            MessageUpdate = no_change,
            Action = prompt_tag(Initial)
        ;
            Binding = toggle_select,
            toggle_select(Screen, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = unselect_all,
            unselect_all(MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = bulk_tag,
            MessageUpdate = no_change,
            Action = bulk_tag
        ;
            Binding = quit,
            MessageUpdate = no_change,
            Action = quit
        )
    ;
        KeyCode = code(key_resize)
    ->
        MessageUpdate = no_change,
        Action = resize
    ;
        MessageUpdate = no_change,
        Action = continue
    ).

:- pred key_binding(keycode::in, binding::out) is semidet.

key_binding(char(Char), Binding) :-
    key_binding_char(Char, Binding).
key_binding(code(Code), Binding) :-
    ( Code = key_up ->
        Binding = scroll_up
    ; Code = key_down ->
        Binding = scroll_down
    ; Code = key_home ->
        Binding = home
    ; Code = key_end ->
        Binding = end
    ; Code = key_pageup ->
        Binding = page_up
    ; Code = key_pagedown ->
        Binding = page_down
    ;
        fail
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
key_binding_char('\r', enter).
key_binding_char('l', enter_limit).
key_binding_char('m', start_compose).
key_binding_char('R', start_recall).
key_binding_char('/', prompt_internal_search).
key_binding_char('n', skip_to_internal_search).
key_binding_char('N', toggle_unread).
key_binding_char('F', toggle_flagged).
key_binding_char('d', set_deleted).
key_binding_char('u', unset_deleted).
key_binding_char('+', prompt_tag("+")).
key_binding_char('-', prompt_tag("-")).
key_binding_char('t', toggle_select).
key_binding_char('T', unselect_all).
key_binding_char(';', bulk_tag).
key_binding_char('q', quit).

:- pred move_cursor(screen::in, int::in, message_update::out,
    index_info::in, index_info::out) is det.

move_cursor(Screen, Delta, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    get_main_rows(Screen, NumRows),
    move_cursor(NumRows, Delta, HitLimit, Scrollable0, Scrollable),
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

:- pred skip_to_unread(screen::in, message_update::out,
    index_info::in, index_info::out) is det.

skip_to_unread(Screen, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    get_main_rows(Screen, NumRows),
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
        unexpected($module, $pred, "no cursor")
    ),
    !Info ^ i_scrollable := Scrollable.

:- pred is_unread_line(index_line::in) is semidet.

is_unread_line(Line) :-
    Line ^ i_unread = unread.

:- pred enter(index_info::in, action::out) is det.

enter(Info, Action) :-
    Scrollable = Info ^ i_scrollable,
    ( get_cursor_line(Scrollable, _, CursorLine) ->
        ThreadId = CursorLine ^ i_id,
        Action = open_pager(ThreadId)
    ;
        Action = continue
    ).

%-----------------------------------------------------------------------------%

:- pred prompt_internal_search(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

prompt_internal_search(Screen, !Info, !IO) :-
    History0 = !.Info ^ i_common_history ^ ch_internal_search_history,
    text_entry(Screen, "Search for: ", History0, complete_none, Return, !IO),
    (
        Return = yes(Search),
        ( Search = "" ->
            !Info ^ i_internal_search := no
        ;
            add_history_nodup(Search, History0, History),
            !Info ^ i_internal_search := yes(Search),
            !Info ^ i_common_history ^ ch_internal_search_history := History,
            skip_to_internal_search(Screen, MessageUpdate, !Info),
            update_message(Screen, MessageUpdate, !IO)
        )
    ;
        Return = no
    ).

:- pred skip_to_internal_search(screen::in, message_update::out,
    index_info::in, index_info::out) is det.

skip_to_internal_search(Screen, MessageUpdate, !Info) :-
    MaybeSearch = !.Info ^ i_internal_search,
    (
        MaybeSearch = yes(Search),
        Scrollable0 = !.Info ^ i_scrollable,
        get_main_rows(Screen, NumRows),
        (
            get_cursor(Scrollable0, Cursor0),
            search_forward(line_matches_internal_search(Search), Scrollable0,
                Cursor0 + 1, Cursor, _)
        ->
            set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
            MessageUpdate = clear_message
        ;
            search_forward(line_matches_internal_search(Search), Scrollable0,
                0, Cursor, _)
        ->
            set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
            MessageUpdate = set_info("Search wrapped to top.")
        ;
            Scrollable = Scrollable0,
            MessageUpdate = set_warning("Not found.")
        ),
        !Info ^ i_scrollable := Scrollable
    ;
        MaybeSearch = no,
        MessageUpdate = set_warning("No search string.")
    ).

:- pred line_matches_internal_search(string::in, index_line::in) is semidet.

line_matches_internal_search(Search, Line) :-
    Line = index_line(_Id, _Selected, _Unread, _Replied, _Flagged, _Deleted,
        _Date, Authors, Subject, Tags, _TagsWidth, _Matched, _Total),
    (
        strcase_str(Authors, Search)
    ;
        strcase_str(Subject, Search)
    ;
        set.member(tag(TagName), Tags),
        strcase_str(TagName, Search)
    ).

%-----------------------------------------------------------------------------%

:- pred modify_tag_cursor_line(pred(index_line, index_line, tag_delta),
    screen, index_info, index_info, io, io).
:- mode modify_tag_cursor_line(in(pred(in, out, out) is det),
    in, in, out, di, uo) is det.
:- mode modify_tag_cursor_line(in(pred(in, out, out) is semidet),
    in, in, out, di, uo) is det.

modify_tag_cursor_line(ModifyPred, Screen, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        ThreadId = CursorLine0 ^ i_id,
        ( ModifyPred(CursorLine0, CursorLine, TagDelta) ->
            tag_threads([TagDelta], [ThreadId], Res, !IO),
            (
                Res = ok,
                set_cursor_line(CursorLine, Scrollable0, Scrollable),
                !Info ^ i_scrollable := Scrollable,
                move_cursor(Screen, 1, _MessageUpdate, !Info),
                MessageUpdate = clear_message
            ;
                Res = error(Error),
                MessageUpdate = set_warning(io.error_message(Error))
            )
        ;
            MessageUpdate = set_warning("Refusing to tag multiple messages.")
        )
    ;
        MessageUpdate = set_warning("No thread.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred toggle_unread(index_line::in, index_line::out, tag_delta::out) is det.

toggle_unread(Line0, Line, TagDelta) :-
    Unread0 = Line0 ^ i_unread,
    (
        Unread0 = unread,
        TagDelta = tag_delta("-unread"),
        Unread = read
    ;
        Unread0 = read,
        TagDelta = tag_delta("+unread"),
        Unread = unread
    ),
    Line = Line0 ^ i_unread := Unread.

:- pred toggle_flagged(index_line::in, index_line::out, tag_delta::out)
    is semidet.

toggle_flagged(Line0, Line, TagDelta) :-
    Flagged0 = Line0 ^ i_flagged,
    (
        Flagged0 = flagged,
        TagDelta = tag_delta("-flagged"),
        Flagged = unflagged
    ;
        Flagged0 = unflagged,
        TagDelta = tag_delta("+flagged"),
        Flagged = flagged,
        % Refuse to flag multiple messages.
        NumMessages = Line0 ^ i_total,
        NumMessages = 1
    ),
    Line = Line0 ^ i_flagged := Flagged.

:- pred set_deleted(index_line::in, index_line::out, tag_delta::out) is det.

set_deleted(Line0, Line, TagDelta) :-
    Line = Line0 ^ i_deleted := deleted,
    TagDelta = tag_delta("+deleted").

:- pred unset_deleted(index_line::in, index_line::out, tag_delta::out) is det.

unset_deleted(Line0, Line, TagDelta) :-
    Line = Line0 ^ i_deleted := not_deleted,
    TagDelta = tag_delta("-deleted").

%-----------------------------------------------------------------------------%

:- pred prompt_tag(screen::in, string::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

prompt_tag(Screen, Initial, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        prompt_arbitrary_tag_changes(Screen, Initial, TagChanges,
            !Info, !IO),
        (
            TagChanges = yes(TagDeltas, AddTags, RemoveTags),
            apply_tag_changes(Screen, CursorLine0, TagDeltas, AddTags,
                RemoveTags, !Info, !IO)
        ;
            TagChanges = no
        )
    ;
        update_message(Screen, set_warning("No thread."), !IO)
    ).

:- pred prompt_arbitrary_tag_changes(screen::in, string::in,
    arbitrary_tag_changes::out, index_info::in, index_info::out,
    io::di, io::uo) is det.

prompt_arbitrary_tag_changes(Screen, Initial, TagChanges, !Info, !IO) :-
    History0 = !.Info ^ i_common_history ^ ch_tag_history,
    Completion = complete_tags(["+", "-"]),
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

:- pred apply_tag_changes(screen::in, index_line::in, list(tag_delta)::in,
    set(tag)::in, set(tag)::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

apply_tag_changes(Screen, CursorLine0, TagDeltas, AddTags, RemoveTags,
        !Info, !IO) :-
    CursorLine0 = index_line(ThreadId, Selected,
        _Unread, _Replied, _Deleted, _Flagged, Date, Authors, Subject,
        TagSet0, _NonstdTagsWidth, Matched, Total),
    tag_threads(TagDeltas, [ThreadId], Res, !IO),
    (
        Res = ok,
        % Notmuch performs tag removals before addition.
        set.difference(TagSet0, RemoveTags, TagSet1),
        set.union(TagSet1, AddTags, TagSet),
        get_standard_tag_state(TagSet, Unread, Replied, Deleted, Flagged),
        get_nonstandard_tags_width(TagSet, NonstdTagsWidth),
        CursorLine = index_line(ThreadId, Selected,
            Unread, Replied, Deleted, Flagged, Date, Authors, Subject,
            TagSet, NonstdTagsWidth, Matched, Total),
        Scrollable0 = !.Info ^ i_scrollable,
        set_cursor_line(CursorLine, Scrollable0, Scrollable),
        !Info ^ i_scrollable := Scrollable
    ;
        Res = error(Error),
        MessageUpdate = set_warning(io.error_message(Error)),
        update_message(Screen, MessageUpdate, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred toggle_select(screen::in, message_update::out,
    index_info::in, index_info::out) is det.

toggle_select(Screen, MessageUpdate, !Info) :-
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
        move_cursor(Screen, 1, MessageUpdate, !Info)
    ;
        MessageUpdate = set_warning("No thread.")
    ).

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

:- pred bulk_tag(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

bulk_tag(Screen, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    ( any_selected_line(Lines0) ->
        Prompt = "Action: (d)elete, (u)ndelete, (N) toggle unread, " ++
            "(+/-) change tags",
        update_message_immed(Screen, set_prompt(Prompt), !IO),
        get_keycode(KeyCode, !IO),
        ( KeyCode = char('-') ->
            bulk_arbitrary_tag_changes(Screen, "-", MessageUpdate, !Info, !IO)
        ; KeyCode = char('+') ->
            bulk_arbitrary_tag_changes(Screen, "+", MessageUpdate, !Info, !IO)
        ; KeyCode = char('d') ->
            TagDeltas = [tag_delta("+deleted")],
            AddTags = set.make_singleton_set(tag("deleted")),
            RemoveTags = set.init,
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO)
        ; KeyCode = char('u') ->
            TagDeltas = [tag_delta("-deleted")],
            AddTags = set.init,
            RemoveTags = set.make_singleton_set(tag("deleted")),
            bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
                !Info, !IO)
        ; KeyCode = char('N') ->
            bulk_toggle_unread(MessageUpdate, !Info, !IO)
        ;
            MessageUpdate = set_info("No changes.")
        )
    ;
        MessageUpdate = set_warning("No threads selected.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred any_selected_line(list(index_line)::in) is semidet.

any_selected_line(Lines) :-
    list.member(Line, Lines),
    Line ^ i_selected = selected.

:- pred bulk_arbitrary_tag_changes(screen::in, string::in, message_update::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

bulk_arbitrary_tag_changes(Screen, Initial, MessageUpdate, !Info, !IO) :-
    prompt_arbitrary_tag_changes(Screen, Initial, TagChanges, !Info, !IO),
    (
        TagChanges = yes(TagDeltas, AddTags, RemoveTags),
        bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate,
            !Info, !IO)
    ;
        TagChanges = no,
        MessageUpdate = clear_message
    ).

:- pred bulk_tag_changes(list(tag_delta)::in, set(tag)::in, set(tag)::in,
    message_update::out, index_info::in, index_info::out, io::di, io::uo) is det.

bulk_tag_changes(TagDeltas, AddTags, RemoveTags, MessageUpdate, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    Lines0 = get_lines_list(Scrollable0),
    list.map_foldl(
        update_selected_line_for_tag_changes(AddTags, RemoveTags),
        Lines0, Lines, [], SelectedThreadIds),
    (
        SelectedThreadIds = [_ | _],
        tag_threads(TagDeltas, SelectedThreadIds, Res, !IO),
        (
            Res = ok,
            set_lines_list(Lines, Scrollable0, Scrollable),
            !Info ^ i_scrollable := Scrollable,
            list.length(SelectedThreadIds, NumThreads),
            string.format("Modified tags in %d threads.", [i(NumThreads)],
                Message),
            MessageUpdate = set_info(Message)
        ;
            Res = error(Error),
            MessageUpdate = set_warning(io.error_message(Error))
        )
    ;
        SelectedThreadIds = [],
        MessageUpdate = set_info("No changes.")
    ).

:- pred update_selected_line_for_tag_changes(set(tag)::in, set(tag)::in,
    index_line::in, index_line::out, list(thread_id)::in, list(thread_id)::out)
    is det.

update_selected_line_for_tag_changes(AddTags, RemoveTags, Line0, Line,
        !ThreadIds) :-
    Line0 = index_line(ThreadId, Selected,
        _Unread, _Replied, _Deleted, _Flagged, Date, Authors, Subject,
        TagSet0, _NonstdTagsWidth, Matched, Total),
    (
        Selected = selected,
        % Notmuch performs tag removals before addition.
        TagSet0 = Line0 ^ i_tags,
        set.difference(TagSet0, RemoveTags, TagSet1),
        set.union(TagSet1, AddTags, TagSet),
        TagSet \= TagSet0
    ->
        get_standard_tag_state(TagSet, Unread, Replied, Deleted, Flagged),
        get_nonstandard_tags_width(TagSet, NonstdTagsWidth),
        Line = index_line(ThreadId, Selected,
            Unread, Replied, Deleted, Flagged, Date, Authors, Subject,
            TagSet, NonstdTagsWidth, Matched, Total),
        list.cons(ThreadId, !ThreadIds)
    ;
        Line = Line0
    ).

:- pred bulk_toggle_unread(message_update::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

bulk_toggle_unread(MessageUpdate, !Info, !IO) :-
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
            !Info, !IO)
    ;
        Message = "Selected threads differ in unread state.",
        MessageUpdate = set_info(Message)
    ).

:- pred common_unread_state(list(index_line)::in,
    maybe(unread)::in, maybe(unread)::out) is semidet.

common_unread_state([], State, State).
common_unread_state([H | T], State0, State) :-
    Selected = H ^ i_selected,
    (
        Selected = selected,
        State0 = no,
        State1 = yes(H ^ i_unread),
        common_unread_state(T, State1, State)
    ;
        Selected = selected,
        State0 = yes(H ^ i_unread),
        common_unread_state(T, State0, State)
    ;
        Selected = not_selected,
        common_unread_state(T, State0, State)
    ).

%-----------------------------------------------------------------------------%

:- pred refresh_all(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

refresh_all(Screen, !Info, !IO) :-
    time(Time, !IO),
    % The user might have changed search aliases and is trying to force a
    % refresh, so expand the search terms from the beginning.
    Terms = !.Info ^ i_search_terms,
    predigest_search_string(Terms, Tokens, !IO),
    search_terms_with_progress(Screen, Tokens, Threads, !IO),
    some [!Scrollable] (
        Scrollable0 = !.Info ^ i_scrollable,
        Top0 = get_top(Scrollable0),
        setup_index_scrollable(Time, Threads, !:Scrollable),
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
                get_main_rows(Screen, NumRows),
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
        !Info ^ i_poll_time := Time,
        !Info ^ i_poll_count := 0
    ).

:- pred line_matches_thread_id(thread_id::in, index_line::in) is semidet.

line_matches_thread_id(ThreadId, Line) :-
    Line ^ i_id = ThreadId.

%-----------------------------------------------------------------------------%

:- pred refresh_index_line(screen::in, thread_id::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

refresh_index_line(Screen, ThreadId, !IndexInfo, !IO) :-
    Term = thread_id_to_search_term(ThreadId),
    run_notmuch([
        "search", "--format=json", "--", Term
    ], parse_threads_list, Result, !IO),
    (
        Result = ok([Thread]),
        time(Time, !IO),
        Nowish = localtime(Time),
        replace_index_cursor_line(Nowish, Thread, !IndexInfo)
    ;
        ( Result = ok([])
        ; Result = ok([_, _ | _])
        ; Result = error(_)
        ),
        update_message(Screen, set_warning("Error refreshing index."), !IO)
    ).

:- pred replace_index_cursor_line(tm::in, thread::in,
    index_info::in, index_info::out) is det.

replace_index_cursor_line(Nowish, Thread, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    thread_to_index_line(Nowish, Thread, Line),
    set_cursor_line(Line, Scrollable0, Scrollable),
    !Info ^ i_scrollable := Scrollable.

%-----------------------------------------------------------------------------%

:- pred maybe_poll(index_info::in, index_info::out, io::di, io::uo) is det.

maybe_poll(!Info, !IO) :-
    time(Time, !IO),
    PollTime = !.Info ^ i_poll_time,
    time_to_int(Time, TimeInt),
    time_to_int(PollTime, PollTimeInt),
    ( TimeInt - PollTimeInt < poll_period_secs ->
        true
    ;
        !Info ^ i_poll_time := Time,
        Tokens = !.Info ^ i_search_tokens,
        SearchTime = !.Info ^ i_search_time,
        time_to_int(SearchTime, SearchTimeInt),
        tokens_to_search_terms(Tokens, Terms1, _ApplyCap, !IO),
        string.format("( %s ) %d.. AND NOT tag:sent",
            [s(Terms1), i(SearchTimeInt)], Terms),
        run_notmuch_count(Terms, ResCount, !IO),
        (
            ResCount = ok(Count),
            !Info ^ i_poll_count := Count
        ;
            ResCount = error(_)
            % XXX show error message?
        )
    ).

:- func poll_period_secs = int.

poll_period_secs = 60.

%-----------------------------------------------------------------------------%

:- pred recreate_screen(screen::in, screen::out,
    index_info::in, index_info::out, io::di, io::uo) is det.

recreate_screen(Screen0, Screen, !IndexInfo, !IO) :-
    destroy_screen(Screen0, !IO),
    create_screen(Screen, !IO),
    % Keep cursor visible.
    Scrollable0 = !.IndexInfo ^ i_scrollable,
    ( get_cursor(Scrollable0, Cursor) ->
        get_main_rows(Screen, NumRows),
        set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
        !IndexInfo ^ i_scrollable := Scrollable
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred draw_index_view(screen::in, index_info::in, io::di, io::uo) is det.

draw_index_view(Screen, Info, !IO) :-
    get_main_panels(Screen, MainPanels),
    Scrollable = Info ^ i_scrollable,
    scrollable.draw(MainPanels, Scrollable, !IO).

:- pred draw_index_line(panel::in, index_line::in, int::in, bool::in,
    io::di, io::uo) is det.

draw_index_line(Panel, Line, _LineNr, IsCursor, !IO) :-
    Line = index_line(_Id, Selected, Unread, Replied, Deleted, Flagged,
        Date, Authors, Subject, Tags, NonstdTagsWidth, Matched, Total),
    (
        IsCursor = yes,
        panel.attr_set(Panel, fg_bg(yellow, red) + bold, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, fg(blue) + bold, !IO)
    ),
    my_addstr_fixed(Panel, 10, Date, ' ', !IO),
    (
        Selected = selected,
        cond_attr_set(Panel, fg_bg(magenta, black) + bold, IsCursor, !IO),
        my_addstr(Panel, "*", !IO)
    ;
        Selected = not_selected,
        my_addstr(Panel, " ", !IO)
    ),
    cond_attr_set(Panel, normal, IsCursor, !IO),
    (
        Unread = unread,
        Base = bold,
        my_addstr(Panel, "n", !IO)
    ;
        Unread = read,
        Base = normal,
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
        cond_attr_set(Panel, fg(red) + bold, IsCursor, !IO),
        my_addstr(Panel, "! ", !IO)
    ;
        Flagged = unflagged,
        my_addstr(Panel, "  ", !IO)
    ),
    cond_attr_set(Panel, Base, IsCursor, !IO),
    my_addstr_fixed(Panel, 25, Authors, ' ', !IO),
    cond_attr_set(Panel, fg(green) + Base, IsCursor, !IO),
    ( Matched = Total ->
        CountStr = format(" %3d     ", [i(Total)])
    ;
        CountStr = format(" %3d/%-3d ", [i(Matched), i(Total)])
    ),
    my_addstr(Panel, CountStr, !IO),
    cond_attr_set(Panel, normal, IsCursor, !IO),
    panel.getyx(Panel, Row, SubjectX0, !IO),
    my_addstr(Panel, Subject, !IO),
    % Draw non-standard tags, overlapping up to half of the subject.
    ( NonstdTagsWidth > 0 ->
        panel.getyx(Panel, _, SubjectX, !IO),
        panel.getmaxyx(Panel, _, MaxX, !IO),
        ( MaxX - SubjectX < NonstdTagsWidth ->
            SubjectMidX = (MaxX + SubjectX0)/2,
            MoveX = max(SubjectMidX, MaxX - NonstdTagsWidth),
            panel.move(Panel, Row, MoveX, !IO)
        ;
            true
        ),
        attr_set(Panel, fg_bg(red, black) + bold, !IO),
        set.fold(draw_nonstandard_tag(Panel), Tags, !IO)
    ;
        true
    ).

:- pred draw_nonstandard_tag(panel::in, tag::in, io::di, io::uo) is det.

draw_nonstandard_tag(Panel, Tag, !IO) :-
    ( standard_tag(Tag) ->
        true
    ;
        Tag = tag(TagName),
        my_addstr(Panel, " ", !IO),
        my_addstr(Panel, TagName, !IO)
    ).

:- pred draw_index_bar(screen::in, index_info::in, io::di, io::uo) is det.

draw_index_bar(Screen, Info, !IO) :-
    Count = Info ^ i_poll_count,
    ( Count = 0 ->
        draw_bar(Screen, !IO)
    ;
        string.format("%+d messages since refresh", [i(Count)], Message),
        draw_bar_with_text(Screen, Message, !IO)
    ).

:- pred cond_attr_set(panel::in, attr::in, bool::in, io::di, io::uo) is det.

cond_attr_set(Panel, Attr, IsCursor, !IO) :-
    (
        IsCursor = no,
        panel.attr_set(Panel, Attr, !IO)
    ;
        IsCursor = yes
    ).

:- func fg(colour) = attr.

fg(C) = curs.fg_bg(C, black).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
