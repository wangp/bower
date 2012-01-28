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
:- import_module require.
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
:- import_module text_entry.
:- import_module thread_pager.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type index_info
    --->    index_info(
                i_scrollable        :: scrollable(index_line),
                i_search_terms      :: string,
                i_search_time       :: time_t,
                i_poll_time         :: time_t,
                i_poll_count        :: int,
                i_search            :: maybe(string),
                i_limit_history     :: history,
                i_search_history    :: history,
                i_compose_history   :: compose_history
            ).

:- type index_line
    --->    index_line(
                i_id        :: thread_id,
                i_new       :: new,
                i_unread    :: unread,
                i_replied   :: replied,
                i_deleted   :: deleted,
                i_flagged   :: flagged,
                i_date      :: string,
                i_authors   :: string,
                i_subject   :: string,
                i_matched   :: int,
                i_total     :: int
            ).

:- type new
    --->    new
    ;       old.

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

:- type binding
    --->    scroll_down
    ;       scroll_up
    ;       home
    ;       end
    ;       page_up
    ;       page_down
    ;       skip_to_unread
    ;       enter
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_search
    ;       skip_to_search
    ;       toggle_unread
    ;       quit.

:- type action
    --->    continue
    ;       resize
    ;       open_pager(thread_id)
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_search
    ;       toggle_unread
    ;       quit.

:- instance scrollable.line(index_line) where [
    pred(draw_line/5) is draw_index_line
].

%-----------------------------------------------------------------------------%

open_index(Screen, Terms, !IO) :-
    time(Time, !IO),
    ( Terms = "" ->
        Threads = [],
        History = init_history
    ;
        search_terms_with_progress(Screen, Terms, Threads, !IO),
        add_history_nodup(Terms, init_history, History)
    ),
    setup_index_scrollable(Time, Threads, Scrollable),
    SearchTime = Time,
    PollTime = Time,
    PollCount = 0,
    MaybeSearch = no,
    IndexInfo = index_info(Scrollable, Terms, SearchTime, PollTime,
        PollCount, MaybeSearch, History, init_history, init_compose_history),
    index_loop(Screen, IndexInfo, !IO).

:- pred search_terms_with_progress(screen::in, string::in,
    list(thread)::out, io::di, io::uo) is det.

search_terms_with_progress(Screen, Terms0, Threads, !IO) :-
    update_message_immed(Screen, set_info("Searching..."), !IO),
    string_to_search_terms(Terms0, Terms, !IO),
    run_notmuch([
        "search", "--format=json", "--", Terms
    ], parse_threads_list, ResThreads, !IO),
    (
        ResThreads = ok(Threads),
        string.format("Found %d threads.", [i(length(Threads))], Message),
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
    Line0 = index_line(Id, old, read, not_replied, not_deleted, unflagged,
        Date, Authors, Subject, Matched, Total),
    list.foldl(apply_tag, Tags, Line0, Line).

:- pred apply_tag(string::in, index_line::in, index_line::out) is det.

apply_tag(Tag, !Line) :-
    ( Tag = "new" ->
        !Line ^ i_new := new
    ; Tag = "unread" ->
        !Line ^ i_unread := unread
    ; Tag = "replied" ->
        !Line ^ i_replied := replied
    ; Tag = "deleted" ->
        !Line ^ i_deleted := deleted
    ; Tag = "flagged" ->
        !Line ^ i_flagged := flagged
    ;
        true
    ).

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
        History0 = !.IndexInfo ^ i_search_history,
        open_thread_pager(Screen, ThreadId, NeedRefresh, History0, History,
            !IO),
        % In case of resize.
        recreate_screen(Screen, NewScreen, !IndexInfo, !IO),
        !IndexInfo ^ i_search_history := History,
        (
            NeedRefresh = yes,
            refresh_index_line(NewScreen, ThreadId, !IndexInfo, !IO)
        ;
            NeedRefresh = no
        ),
        index_loop(NewScreen, !.IndexInfo, !IO)
    ;
        Action = enter_limit,
        History0 = !.IndexInfo ^ i_limit_history,
        text_entry(Screen, "Limit to messages matching: ", History0,
            complete_none, Return, !IO),
        (
            Return = yes(Terms),
            time(Time, !IO),
            search_terms_with_progress(Screen, Terms, Threads, !IO),
            setup_index_scrollable(Time, Threads, Scrollable),
            add_history_nodup(Terms, History0, History),
            !IndexInfo ^ i_scrollable := Scrollable,
            !IndexInfo ^ i_search_terms := Terms,
            !IndexInfo ^ i_search_time := Time,
            !IndexInfo ^ i_poll_time := Time,
            !IndexInfo ^ i_poll_count := 0,
            !IndexInfo ^ i_limit_history := History,
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
        Action = prompt_search,
        prompt_search(Screen, !IndexInfo, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = toggle_unread,
        modify_tag_cursor_line(toggle_unread, Screen, !IndexInfo, !IO),
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
            Binding = prompt_search,
            MessageUpdate = no_change,
            Action = prompt_search
        ;
            Binding = skip_to_search,
            skip_to_search(Screen, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = toggle_unread,
            MessageUpdate = no_change,
            Action = toggle_unread
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
key_binding_char('\t', skip_to_unread).
key_binding_char('\r', enter).
key_binding_char('l', enter_limit).
key_binding_char('m', start_compose).
key_binding_char('R', start_recall).
key_binding_char('/', prompt_search).
key_binding_char('n', skip_to_search).
key_binding_char('N', toggle_unread).
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

:- pred prompt_search(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

prompt_search(Screen, !Info, !IO) :-
    History0 = !.Info ^ i_search_history,
    text_entry(Screen, "Search for: ", History0, complete_none, Return, !IO),
    (
        Return = yes(Search),
        ( Search = "" ->
            !Info ^ i_search := no
        ;
            add_history_nodup(Search, History0, History),
            !Info ^ i_search := yes(Search),
            !Info ^ i_search_history := History,
            skip_to_search(Screen, MessageUpdate, !Info),
            update_message(Screen, MessageUpdate, !IO)
        )
    ;
        Return = no
    ).

:- pred skip_to_search(screen::in, message_update::out,
    index_info::in, index_info::out) is det.

skip_to_search(Screen, MessageUpdate, !Info) :-
    MaybeSearch = !.Info ^ i_search,
    (
        MaybeSearch = yes(Search),
        Scrollable0 = !.Info ^ i_scrollable,
        get_main_rows(Screen, NumRows),
        (
            get_cursor(Scrollable0, Cursor0),
            search_forward(line_matches_search(Search), Scrollable0,
                Cursor0 + 1, Cursor, _)
        ->
            set_cursor_visible(Cursor, NumRows, Scrollable0, Scrollable),
            MessageUpdate = clear_message
        ;
            search_forward(line_matches_search(Search), Scrollable0,
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

:- pred line_matches_search(string::in, index_line::in) is semidet.

line_matches_search(Search, Line) :-
    Line = index_line(_Id, _New, _Unread, _Replied, _Flagged, _Deleted, _Date,
        Authors, Subject, _Matched, _Total),
    (
        strcase_str(Authors, Search)
    ;
        strcase_str(Subject, Search)
    ).

%-----------------------------------------------------------------------------%

:- pred modify_tag_cursor_line(
    pred(index_line, index_line, tag_delta)::in(pred(in, out, out) is det),
    screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

modify_tag_cursor_line(ModifyPred, Screen, !Info, !IO) :-
    Scrollable0 = !.Info ^ i_scrollable,
    ( get_cursor_line(Scrollable0, _Cursor0, CursorLine0) ->
        ThreadId = CursorLine0 ^ i_id,
        ModifyPred(CursorLine0, CursorLine, TagDelta),
        tag_thread([TagDelta], ThreadId, Res, !IO),
        (
            Res = ok,
            set_cursor_line(CursorLine, Scrollable0, Scrollable),
            !Info ^ i_scrollable := Scrollable,
            move_cursor(Screen, 1, _MessageUpdate, !Info)
        ;
            Res = error(Error),
            MessageUpdate = set_warning(io.error_message(Error)),
            update_message(Screen, MessageUpdate, !IO)
        )
    ;
        update_message(Screen, set_warning("No thread."), !IO)
    ).

:- pred toggle_unread(index_line::in, index_line::out, tag_delta::out) is det.

toggle_unread(Line0, Line, TagDelta) :-
    Unread0 = Line0 ^ i_unread,
    (
        Unread0 = unread,
        TagDelta = "-unread",
        Unread = read
    ;
        Unread0 = read,
        TagDelta = "+unread",
        Unread = unread
    ),
    Line = Line0 ^ i_unread := Unread.

%-----------------------------------------------------------------------------%

:- pred refresh_all(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

refresh_all(Screen, !Info, !IO) :-
    Terms = !.Info ^ i_search_terms,
    time(Time, !IO),
    search_terms_with_progress(Screen, Terms, Threads, !IO),
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
        Terms0 = !.Info ^ i_search_terms,
        SearchTime = !.Info ^ i_search_time,
        time_to_int(SearchTime, SearchTimeInt),
        string_to_search_terms(Terms0, Terms1, !IO),
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

:- pred draw_index_line(panel::in, index_line::in, bool::in,
    io::di, io::uo) is det.

draw_index_line(Panel, Line, IsCursor, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Deleted, Flagged,
        Date, Authors, Subject, Matched, Total),
    (
        IsCursor = yes,
        panel.attr_set(Panel, fg_bg(yellow, red) + bold, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, fg(blue) + bold, !IO)
    ),
    my_addstr_fixed(Panel, 11, Date, ' ', !IO),
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
    my_addstr(Panel, Subject, !IO).

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
