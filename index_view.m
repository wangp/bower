%-----------------------------------------------------------------------------%

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
:- import_module recall.
:- import_module scrollable.
:- import_module string_util.
:- import_module text_entry.
:- import_module thread_pager.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type index_info
    --->    index_info(
                i_scrollable        :: scrollable(index_line),
                i_search_terms      :: string,
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

:- type flagged
    --->    flagged
    ;       unflagged.

:- type binding
    --->    scroll_down
    ;       scroll_up
    ;       skip_to_unread
    ;       enter
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_search
    ;       skip_to_search
    ;       quit.

:- type action
    --->    continue
    ;       open_pager(thread_id)
    ;       enter_limit
    ;       refresh_all
    ;       start_compose
    ;       start_recall
    ;       prompt_search
    ;       quit.

:- instance scrollable.line(index_line) where [
    pred(draw_line/5) is draw_index_line
].

%-----------------------------------------------------------------------------%

open_index(Screen, Terms, !IO) :-
    ( Terms = "" ->
        Threads = [],
        History = init_history
    ;
        search_terms_with_progress(Screen, Terms, Threads, !IO),
        add_history_nodup(Terms, init_history, History)
    ),
    setup_index_scrollable_now(Threads, Scrollable, !IO),
    MaybeSearch = no,
    IndexInfo = index_info(Scrollable, Terms, MaybeSearch, History,
        init_history, init_compose_history),
    index_loop(Screen, IndexInfo, !IO).

:- pred search_terms_with_progress(screen::in, string::in,
    list(thread)::out, io::di, io::uo) is det.

search_terms_with_progress(Screen, Terms, Threads, !IO) :-
    update_message(Screen, set_info("Searching..."), !IO),
    panel.update_panels(!IO),
    run_notmuch([
        "search", "--format=json", "--", Terms
    ], parse_threads_list, Result, !IO),
    (
        Result = ok(Threads),
        string.format("Found %d threads.", [i(length(Threads))], Message),
        MessageUpdate = set_info(Message)
    ;
        Result = error(Error),
        Message = "Error: " ++ io.error_message(Error),
        MessageUpdate = set_warning(Message),
        Threads = []
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred setup_index_scrollable_now(list(thread)::in,
    scrollable(index_line)::out, io::di, io::uo) is det.

setup_index_scrollable_now(Threads, Scrollable, !IO) :-
    time(Time, !IO),
    Nowish = localtime(Time),
    setup_index_scrollable(Nowish, Threads, Scrollable).

:- pred setup_index_scrollable(tm::in, list(thread)::in,
    scrollable(index_line)::out) is det.

setup_index_scrollable(Nowish, Threads, Scrollable) :-
    list.foldl(add_thread(Nowish), Threads, cord.init, LinesCord),
    Lines = list(LinesCord),
    (
        Lines = [],
        Scrollable = scrollable.init(Lines)
    ;
        Lines = [_ | _],
        Cursor = 0,
        Scrollable = scrollable.init_with_cursor(Lines, Cursor)
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
    Line0 = index_line(Id, old, read, not_replied, unflagged, Date, Authors,
        Subject, Matched, Total),
    list.foldl(apply_tag, Tags, Line0, Line).

:- pred apply_tag(string::in, index_line::in, index_line::out) is det.

apply_tag(Tag, !Line) :-
    ( Tag = "new" ->
        !Line ^ i_new := new
    ; Tag = "unread" ->
        !Line ^ i_unread := unread
    ; Tag = "replied" ->
        !Line ^ i_replied := replied
    ; Tag = "flagged" ->
        !Line ^ i_flagged := flagged
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred index_loop(screen::in, index_info::in, io::di, io::uo) is det.

index_loop(Screen, !.IndexInfo, !IO) :-
    draw_index_view(Screen, !.IndexInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    index_view_input(Screen, Char, MessageUpdate, Action, !IndexInfo),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = open_pager(ThreadId),
        History0 = !.IndexInfo ^ i_search_history,
        open_thread_pager(Screen, ThreadId, NeedRefresh, History0, History,
            !IO),
        !IndexInfo ^ i_search_history := History,
        (
            NeedRefresh = yes,
            refresh_index_line(Screen, ThreadId, !IndexInfo, !IO)
        ;
            NeedRefresh = no
        ),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = enter_limit,
        History0 = !.IndexInfo ^ i_limit_history,
        text_entry(Screen, "Limit to messages matching: ", History0,
            Return, !IO),
        (
            Return = yes(Terms),
            search_terms_with_progress(Screen, Terms, Threads, !IO),
            setup_index_scrollable_now(Threads, Scrollable, !IO),
            add_history_nodup(Terms, History0, History),
            !IndexInfo ^ i_scrollable := Scrollable,
            !IndexInfo ^ i_search_terms := Terms,
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
        Action = quit
    ).

%-----------------------------------------------------------------------------%

:- pred index_view_input(screen::in, char::in, message_update::out,
    action::out, index_info::in, index_info::out) is det.

index_view_input(Screen, Char, MessageUpdate, Action, !IndexInfo) :-
    ( key_binding(Char, Binding) ->
        (
            Binding = scroll_down,
            move_cursor(Screen, 1, MessageUpdate, !IndexInfo),
            Action = continue
        ;
            Binding = scroll_up,
            move_cursor(Screen, -1, MessageUpdate, !IndexInfo),
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
            Binding = quit,
            MessageUpdate = no_change,
            Action = quit
        )
    ;
        MessageUpdate = no_change,
        Action = continue
    ).

:- pred key_binding(char::in, binding::out) is semidet.

key_binding('=', refresh_all).
key_binding('j', scroll_down).
key_binding('k', scroll_up).
key_binding('\t', skip_to_unread).
key_binding('\r', enter).
key_binding('l', enter_limit).
key_binding('m', start_compose).
key_binding('R', start_recall).
key_binding('/', prompt_search).
key_binding('n', skip_to_search).
key_binding('q', quit).

:- pred move_cursor(screen::in, int::in, message_update::out,
    index_info::in, index_info::out) is det.

move_cursor(Screen, Delta, MessageUpdate, !Info) :-
    Scrollable0 = !.Info ^ i_scrollable,
    NumRows = list.length(Screen ^ main_panels),
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
    NumRows = list.length(Screen ^ main_panels),
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
    text_entry(Screen, "Search for: ", History0, Return, !IO),
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
        NumRows = list.length(Screen ^ main_panels),
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
    Line = index_line(_Id, _New, _Unread, _Replied, _Flagged, _Date,
        Authors, Subject, _Matched, _Total),
    (
        strcase_str(Authors, Search)
    ;
        strcase_str(Subject, Search)
    ).

%-----------------------------------------------------------------------------%

:- pred refresh_all(screen::in, index_info::in, index_info::out,
    io::di, io::uo) is det.

refresh_all(Screen, !Info, !IO) :-
    Terms = !.Info ^ i_search_terms,
    search_terms_with_progress(Screen, Terms, Threads, !IO),
    some [!Scrollable] (
        Scrollable0 = !.Info ^ i_scrollable,
        Top0 = get_top(Scrollable0),
        setup_index_scrollable_now(Threads, !:Scrollable, !IO),
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
                NumRows = list.length(Screen ^ main_panels),
                set_cursor_visible(Cursor, NumRows, !Scrollable)
            ;
                true
            )
        ;
            true
        ),
        !Info ^ i_scrollable := !.Scrollable
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

:- pred draw_index_view(screen::in, index_info::in, io::di, io::uo) is det.

draw_index_view(Screen, Info, !IO) :-
    MainPanels = Screen ^ main_panels,
    Scrollable = Info ^ i_scrollable,
    scrollable.draw(MainPanels, Scrollable, !IO).

:- pred draw_index_line(panel::in, index_line::in, bool::in,
    io::di, io::uo) is det.

draw_index_line(Panel, Line, IsCursor, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Flagged, Date, Authors,
        Subject, Matched, Total),
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
