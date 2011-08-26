%-----------------------------------------------------------------------------%

:- module index_view.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type index_info.

:- pred setup_index_view(list(thread)::in, index_info::out, io::di, io::uo)
    is det.

:- type action
    --->    continue
    ;       open_pager(thread_id)
    ;       enter_limit
    ;       quit.

:- pred index_view_input(screen::in, char::in, action::out,
    index_info::in, index_info::out) is det.

:- pred draw_index_view(screen::in, index_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module string.
:- import_module time.

:- import_module curs.
:- import_module curs.panel.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type index_info
    --->    index_info(
                i_lines     :: list(index_line),
                i_numlines  :: int,
                i_top       :: int,
                i_cursor    :: int
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
    ;       enter
    ;       enter_limit
    ;       quit.

%-----------------------------------------------------------------------------%

setup_index_view(Threads, Info, !IO) :-
    time(Time, !IO),
    Nowish = localtime(Time),
    list.foldl(add_thread(Nowish), Threads, cord.init, LinesCord),
    Lines = list(LinesCord),
    NumLines = list.length(Lines),
    Top = 0,
    Cursor = 0,
    Info = index_info(Lines, NumLines, Top, Cursor).

:- pred add_thread(tm::in, thread::in,
    cord(index_line)::in, cord(index_line)::out) is det.

add_thread(Nowish, Thread, !Lines) :-
    Thread = thread(Id, Timestamp, Authors, Subject, Tags, _Matched, Total),
    make_date_column(Nowish, Timestamp, Date),
    Line0 = index_line(Id, old, read, not_replied, unflagged, Date, Authors,
        Subject, Total),
    list.foldl(apply_tag, Tags, Line0, Line),
    snoc(Line, !Lines).

:- pred make_date_column(tm::in, int::in, string::out) is det.

make_date_column(Nowish, Timestamp, String) :-
    NowYear = 1900 + Nowish ^ tm_year,
    NowMonth = 1 + Nowish ^ tm_mon,
    NowDay = Nowish ^ tm_mday,

    timestamp_to_tm(Timestamp, TM),
    Year = 1900 + TM ^ tm_year,
    Month = 1 + TM ^ tm_mon,
    Day = TM ^ tm_mday,
    Hour = TM ^ tm_hour,
    Min = TM ^ tm_min,

    (
        Year = NowYear,
        Month = NowMonth,
        Day = NowDay
    ->
        String = string.format("%02d:%02d     ", [i(Hour), i(Min)])
    ;
        Year = NowYear,
        month_short_name(Month, MonthName)
    ->
        String = string.format("%s %02d    ", [s(MonthName), i(Day)])
    ;
        String = string.format("%04d-%02d-%02d", [i(Year), i(Month), i(Day)])
    ).

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

index_view_input(Screen, Char, Action, !IndexInfo) :-
    ( key_binding(Char, Binding) ->
        (
            Binding = scroll_down,
            cursor_down(Screen, !IndexInfo),
            Action = continue
        ;
            Binding = scroll_up,
            cursor_up(!IndexInfo),
            Action = continue
        ;
            Binding = enter,
            enter(!.IndexInfo, Action)
        ;
            Binding = enter_limit,
            Action = enter_limit
        ;
            Binding = quit,
            Action = quit
        )
    ;
        Action = continue
    ).

:- pred key_binding(char::in, binding::out) is semidet.

key_binding('j', scroll_down).
key_binding('k', scroll_up).
key_binding('\r', enter).
key_binding('l', enter_limit).
key_binding('q', quit).

:- pred cursor_down(screen::in, index_info::in, index_info::out) is det.

cursor_down(Screen, !Info) :-
    Rows = list.length(Screen ^ main_panels),
    !.Info = index_info(Lines, NumLines, Top0, Cursor0),
    Cursor = int.min(Cursor0 + 1, NumLines - 1),
    Top = int.max(Cursor - Rows + 1, Top0),
    !:Info = index_info(Lines, NumLines, Top, Cursor).

:- pred cursor_up(index_info::in, index_info::out) is det.

cursor_up(!Info) :-
    !.Info = index_info(Lines, NumLines, Top0, Cursor0),
    int.max(0, Cursor0 - 1, Cursor),
    int.min(Cursor, Top0, Top),
    !:Info = index_info(Lines, NumLines, Top, Cursor).

:- pred enter(index_info::in, action::out) is det.

enter(Info, Action) :-
    IndexLines = Info ^ i_lines,
    Cursor = Info ^ i_cursor,
    ( list.index0(IndexLines, Cursor, CursorLine) ->
        ThreadId = CursorLine ^ i_id,
        Action = open_pager(ThreadId)
    ;
        Action = continue
    ).

%-----------------------------------------------------------------------------%

draw_index_view(Screen, Info, !IO) :-
    MainPanels = Screen ^ main_panels,
    Info = index_info(Lines0, _NumLines, Top, Cursor),
    ( list.drop(Top, Lines0, Lines1) ->
        Lines = Lines1
    ;
        Lines = []
    ),
    draw_index_lines(MainPanels, Lines, Top, Cursor, !IO).

:- pred draw_index_lines(list(panel)::in, list(index_line)::in,
    int::in, int::in, io::di, io::uo) is det.

draw_index_lines(Panels, IndexLines, Cur, Cursor, !IO) :-
    (
        Panels = []
    ;
        Panels = [Panel | RestPanels],
        panel.erase(Panel, !IO),
        (
            IndexLines = [IndexLine | RestIndexLines],
            ( Cur = Cursor ->
                IsCursor = yes
            ;
                IsCursor = no
            ),
            draw_index_line(Panel, IndexLine, IsCursor, !IO)
        ;
            IndexLines = [],
            RestIndexLines = []
        ),
        draw_index_lines(RestPanels, RestIndexLines, Cur + 1, Cursor, !IO)
    ).

:- pred draw_index_line(panel::in, index_line::in, bool::in,
    io::di, io::uo) is det.

draw_index_line(Panel, Line, IsCursor, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Flagged, Date, Authors,
        Subject, Total),
    (
        IsCursor = yes,
        panel.attr_set(Panel, fg_bg(yellow, red) + bold, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, fg(blue) + bold, !IO)
    ),
    my_addstr(Panel, Date, !IO),
    my_addstr(Panel, " ", !IO),
    (
        Unread = unread,
        Base = bold
    ;
        Unread = read,
        Base = normal
    ),
    cond_attr_set(Panel, Base, IsCursor, !IO),
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
    cond_attr_set(Panel, fg(cyan) + Base, IsCursor, !IO),
    my_addstr_fixed(Panel, 25, Authors, !IO),
    cond_attr_set(Panel, fg(green) + Base, IsCursor, !IO),
    my_addstr(Panel, format(" %-3d ", [i(Total)]), !IO),
    cond_attr_set(Panel, Base, IsCursor, !IO),
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
