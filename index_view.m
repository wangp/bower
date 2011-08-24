%-----------------------------------------------------------------------------%

:- module index_view.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type index_info.

:- pred setup_index_view(list(thread)::in, index_info::out, io::di, io::uo)
    is det.

:- pred draw_index_view(screen::in, index_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module string.
:- import_module time.

:- import_module curs.
:- import_module curs.panel.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type index_info == cord(index_line).

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

%-----------------------------------------------------------------------------%

setup_index_view(Threads, Lines, !IO) :-
    time(Time, !IO),
    Nowish = localtime(Time),
    list.foldl(add_thread(Nowish), Threads, cord.init, Lines).

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

draw_index_view(Screen, Lines, !IO) :-
    MainPanels = Screen ^ main_panels,
    draw_index_lines(MainPanels, Lines, !IO).

:- pred draw_index_lines(list(panel)::in, cord(index_line)::in,
    io::di, io::uo) is det.

draw_index_lines(Panels, IndexLines, !IO) :-
    (
        Panels = []
    ;
        Panels = [Panel | RestPanels],
        panel.erase(Panel, !IO),
        ( cord.head_tail(IndexLines, IndexLine, RestIndexLines) ->
            draw_index_line(Panel, IndexLine, !IO),
            draw_index_lines(RestPanels, RestIndexLines, !IO)
        ;
            draw_index_lines(RestPanels, IndexLines, !IO)
        )
    ).

:- pred draw_index_line(panel::in, index_line::in, io::di, io::uo) is det.

draw_index_line(Panel, Line, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Flagged, Date, Authors,
        Subject, Total),
    panel.attr_set(Panel, fg(blue) + bold, !IO),
    my_addstr(Panel, Date, !IO),
    my_addstr(Panel, " ", !IO),
    (
        Unread = unread,
        Base = bold
    ;
        Unread = read,
        Base = normal
    ),
    panel.attr_set(Panel, Base, !IO),
    (
        Replied = replied,
        my_addstr(Panel, "r", !IO)
    ;
        Replied = not_replied,
        my_addstr(Panel, " ", !IO)
    ),
    (
        Flagged = flagged,
        panel.attr_set(Panel, fg(red) + bold, !IO),
        my_addstr(Panel, "! ", !IO)
    ;
        Flagged = unflagged,
        my_addstr(Panel, "  ", !IO)
    ),
    panel.attr_set(Panel, fg(cyan) + Base, !IO),
    my_addstr_fixed(Panel, 25, Authors, !IO),
    panel.attr_set(Panel, fg(green) + Base, !IO),
    my_addstr(Panel, format(" %-3d ", [i(Total)]), !IO),
    panel.attr_set(Panel, Base, !IO),
    my_addstr(Panel, Subject, !IO).

:- func fg(colour) = attr.

fg(C) = curs.fg_bg(C, black).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
