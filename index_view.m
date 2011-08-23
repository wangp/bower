%-----------------------------------------------------------------------------%

:- module index_view.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.
:- import_module views.

%-----------------------------------------------------------------------------%

:- type index_info.

:- pred setup_index_view(list(thread)::in, index_info::out, io::di, io::uo)
    is det.

:- pred draw_index_view(panels::in, index_info::in, io::di, io::uo) is det.

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
    Thread = thread(Id, Timestamp, Authors0, Subject, Tags, _Matched, Total),
    make_date_column(Nowish, Timestamp, Date),
    make_column(25, Authors0, Authors),
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

:- pred make_column(int::in, string::in, string::out) is det.

make_column(ColumnWidth, S0, S) :-
    % XXX not the proper width
    string.pad_right(S0, ' ', ColumnWidth, S1),
    string.left(S1, ColumnWidth, S).

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

draw_index_view(Panels, Lines, !IO) :-
    Panels = panels(_Rows, _Cols, MainPanel, _BarPanel, _MsgEntryPanel),
    panel.clear(MainPanel, !IO),
    cord.foldl_pred(draw_index_line(MainPanel), Lines, !IO).

:- pred draw_index_line(panel::in, index_line::in, io::di, io::uo) is det.

draw_index_line(Panel, Line, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Flagged, Date, Authors,
        Subject, Total),
    % XXX we need to truncate long lines
    panel.attr_set(Panel, fg(blue) + bold, !IO),
    panel.addstr(Panel, Date, !IO),
    panel.addstr(Panel, " ", !IO),
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
        panel.addstr(Panel, "r", !IO)
    ;
        Replied = not_replied,
        panel.addstr(Panel, " ", !IO)
    ),
    (
        Flagged = flagged,
        panel.attr_set(Panel, fg(red) + bold, !IO),
        panel.addstr(Panel, "! ", !IO)
    ;
        Flagged = unflagged,
        panel.addstr(Panel, "  ", !IO)
    ),
    panel.attr_set(Panel, fg(cyan) + Base, !IO),
    panel.addstr(Panel, Authors, !IO),
    panel.attr_set(Panel, fg(green) + Base, !IO),
    panel.addstr(Panel, format(" %-3d ", [i(Total)]), !IO),
    panel.attr_set(Panel, Base, !IO),
    panel.addstr(Panel, Subject, !IO),
    panel.addstr(Panel, "\n", !IO).

:- func fg(colour) = attr.

fg(C) = curs.fg_bg(C, black).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
