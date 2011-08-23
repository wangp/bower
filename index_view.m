%-----------------------------------------------------------------------------%

:- module index_view.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.

:- pred index_view(list(thread)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module string.
:- import_module time.

:- import_module ansi_color.
:- import_module time_util.

%-----------------------------------------------------------------------------%

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

index_view(Threads, !IO) :-
    time(Time, !IO),
    Nowish = localtime(Time),
    list.foldl(add_thread(Nowish), Threads, cord.init, Lines),
    cord.foldl_pred(print_line, Lines, !IO).

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

:- pred print_line(index_line::in, io::di, io::uo) is det.

print_line(Line, !IO) :-
    Line = index_line(_Id, _New, Unread, Replied, Flagged, Date, Authors,
        Subject, Total),
    io.write_string(ansi_bright_blue, !IO),
    io.write_string(Date, !IO),
    io.write_string(" ", !IO),
    (
        Unread = unread,
        Base = ansi_bright_white
    ;
        Unread = read,
        Base = ansi_reset
    ),
    io.write_string(Base, !IO),
    (
        Replied = replied,
        io.write_string("r", !IO)
    ;
        Replied = not_replied,
        io.write_string(" ", !IO)
    ),
    (
        Flagged = flagged,
        io.write_string(ansi_bright_red, !IO),
        io.write_string("! ", !IO)
    ;
        Flagged = unflagged,
        io.write_string("  ", !IO)
    ),
    io.write_string(ansi_cyan, !IO),
    io.write_string(Authors, !IO),
    io.write_string(ansi_green, !IO),
    io.format(" %-3d ", [i(Total)], !IO),
    io.write_string(Base, !IO),
    io.write_string(Subject, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
