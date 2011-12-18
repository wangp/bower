% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module time_util.
:- interface.

:- import_module bool.
:- import_module time.

%-----------------------------------------------------------------------------%

:- pred timestamp_to_tm(int::in, tm::out) is det.

:- pred time_to_int(time_t::in, int::out) is det.

:- pred get_timezone(time_t::in, int::out) is det.

:- pred month_short_name(int, string).
:- mode month_short_name(in, out) is semidet.
:- mode month_short_name(out, in) is semidet.

:- pred weekday_short_name(int, string).
:- mode weekday_short_name(in, out) is semidet.
:- mode weekday_short_name(out, in) is semidet.

:- pred make_reldate(tm::in, tm::in, bool::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- pragma foreign_decl("C", local,
"
    #include <time.h>
").

%-----------------------------------------------------------------------------%

timestamp_to_tm(Timestamp, TM) :-
    timestamp_tm_2(Timestamp, Y, Mth, Md, H, Min, S, Yd, Wd, IsDst),
    ( IsDst = 0 ->
        MaybeDst = yes(standard_time)
    ; IsDst > 0 ->
        MaybeDst = yes(daylight_time)
    ;
        MaybeDst = no
    ),
    TM = tm(Y, Mth, Md, H, Min, S, Yd, Wd, MaybeDst).

:- pred timestamp_tm_2(int::in, int::out, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
    timestamp_tm_2(Timestamp::in, Y::out, Mth::out, Md::out,
        H::out, Min::out, S::out, Yd::out, Wd::out, IsDst::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    time_t t;
    struct tm tm;

    t = Timestamp;
    localtime_r(&t, &tm);
    Y   = tm.tm_year;
    Mth = tm.tm_mon;
    Md  = tm.tm_mday;
    H   = tm.tm_hour;
    Min = tm.tm_min;
    S   = tm.tm_sec;
    Yd  = tm.tm_yday;
    Wd  = tm.tm_wday;
    IsDst = tm.tm_isdst;
").

:- pragma foreign_proc("C",
    time_to_int(Time::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Int = Time;
").

:- pragma foreign_proc("C",
    get_timezone(Time::in, Offset::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    time_t t;
    struct tm tm;

    t = Time;
    localtime_r(&t, &tm);
    Offset = tm.tm_gmtoff; /* GNU extension */
").

month_short_name(1, "Jan").
month_short_name(2, "Feb").
month_short_name(3, "Mar").
month_short_name(4, "Apr").
month_short_name(5, "May").
month_short_name(6, "Jun").
month_short_name(7, "Jul").
month_short_name(8, "Aug").
month_short_name(9, "Sep").
month_short_name(10, "Oct").
month_short_name(11, "Nov").
month_short_name(12, "Dec").

weekday_short_name(0, "Sun").
weekday_short_name(1, "Mon").
weekday_short_name(2, "Tue").
weekday_short_name(3, "Wed").
weekday_short_name(4, "Thu").
weekday_short_name(5, "Fri").
weekday_short_name(6, "Sat").

make_reldate(Nowish, TM, Shorter, String) :-
    NowYear = 1900 + Nowish ^ tm_year,
    NowMonth = 1 + Nowish ^ tm_mon,
    NowDay = Nowish ^ tm_mday,
    NowYday = Nowish ^ tm_yday,

    Year = 1900 + TM ^ tm_year,
    Month = 1 + TM ^ tm_mon,
    Day = TM ^ tm_mday,
    Hour = TM ^ tm_hour,
    Min = TM ^ tm_min,
    Yday = TM ^ tm_yday,
    Wday = TM ^ tm_wday,

    (
        Year = NowYear,
        Month = NowMonth,
        Day = NowDay
    ->
        (
            Shorter = yes,
            String = string.format("%02d:%02d", [i(Hour), i(Min)])
        ;
            Shorter = no,
            String = string.format("Today %02d:%02d", [i(Hour), i(Min)])
        )
    ;
        (NowYear * 365 + NowYday) - (Year * 365 + Yday) < 7
    ->
        ( weekday_short_name(Wday, WdayName) ->
            String = string.format("%s %02d:%02d",
                [s(WdayName), i(Hour), i(Min)])
        ;
            unexpected($module, $pred, "bad weekday")
        )
    ;
        Year = NowYear,
        month_short_name(Month, MonthName)
    ->
        (
            Shorter = yes,
            String = string.format("%02d %s", [i(Day), s(MonthName)])
        ;
            Shorter = no,
            String = string.format("%02d %s %02d:%02d",
                [i(Day), s(MonthName), i(Hour), i(Min)])
        )
    ;
        String = string.format("%04d-%02d-%02d", [i(Year), i(Month), i(Day)])
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
