% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module time_util.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module time.

%-----------------------------------------------------------------------------%

    % Seconds since Unix epoch 1970-01-01 00:00:00 +0000 (UTC)
    % The value should be an integer only.  Double-precision floats can
    % represent a integral values up to 2^53 precisely, compared to 32-bit
    % signed integers that will overflow some time in the year 2038.
    % (Not that we expect to be running on 32-bit machines then.)
    %
:- type timestamp
    --->    timestamp(float).

:- func timestamp + float = timestamp.

:- func timestamp - timestamp = float.

:- func timestamp_to_int_string(timestamp) = string.

:- pred current_timestamp(timestamp::out, io::di, io::uo) is det.

:- pred localtime(timestamp::in, tm::out, io::di, io::uo) is det.
:- pred localtime(timestamp::in, tm::out, int::out, io::di, io::uo) is det.

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

:- import_module float.
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

timestamp(T) + Secs = timestamp(T + Secs).

timestamp(Ta) - timestamp(Tb) = Ta - Tb.

%-----------------------------------------------------------------------------%

timestamp_to_int_string(timestamp(Timestamp)) =
    timestamp_to_int_string_2(Timestamp).

:- func timestamp_to_int_string_2(float) = string.

:- pragma foreign_proc("C",
    timestamp_to_int_string_2(Timestamp::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = MR_make_string(MR_ALLOC_ID, ""%lld"", (long long) Timestamp);
").

%-----------------------------------------------------------------------------%

current_timestamp(timestamp(Timestamp), !IO) :-
    current_timestamp_2(Timestamp, !IO).

:- pred current_timestamp_2(float::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    current_timestamp_2(Timestamp::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Timestamp = time(0);
").

%-----------------------------------------------------------------------------%

localtime(Timestamp, TM, !IO) :-
    localtime(Timestamp, TM, _GMTOff, !IO).

localtime(timestamp(Timestamp), TM, GMTOff, !IO) :-
    localtime_2(Timestamp, Y, Mth, Md, H, Min, S, Yd, Wd, IsDst, GMTOff, !IO),
    ( IsDst = 0 ->
        MaybeDst = yes(standard_time)
    ; IsDst > 0 ->
        MaybeDst = yes(daylight_time)
    ;
        MaybeDst = no
    ),
    TM = tm(Y, Mth, Md, H, Min, S, Yd, Wd, MaybeDst).

:- pred localtime_2(float::in, int::out, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    localtime_2(Timestamp::in, Y::out, Mth::out, Md::out,
        H::out, Min::out, S::out, Yd::out, Wd::out, IsDst::out, GMTOff::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
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
    GMTOff = tm.tm_gmtoff; /* GNU extension */
").

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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
        String = reldate_today(Shorter, Hour, Min)
    ;
        Days0 = NowYear * 365 + NowYday,
        Days1 = Year * 365 + Yday,
        Diff = Days0 - Days1,
        Diff >= 0,  % don't use this format for timestamps in future
        Diff < 7
    ->
        String = reldate_weekday(Wday, Hour, Min)
    ;
        Year = NowYear
    ->
        String = reldate_month_day(Shorter, Month, Day, Hour, Min)
    ;
        String = string.format("%04d-%02d-%02d", [i(Year), i(Month), i(Day)])
    ).

:- func reldate_today(bool, int, int) = string.

reldate_today(Shorter, Hour, Min) = String :-
    (
        Shorter = yes,
        String = string.format("%02d:%02d", [i(Hour), i(Min)])
    ;
        Shorter = no,
        String = string.format("Today %02d:%02d", [i(Hour), i(Min)])
    ).

:- func reldate_weekday(int, int, int) = string.

reldate_weekday(Wday, Hour, Min) = String :-
    ( weekday_short_name(Wday, WdayName) ->
        String = string.format("%s %02d:%02d", [s(WdayName), i(Hour), i(Min)])
    ;
        unexpected($module, $pred, "bad weekday")
    ).

:- func reldate_month_day(bool, int, int, int, int) = string.

reldate_month_day(Shorter, Month, Day, Hour, Min) = String :-
    ( month_short_name(Month, MonthName) ->
        (
            Shorter = yes,
            String = string.format("%02d %s", [i(Day), s(MonthName)])
        ;
            Shorter = no,
            String = string.format("%02d %s %02d:%02d",
                [i(Day), s(MonthName), i(Hour), i(Min)])
        )
    ;
        unexpected($module, $pred, "bad month")
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
