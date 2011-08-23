%-----------------------------------------------------------------------------%

:- module time_util.
:- interface.

:- import_module time.

%-----------------------------------------------------------------------------%

:- pred timestamp_to_tm(int::in, tm::out) is det.

:- pred month_short_name(int, string).
:- mode month_short_name(in, out) is semidet.
:- mode month_short_name(out, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.

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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
