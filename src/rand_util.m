% Bower - a frontend for the Notmuch email system
% Copyright (C) 2026 Peter Wang

:- module rand_util.
:- interface.

:- import_module io.

:- type rs.

    % There is no set_random_supply as the random state is destructively
    % updated anyway, and we do not need to (or particularly want to)
    % require the caller to put the random supply back.
    %
:- pred get_random_supply(rs::uo, io::di, io::uo) is det.

    % Note: generating random numbers is not thread-safe.
    %
:- pred generate_uint32(uint32::out, rs::di, rs::uo) is det.
:- pred generate_uint64(uint64::out, rs::di, rs::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module random.
:- import_module random.sfc64.
:- import_module uint64.

:- type rs
    --->    rs(sfc64.ustate).   % destructively updated

:- mutable(random, maybe(rs), no, ground, [untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
    #include <sys/time.h>
").

:- pred gettimeofday(int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gettimeofday(Sec::out, Usec::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    Sec = tv.tv_sec;
    Usec = tv.tv_usec;
").

%-----------------------------------------------------------------------------%

get_random_supply(RS, !IO) :-
    get_random(MaybeRS, !IO),
    (
        MaybeRS = yes(RS0),
        RS = unsafe_promise_unique(RS0)
    ;
        MaybeRS = no,
        % If we bump the Mercury version requirement, we can use the
        % random.system_rng module to seed the RNG. Until then, seeding it
        % with the time also works.
        gettimeofday(A, B, !IO),
        sfc64.seed(cast_from_int(A), cast_from_int(B), 0xBF58476D1CE4E5B9_u64,
            _Params, State),
        RS = rs(State)
    ).

%-----------------------------------------------------------------------------%

generate_uint32(N, RS0, RS) :-
    RS0 = rs(State0),
    sfc64.generate_uint32(N, State0, State),
    RS = rs(State).

generate_uint64(N, RS0, RS) :-
    RS0 = rs(State0),
    sfc64.generate_uint64(N, State0, State),
    RS = rs(State).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
