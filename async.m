% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module async.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type async_op
    --->    async_shell_command(
                command_prefix      :: string,          % will not be quoted
                command_args        :: list(string),    % will be quoted
                remaining_attempts  :: int
            ).

:- type async_return
    --->    none
    ;       child_succeeded
    ;       child_failed(
                child_op        :: async_op,
                child_failure   :: async_failure
            ).

:- type async_failure
    --->    failure_nonzero_exit(int)
    ;       failure_signal(int)
    ;       failure_abnormal_exit
    ;       failure_error(io.error).

:- pred push_async(async_op::in, io::di, io::uo) is det.

:- pred retry_async(int::in, async_op::in, io::di, io::uo) is det.

:- pred poll_async_nonblocking(async_return::out, io::di, io::uo) is det.

:- pred poll_async_blocking(async_return::out, io::di, io::uo) is det.

:- pred async_count(int::out, io::di, io::uo) is det.

:- pred have_child_process(bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred install_sigchld_handler(io::di, io::uo) is det.

    % We may receive SIGCHLD signals for other child processes,
    % not just processes spawned by this module.
    %
:- pred received_sigchld_since_spawn(bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module queue.
:- import_module require.
:- import_module string.

:- import_module process.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

:- type async_info
    --->    async_info(
                ai_queue            :: queue(queue_elem),
                % Current child process if any, and the SIGCHLD count
                % before the last async process was started.
                ai_maybe_pid        :: maybe(pid),
                ai_current_op       :: async_op,
                ai_sigchld_count    :: int
            ).

:- type queue_elem
    --->    q_op(async_op)
    ;       q_timeout(timeout).

:- type timeout
    --->    timeout(int, int).

:- mutable(async_info, async_info, init_async_info, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

:- func init_async_info = async_info.

init_async_info = async_info(queue.init, no, dummy_op, 0).

:- func dummy_op = async_op.

dummy_op = async_shell_command("false", [], 0).

%-----------------------------------------------------------------------------%

push_async(Op, !IO) :-
    get_async_info(Info0, !IO),
    Queue0 = Info0 ^ ai_queue,
    queue.put(q_op(Op), Queue0, Queue),
    Info = Info0 ^ ai_queue := Queue,
    set_async_info(Info, !IO).

retry_async(Delay, Op, !IO) :-
    get_async_info(Info0, !IO),
    Queue0 = Info0 ^ ai_queue,
    queue.put_on_front(q_op(Op), Queue0, Queue1),
    ( Delay > 0 ->
        make_timeout(Delay, Timeout, !IO),
        queue.put_on_front(q_timeout(Timeout), Queue1, Queue)
    ;
        Queue = Queue1
    ),
    Info = Info0 ^ ai_queue := Queue,
    set_async_info(Info, !IO).

poll_async_nonblocking(Return, !IO) :-
    get_async_info(Info0, !IO),
    do_poll(nonblocking, Return, Info0, Info, !IO),
    set_async_info(Info, !IO).

poll_async_blocking(Return, !IO) :-
    get_async_info(Info0, !IO),
    do_poll(blocking, Return, Info0, Info, !IO),
    set_async_info(Info, !IO).

async_count(Count, !IO) :-
    get_async_info(Info, !IO),
    Queue = Info ^ ai_queue,
    queue.length(Queue, QueueLength),
    MaybePid = Info ^ ai_maybe_pid,
    (
        MaybePid = yes(_),
        Count = 1 + QueueLength
    ;
        MaybePid = no,
        Count = QueueLength
    ).

have_child_process(HaveChild, !IO) :-
    get_async_info(Info, !IO),
    MaybePid = Info ^ ai_maybe_pid,
    (
        MaybePid = yes(_),
        HaveChild = yes
    ;
        MaybePid = no,
        HaveChild = no
    ).

%-----------------------------------------------------------------------------%

:- pred do_poll(wait_pid_blocking::in, async_return::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

do_poll(Blocking, Return, !Info, !IO) :-
    MaybePid = !.Info ^ ai_maybe_pid,
    (
        MaybePid = no,
        start_next_op(Return, !Info, !IO)
    ;
        MaybePid = yes(Pid),
        do_poll_in_progress(Pid, Blocking, Return, !Info, !IO)
    ).

:- pred do_poll_in_progress(pid::in, wait_pid_blocking::in, async_return::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

do_poll_in_progress(Pid, Blocking, Return, !Info, !IO) :-
    Op = !.Info ^ ai_current_op,
    wait_pid(Pid, Blocking, WaitRes, !IO),
    (
        WaitRes = no_hang,
        % Child not done.
        Return = none
    ;
        (
            WaitRes = child_exit(Status),
            ( Status = 0 ->
                Return = child_succeeded
            ;
                Return = child_failed(Op, failure_nonzero_exit(Status))
            )
        ;
            WaitRes = child_signalled(Signal),
            Return = child_failed(Op, failure_signal(Signal))
        ;
            WaitRes = child_abnormal_exit,
            Return = child_failed(Op, failure_abnormal_exit)
        ;
            WaitRes = error(Error),
            Return = child_failed(Op, failure_error(Error))
        ),
        !Info ^ ai_maybe_pid := no,
        !Info ^ ai_current_op := dummy_op
    ).

:- pred start_next_op(async_return::out, async_info::in, async_info::out,
    io::di, io::uo) is det.

start_next_op(Return, !Info, !IO) :-
    Queue0 = !.Info ^ ai_queue,
    ( queue.get(Next, Queue0, Queue) ->
        (
            Next = q_op(Op),
            !Info ^ ai_queue := Queue,
            start_op(Op, Return, !Info, !IO)
        ;
            Next = q_timeout(Timeout),
            check_timeout(Timeout, TimedOut, !IO),
            (
                TimedOut = yes,
                !Info ^ ai_queue := Queue,
                start_next_op(Return, !Info, !IO)
            ;
                TimedOut = no,
                Return = none
            )
        )
    ;
        Return = none
    ).

:- pred start_op(async_op::in, async_return::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

start_op(Op, Return, !Info, !IO) :-
    MaybePid0 = !.Info ^ ai_maybe_pid,
    (
        MaybePid0 = yes(_),
        unexpected($module, $pred, "already have child process")
    ;
        MaybePid0 = no
    ),

    get_sigchld_count(PreSigchldCount, !IO),
    Op = async_shell_command(CommandPrefix, UnquotedArgs, _RemainingAttempts),
    QuotedArgs = list.map(quote_arg, UnquotedArgs),
    ShellCommand = string.join_list(" ", [CommandPrefix | QuotedArgs]),
    Shell = "/bin/sh",
    ShellArgs = ["-c", ShellCommand],
    posix_spawn(Shell, ShellArgs, SpawnRes, !IO),
    (
        SpawnRes = ok(Pid),
        !Info ^ ai_maybe_pid := yes(Pid),
        !Info ^ ai_current_op := Op,
        !Info ^ ai_sigchld_count := PreSigchldCount,
        Return = none
    ;
        SpawnRes = error(Error),
        Return = child_failed(Op, failure_error(Error))
    ).

%-----------------------------------------------------------------------------%

% SIGCHLD handler

:- pragma foreign_decl("C", local,
"
#include <signal.h>

static sig_atomic_t sigchld_count;
static void         sigchld_handler(int sig);
").

:- pragma foreign_code("C",
"
static void
sigchld_handler(int sig)
{
    (void) sig;
    sigchld_count++;
}
").

:- pragma foreign_proc("C",
    install_sigchld_handler(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = sigchld_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_NOCLDSTOP;

    sigaction(SIGCHLD, &act, NULL);
").

:- pred get_sigchld_count(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_sigchld_count(N::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    N = sigchld_count;
").

received_sigchld_since_spawn(Increased, !IO) :-
    get_async_info(Info0, !IO),
    Count0 = Info0 ^ ai_sigchld_count,
    get_sigchld_count(Count, !IO),
    Increased = ( Count > Count0 -> yes ; no ).

%-----------------------------------------------------------------------------%

% Timeouts

:- pred make_timeout(int::in, timeout::out, io::di, io::uo) is det.

make_timeout(DelaySecs, timeout(TimeoutSec, TimeoutUsec), !IO) :-
    make_timeout_2(DelaySecs, TimeoutSec, TimeoutUsec, !IO).

:- pred make_timeout_2(int::in, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_timeout_2(DelaySecs::in, TimeoutSec::out, TimeoutUsec::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    TimeoutSec = tv.tv_sec + DelaySecs;
    TimeoutUsec = tv.tv_usec;
").

:- pred check_timeout(timeout::in, bool::out, io::di, io::uo) is det.

check_timeout(timeout(TimeoutSec, TimeoutUsec), TimedOut, !IO) :-
    check_timeout_2(TimeoutSec, TimeoutUsec, TimedOut, !IO).

:- pred check_timeout_2(int::in, int::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    check_timeout_2(TimeoutSec::in, TimeoutUsec::in, TimedOut::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    struct timeval now;
    gettimeofday(&now, NULL);
    if (now.tv_sec > TimeoutSec) {
        TimedOut = MR_YES;
    } else if (now.tv_sec == TimeoutSec && now.tv_usec >= TimeoutUsec) {
        TimedOut = MR_YES;
    } else {
        TimedOut = MR_NO;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
