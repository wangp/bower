% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module async.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module quote_arg.

%-----------------------------------------------------------------------------%

:- type async_op
    --->    async_shell_command(
                command_prefix      :: command_prefix,
                command_args        :: list(string),    % will be quoted
                remaining_attempts  :: int
            )
    ;       async_lowprio_command(
                % Low priority operations return the output of a command.
                % They are for polling status only, and won't be retried on
                % failure.
                lowprio_command_prefix  :: command_prefix,
                lowprio_args            :: list(string), % will be quoted
                lowprio_stdin_contents  :: maybe(string)
            ).

:- inst async_shell_command
    --->    async_shell_command(ground, ground, ground).

:- inst async_lowprio_command
    --->    async_lowprio_command(ground, ground, ground).

:- type async_return
    --->    none
    ;       child_succeeded
    ;       child_lowprio_output(string)
    ;       child_failed(
                child_op        :: async_op,
                child_failure   :: async_failure
            ).

:- type async_failure
    --->    failure_nonzero_exit(int)
    ;       failure_signal(int)
    ;       failure_abnormal_exit
    ;       failure_error(io.error).

:- pred push_async(async_op::in(async_shell_command), io::di, io::uo) is det.

:- pred retry_async(int::in, async_op::in(async_shell_command),
    io::di, io::uo) is det.

:- pred push_lowprio_async(async_op::in(async_lowprio_command), bool::out,
    io::di, io::uo) is det.

:- pred clear_lowprio_async(io::di, io::uo) is det.

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
:- import_module queue.
:- import_module require.

:- import_module process.

%-----------------------------------------------------------------------------%

:- type async_info
    --->    async_info(
                ai_queue            :: queue(queue_elem),

                % Only a single low priority polling command queued at a time.
                % It will only be executed if the main queue is empty.
                ai_lowprio_queue    :: maybe(async_op),

                % Current child process, if any.
                ai_maybe_child      :: maybe(current_child)
            ).

:- type queue_elem
    --->    q_op(async_op)
    ;       q_timeout(timeout).

:- type timeout
    --->    timeout(int, int).

:- type current_child
    --->    current_child(
                cc_op               :: async_op,
                cc_pid              :: pid,
                cc_stdout_pipe      :: maybe(pipe_read),
                % The SIGCHLD count before the last async process was started.
                cc_sigchld_count    :: int
            ).

:- mutable(async_info, async_info, init_async_info, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

:- func init_async_info = async_info.

init_async_info = async_info(queue.init, no, no).

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

push_lowprio_async(Op, Pushed, !IO) :-
    get_async_info(Info0, !IO),
    Queue0 = Info0 ^ ai_lowprio_queue,
    (
        Queue0 = no,
        Info = Info0 ^ ai_lowprio_queue := yes(Op),
        set_async_info(Info, !IO),
        Pushed = yes
    ;
        Queue0 = yes(_),
        Pushed = no
    ).

clear_lowprio_async(!IO) :-
    get_async_info(Info0, !IO),
    Info = Info0 ^ ai_lowprio_queue := no,
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
    MaybeChild = Info ^ ai_maybe_child,
    (
        MaybeChild = yes(_),
        Count = 1 + QueueLength
    ;
        MaybeChild = no,
        Count = QueueLength
    ).

have_child_process(HaveChild, !IO) :-
    get_async_info(Info, !IO),
    MaybeChild = Info ^ ai_maybe_child,
    (
        MaybeChild = yes(_),
        HaveChild = yes
    ;
        MaybeChild = no,
        HaveChild = no
    ).

%-----------------------------------------------------------------------------%

:- pred do_poll(wait_pid_blocking::in, async_return::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

do_poll(Blocking, Return, !Info, !IO) :-
    MaybeChild = !.Info ^ ai_maybe_child,
    (
        MaybeChild = no,
        start_next_op(Return, !Info, !IO)
    ;
        MaybeChild = yes(Child),
        do_poll_in_progress(Child, Blocking, Return, !Info, !IO)
    ).

:- pred do_poll_in_progress(current_child::in, wait_pid_blocking::in,
    async_return::out, async_info::in, async_info::out, io::di, io::uo) is det.

do_poll_in_progress(Child, Blocking, Return, !Info, !IO) :-
    Child = current_child(Op, Pid, StdoutPipe, _SigchldCount),
    wait_pid(Pid, Blocking, WaitRes, !IO),
    (
        WaitRes = no_hang,
        % Child not done.
        Return = none
    ;
        (
            WaitRes = child_exit(Status),
            ( Status = 0 ->
                (
                    StdoutPipe = no,
                    Return = child_succeeded
                ;
                    StdoutPipe = yes(PipeRead),
                    drain_pipe(PipeRead, DrainRes, Buffers, !IO),
                    close_pipe_read(PipeRead, !IO),
                    (
                        DrainRes = ok,
                        make_utf8_string(no, Buffers, String)
                    ->
                        Return = child_lowprio_output(String)
                    ;
                        % XXX what else can we do?
                        Return = child_lowprio_output("")
                    )
                )
            ;
                maybe_close_pipe(StdoutPipe, !IO),
                Return = child_failed(Op, failure_nonzero_exit(Status))
            )
        ;
            WaitRes = child_signalled(Signal),
            maybe_close_pipe(StdoutPipe, !IO),
            Return = child_failed(Op, failure_signal(Signal))
        ;
            WaitRes = child_abnormal_exit,
            maybe_close_pipe(StdoutPipe, !IO),
            Return = child_failed(Op, failure_abnormal_exit)
        ;
            WaitRes = error(Error),
            maybe_close_pipe(StdoutPipe, !IO),
            Return = child_failed(Op, failure_error(Error))
        ),
        !Info ^ ai_maybe_child := no
    ).

:- pred maybe_close_pipe(maybe(pipe_read)::in, io::di, io::uo) is det.

maybe_close_pipe(no, !IO).
maybe_close_pipe(yes(PipeRead), !IO) :-
    close_pipe_read(PipeRead, !IO).

%-----------------------------------------------------------------------------%

:- pred start_next_op(async_return::out, async_info::in, async_info::out,
    io::di, io::uo) is det.

start_next_op(Return, !Info, !IO) :-
    Queue0 = !.Info ^ ai_queue,
    LowQueue0 = !.Info ^ ai_lowprio_queue,
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
    ; LowQueue0 = yes(Op) ->
        !Info ^ ai_lowprio_queue := no,
        start_op(Op, Return, !Info, !IO)
    ;
        Return = none
    ).

:- pred start_op(async_op::in, async_return::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

start_op(Op, Return, !Info, !IO) :-
    MaybeChild0 = !.Info ^ ai_maybe_child,
    expect(unify(MaybeChild0, no), $module, $pred,
        "already have child process"),
    get_sigchld_count(PreSigchldCount, !IO),
    spawn_process_for_op(Op, PreSigchldCount, Res, !Info, !IO),
    (
        Res = ok(Child),
        !Info ^ ai_maybe_child := yes(Child),
        Return = none
    ;
        Res = error(Error),
        Return = child_failed(Op, failure_error(Error))
    ).

:- pred spawn_process_for_op(async_op::in, int::in, io.res(current_child)::out,
    async_info::in, async_info::out, io::di, io::uo) is det.

spawn_process_for_op(Op, PreSigchldCount, Res, !Info, !IO) :-
    Op = async_shell_command(CommandPrefix, UnquotedArgs, _RemainingAttempts),
    shell_and_args(CommandPrefix, UnquotedArgs, redirect_input("/dev/null"),
        Shell, ShellArgs),
    posix_spawn(Shell, ShellArgs, SpawnRes, !IO),
    (
        SpawnRes = ok(Pid),
        Child = current_child(Op, Pid, no, PreSigchldCount),
        Res = ok(Child)
    ;
        SpawnRes = error(Error),
        Res = error(Error)
    ).
spawn_process_for_op(Op, PreSigchldCount, Res, !Info, !IO) :-
    Op = async_lowprio_command(CommandPrefix, UnquotedArgs,
        MaybeStdinContents),
    (
        MaybeStdinContents = no,
        shell_and_args(CommandPrefix, UnquotedArgs,
            redirect_input("/dev/null"), Shell, ShellArgs),
        posix_spawn_get_stdout(Shell, ShellArgs, SpawnRes, !IO),
        (
            SpawnRes = ok({Pid, StdoutPipe}),
            Child = current_child(Op, Pid, yes(StdoutPipe), PreSigchldCount),
            Res = ok(Child)
        ;
            SpawnRes = error(Error),
            Res = error(Error)
        )
    ;
        MaybeStdinContents = yes(Contents),
        shell_and_args(CommandPrefix, UnquotedArgs, no_redirect,
            Shell, ShellArgs),
        posix_spawn_get_stdin_stdout(Shell, ShellArgs, SpawnRes, !IO),
        (
            SpawnRes = ok({Pid, StdinPipe, StdoutPipe}),
            % This depends on Contents fitting within the pipe buffer all at
            % once. In practice notmuch queries should not come anywhere near
            % exceeding the pipe capacity.
            write_string_to_pipe(StdinPipe, Contents, WriteRes, !IO),
            close_pipe_write(StdinPipe, !IO),
            (
                WriteRes = ok,
                Child = current_child(Op, Pid, yes(StdoutPipe),
                    PreSigchldCount),
                Res = ok(Child)
            ;
                (
                    WriteRes = error(WriteError)
                ;
                    WriteRes = partial_write(_),
                    WriteError = io.make_io_error("write: partial write")
                ),
                close_pipe_read(StdoutPipe, !IO),
                kill(Pid, sigterm, KillRes, !IO),
                (
                    KillRes = ok,
                    wait_pid(Pid, blocking, _WaitResult, !IO)
                    % What can we do if waitpid fails?
                ;
                    KillRes = error(_KillError)
                    % What can we do if kill fails?
                ),
                Res = error(WriteError)
            )
        ;
            SpawnRes = error(Error),
            Res = error(Error)
        )
    ).

:- pred shell_and_args(command_prefix::in, list(string)::in,
    redirect_input::in, string::out, list(string)::out) is det.

shell_and_args(CommandPrefix, UnquotedArgs, RedirectInput, Shell, ShellArgs)
        :-
    make_quoted_command(CommandPrefix, UnquotedArgs,
        RedirectInput, no_redirect, ShellCommand),
    Shell = "/bin/sh",
    ShellArgs = ["-c", ShellCommand].

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
    get_async_info(Info, !IO),
    MaybeChild = Info ^ ai_maybe_child,
    (
        MaybeChild = yes(Child),
        Count0 = Child ^ cc_sigchld_count,
        get_sigchld_count(Count, !IO),
        Increased = ( Count > Count0 -> yes ; no )
    ;
        MaybeChild = no,
        Increased = no
    ).

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
