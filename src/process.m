% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module process.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- type pid
    --->    pid(int).

:- type pipe_read.

:- pred posix_spawn(string::in, list(string)::in, io.res(pid)::out,
    io::di, io::uo) is det.

:- pred posix_spawn_capture_stdout(string::in, list(string)::in,
    io.res({pid, pipe_read})::out, io::di, io::uo) is det.

:- type wait_pid_blocking
    --->    blocking
    ;       nonblocking.

:- type wait_pid_result
    --->    no_hang
    ;       child_exit(int)
    ;       child_signalled(int)
    ;       child_abnormal_exit
    ;       error(io.error).

:- pred wait_pid(pid::in, wait_pid_blocking::in, wait_pid_result::out,
    io::di, io::uo) is det.

:- pred drain_pipe(pipe_read::in, maybe(int)::in, io.res(string)::out,
    io::di, io::uo) is det.

:- pred close_pipe(pipe_read::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.

:- import_module byte_array.
:- import_module make_utf8.

:- type pipe_read
    --->    pipe_read(
                fd  :: int
            ).

:- pragma foreign_decl("C", local, "
static int close_eintr(int fd)
{
    int rc;
    do {
        rc = close(fd);
    } while (rc == -1 && errno == EINTR);
    return rc;
}
").

%-----------------------------------------------------------------------------%

posix_spawn(Prog, Args, Res, !IO) :-
    list.length(Args, NumArgs),
    CaptureStdout = no,
    posix_spawn_2(Prog, Args, NumArgs, CaptureStdout, Pid, _Fd, !IO),
    ( Pid >= 0 ->
        Res = ok(pid(Pid))
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

posix_spawn_capture_stdout(Prog, Args, Res, !IO) :-
    list.length(Args, NumArgs),
    CaptureStdout = yes,
    posix_spawn_2(Prog, Args, NumArgs, CaptureStdout, Pid, Fd, !IO),
    ( Pid >= 0 ->
        Res = ok({pid(Pid), pipe_read(Fd)})
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

:- pred posix_spawn_2(string::in, list(string)::in, int::in, bool::in,
    int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    posix_spawn_2(Prog::in, Args::in, NumArgs::in, CaptureStdout::in,
        Pid::out, PipeRead::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    pid_t pid;
    char *argv[1 + NumArgs + 1];    /* C99 stack allocation */
    int pipefd[2] = {-1, -1};
    int i;
    int rc;

    argv[0] = Prog;
    for (i = 1; i <= NumArgs; i++) {
        argv[i] = MR_make_string(MR_ALLOC_ID, ""%s"",
            (MR_ConstString) MR_list_head(Args));
        Args = MR_list_tail(Args);
    }
    argv[i] = NULL;

    rc = 0;

    if (CaptureStdout) {
        rc = pipe(pipefd);
    }

    if (rc == 0) {
        rc = do_posix_spawn(&pid, Prog, argv, pipefd);
    }

    if (rc == 0) {
        Pid = pid;
        PipeRead = pipefd[0];
        /* Close the write end of the pipe in the parent. */
        if (pipefd[1] != -1) {
            close_eintr(pipefd[1]);
        }
    } else {
        Pid = -1;
        if (pipefd[0] != -1) {
            close_eintr(pipefd[0]);
        }
        if (pipefd[1] != -1) {
            close_eintr(pipefd[1]);
        }
    }
").

:- pragma foreign_decl("C", local, "
#if defined(__APPLE__) && defined(__MACH__)
    /*
    ** On Darwin, shared libraries and bundles don't have direct access to
    ** environ.
    */
    #include <crt_externs.h>
    #define ENVIRON (*_NSGetEnviron())
#else
    /* POSIX does not require environ to be declared. */
    extern char **environ;
    #define ENVIRON (environ)
#endif

static int
do_posix_spawn(pid_t *pid_ptr, const char *Prog, char *argv[], int pipefd[2])
{
    posix_spawn_file_actions_t file_actions;
    posix_spawnattr_t attr;
    sigset_t sigmask;
    int rc;

    posix_spawn_file_actions_init(&file_actions);

    /*
    ** Close stdin in the child.  This prevents interference with reading
    ** input in the parent process.
    */
    posix_spawn_file_actions_addclose(&file_actions, STDIN_FILENO);

    /*
    ** Close the read end of the pipe in the child, then redirect
    ** stdout to the write end of the pipe.
    */
    if (pipefd[0] != -1) {
        posix_spawn_file_actions_addclose(&file_actions, pipefd[0]);
        posix_spawn_file_actions_adddup2(&file_actions, pipefd[1],
            STDOUT_FILENO);
    }

    /*
    ** Block SIGWINCH in the child so that resizing the terminal
    ** window does not kill the child.
    */
    sigemptyset(&sigmask);
    sigaddset(&sigmask, SIGWINCH);
    posix_spawnattr_init(&attr);
    posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK);
    posix_spawnattr_setsigmask(&attr, &sigmask);

    rc = posix_spawnp(pid_ptr, Prog, &file_actions, &attr, argv, ENVIRON);

    posix_spawnattr_destroy(&attr);
    posix_spawn_file_actions_destroy(&file_actions);

    return rc;
}
").

%-----------------------------------------------------------------------------%

wait_pid(pid(Pid), Blocking, Res, !IO) :-
    (
        Blocking = blocking,
        BlockingBool = yes
    ;
        Blocking = nonblocking,
        BlockingBool = no
    ),
    wait_pid_2(Pid, BlockingBool, RC, Exited, ExitStatus, Signal, !IO),
    ( RC = -1 ->
        Res = error(io.make_io_error("wait_pid failed"))
    ; RC = 0 ->
        % Child process not done.
        Res = no_hang
    ;
        ( Exited = yes ->
            Res = child_exit(ExitStatus)
        ; Signal > 0 ->
            Res = child_signalled(Signal)
        ;
            Res = child_abnormal_exit
        )
    ).

:- pred wait_pid_2(int::in, bool::in, int::out, bool::out, int::out,
    int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    wait_pid_2(Pid::in, Blocking::in, RC::out, Exited::out,
        ExitStatus::out, Signal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int status;
    int options;

    if (Blocking) {
        options = 0;
    } else {
        options = WNOHANG;
    }
    do {
        RC = waitpid(Pid, &status, options);
    } while (RC == -1 && errno == EINTR);
    if (RC == -1) {
        Exited = MR_NO;
        ExitStatus = -1;
        Signal = -1;
    } else if (WIFEXITED(status)) {
        Exited = MR_YES;
        ExitStatus = WEXITSTATUS(status);
        Signal = -1;
    } else if (WIFSIGNALED(status)) {
        Exited = MR_NO;
        Signal = WTERMSIG(status);
    } else {
        Exited = MR_NO;
        ExitStatus = -1;
        Signal = -1;
    }
").

%-----------------------------------------------------------------------------%

drain_pipe(pipe_read(Fd), ErrorLimit, Res, !IO) :-
    drain(Fd, Result, [], RevBuffers, !IO),
    (
        Result = ok,
        uniq_reverse(RevBuffers, Buffers),
        ( make_utf8_string(ErrorLimit, Buffers, String) ->
            Res = ok(String)
        ;
            Res = error(io.make_io_error("not UTF-8 text"))
        )
    ;
        Result = error(Error),
        Res = error(io.make_io_error(Error))
    ).

:- pred drain(int::in, maybe_error::out,
    list(byte_array)::di, list(byte_array)::uo, io::di, io::uo) is det.

drain(Fd, Result, !RevBuffers, !IO) :-
    Capacity = 16384,
    MaxRead = Capacity - make_utf8.buffer_margin,
    byte_array.allocate(Capacity, Buffer0),
    read(Fd, MaxRead, N, Error, Buffer0, Buffer, !IO),
    ( N < 0 ->
        Result = error(Error)
    ; N = 0 ->
        Result = ok
    ;
        !:RevBuffers = [Buffer | !.RevBuffers],
        drain(Fd, Result, !RevBuffers, !IO)
    ).

:- pred read(int::in, int::in, int::out, string::out,
    byte_array::di, byte_array::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read(Fd::in, MaxRead::in, N::out, Error::out, Buffer0::di, Buffer::uo,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Buffer = Buffer0;
    assert(Buffer->len == 0);
    do {
        N = read(Fd, Buffer->data, MaxRead);
    } while (N == -1 && errno == EINTR);
    if (N < 0) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Buffer->len = N;
        Error = MR_make_string_const("""");
    }
").

:- pred uniq_reverse(list(T)::di, list(T)::uo) is det.

uniq_reverse([], []).
uniq_reverse([X | Xs], Rev) :-
    uniq_reverse(Xs, [X], Rev).

:- pred uniq_reverse(list(T)::di, list(T)::di, list(T)::uo) is det.

uniq_reverse([], Acc, Acc).
uniq_reverse([X | Xs], Acc0, Acc) :-
    uniq_reverse(Xs, [X | Acc0], Acc).

%-----------------------------------------------------------------------------%

close_pipe(pipe_read(Fd), !IO) :-
    close_pipe_2(Fd, !IO).

:- pred close_pipe_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_pipe_2(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    close_eintr(Fd);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
