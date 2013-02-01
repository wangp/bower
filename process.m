% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module process.
:- interface.

:- import_module io.
:- import_module list.

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

:- pred drain_and_close_pipe(pipe_read::in, io.res(string)::out,
    io::di, io::uo) is det.

:- pred close_pipe(pipe_read::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module string.

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
    char *argv[NumArgs + 1];    /* C99 stack allocation */
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

    posix_spawnattr_init(&attr);

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

drain_and_close_pipe(pipe_read(Fd), Res, !IO) :-
    drain_and_close_pipe_2(Fd, Error, RevList, !IO),
    ( Error \= "" ->
        Res = error(io.make_io_error(Error))
    ; RevList = [String] ->
        Res = ok(String)
    ;
        list.reverse(RevList, List),
        string.append_list(List, String),
        Res = ok(String)
    ).

:- pred drain_and_close_pipe_2(int::in, string::out, list(string)::uo,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    drain_and_close_pipe_2(Fd::in, Error::out, RevList::uo, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    static char buf[16384];
    MR_String s;
    int n;

    RevList = MR_list_empty();

    for (;;) {
        do {
            n = read(Fd, buf, sizeof(buf));
        } while (n == -1 && errno == EINTR);
        if (n < 0) {
            Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
            break;
        } else if (n == 0) {
            Error = MR_make_string_const("""");
            break;
        } else {
            buf[n] = '\\0';
            MR_make_aligned_string_copy_msg(s, buf, MR_ALLOC_ID);
            RevList = MR_list_cons((MR_Word)s, RevList);
        }
    }

    close_eintr(Fd);
").

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
