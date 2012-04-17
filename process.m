% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module process.
:- interface.

:- import_module io.
:- import_module list.

:- pred posix_spawn(string::in, list(string)::in, io.res(int)::out,
    io::di, io::uo) is det.

:- type wait_pid_blocking
    --->    blocking
    ;       nonblocking.

:- type wait_pid_result
    --->    no_hang
    ;       child_exit(int)
    ;       child_signalled(int)
    ;       child_abnormal_exit
    ;       error(io.error).

:- pred wait_pid(int::in, wait_pid_blocking::in, wait_pid_result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.

%-----------------------------------------------------------------------------%

posix_spawn(Prog, Args, Res, !IO) :-
    list.length(Args, NumArgs),
    posix_spawn_2(Prog, Args, NumArgs, Pid, !IO),
    ( Pid >= 0 ->
        Res = ok(Pid)
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

:- pred posix_spawn_2(string::in, list(string)::in, int::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    posix_spawn_2(Prog::in, Args::in, NumArgs::in, Pid::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    pid_t pid;
    const posix_spawn_file_actions_t *file_actions = NULL;
    posix_spawnattr_t attr;
    char *argv[NumArgs + 1];    /* C99 stack allocation */
    int i;
    int rc;

    argv[0] = Prog;
    for (i = 1; i <= NumArgs; i++) {
        argv[i] = MR_make_string(MR_ALLOC_ID, ""%s"",
            (MR_ConstString) MR_list_head(Args));
        Args = MR_list_tail(Args);
    }
    argv[i] = NULL;

    posix_spawnattr_init(&attr);
    rc = posix_spawnp(&pid, Prog, file_actions, &attr, argv, environ);
    posix_spawnattr_destroy(&attr);

    if (rc == 0) {
        Pid = pid;
    } else {
        Pid = -1;
    }
").

%-----------------------------------------------------------------------------%

wait_pid(Pid, Blocking, Res, !IO) :-
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
    RC = waitpid(Pid, &status, options);
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
% vim: ft=mercury ts=4 sts=4 sw=4 et
