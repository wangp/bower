% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module process.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- type pid
    --->    pid(int).

    % Read or write end of a pipe.
    %
:- type pipe_read.
:- type pipe_write.

:- type spawn_env
    --->    environ(list(env_modification)).
    %       reset(list(env_modification))

:- type env_modification
    --->    set_var(string, string)
    ;       delete_var(string).

:- pred posix_spawn(string::in, list(string)::in, spawn_env::in,
    io.res(pid)::out, io::di, io::uo) is det.

:- pred posix_spawn_get_stdin(string::in, list(string)::in, spawn_env::in,
    io.res({pid, pipe_write})::out, io::di, io::uo) is det.

:- pred posix_spawn_get_stdout(string::in, list(string)::in, spawn_env::in,
    io.res({pid, pipe_read})::out, io::di, io::uo) is det.

    % Warning: remember to avoid deadlocks due to a full pipe buffer.
    %
    % The pipe connected to standard input of the child process has the flag
    % O_NONBLOCK set so that write(2) will perform a partial write instead of
    % blocking if the pipe is full.
    %
:- pred posix_spawn_get_stdin_stdout(string::in, list(string)::in, spawn_env::in,
    io.res({pid, pipe_write, pipe_read})::out, io::di, io::uo) is det.

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

:- type buffers.

:- pred drain_pipe(pipe_read::in, io.res::out, buffers::uo, io::di, io::uo)
    is det.

:- type write_string_result
    --->    ok
    ;       partial_write(int)
    ;       error(io.error).

:- pred write_string_to_pipe(pipe_write::in, string::in,
    write_string_result::out, io::di, io::uo) is det.

    % Warning: for consistency, both pipes will be closed after this call,
    % do not close them again.
    %
:- pred write_and_read_concurrently_and_close_both(pipe_write::in, string::in,
    pipe_read::in, io.res::out, buffers::uo, io::di, io::uo) is det.

:- pred make_utf8_string(maybe(int)::in, buffers::di, string::out)
    is semidet.

:- pred close_pipe_read(pipe_read::in, io::di, io::uo) is det.
:- pred close_pipe_write(pipe_write::in, io::di, io::uo) is det.

:- pred kill(pid::in, int::in, io.res::out, io::di, io::uo) is det.

:- func sigterm = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module string.

:- import_module byte_array.
:- import_module make_utf8.

:- type env.
:- pragma foreign_type("C", env, "char **").

:- type pipe_read
    --->    pipe_read(read_fd :: int).

:- type pipe_write
    --->    pipe_write(write_fd :: int).

:- type buffers == list(buffer).

:- pragma foreign_decl("C", local, "
    /* for posix_spawn */
    #include <spawn.h>

    /* for select -- POSIX.1-2001, POSIX.1-2008 */
    #include <sys/select.h>
    /* for select -- earlier standards */
    #include <sys/time.h>
    #include <sys/types.h>
    #include <unistd.h>

    /* for fcntl, ioctl */
    #include <sys/fcntl.h>
    #include <sys/ioctl.h>
").

:- pragma foreign_decl("C", local, "
static int do_close(int fd)
{
    int rc = 0;

    if (fd >= 0) {
        do {
            rc = close(fd);
        } while (rc == -1 && errno == EINTR);
    }

    return rc;
}
").

%-----------------------------------------------------------------------------%

posix_spawn(Prog, Args, SpawnEnv, Res, !IO) :-
    list.length(Args, NumArgs),
    get_spawn_env(SpawnEnv, Env, !IO),
    GetStdin = no,
    GetStdout = no,
    posix_spawn_2(Prog, Args, NumArgs, Env, GetStdin, GetStdout, Pid,
        _StdinFd, _StdoutFd, !IO),
    ( Pid >= 0 ->
        Res = ok(pid(Pid))
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

posix_spawn_get_stdin(Prog, Args, SpawnEnv, Res, !IO) :-
    list.length(Args, NumArgs),
    get_spawn_env(SpawnEnv, Env, !IO),
    GetStdin = yes,
    GetStdout = no,
    posix_spawn_2(Prog, Args, NumArgs, Env, GetStdin, GetStdout, Pid,
        StdinFd, _StdoutFd, !IO),
    ( Pid >= 0 ->
        Res = ok({pid(Pid), pipe_write(StdinFd)})
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

posix_spawn_get_stdout(Prog, Args, SpawnEnv, Res, !IO) :-
    list.length(Args, NumArgs),
    get_spawn_env(SpawnEnv, Env, !IO),
    GetStdin = no,
    GetStdout = yes,
    posix_spawn_2(Prog, Args, NumArgs, Env, GetStdin, GetStdout, Pid,
        _StdinFd, StdoutFd, !IO),
    ( Pid >= 0 ->
        Res = ok({pid(Pid), pipe_read(StdoutFd)})
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

posix_spawn_get_stdin_stdout(Prog, Args, SpawnEnv, Res, !IO) :-
    list.length(Args, NumArgs),
    get_spawn_env(SpawnEnv, Env, !IO),
    GetStdin = yes,
    GetStdout = yes,
    posix_spawn_2(Prog, Args, NumArgs, Env, GetStdin, GetStdout, Pid,
        StdinFd, StdoutFd, !IO),
    ( Pid >= 0 ->
        Res = ok({pid(Pid), pipe_write(StdinFd), pipe_read(StdoutFd)})
    ;
        Res = error(io.make_io_error("posix_spawn failed"))
    ).

:- pred posix_spawn_2(string::in, list(string)::in, int::in, env::in,
    bool::in, bool::in, int::out, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    posix_spawn_2(Prog::in, Args::in, NumArgs::in, Env::in,
        GetStdin::in, GetStdout::in, Pid::out, StdinFd::out, StdoutFd::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    pid_t pid;
    char *argv[1 + NumArgs + 1];    /* C99 stack allocation */
    int stdin_pipe[2] = {-1, -1};
    int stdout_pipe[2] = {-1, -1};
    int i;
    int rc;
    int flags;

    argv[0] = Prog;
    for (i = 1; i <= NumArgs; i++) {
        argv[i] = MR_make_string(MR_ALLOC_ID, ""%s"",
            (MR_ConstString) MR_list_head(Args));
        Args = MR_list_tail(Args);
    }
    argv[i] = NULL;

    rc = 0;

    if (GetStdin) {
        rc = pipe(stdin_pipe);
    }

    if (rc == 0 && GetStdout) {
        rc = pipe(stdout_pipe);
    }

    if (rc == 0) {
        rc = do_posix_spawn(&pid, Prog, argv, Env, stdin_pipe, stdout_pipe);
    }

    if (rc == 0) {
        Pid = pid;
        StdinFd = stdin_pipe[1];    /* keep write end of stdin_pipe */
        do_close(stdin_pipe[0]);    /* close read end of stdin_pipe */
        StdoutFd = stdout_pipe[0];  /* keep read end of stdout_pipe */
        do_close(stdout_pipe[1]);   /* close write end of stdout_pipe */
        /*
        ** Make writing to the standard input pipe non-blocking (and hope that
        ** it doesn't fail, otherwise cleanup would be messy).
        */
        flags = fcntl(StdinFd, F_GETFL);
        fcntl(StdinFd, F_SETFL, flags | O_NONBLOCK);
    } else {
        Pid = -1;
        StdinFd = -1;
        StdoutFd = -1;
        do_close(stdin_pipe[0]);
        do_close(stdin_pipe[1]);
        do_close(stdout_pipe[0]);
        do_close(stdout_pipe[1]);
    }
").

:- pragma foreign_decl("C", local, "
static int
do_posix_spawn(pid_t *pid_ptr, const char *Prog, char *argv[], char *envp[],
    int stdin_pipe[2], int stdout_pipe[2])
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
    ** Close the write end of stdin_pipe in the child, then redirect
    ** stdin from the read end of the pipe.
    */
    if (stdin_pipe[1] != -1) {
        posix_spawn_file_actions_addclose(&file_actions, stdin_pipe[1]);
        posix_spawn_file_actions_adddup2(&file_actions, stdin_pipe[0],
            STDIN_FILENO);
    }

    /*
    ** Close the read end of stdout_pipe in the child, then redirect
    ** stdout to the write end of the pipe.
    */
    if (stdout_pipe[0] != -1) {
        posix_spawn_file_actions_addclose(&file_actions, stdout_pipe[0]);
        posix_spawn_file_actions_adddup2(&file_actions, stdout_pipe[1],
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

    rc = posix_spawnp(pid_ptr, Prog, &file_actions, &attr, argv, envp);

    posix_spawnattr_destroy(&attr);
    posix_spawn_file_actions_destroy(&file_actions);

    return rc;
}
").

%-----------------------------------------------------------------------------%

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
").

:- pred get_spawn_env(spawn_env::in, env::out, io::di, io::uo) is det.

get_spawn_env(SpawnEnv, Env, !IO) :-
    SpawnEnv = environ(Modifications),
    (
        Modifications = [],
        get_environ(Env, !IO)
    ;
        Modifications = [_ | _],
        get_environ_list(List0, !IO),
        foldl(apply_env_modification, Modifications, List0, List),
        make_env_array(List, Env)
    ).

:- pred get_environ(env::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_environ(Env::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Env = ENVIRON;
").

:- pred get_environ_list(list(string)::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_environ_list(Env::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    char **envp = ENVIRON;
    int i;

    Env = MR_list_empty();
    for (i = 0; envp[i] != NULL; i++) {
    }
    while (--i >= 0) {
        MR_String s;
        MR_make_aligned_string_copy_msg(s, envp[i], MR_ALLOC_ID);
        Env = MR_list_cons((MR_Word) s, Env);
    }
").

:- pred make_env_array(list(string)::in, env::out) is det.

:- pragma foreign_proc("C",
    make_env_array(List::in, Array::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    MR_Word xs;
    int length = 0;
    int i = 0;

    for (xs = List; !MR_list_is_empty(xs); xs = MR_list_tail(xs)) {
        length++;
    }
    Array = MR_GC_NEW_ARRAY(char *, length + 1);
    for (xs = List; !MR_list_is_empty(xs); xs = MR_list_tail(xs)) {
        Array[i] = (char *) MR_list_head(xs);
        i++;
    }
    Array[i] = NULL;
").

:- pred apply_env_modification(env_modification::in,
    list(string)::in, list(string)::out) is det.

apply_env_modification(Modification, !List) :-
    (
        Modification = set_var(Var, Value),
        VarEquals = Var ++ "=",
        ( replace_env_var(VarEquals, Value, !List) ->
            true
        ;
            !:List = !.List ++ [VarEquals ++ Value]
        )
    ;
        Modification = delete_var(Var),
        VarEquals = Var ++ "=",
        delete_env_var(VarEquals, !List)
    ).

:- pred replace_env_var(string::in, string::in, list(string)::in,
    list(string)::out) is semidet.

replace_env_var(VarEquals, Value, List0, List) :-
    List0 = [Head0 | Tail0],
    ( string.prefix(Head0, VarEquals) ->
        List = [VarEquals ++ Value | Tail0]
    ;
        replace_env_var(VarEquals, Value, Tail0, Tail),
        List = [Head0 | Tail]
    ).

:- pred delete_env_var(string::in, list(string)::in, list(string)::out) is det.

delete_env_var(_VarEquals, [], []).
delete_env_var(VarEquals, [Head0 | Tail0], List) :-
    ( string.prefix(Head0, VarEquals) ->
        List = Tail0
    ;
        delete_env_var(VarEquals, Tail0, Tail),
        List = [Head0 | Tail]
    ).

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
        ExitStatus = -1;
        Signal = WTERMSIG(status);
    } else {
        Exited = MR_NO;
        ExitStatus = -1;
        Signal = -1;
    }
").

%-----------------------------------------------------------------------------%

drain_pipe(PipeRead, Res, Buffers, !IO) :-
    drain_loop(PipeRead, Res, [], RevBuffers, !IO),
    (
        Res = ok,
        uniq_reverse(RevBuffers, Buffers)
    ;
        Res = error(_Error),
        Buffers = []
    ).

:- pred drain_loop(pipe_read::in, io.res::out,
    list(byte_array)::di, list(byte_array)::uo, io::di, io::uo) is det.

drain_loop(PipeRead, Res, !RevBuffers, !IO) :-
    read_some(PipeRead, Res0, Buffer, !IO),
    (
        Res0 = ok,
        !:RevBuffers = [Buffer | !.RevBuffers],
        drain_loop(PipeRead, Res, !RevBuffers, !IO)
    ;
        Res0 = eof,
        Res = ok
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred uniq_reverse(list(T)::di, list(T)::uo) is det.

uniq_reverse([], []).
uniq_reverse([X | Xs], Rev) :-
    uniq_reverse(Xs, [X], Rev).

:- pred uniq_reverse(list(T)::di, list(T)::di, list(T)::uo) is det.

uniq_reverse([], Acc, Acc).
uniq_reverse([X | Xs], Acc0, Acc) :-
    uniq_reverse(Xs, [X | Acc0], Acc).

%-----------------------------------------------------------------------------%

write_string_to_pipe(PipeWrite, String, Res, !IO) :-
    Count = string.count_code_units(String),
    unsafe_write_substring(PipeWrite, String, 0, Count, Res0, !IO),
    (
        Res0 = bytes_written(NumBytes),
        ( NumBytes < Count ->
            Res = partial_write(NumBytes)
        ;
            Res = ok
        )
    ;
        Res0 = write_error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

write_and_read_concurrently_and_close_both(PipeWrite, WriteString,
        PipeRead, Res, Buffers, !IO) :-
    WriteCount = string.count_code_units(WriteString),
    write_and_read_until_written(PipeWrite, WriteString, 0, WriteCount,
        PipeRead, WriteAllRes, [], RevBuffers0, no, SeenEOF, !IO),
    (
        WriteAllRes = ok,
        % Close PipeWrite so the child process knows there is no more input.
        close_pipe_write(PipeWrite, !IO),
        (
            SeenEOF = no,
            drain_loop(PipeRead, DrainRes, RevBuffers0, RevBuffers, !IO)
        ;
            SeenEOF = yes,
            DrainRes = ok,
            RevBuffers = RevBuffers0
        ),
        (
            DrainRes = ok,
            Res = ok,
            uniq_reverse(RevBuffers, Buffers)
        ;
            DrainRes = error(Error),
            Res = error(Error),
            Buffers = []
        )
    ;
        WriteAllRes = error(Error),
        Res = error(Error),
        Buffers = [],
        % Close PipeWrite in this branch as well.
        close_pipe_write(PipeWrite, !IO)
    ),
    % For consistency, close PipeRead as well.
    close_pipe_read(PipeRead, !IO).

:- pred write_and_read_until_written(pipe_write::in,
    string::in, int::in, int::in, pipe_read::in, io.res::out,
    list(byte_array)::di, list(byte_array)::uo, bool::in, bool::out,
    io::di, io::uo) is det.

write_and_read_until_written(PipeWrite, WriteString, !.WritePos, WriteEnd,
        PipeRead, Res, !RevBuffers, !SeenEOF, !IO) :-
    ( if !.WritePos >= WriteEnd then
        Res = ok
    else
        (
            !.SeenEOF = no,
            ReadFds = yes(PipeRead)
        ;
            !.SeenEOF = yes,
            ReadFds = no
        ),
        select(ReadFds, yes(PipeWrite), SelectRes, !IO),
        some [!Res]
        (
            SelectRes = select_ok(CanRead, CanWrite),
            !:Res = ok : io.res,
            (
                CanWrite = yes,
                unsafe_write_substring(PipeWrite, WriteString, !.WritePos,
                    WriteEnd, WriteRes, !IO),
                (
                    WriteRes = bytes_written(NumBytes),
                    !:WritePos = !.WritePos + NumBytes
                ;
                    WriteRes = write_error(WriteError),
                    !:Res = error(WriteError)
                )
            ;
                CanWrite = no
            ),
            ( if !.Res = ok, CanRead = yes then
                read_some(PipeRead, ReadRes, Buffer, !IO),
                (
                    ReadRes = ok,
                    !:RevBuffers = [Buffer | !.RevBuffers]
                ;
                    ReadRes = eof,
                    !:SeenEOF = yes
                ;
                    ReadRes = error(ReadError),
                    !:Res = error(ReadError)
                )
            else
                true
            ),
            (
                !.Res = ok,
                write_and_read_until_written(PipeWrite,
                    WriteString, !.WritePos, WriteEnd,
                    PipeRead, Res, !RevBuffers, !SeenEOF, !IO)
            ;
                !.Res = error(Error),
                Res = error(Error)
            )
        ;
            SelectRes = select_error(Error),
            Res = error(Error)
        )
    ).

%-----------------------------------------------------------------------------%

:- type select_result
    --->    select_ok(bool, bool)
    ;       select_error(io.error).

:- pred select(maybe(pipe_read)::in, maybe(pipe_write)::in, select_result::out,
    io::di, io::uo) is det.

select(ReadFds, WriteFds, Res, !IO) :-
    (
        ReadFds = yes(pipe_read(MaybeReadFd))
    ;
        ReadFds = no,
        MaybeReadFd = -1
    ),
    (
        WriteFds = yes(pipe_write(MaybeWriteFd))
    ;
        WriteFds = no,
        MaybeWriteFd = -1
    ),
    select_2(MaybeReadFd, MaybeWriteFd, Ok, CanRead, CanWrite, Error, !IO),
    (
        Ok = yes,
        Res = select_ok(CanRead, CanWrite)
    ;
        Ok = no,
        Res = select_error(io.make_io_error(Error))
    ).

:- pred select_2(int::in, int::in, bool::out,
    bool::out, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    select_2(MaybeReadFd::in, MaybeWriteFd::in, Ok::out,
        CanRead::out, CanWrite::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe /* strerror */,
        tabled_for_io, may_not_duplicate],
"
    fd_set readfds;
    fd_set writefds;
    int nfds;
    int rc;

    Ok = MR_NO;
    Error = MR_make_string_const("""");
    CanRead = MR_NO;
    CanWrite = MR_NO;

    if (MaybeReadFd >= FD_SETSIZE || MaybeWriteFd >= FD_SETSIZE) {
        Error = MR_make_string_const(""fd too big for select()"");
    } else {
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
        nfds = 0;

        if (MaybeReadFd >= 0) {
            FD_SET(MaybeReadFd, &readfds);
            nfds = MaybeReadFd + 1;
        }

        if (MaybeWriteFd >= 0) {
            FD_SET(MaybeWriteFd, &writefds);
            if (nfds <= MaybeWriteFd) {
                nfds = MaybeWriteFd + 1;
            }
        }

        rc = select(nfds, &readfds, &writefds, NULL, NULL);

        if (rc < 0) {
            Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
        } else {
            Ok = MR_YES;
            if (MaybeReadFd >= 0 && FD_ISSET(MaybeReadFd, &readfds)) {
                CanRead = MR_YES;
            }
            if (MaybeWriteFd >= 0 && FD_ISSET(MaybeWriteFd, &writefds)) {
                CanWrite = MR_YES;
            }
        }
    }
").

%-----------------------------------------------------------------------------%

:- pred read_some(pipe_read::in, io.result::out, buffer::uo, io::di, io::uo)
    is det.

read_some(pipe_read(Fd), Res, Buffer, !IO) :-
    get_readable_bytes(Fd, NumBytesOrError, !IO),
    ( NumBytesOrError > 0 ->
        MaxRead = NumBytesOrError,
        Capacity = MaxRead + make_utf8.buffer_margin
    ;
        ( NumBytesOrError = 0 ->
            Capacity = 64       % reasonable?
        ;
            Capacity = 16384    % FIONREAD not supported?
        ),
        MaxRead = Capacity - make_utf8.buffer_margin
    ),
    byte_array.allocate(Capacity, Buffer0),
    read_into_buffer(Fd, MaxRead, N, Error, Buffer0, Buffer, !IO),
    ( N < 0 ->
        Res = error(io.make_io_error(Error))
    ; N = 0 ->
        Res = eof
    ;
        Res = ok
    ).

:- pred read_into_buffer(int::in, int::in, int::out, string::out,
    byte_array::di, byte_array::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_into_buffer(Fd::in, MaxRead::in, N::out, Error::out,
        Buffer0::di, Buffer::uo, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe /* strerror */,
        tabled_for_io, may_not_duplicate],
"
    Buffer = Buffer0;
    assert(Buffer->len == 0);
    assert(MaxRead > 0);
    assert(MaxRead <= Buffer->cap);
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

:- pred get_readable_bytes(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_readable_bytes(Fd::in, NumBytesOrError::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    NumBytesOrError = -1;

#ifdef FIONREAD
{
    int nbytes;
    if (ioctl(Fd, FIONREAD, &nbytes) == 0) {
        NumBytesOrError = nbytes;
    }
}
#endif
").

%-----------------------------------------------------------------------------%

:- type write_result
    --->    bytes_written(int)
    ;       write_error(io.error).

:- pred unsafe_write_substring(pipe_write::in, string::in, int::in, int::in,
    write_result::out, io::di, io::uo) is det.

unsafe_write_substring(pipe_write(Fd), String, Start, End, Res, !IO) :-
    unsafe_write_substring_2(Fd, String, Start, End, N, Error, !IO),
    ( N < 0 ->
        Res = write_error(io.make_io_error(Error))
    ;
        Res = bytes_written(N)
    ).

:- pred unsafe_write_substring_2(int::in, string::in, int::in, int::in,
    int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unsafe_write_substring_2(Fd::in, Buf::in, Start::in, End::in,
        N::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe /* strerror */,
        tabled_for_io, may_not_duplicate],
"
    N = write(Fd, Buf + Start, End - Start);
    if (N < 0) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

make_utf8_string(ErrorLimit, Buffers, String) :-
    make_utf8.make_utf8_string(ErrorLimit, Buffers, String).

%-----------------------------------------------------------------------------%

close_pipe_read(pipe_read(Fd), !IO) :-
    close_fd(Fd, !IO).

close_pipe_write(pipe_write(Fd), !IO) :-
    close_fd(Fd, !IO).

:- pred close_fd(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_fd(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    do_close(Fd);
").

%-----------------------------------------------------------------------------%

kill(pid(Pid), Signal, Res, !IO) :-
    kill_2(Pid, Signal, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(io.make_io_error(Error))
    ).

:- pred kill_2(int::in, int::in, bool::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    kill_2(Pid::in, Signal::in, Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe /* stderr */,
        tabled_for_io, may_not_duplicate],
"
    int rc = kill(Pid, Signal);
    if (rc < 0) {
        Ok = MR_NO;
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    }
").

:- pragma foreign_proc("C",
    sigterm = (Signal::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Signal = SIGTERM;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
