% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module call_system.
:- interface.

:- import_module io.
:- import_module maybe.

    % Replacement for popen() which blocks SIGWINCH signals in the child.
    %
:- pred call_system_capture_stdout(string::in, maybe(int)::in,
    io.res(string)::out, io::di, io::uo) is det.

    % Like call_system_capture_stdout, but writes a string to standard input
    % of child process.
    %
:- pred call_system_filter(string::in, string::in, maybe(int)::in,
    io.res(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module process.

%-----------------------------------------------------------------------------%

call_system_capture_stdout(Command, ErrorLimit, Res, !IO) :-
    posix_spawn_get_stdout("/bin/sh", ["-c", Command], SpawnRes, !IO),
    (
        SpawnRes = ok({Pid, PipeRead}),
        drain_pipe(PipeRead, DrainRes, Buffers, !IO),
        close_pipe_read(PipeRead, !IO),
        do_wait(Pid, no, WaitRes, !IO),
        (
            WaitRes = ok,
            (
                DrainRes = ok,
                ( make_utf8_string(ErrorLimit, Buffers, String) ->
                    Res = ok(String)
                ;
                    Res = error(io.make_io_error("not UTF-8 text"))
                )
            ;
                DrainRes = error(Error),
                Res = error(Error)
            )
        ;
            WaitRes = error(Error),
            Res = error(Error)
        )
    ;
        SpawnRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

call_system_filter(Command, Input, ErrorLimit, Res, !IO) :-
    posix_spawn_get_stdin_stdout("/bin/sh", ["-c", Command], SpawnRes, !IO),
    (
        SpawnRes = ok({Pid, PipeWrite, PipeRead}),
        write_and_read_concurrently_and_close_both(PipeWrite, Input,
            PipeRead, WriteAndReadRes, Buffers, !IO),
        % Both PipeWrite and PipeRead are closed at this point.
        (
            WriteAndReadRes = ok,
            do_wait(Pid, no, WaitRes, !IO)
        ;
            WriteAndReadRes = error(_Error),
            kill(Pid, sigterm, KillRes, !IO),
            (
                KillRes = ok,
                do_wait(Pid, yes(sigterm), WaitRes, !IO)
            ;
                KillRes = error(KillError),
                % What can we do if kill fails?
                WaitRes = error(KillError)
            )
        ),
        (
            WaitRes = ok,
            (
                WriteAndReadRes = ok,
                ( make_utf8_string(ErrorLimit, Buffers, String) ->
                    Res = ok(String)
                ;
                    Res = error(io.make_io_error("not UTF-8 text"))
                )
            ;
                WriteAndReadRes = error(Error),
                Res = error(Error)
            )
        ;
            WaitRes = error(Error),
            Res = error(Error)
        )
    ;
        SpawnRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred do_wait(pid::in, maybe(int)::in, io.res::out, io::di, io::uo) is det.

do_wait(Pid, ExpectSignal, Res, !IO) :-
    wait_pid(Pid, blocking, WaitRes, !IO),
    (
        WaitRes = no_hang,
        % Should not occur.
        Res = error(io.make_io_error("process not finished"))
    ;
        WaitRes = child_exit(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = "process returned with exit code " ++
                string.from_int(ExitStatus),
            Res = error(io.make_io_error(Msg))
        )
    ;
        WaitRes = child_signalled(Signal),
        ( ExpectSignal = yes(Signal) ->
            Res = ok
        ;
            Msg = "process received signal " ++ string.from_int(Signal),
            Res = error(io.make_io_error(Msg))
        )
    ;
        WaitRes = child_abnormal_exit,
        Msg = "process exited abnormally",
        Res = error(io.make_io_error(Msg))
    ;
        WaitRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
