% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module call_system.
:- interface.

:- import_module io.

    % Replacement for popen() which blocks SIGWINCH signals in the child.
    %
:- pred call_system_capture_stdout(string::in, io.res(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module process.

%-----------------------------------------------------------------------------%

call_system_capture_stdout(Command, Res, !IO) :-
    posix_spawn_capture_stdout("/bin/sh", ["-c", Command], SpawnRes, !IO),
    (
        SpawnRes = ok({Pid, PipeRead}),
        drain_pipe(PipeRead, DrainRes, !IO),
        close_pipe(PipeRead, !IO),
        wait_pid(Pid, blocking, WaitRes, !IO),
        (
            WaitRes = no_hang,
            % Should not occur.
            Res = error(io.make_io_error("process not finished"))
        ;
            WaitRes = child_exit(ExitStatus),
            ( ExitStatus = 0 ->
                Res = DrainRes
            ;
                Msg = "process returned with exit code " ++
                    string.from_int(ExitStatus),
                Res = error(io.make_io_error(Msg))
            )
        ;
            WaitRes = child_signalled(Signal),
            Msg = "process received signal " ++ string.from_int(Signal),
            Res = error(io.make_io_error(Msg))
        ;
            WaitRes = child_abnormal_exit,
            Msg = "process exited abnormally",
            Res = error(io.make_io_error(Msg))
        ;
            WaitRes = error(Error),
            Res = error(Error)
        )
    ;
        SpawnRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
