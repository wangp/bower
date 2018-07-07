%-----------------------------------------------------------------------------%

:- module test_process.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module process.

%-----------------------------------------------------------------------------%

main(!IO) :-
    % Send input through rev(1). At least the util-linux version will deadlock
    % if we do not interleave reading and writing.
    posix_spawn_get_stdin_stdout("rev", [], SpawnRes, !IO),
    (
        SpawnRes = ok({Pid, StdinPipe, StdoutPipe}),
        write_and_read_concurrently_and_close_both(StdinPipe, input,
            StdoutPipe, ReadWriteRes, Buffers, !IO),
        (
            ReadWriteRes = ok,
            ( if make_utf8_string(no, Buffers, Output) then
                ( if Output = expected_output then
                    io.write_string("ok\n", !IO)
                else
                    io.write_string("unexpected output:\n", !IO),
                    io.write_string(Output, !IO),
                    io.set_exit_status(1, !IO)
                )
            else
                io.write_string("output not UTF-8\n", !IO),
                io.set_exit_status(1, !IO)
            ),
            wait_pid(Pid, blocking, WaitRes, !IO),
            ( if WaitRes = child_exit(0) then
                true
            else
                report_error(WaitRes, !IO)
            )
        ;
            ReadWriteRes = error(Error),
            report_error(Error, !IO)
        )
    ;
        SpawnRes = error(Error),
        report_error(Error, !IO)
    ).

    % Generate >64 KB of input, enough to exceed the default pipe capacity on
    % Linux.
:- func input = string.

input = join_list("\n", lines).

:- func lines = list(string).

lines =  Lines :-
    S16 = "123456789ABCDEF.",
    S15 = "123456789abcdef",
    Line = append_list(duplicate(63, S16) ++ [S15]),
    Lines = duplicate(65, Line) ++ ["abcd"].

:- func expected_output = string.

expected_output = join_list("\n", map(reverse_chars, lines)).

:- func reverse_chars(string) = string.

reverse_chars(S) = string.from_rev_char_list(string.to_char_list(S)).

:- pred report_error(T::in, io::di, io::uo) is det.

report_error(Error, !IO) :-
    io.write(Error, !IO),
    io.nl(!IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
