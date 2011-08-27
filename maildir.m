%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.

:- pred generate_unique_name(string::out, io::di, io::uo) is det.

:- pred add_draft(string::in, string::in, io.res::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- import_module quote_arg.

:- mutable(counter, int, -1, ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

generate_unique_name(Name, !IO) :-
    unique_name_components(Time, Pid, Host, !IO),
    get_counter(Counter0, !IO),
    ( Counter0 < 0 ->
        Counter = (Time * Pid) mod 10000
    ;
        Counter = Counter0 + 1
    ),
    set_counter(Counter, !IO),
    string.format("%d.%d_%d.%s", [i(Time), i(Pid), i(Counter), s(Host)], Name).

:- pred unique_name_components(int::out, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unique_name_components(Time::out, Pid::out, Host::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char buf[256];

    Time = (MR_Integer) time(NULL);
    Pid = (MR_Integer) getpid();

    if (gethostname(buf, sizeof(buf)) == 0) {
        buf[sizeof(buf) - 1] = '\\0';
        MR_make_aligned_string_copy(Host, buf);
    } else {
        MR_string_const(Host, ""localhost"");
    }
    
    IO = IO0;
").

%-----------------------------------------------------------------------------%

add_draft(DbPath, FileName, Res, !IO) :-
    generate_unique_name(UniqueName, !IO),
    TmpFileName = DbPath ++ "/.Drafts/tmp/" ++ UniqueName,
    NewFileName = DbPath ++ "/.Drafts/new/" ++ UniqueName,
    % Copy FileName to tmp first.
    args_to_quoted_command(["cp", "-n", FileName, TmpFileName], CopyCommand),
    io.call_system(CopyCommand, CopyRes, !IO),
    (
        CopyRes = ok(CopyStatus),
        ( CopyStatus = 0 ->
            % Link from tmp to new.
            args_to_quoted_command( ["cp", "-l", TmpFileName, NewFileName],
                LinkCommand),
            io.call_system(LinkCommand, LinkRes, !IO),
            (
                LinkRes = ok(LinkStatus),
                ( LinkStatus = 0 ->
                    Res = ok
                ;
                    Msg = string.format("cp returned exit status %d",
                        [i(LinkStatus)]),
                    Res = error(io.make_io_error(Msg))
                )
            ;
                LinkRes = error(Error),
                Res = error(Error)
            ),
            io.remove_file(TmpFileName, _, !IO)
        ;
            Msg = string.format("cp returned exit status %d", [i(CopyStatus)]),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CopyRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
