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
:- import_module time.

:- import_module quote_arg.
:- import_module sys_util.
:- import_module time_util.

:- mutable(counter, int, -1, ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

generate_unique_name(Name, !IO) :-
    time(Time0, !IO),
    time_to_int(Time0, Time),
    get_pid(Pid, !IO),
    get_hostname(Host, !IO),
    get_counter(Counter0, !IO),
    ( Counter0 < 0 ->
        Counter = (Time * Pid) mod 10000
    ;
        Counter = Counter0 + 1
    ),
    set_counter(Counter, !IO),
    string.format("%d.%d_%d.%s", [i(Time), i(Pid), i(Counter), s(Host)], Name).

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
