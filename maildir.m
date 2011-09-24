%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.

:- pred add_draft(string::in, io.res::out, io::di, io::uo) is det.

:- pred find_drafts(io.res(list(string))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module callout.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

add_draft(FileName, Res, !IO) :-
    Args = [
        "notmuch-deliver",
        "-t", "draft",
        "-r", "inbox",
        "-r", "unread",
        "-f", drafts_dir
    ],
    args_to_quoted_command(Args, redirect_input(FileName), no, Command),
    io.call_system(Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = string.format("notmuch-deliver returned with exit status %d",
                [i(ExitStatus)]),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

:- func drafts_dir = string.

drafts_dir = "Drafts".

%-----------------------------------------------------------------------------%

find_drafts(Res, !IO) :-
    get_notmuch_config("database.path", ResDbPath, !IO),
    (
        ResDbPath = ok(DbPath),
        DirName = DbPath / drafts_dir / "cur",
        dir.foldl2(find_drafts_2, DirName, [], ResFold, !IO),
        (
            ResFold = ok(FileNames),
            Res = ok(FileNames)
        ;
            ResFold = error(_, Error),
            Res = error(Error)
        )
    ;
        ResDbPath = error(Error),
        Res = error(Error)
    ).

:- pred find_drafts_2(string::in, string::in, io.file_type::in, bool::out,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

find_drafts_2(DirName, BaseName, FileType, Continue, !FileNames, !IO) :-
    ( FileType = regular_file ->
        FileName = DirName / BaseName,
        !:FileNames = [FileName | !.FileNames]
    ;
        true
    ),
    Continue = yes.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
