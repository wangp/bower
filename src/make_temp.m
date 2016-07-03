% Bower - a frontend for the Notmuch email system
% Copyright (C) 2016 Peter Wang

:- module make_temp.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred make_temp_suffix(string::in, maybe_error(string)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module string.

:- pragma foreign_decl("C", local, "#include <stdlib.h>").

make_temp_suffix(Suffix, Res, !IO) :-
    io.get_environment_var("TMPDIR", MaybeTMPDIR, !IO),
    (
        MaybeTMPDIR = yes(Dir)
    ;
        MaybeTMPDIR = no,
        io.get_environment_var("TMP", MaybeTMP, !IO),
        (
            MaybeTMP = yes(Dir)
        ;
            MaybeTMP = no,
            Dir = "/tmp"
        )
    ),
    DirSep = char_to_string(dir.directory_separator),
    mkstemps(Dir, DirSep, "mtmp", Suffix, Error, Name, !IO),
    ( Error = 0 ->
        Res = ok(Name)
    ;
        Res = error("mkstemps failed (errno " ++ from_int(Error) ++ ")")
    ).

:- pred mkstemps(string::in, string::in, string::in, string::in, int::out,
    string::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    mkstemps(Dir::in, DirSep::in, Prefix::in, Suffix::in, Error::out,
        FileName::uo, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int fd, err;

    FileName = MR_make_string(MR_ALLOC_ID, ""%s%s%sXXXXXX%s"", Dir, DirSep,
        Prefix, Suffix);
    fd = mkstemps(FileName, strlen(Suffix));
    if (fd == -1) {
        Error = -1;
    } else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        Error = err;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
