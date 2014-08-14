% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module sys_util.
:- interface.

:- import_module io.

:- pred get_pid(int::out, io::di, io::uo) is det.

:- pred get_hostname(string::out, io::di, io::uo) is det.

:- pred get_domainname(string::out, io::di, io::uo) is det.

:- pred make_temp_suffix(string::in, string::uo, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module exception.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- pragma foreign_decl("C", local,
"
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_pid(Pid::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Pid = (MR_Integer) getpid();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_hostname(Host::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    char buf[256];

    if (gethostname(buf, sizeof(buf)) == 0) {
        buf[sizeof(buf) - 1] = '\\0';
        MR_make_aligned_string_copy(Host, buf);
    } else {
        MR_string_const(Host, ""localhost"");
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

get_domainname(Domain, !IO) :-
    % Mutt does something like this.
    io.open_input("/etc/resolv.conf", ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        get_domainname_2(Stream, MaybeDomain, !IO),
        (
            MaybeDomain = yes(Domain)
        ;
            MaybeDomain = no,
            Domain = "localdomain"
        ),
        io.close_input(Stream, !IO)
    ;
        ResOpen = error(_),
        Domain = "localdomain"
    ).

:- pred get_domainname_2(io.input_stream::in, maybe(string)::out,
    io::di, io::uo) is det.

get_domainname_2(Stream, MaybeDomain, !IO) :-
    io.read_line_as_string(Stream, ResRead, !IO),
    (
        ResRead = ok(Line),
        Words = string.words(Line),
        ( Words = ["domain", Domain] ->
            MaybeDomain = yes(Domain)
        ; Words = ["search", Domain] ->
            MaybeDomain = yes(Domain)
        ;
            get_domainname_2(Stream, MaybeDomain, !IO)
        )
    ;
        ResRead = eof,
        MaybeDomain = no
    ;
        ResRead = error(_),
        MaybeDomain = no
    ).

%-----------------------------------------------------------------------------%

make_temp_suffix(Suffix, Name, !IO) :-
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
        true
    ;
        throw(software_error("mkstemps failed: error = " ++ from_int(Error)))
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
