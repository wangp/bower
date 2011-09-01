%-----------------------------------------------------------------------------%

:- module sys_util.
:- interface.

:- import_module io.

:- pred get_pid(int::out, io::di, io::uo) is det.

:- pred get_hostname(string::out, io::di, io::uo) is det.

:- pred get_domainname(string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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
% vim: ft=mercury ts=4 sts=4 sw=4 et
