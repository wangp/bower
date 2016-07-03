% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module sys_util.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred get_pid(int::out, io::di, io::uo) is det.

:- pred get_hostname_fqdn(maybe(string)::out, maybe(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local,
"
    #include <unistd.h>
    #include <netdb.h>
    #include <sys/socket.h>
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

get_hostname_fqdn(MaybeHostName, MaybeFQDN, !IO) :-
    get_hostname_fqdn_2(HostName, FQDN, !IO),
    ( HostName = "" ->
        MaybeHostName = no
    ;
        MaybeHostName = yes(HostName)
    ),
    ( FQDN = "" ->
        MaybeFQDN = no
    ;
        MaybeFQDN = yes(FQDN)
    ).

:- pred get_hostname_fqdn_2(string::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_hostname_fqdn_2(HostName::out, FQDN::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    char node[256];
    struct addrinfo hints;
    struct addrinfo *h;

    HostName = MR_make_string_const("""");
    FQDN = MR_make_string_const("""");

    memset(&hints, 0, sizeof(hints));
    hints.ai_flags = AI_CANONNAME;
    hints.ai_family = AF_UNSPEC;

    if (gethostname(node, sizeof(node)) == 0) {
        MR_make_aligned_string_copy(HostName, node);

        if (getaddrinfo(node, NULL, &hints, &h) == 0) {
            const char *p = strchr(h->ai_canonname, '.');
            if (p != NULL) {
                MR_make_aligned_string_copy(FQDN, h->ai_canonname);
            }
            freeaddrinfo(h);
        }
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
