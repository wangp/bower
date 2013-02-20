% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module signal.
:- interface.

:- import_module bool.
:- import_module io.

:- pred ignore_sigint(bool::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <signal.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    ignore_sigint(Ignore::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = (Ignore ? SIG_IGN : SIG_DFL);
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGINT, &act, NULL);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
