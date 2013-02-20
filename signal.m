% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module signal.
:- interface.

:- import_module bool.
:- import_module io.

:- pred ignore_sigint(bool::in, io::di, io::uo) is det.

:- pred get_sigint_count(int::out, io::di, io::uo) is det.

:- pred kill_self_with_sigint(io::di, io::uo) is erroneous.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <signal.h>

    static sig_atomic_t sigint_count;
    static void sigint_handler(int sig);
").

:- pragma foreign_code("C", "
static void
sigint_handler(int sig)
{
    (void) sig;
    sigint_count++;
}
").

:- pragma foreign_proc("C",
    ignore_sigint(Ignore::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = (Ignore ? SIG_IGN : sigint_handler);
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGINT, &act, NULL);
").

:- pragma foreign_proc("C",
    get_sigint_count(N::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    N = sigint_count;
").

:- pragma foreign_proc("C",
    kill_self_with_sigint(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    struct sigaction act;

    /* Restore default signal handler. */
    act.sa_handler = SIG_DFL;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGINT, &act, NULL);

    /* Kill self. */
    kill(getpid(), SIGINT);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
