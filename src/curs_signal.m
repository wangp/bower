% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module curs_signal.
:- interface.

:- import_module io.

:- pred install_suspend_handlers(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <signal.h>
    #include <ncurses.h>

    static void curs_signal_handler(int sig);
").

:- pragma foreign_code("C", "

/* Inspired by mutt... */

static bool isendwin_state = FALSE;

static void curs_signal_handler(int sig)
{
    int save_errno = errno;

    switch (sig) {
        case SIGTSTP:
            isendwin_state = isendwin();
            curs_set(1);
            if (!isendwin_state) {
                endwin();
            }
            kill(0, SIGSTOP);
            /* fallthrough */

        case SIGCONT:
            if (!isendwin_state) {
                refresh();
            }
            /* We never hide the cursor so no need to restore */
            /* a hidden cursor state. */
            break;
    }

    errno = save_errno;
}

").

:- pragma foreign_proc("C",
    install_suspend_handlers(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = curs_signal_handler;
    sigemptyset(&act.sa_mask);
    sigaddset(&act.sa_mask, SIGTSTP);
    act.sa_flags = 0;

#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;
#endif

    sigaction(SIGCONT, &act, NULL);
    sigaction(SIGTSTP, &act, NULL);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
