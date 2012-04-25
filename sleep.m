% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module sleep.
:- interface.

:- import_module io.

:- pred sleep(int::in, io::di, io::uo) is det.

:- pred usleep(int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <unistd.h>
").

:- pragma foreign_proc("C",
    sleep(Secs::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    sleep(Secs);
").

:- pragma foreign_proc("C",
    usleep(Usecs::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    usleep(Usecs);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
