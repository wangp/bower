% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module sys_util.
:- interface.

:- import_module io.

:- pred get_pid(int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local,
"
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_pid(Pid::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Pid = (MR_Integer) getpid();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
