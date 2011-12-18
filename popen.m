% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module popen.
:- interface.

:- import_module io.

:- pred popen(string::in, io.res(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- pragma foreign_decl("C", local, "
    #include <stdio.h>
").

%-----------------------------------------------------------------------------%

popen(Command, Res, !IO) :-
    popen_2(Command, Errno, ExitStatus, RevList, !IO),
    ( Errno = 0 ->
        ( ExitStatus = 0 ->
            ( RevList = [String0] ->
                String = String0
            ;
                list.reverse(RevList, List),
                string.append_list(List, String)
            ),
            Res = ok(String)
        ;
            Msg = "process returned with exit code " ++
                string.from_int(ExitStatus),
            Res = error(io.make_io_error(Msg))
        )
    ;
        Res = error(io.make_io_error(strerror(Errno)))
    ).

:- pred popen_2(string::in, int::out, int::out, list(string)::uo,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    popen_2(Command::in, Errno::out, ExitStatus::out, RevList::uo,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    static char buf[16384];
    FILE *fp;
    int n;
    MR_String s;

    RevList = MR_list_empty();

    fp = popen(Command, ""r"");

    if (fp) {
        for (;;) {
            n = fread(buf, 1, sizeof(buf) - 1, fp);
            if (n < 0) {
                Errno = errno;
                break;
            } else if (n == 0) {
                Errno = 0;
                break;
            } else {
                buf[n] = '\\0';
                MR_make_aligned_string_copy(s, buf);
                RevList = MR_list_cons(s, RevList); 
            }
        }
        ExitStatus = pclose(fp);
        ExitStatus = WEXITSTATUS(ExitStatus);
    } else {
        Errno = errno;
        ExitStatus = -1;
    }
    IO = IO0;
").

:- func strerror(int::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    strerror(Errno::in) = (String::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, may_not_duplicate],
"
    MR_make_aligned_string_copy(String, strerror(Errno));
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
