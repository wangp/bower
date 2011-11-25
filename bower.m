%-----------------------------------------------------------------------------%

:- module bower.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module curs.
:- import_module index_view.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    #include <locale.h>
").

%-----------------------------------------------------------------------------%

main(!IO) :-
    setlocale(!IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        Terms = "~lw"
    ;
        Args = [_ | _],
        Terms = string.join_list(" ", Args)
    ),
    curs.start(!IO),
    create_screen(Screen, !IO),
    open_index(Screen, Terms, !IO),
    curs.stop(!IO).

:- pred setlocale(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setlocale(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    setlocale(LC_ALL, """");
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
