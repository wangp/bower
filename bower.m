% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module bower.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module async.
:- import_module callout.
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
    async.install_sigchld_handler(!IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        get_default_search_terms(Terms, !IO)
    ;
        Args = [_ | _],
        Terms = string.join_list(" ", Args)
    ),
    curs.start(!IO),
    create_screen(Screen, !IO),
    draw_bar(Screen, !IO),
    curs.refresh(!IO),
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

:- pred get_default_search_terms(string::out, io::di, io::uo) is det.

get_default_search_terms(Terms, !IO) :-
    get_notmuch_config("bower:search_alias", "default", Res, !IO),
    (
        Res = ok(Value),
        Value \= ""
    ->
        Terms = Value
    ;
        Terms = "~d {last week}.."
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
