%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

:- import_module callout.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module index_view.
:- import_module pager.
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
    ( Args = ["--show-thread", TId] ->
        run_notmuch(["show", "--format=json", "thread:" ++ TId],
            parse_messages_list, Messages : list(message), !IO),
        io.write(Messages, !IO),
        io.nl(!IO)
    ; Args = ["--pager", TId] ->
        run_notmuch(["show", "--format=json", "thread:" ++ TId],
            parse_messages_list, Messages : list(message), !IO),
        pager(Messages, !IO)
    ; Args = ["--search" | Terms] ->
        run_notmuch(["search", "--format=json" | Terms],
            parse_threads_list, Threads, !IO),
        io.write(Threads, !IO)
    ; Args = ["--index" | Terms] ->
        run_notmuch(["search", "--format=json" | Terms],
            parse_threads_list, Threads, !IO),
        curs.session(interactive(Threads), !IO)
    ;
        io.write_string("command line error\n", !IO)
    ).

:- pred setlocale(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setlocale(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    setlocale(LC_ALL, """");
    IO = IO0;
").

:- pred interactive(list(thread)::in, io::di, io::uo) is det.

interactive(Threads, !IO) :-
    create_screen(Screen, !IO),
    setup_index_view(Threads, IndexInfo, !IO),
    interactive_loop(Screen, IndexInfo, !IO).

:- pred interactive_loop(screen::in, index_info::in, io::di, io::uo) is det.

interactive_loop(Screen, IndexInfo, !IO) :-
    draw_index_view(Screen, IndexInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    curs.getch(C, !IO),
    ( C = char.to_int('q') ->
        true
    ;
        interactive_loop(Screen, IndexInfo, !IO)
    ).

:- pred draw_bar(screen::in, io::di, io::uo) is det.

draw_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
