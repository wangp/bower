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
    Terms = Args,
    run_notmuch(["search", "--format=json" | Terms], parse_threads_list,
        Threads, !IO),
    curs.start(!IO),
    create_screen(Screen, !IO),
    open_index(Screen, Threads, !IO),
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

:- pred open_index(screen::in, list(thread)::in, io::di, io::uo) is det.

open_index(Screen, Threads, !IO) :-
    setup_index_view(Threads, IndexInfo, !IO),
    index_loop(Screen, IndexInfo, !IO).

:- pred index_loop(screen::in, index_info::in, io::di, io::uo) is det.

index_loop(Screen, !.IndexInfo, !IO) :-
    draw_index_view(Screen, !.IndexInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    index_view_input(Screen, Char, Action, !IndexInfo),
    (
        Action = continue,
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = open_pager(ThreadId),
        open_pager(Screen, ThreadId, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = quit
    ).

%-----------------------------------------------------------------------------%

:- pred open_pager(screen::in, thread_id::in, io::di, io::uo) is det.

open_pager(Screen, thread_id(ThreadId), !IO) :-
    run_notmuch(["show", "--format=json", "thread:" ++ ThreadId],
        parse_messages_list, Messages : list(message), !IO),
    Cols = Screen ^ cols,
    setup_pager(Cols, Messages, PagerInfo, !IO),
    pager_loop(Screen, PagerInfo, !IO).

:- pred pager_loop(screen::in, pager_info::in, io::di, io::uo)
    is det.

pager_loop(Screen, !.PagerInfo, !IO) :-
    draw_pager(Screen, !.PagerInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    pager_input(Screen, Char, Action, MessageUpdate, !PagerInfo),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        pager_loop(Screen, !.PagerInfo, !IO)
    ;
        Action = leave_pager
    ).

%-----------------------------------------------------------------------------%

:- pred get_char(char::out, io::di, io::uo) is det.

get_char(Char, !IO) :-
    curs.getch(C, !IO),
    ( char.from_int(C, Char0) ->
        Char = Char0
    ;
        get_char(Char, !IO)
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
