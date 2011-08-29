%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module callout.
:- import_module compose.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module index_view.
:- import_module pager.
:- import_module screen.
:- import_module text_entry.
:- import_module thread_pager.

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
    curs.start(!IO),
    create_screen(Screen, !IO),
    search_and_open_index(Screen, Terms, !IO),
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

:- pred search_and_open_index(screen::in, list(string)::in, io::di, io::uo)
    is det.

search_and_open_index(Screen, Terms, !IO) :-
    update_message(Screen, set_info("Searching..."), !IO),
    panel.update_panels(!IO),
    run_notmuch(["search", "--format=json" | Terms], parse_threads_list,
        Threads, !IO),
    setup_index_view(Threads, IndexInfo, !IO),
    string.format("Found %d threads.", [i(length(Threads))], Message),
    update_message(Screen, set_info(Message), !IO),
    index_loop(Screen, IndexInfo, !IO).

:- pred index_loop(screen::in, index_info::in, io::di, io::uo) is det.

index_loop(Screen, !.IndexInfo, !IO) :-
    draw_index_view(Screen, !.IndexInfo, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    index_view_input(Screen, Char, MessageUpdate, Action, !IndexInfo),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        update_message(Screen, clear_message, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = open_pager(ThreadId),
        open_thread_pager(Screen, ThreadId, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = enter_limit,
        text_entry(Screen, "Limit to messages matching: ", Return, !IO),
        (
            Return = yes(String),
            search_and_open_index(Screen, [String], !IO)
        ;
            Return = no,
            update_message(Screen, clear_message, !IO),
            index_loop(Screen, !.IndexInfo, !IO)
        )
    ;
        Action = start_compose,
        start_compose(Screen, !IO),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = quit
    ).

%-----------------------------------------------------------------------------%

:- pred open_thread_pager(screen::in, thread_id::in, io::di, io::uo) is det.

open_thread_pager(Screen, thread_id(ThreadId), !IO) :-
    run_notmuch(["show", "--format=json", "thread:" ++ ThreadId],
        parse_messages_list, Messages : list(message), !IO),
    Rows = Screen ^ rows,
    Cols = Screen ^ cols,
    setup_thread_pager(Rows - 2, Cols, Messages, ThreadPagerInfo),
    thread_pager_loop(Screen, ThreadPagerInfo, !IO).

:- pred thread_pager_loop(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

thread_pager_loop(Screen, !.Info, !IO) :-
    draw_thread_pager(Screen, !.Info, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    thread_pager_input(Char, Action, MessageUpdate, !Info),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        thread_pager_loop(Screen, !.Info, !IO)
    ;
        Action = leave
    ).

%-----------------------------------------------------------------------------%

:- pred open_pager(screen::in, thread_id::in, io::di, io::uo) is det.

open_pager(Screen, thread_id(ThreadId), !IO) :-
    run_notmuch(["show", "--format=json", "thread:" ++ ThreadId],
        parse_messages_list, Messages : list(message), !IO),
    Cols = Screen ^ cols,
    setup_pager(Cols, Messages, PagerInfo),
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

:- pred draw_bar(screen::in, io::di, io::uo) is det.

draw_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
