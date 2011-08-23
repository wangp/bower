%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module callout.
:- import_module data.
:- import_module index_view.
:- import_module pager.

%-----------------------------------------------------------------------------%

main(!IO) :-
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
        index_view(Threads, !IO)
    ;
        io.write_string("command line error\n", !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
