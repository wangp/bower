%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module string.
:- import_module time.

:- import_module callout.
:- import_module compose.
:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module index_view.
:- import_module pager.
:- import_module quote_arg.
:- import_module recall.
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
    (
        Terms = [],
        open_index(Screen, [], !IO)
    ;
        Terms = [_ | _],
        search_and_open_index(Screen, Terms, !IO)
    ),
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
    string.format("Found %d threads.", [i(length(Threads))], Message),
    update_message(Screen, set_info(Message), !IO),
    open_index(Screen, Threads, !IO).

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
    index_view_input(Screen, Char, MessageUpdate, Action, !IndexInfo),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = open_pager(ThreadId),
        open_thread_pager(Screen, ThreadId, NeedRefresh, !IO),
        (
            NeedRefresh = yes,
            refresh_index_line(Screen, ThreadId, !IndexInfo, !IO)
        ;
            NeedRefresh = no
        ),
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
        Action = start_recall,
        select_recall(Screen, MaybeSelected, !IO),
        (
            MaybeSelected = yes(Message),
            continue_postponed(Screen, Message, !IO)
        ;
            MaybeSelected = no
        ),
        index_loop(Screen, !.IndexInfo, !IO)
    ;
        Action = quit
    ).

:- pred refresh_index_line(screen::in, thread_id::in,
    index_info::in, index_info::out, io::di, io::uo) is det.

refresh_index_line(Screen, ThreadId, !IndexInfo, !IO) :-
    Term = thread_id_to_search_term(ThreadId),
    run_notmuch(["search", "--format=json", Term],
        parse_threads_list, Threads, !IO),
    (
        Threads = [Thread],
        time(Time, !IO),
        Nowish = localtime(Time),
        replace_index_cursor_line(Nowish, Thread, !IndexInfo)
    ;
        ( Threads = []
        ; Threads = [_, _ | _]
        ),
        update_message(Screen, set_warning("Error refreshing index."), !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred open_thread_pager(screen::in, thread_id::in, bool::out, io::di, io::uo)
    is det.

open_thread_pager(Screen, ThreadId, NeedRefresh, !IO) :-
    run_notmuch(["show", "--format=json", thread_id_to_search_term(ThreadId)],
        parse_messages_list, Messages0 : list(message), !IO),
    list.filter_map(filter_unwanted_messages, Messages0, Messages),

    time(Time, !IO),
    Nowish = localtime(Time),
    Rows = Screen ^ rows,
    Cols = Screen ^ cols,
    setup_thread_pager(Nowish, Rows - 2, Cols, Messages, ThreadPagerInfo, Count),
    string.format("Showing message 1 of %d.", [i(Count)], Msg),
    update_message(Screen, set_info(Msg), !IO),
    thread_pager_loop(Screen, NeedRefresh, ThreadPagerInfo, _, !IO).

:- pred filter_unwanted_messages(message::in, message::out) is semidet.

filter_unwanted_messages(!Message) :-
    Tags = !.Message ^ m_tags,
    not list.contains(Tags, "draft"),
    not list.contains(Tags, "deleted"),

    Replies0 = !.Message ^ m_replies,
    list.filter_map(filter_unwanted_messages, Replies0, Replies),
    !Message ^ m_replies := Replies.

:- pred thread_pager_loop(screen::in, bool::out,
    thread_pager_info::in, thread_pager_info::out,
    io::di, io::uo) is det.

thread_pager_loop(Screen, NeedRefresh, !Info, !IO) :-
    draw_thread_pager(Screen, !.Info, !IO),
    draw_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    thread_pager_input(Char, Action, MessageUpdate, !Info),
    update_message(Screen, MessageUpdate, !IO),
    (
        Action = continue,
        thread_pager_loop(Screen, NeedRefresh, !Info, !IO)
    ;
        Action = start_reply(Message, ReplyKind),
        start_reply(Screen, Message, ReplyKind, !IO),
        thread_pager_loop(Screen, NeedRefresh, !Info, !IO)
    ;
        Action = prompt_save_attachment(Content),
        prompt_save_attachment(Screen, Content, !IO),
        thread_pager_loop(Screen, NeedRefresh, !Info, !IO)
    ;
        Action = leave(TagGroups),
        ( map.is_empty(TagGroups) ->
            NeedRefresh = no
        ;
            map.foldl2(apply_tag_delta, TagGroups, yes, Res, !IO),
            (
                Res = yes,
                update_message(Screen, clear_message, !IO)
            ;
                Res = no,
                Msg = "Encountered problems while applying tags.",
                update_message(Screen, set_warning(Msg), !IO)
            ),
            NeedRefresh = yes
        )
    ).

:- pred apply_tag_delta(set(tag_delta)::in, list(message_id)::in,
    bool::in, bool::out, io::di, io::uo) is det.

apply_tag_delta(TagDeltaSet, MessageIds, !AccRes, !IO) :-
    set.to_sorted_list(TagDeltaSet, TagDeltas),
    tag_messages(TagDeltas, MessageIds, Res, !IO),
    (
        Res = ok
    ;
        Res = error(_),
        !:AccRes = no
    ).

:- pred tag_messages(list(tag_delta)::in, list(message_id)::in, io.res::out,
    io::di, io::uo) is det.

tag_messages(TagDeltas, MessageIds, Res, !IO) :-
    (
        MessageIds = [],
        Res = ok
    ;
        MessageIds = [_ | _],
        IdStrings = list.map(message_id_to_search_term, MessageIds),
        Args = ["notmuch", "tag"] ++ TagDeltas ++ ["--" | IdStrings],
        args_to_quoted_command(Args, Command),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                Res = ok
            ;
                string.format("notmuch tag returned exit status %d",
                    [i(ExitStatus)], Msg),
                Res = error(io.make_io_error(Msg))
            )
        ;
            CallRes = error(Error),
            Res = error(Error)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred prompt_save_attachment(screen::in, part::in, io::di, io::uo)
    is det.

prompt_save_attachment(Screen, Part, !IO) :-
    MessageId = Part ^ pt_msgid,
    PartId = Part ^ pt_part,
    MaybeInitial = Part ^ pt_filename,
    (
        MaybeInitial = yes(Initial)
    ;
        MaybeInitial = no,
        MessageId = message_id(IdStr),
        Initial = string.format("%s.part_%d", [s(IdStr), i(PartId)])
    ),
    text_entry_initial(Screen, "Save to file: ", Initial, Return, !IO),
    (
        Return = yes(FileName),
        FileName \= ""
    ->
        FollowSymLinks = no,
        io.file_type(FollowSymLinks, FileName, ResType, !IO),
        (
            ResType = ok(_),
            % XXX prompt to overwrite
            Error = FileName ++ " already exists.",
            MessageUpdate = set_warning(Error)
        ;
            ResType = error(_),
            % This assumes the file doesn't exist.
            do_save_attachment(MessageId, PartId, FileName, Res, !IO),
            (
                Res = ok,
                MessageUpdate = set_info("Attachment saved.")
            ;
                Res = error(Error),
                ErrorMessage = io.error_message(Error),
                MessageUpdate = set_warning(ErrorMessage)
            )
        )
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_save_attachment(message_id::in, int::in, string::in,
    io.res::out, io::di, io::uo) is det.

do_save_attachment(MessageId, Part, FileName, Res, !IO) :-
    Args = ["notmuch", "show", "--format=raw", "--part=" ++ from_int(Part),
        message_id_to_search_term(MessageId)],
    args_to_quoted_command(Args, no, redirect_output(FileName), Command),
    io.call_system(Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("notmuch show returned exit status %d",
                [i(ExitStatus)], Msg),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred open_pager(screen::in, thread_id::in, io::di, io::uo) is det.

open_pager(Screen, ThreadId, !IO) :-
    run_notmuch(["show", "--format=json", thread_id_to_search_term(ThreadId)],
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
