%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.

%-----------------------------------------------------------------------------%

:- type tag_delta == string. % +tag or -tag

:- pred add_sent(string::in, io.res::out, io::di, io::uo) is det.

:- pred add_draft(string::in, io.res::out, io::di, io::uo) is det.

:- pred find_drafts(list(message_id)::out, io::di, io::uo) is det.

:- pred tag_messages(list(tag_delta)::in, list(message_id)::in, io.res::out,
    io::di, io::uo) is det.

:- pred tag_thread(list(tag_delta)::in, thread_id::in, io.res::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- import_module callout.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

add_sent(FileName, Res, !IO) :-
    call_notmuch_deliver(FileName, "Sent",
        ["--tag=sent", "--remove-tag=unread"],
        Res, !IO).

add_draft(FileName, Res, !IO) :-
    call_notmuch_deliver(FileName, "Drafts",
        ["--tag=draft", "--remove-tag=inbox", "--remove-tag=unread"],
        Res, !IO).

:- pred call_notmuch_deliver(string::in, string::in, list(string)::in,
    io.res::out, io::di, io::uo) is det.

call_notmuch_deliver(FileName, Folder, TagOps, Res, !IO) :-
    % XXX do we need -f?
    Args = [Folder | TagOps],
    args_to_quoted_command(Args, redirect_input(FileName), no, Command),
    get_notmuch_deliver_prefix(NotmuchDeliver, !IO),
    io.call_system(NotmuchDeliver ++ Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = string.format("notmuch-deliver returned with exit status %d",
                [i(ExitStatus)]),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

find_drafts(MessageIds, !IO) :-
    run_notmuch([
        "search", "--format=json", "--output=messages", "--",
        "tag:draft", "-tag:deleted"
    ], parse_message_id_list, Result, !IO),
    (
        Result = ok(MessageIds)
    ;
        Result = error(Error),
        unexpected($module, $pred, io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%

tag_messages(TagDeltas, MessageIds, Res, !IO) :-
    (
        MessageIds = [],
        Res = ok
    ;
        MessageIds = [_ | _],
        IdStrings = list.map(message_id_to_search_term, MessageIds),
        do_tag(TagDeltas, IdStrings, Res, !IO)
    ).

tag_thread(TagDeltas, ThreadId, Res, !IO) :-
    do_tag(TagDeltas, [thread_id_to_search_term(ThreadId)], Res, !IO).

:- pred do_tag(list(tag_delta)::in, list(string)::in, io.res::out,
    io::di, io::uo) is det.

do_tag(TagDeltas, SearchTerms, Res, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    Args = list.condense([["tag"], TagDeltas, ["--"], SearchTerms]),
    args_to_quoted_command(Args, Command),
    io.call_system(Notmuch ++ Command, CallRes, !IO),
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
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
