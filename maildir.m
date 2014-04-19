% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module data.
:- import_module prog_config.
:- import_module tags.

%-----------------------------------------------------------------------------%

:- pred add_sent(prog_config::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred add_draft(prog_config::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred find_drafts(prog_config::in, maybe(thread_id)::in,
    list(message_id)::out, io::di, io::uo) is det.

:- pred tag_messages(prog_config::in, list(tag_delta)::in,
    list(message_id)::in, maybe_error::out, io::di, io::uo) is det.

:- pred tag_threads(prog_config::in, list(tag_delta)::in, list(thread_id)::in,
    maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- import_module callout.
:- import_module prog_config.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

add_sent(Config, FileName, Res, !IO) :-
    get_notmuch_config(Config, "bower:maildir.sent_folder", ConfigRes, !IO),
    (
        ConfigRes = ok(SentFolder)
    ;
        ConfigRes = error(_),
        SentFolder = default_sent_folder
    ),
    call_notmuch_deliver(Config, FileName, SentFolder,
        ["--tag=sent", "--remove-tag=unread"],
        Res, !IO).

add_draft(Config, FileName, Res, !IO) :-
    get_notmuch_config(Config, "bower:maildir.drafts_folder", ConfigRes, !IO),
    (
        ConfigRes = ok(DraftsFolder)
    ;
        ConfigRes = error(_),
        DraftsFolder = default_drafts_folder
    ),
    call_notmuch_deliver(Config, FileName, DraftsFolder,
        ["--tag=draft", "--remove-tag=inbox", "--remove-tag=unread"],
        Res, !IO).

:- pred call_notmuch_deliver(prog_config::in, string::in, string::in,
    list(string)::in, maybe_error::out, io::di, io::uo) is det.

call_notmuch_deliver(Config, FileName, Folder, TagOps, Res, !IO) :-
    get_notmuch_deliver_command(Config, NotmuchDeliver),
    % XXX do we need -f?
    make_quoted_command(NotmuchDeliver, [Folder | TagOps],
        redirect_input(FileName), no_redirect, Command),
    io.call_system(Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = string.format("notmuch-deliver returned with exit status %d",
                [i(ExitStatus)]),
            Res = error(Msg)
        )
    ;
        CallRes = error(Error),
        Res = error(io.error_message(Error))
    ).

:- func default_sent_folder = string.

default_sent_folder = "Sent".

:- func default_drafts_folder = string.

default_drafts_folder = "Drafts".

%-----------------------------------------------------------------------------%

find_drafts(Config, MaybeThreadId, MessageIds, !IO) :-
    (
        MaybeThreadId = yes(ThreadId),
        ThreadSearchTerm = [thread_id_to_search_term(ThreadId)]
    ;
        MaybeThreadId = no,
        ThreadSearchTerm = []
    ),
    run_notmuch(Config, [
        "search", "--format=json", "--output=messages", "--",
        "tag:draft", "-tag:deleted" | ThreadSearchTerm
    ], parse_message_id_list, Result, !IO),
    (
        Result = ok(MessageIds)
    ;
        Result = error(Error),
        unexpected($module, $pred, Error)
    ).

%-----------------------------------------------------------------------------%

tag_messages(Config, TagDeltas, MessageIds, Res, !IO) :-
    (
        MessageIds = [],
        Res = ok
    ;
        MessageIds = [_ | _],
        IdStrings = list.map(message_id_to_search_term, MessageIds),
        do_tag(Config, TagDeltas, IdStrings, Res, !IO)
    ).

tag_threads(Config, TagDeltas, ThreadIds, Res, !IO) :-
    SearchTerms = list.map(thread_id_to_search_term, ThreadIds),
    do_tag(Config, TagDeltas, SearchTerms, Res, !IO).

:- pred do_tag(prog_config::in, list(tag_delta)::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

do_tag(Config, TagDeltas, SearchTerms, Res, !IO) :-
    get_notmuch_command(Config, Notmuch),
    TagDeltaStrings = list.map(tag_delta_to_string, TagDeltas),
    make_quoted_command(Notmuch,
        ["tag" | TagDeltaStrings] ++ ["--" | SearchTerms],
        Command),
    io.call_system(Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("notmuch tag returned exit status %d",
                [i(ExitStatus)], Msg),
            Res = error(Msg)
        )
    ;
        CallRes = error(Error),
        Res = error(io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
