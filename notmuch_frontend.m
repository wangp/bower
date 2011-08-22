%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.

:- import_module data.
:- import_module json.
:- import_module pager.
:- import_module popen.

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
        io.write(Threads, !IO),
        io.nl(!IO)
    ;
        io.write_string("command line error\n", !IO)
    ).

:- pred run_notmuch(list(string)::in, pred(json, T)::in(pred(in, out) is det),
    T::out, io::di, io::uo) is det.

run_notmuch(Args, P, Result, !IO) :-
    % XXX escape shell characters
    Command = ["notmuch" | Args],
    CommandString = string.join_list(" ", Command),
    popen(CommandString, CommandResult, !IO),
    (
        CommandResult = ok(String),
        parse_json(String, ParseResult),
        (
            ParseResult = ok(JSON),
            P(JSON, Result)
        ;
            ParseResult = error(_, _, _),
            error(string(ParseResult))
        )
    ;
        CommandResult = error(_),
        error(string(CommandResult))
    ).

%-----------------------------------------------------------------------------%

:- pred parse_messages_list(json::in, list(message)::out) is det.

parse_messages_list(JSON, Messages) :-
    ( JSON = array([List]) ->
        parse_messages_list(List, cord.init, Cord),
        Messages = list(Cord)
    ; JSON = array([]) ->
        Messages = []
    ;
        notmuch_json_error
    ).

:- pred parse_messages_list(json::in, cord(message)::in, cord(message)::out)
    is det.

parse_messages_list(JSON, !Messages) :-
    ( JSON = array(List) ->
        list.foldl(parse_messages_cons_list, List, !Messages)
    ;
        notmuch_json_error
    ).

:- pred parse_messages_cons_list(json::in,
    cord(message)::in, cord(message)::out) is det.

parse_messages_cons_list(JSON, !Messages) :-
    ( JSON = array([]) ->
        true
    ; JSON = array([Head | Tail]) ->
        ( Head = array(_) ->
            parse_messages_cons_list(Head, !Messages)
        ;
            parse_message(Head, Message),
            snoc(Message, !Messages)
        ),
        parse_messages_cons_list(array(Tail), !Messages)
    ;
        notmuch_json_error
    ).

:- pred parse_message(json::in, message::out) is det.

parse_message(JSON, Message) :-
    (
        JSON/"id" = string(Id),
        JSON/"headers" = Headers,
        Headers/"Subject" = string(Subject),
        Headers/"From" = string(From),
        Headers/"To" = string(To),
        Headers/"Date" = string(Date),
        JSON/"body" = array(BodyList),
        list.foldl(parse_content, BodyList, cord.init, Body)
    ->
        Message = message(Id, Subject, From, To, Date, Body)
    ;
        notmuch_json_error
    ).

:- pred parse_content(json::in, cord(content)::in, cord(content)::out) is det.

parse_content(JSON, !Contents) :-
    (
        JSON/"id" = int(Id),
        JSON/"content-type" = string(ContentType)
    ->
        ( JSON/"filename" = string(Filename) ->
            MaybeFilename = yes(Filename)
        ;
            MaybeFilename = no
        ),
        ( JSON/"content" = string(ContentString) ->
            Content = content(Id, ContentType, yes(ContentString),
                MaybeFilename),
            snoc(Content, !Contents)
        ; JSON/"content" = array(SubParts) ->
            list.foldl(parse_content, SubParts, !Contents)
        ;
            % "content" is unavailable for non-text parts.
            % We can those by running notmuch show --part=N id:NNN
            Content = content(Id, ContentType, no, MaybeFilename),
            snoc(Content, !Contents)
        )
    ;
        notmuch_json_error
    ).

%-----------------------------------------------------------------------------%

:- pred parse_threads_list(json::in, list(thread)::out) is det.

parse_threads_list(Json, Threads) :-
    ( Json = array(List) ->
        list.map(parse_thread, List, Threads)
    ;
        notmuch_json_error
    ).

:- pred parse_thread(json::in, thread::out) is det.

parse_thread(Json, Thread) :-
    (
        Json/"thread" = string(Id),
        Json/"timestamp" = int(Timestamp),
        Json/"authors" = string(Authors),
        Json/"subject" = string(Subject),
        Json/"tags" = array(TagsList),
        Json/"matched" = int(Matched),
        Json/"total" = int(Total),
        list.map(parse_tag, TagsList, Tags)
    ->
        Thread = thread(Id, Timestamp, Authors, Subject, Tags, Matched, Total)
    ;
        notmuch_json_error
    ).

:- pred parse_tag(json::in, string::out) is semidet.

parse_tag(Json, Tag) :-
    Json = string(Tag).

%-----------------------------------------------------------------------------%

:- func json / string = json is semidet.

map(Map) / Key = Value :-
    map.search(Map, Key, Value).

:- pred notmuch_json_error is erroneous.

notmuch_json_error :-
    error("notmuch_json_error").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
