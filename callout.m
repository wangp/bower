%-----------------------------------------------------------------------------%

:- module callout.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.
:- import_module json.

%-----------------------------------------------------------------------------%

:- pred run_notmuch(list(string)::in, pred(json, T)::in(pred(in, out) is det),
    T::out, io::di, io::uo) is det.

:- pred parse_messages_list(json::in, list(message)::out) is det.

:- pred parse_threads_list(json::in, list(thread)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.

:- import_module popen.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

run_notmuch(Args, P, Result, !IO) :-
    args_to_quoted_command(["notmuch" | Args], Command),
    popen(Command, CommandResult, !IO),
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

parse_messages_list(JSON, Messages) :-
    ( JSON = array([List]) ->
        parse_inner_message_list(List, Messages)
    ; JSON = array([]) ->
        Messages = []
    ;
        notmuch_json_error
    ).

:- pred parse_inner_message_list(json::in, list(message)::out) is det.

parse_inner_message_list(JSON, Messages) :-
    ( JSON = array(Array) ->
        list.map(parse_message, Array, Messages)
    ;
        notmuch_json_error
    ).

:- pred parse_message(json::in, message::out) is det.

parse_message(JSON, Message) :-
    ( JSON = array([JSON1, JSON2]) ->
        parse_inner_message_list(JSON2, Replies),
        parse_message_details(JSON1, Replies, Message)
    ;
        notmuch_json_error
    ).

:- pred parse_message_details(json::in, list(message)::in, message::out) is det.

parse_message_details(JSON, Replies, Message) :-
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
        Message = message(message_id(Id), Subject, From, To, Date, Body, Replies)
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
        Thread = thread(thread_id(Id), Timestamp, Authors, Subject, Tags,
            Matched, Total)
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
