% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module callout.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module data.
:- import_module json.

%-----------------------------------------------------------------------------%

:- pred get_notmuch_config(string::in, io.res(string)::out, io::di, io::uo)
    is det.

:- pred get_notmuch_config(string::in, string::in, io.res(string)::out,
    io::di, io::uo) is det.

:- pred run_notmuch(list(string)::in, pred(json, T)::in(pred(in, out) is det),
    maybe_error(T)::out, io::di, io::uo) is det.

:- pred parse_messages_list(json::in, list(message)::out) is det.

:- pred parse_top_message(json::in, message::out) is det.

:- pred parse_threads_list(json::in, list(thread)::out) is det.

:- pred parse_message_id_list(json::in, list(message_id)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module parsing_utils.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module call_system.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module string_util.

%-----------------------------------------------------------------------------%

get_notmuch_config(Key, Res, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    % Key is assumed to be quoted already.
    Command = Notmuch ++ "config get " ++ Key ++ " 2>/dev/null",
    call_system_capture_stdout(Command, no, Res0, !IO),
    (
        Res0 = ok(Value0),
        Value = string.strip(Value0),
        Res = ok(Value)
    ;
        Res0 = error(_),
        Res = Res0
    ).

get_notmuch_config(Section, Key, Res, !IO) :-
    get_notmuch_config(quote_arg(Section ++ "." ++ Key), Res, !IO).

%-----------------------------------------------------------------------------%

run_notmuch(Args, P, Result, !IO) :-
    promise_equivalent_solutions [Result, !:IO] (
        run_notmuch_cc(Args, P, Result, !IO)
    ).

:- pred run_notmuch_cc(list(string)::in,
    pred(json, T)::in(pred(in, out) is det), maybe_error(T)::out,
    io::di, io::uo) is cc_multi.

run_notmuch_cc(Args, P, Result, !IO) :-
    args_to_quoted_command(Args, Command),
    get_notmuch_prefix(Notmuch, !IO),
    call_system_capture_stdout(Notmuch ++ Command, no, CommandResult, !IO),
    (
        CommandResult = ok(String),
        parse_json(String, ParseResult),
        (
            ParseResult = ok(JSON),
            P(JSON, T),
            Result = ok(T)
        ;
            ParseResult = error(yes(Msg), Line, Column),
            string.format("line %d, column %d: %s",
                [i(Line), i(Column), s(Msg)], ErrorMsg),
            Result = error(ErrorMsg)
        ;
            ParseResult = error(no, Line, Column),
            string.format("line %d, column %d",
                [i(Line), i(Column)], ErrorMsg),
            Result = error(ErrorMsg)
        )
    ;
        CommandResult = error(Error),
        Result = error(io.error_message(Error))
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

parse_top_message(JSON, Message) :-
    parse_message_details(JSON, [], Message).

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
        JSON/"id" = unesc_string(Id),
        MessageId = message_id(Id),
        JSON/"timestamp" = int(Timestamp),
        JSON/"headers" = map(HeaderMap),
        map.foldl(parse_header, HeaderMap, init_headers, Headers),
        JSON/"tags" = array(TagsList),
        list.map(parse_tag, TagsList, Tags),
        JSON/"body" = array(BodyList),
        list.map(parse_part(MessageId), BodyList, Body)
    ->
        TagSet = set.from_list(Tags),
        Message = message(MessageId, Timestamp, Headers, TagSet, Body, Replies)
    ;
        notmuch_json_error
    ).

:- pred parse_header(key::in, json::in, headers::in, headers::out) is semidet.

parse_header(Key, unesc_string(Value), !Headers) :-
    ( Key = "Date" ->
        !Headers ^ h_date := header_value(Value)
    ; Key = "From" ->
        !Headers ^ h_from := header_value(Value)
    ; Key = "To" ->
        !Headers ^ h_to := header_value(Value)
    ; Key = "Cc" ->
        !Headers ^ h_cc := header_value(Value)
    ; Key = "Bcc" ->
        !Headers ^ h_bcc := header_value(Value)
    ; Key = "Subject" ->
        % notmuch should provide the decoded value.
        !Headers ^ h_subject := decoded_unstructured(Value)
    ; Key = "Reply-To" ->
        !Headers ^ h_replyto := header_value(Value)
    ; Key = "References" ->
        !Headers ^ h_references := header_value(Value)
    ; Key = "In-Reply-To" ->
        !Headers ^ h_inreplyto := header_value(Value)
    ;
        % Some other headers should be decoded_unstructured as well.
        Rest0 = !.Headers ^ h_rest,
        map.insert(Key, header_value(Value), Rest0, Rest),
        !Headers ^ h_rest := Rest
    ).

:- pred parse_part(message_id::in, json::in, part::out) is det.

parse_part(MessageId, JSON, Part) :-
    (
        JSON/"id" = int(PartId),
        JSON/"content-type" = unesc_string(ContentType)
    ->
        % NOTE: ContentType must be compared case-insensitively.
        ( strcase_prefix(ContentType, "multipart/") ->
            ( JSON/"content" = array(SubParts0) ->
                list.map(parse_part(MessageId), SubParts0, SubParts),
                Content = subparts(SubParts),
                MaybeFilename = no,
                MaybeEncoding = no,
                MaybeLength = no
            ;
                notmuch_json_error
            )
        ; strcase_equal(ContentType, "message/rfc822") ->
            ( JSON/"content" = array(List) ->
                list.map(parse_encapsulated_message(MessageId), List,
                    EncapMessages),
                Content = encapsulated_messages(EncapMessages),
                MaybeFilename = no,
                MaybeEncoding = no,
                MaybeLength = no
            ;
                notmuch_json_error
            )
        ;
            % Leaf part.
            ( JSON/"content" = unesc_string(ContentString) ->
                Content = text(ContentString)
            ; strcase_equal(ContentType, "text/html") ->
                % As a special case, display text/html parts inline by default.
                Content = unsupported_inline
            ;
                % "content" is unavailable for non-text parts.
                % We can those by running notmuch show --part=N id:NNN
                Content = unsupported
            ),
            ( JSON/"filename" = unesc_string(Filename) ->
                MaybeFilename = yes(Filename)
            ;
                MaybeFilename = no
            ),
            ( JSON/"content-transfer-encoding" = unesc_string(Encoding) ->
                MaybeEncoding = yes(Encoding)
            ;
                MaybeEncoding = no
            ),
            ( JSON/"content-length" = int(Length) ->
                MaybeLength = yes(Length)
            ;
                MaybeLength = no
            )
        ),
        Part = part(MessageId, PartId, ContentType, Content, MaybeFilename,
            MaybeEncoding, MaybeLength)
    ;
        notmuch_json_error
    ).

:- pred parse_encapsulated_message(message_id::in, json::in,
    encapsulated_message::out) is det.

parse_encapsulated_message(MessageId, JSON, EncapMessage) :-
    (
        JSON/"headers" = map(HeaderMap),
        map.foldl(parse_header, HeaderMap, init_headers, Headers),
        JSON/"body" = array(BodyList),
        list.map(parse_part(MessageId), BodyList, Body)
    ->
        EncapMessage = encapsulated_message(Headers, Body)
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
        Json/"thread" = unesc_string(Id),
        Json/"timestamp" = int(Timestamp),
        Json/"authors" = unesc_string(Authors),
        Json/"subject" = unesc_string(Subject),
        Json/"tags" = array(TagsList),
        Json/"matched" = int(Matched),
        Json/"total" = int(Total),
        list.map(parse_tag, TagsList, Tags)
    ->
        TagSet = set.from_list(Tags),
        Thread = thread(thread_id(Id), Timestamp, Authors, Subject, TagSet,
            Matched, Total)
    ;
        notmuch_json_error
    ).

:- pred parse_tag(json::in, tag::out) is semidet.

parse_tag(Json, tag(Tag)) :-
    Json = unesc_string(Tag).

%-----------------------------------------------------------------------------%

parse_message_id_list(JSON, MessageId) :-
    (
        JSON = array(List),
        list.map(parse_message_id, List, MessageId0)
    ->
        MessageId = MessageId0
    ;
        notmuch_json_error
    ).

:- pred parse_message_id(json::in, message_id::out) is semidet.

parse_message_id(unesc_string(Id), message_id(Id)).

%-----------------------------------------------------------------------------%

:- func json / string = json is semidet.

map(Map) / Key = Value :-
    map.search(Map, Key, Value).

:- func unesc_string(string::out) = (json::in) is semidet.

unesc_string(unescape(EscString)) = string(EscString).

:- pred notmuch_json_error is erroneous.

notmuch_json_error :-
    error("notmuch_json_error").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
