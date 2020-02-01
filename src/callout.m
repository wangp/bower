% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module callout.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module set.

:- import_module data.
:- import_module json.
:- import_module prog_config.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

:- pred get_notmuch_config(prog_config::in, string::in,
    maybe_error(string)::out, io::di, io::uo) is det.

:- pred get_notmuch_config(prog_config::in, string::in, string::in,
    maybe_error(string)::out, io::di, io::uo) is det.

:- type suspend_curses
    --->    soft_suspend_curses
    ;       no_suspend_curses.

:- pred run_notmuch(prog_config, list(string), suspend_curses,
    pred(json, T), maybe_error(T), io, io).
:- mode run_notmuch(in, in, in,
    in(pred(in, out) is det), out, di, uo) is det.

:- pred run_notmuch(prog_config, list(string), redirect_stderr, suspend_curses,
    pred(json, T), maybe_error(T), io, io).
:- mode run_notmuch(in, in, in, in,
    in(pred(in, out) is det), out, di, uo) is det.

:- pred parse_messages_list(maybe(set(tag))::in, json::in, list(message)::out)
    is det.

:- pred parse_top_message(json::in, message::out) is det.

:- pred parse_message_for_recall(json::in, message_for_recall::out) is det.

:- pred parse_part(message_id::in, maybe_decrypted::in, json::in, part::out)
    is det.

:- pred parse_threads_list(json::in, list(thread)::out) is det.

:- pred parse_message_id_list(json::in, list(message_id)::out) is det.

:- pred parse_address_count_list(json::in, list(pair(int, string))::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module integer.
:- import_module map.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.

:- import_module call_system.
:- import_module mime_type.
:- import_module time_util.

:- use_module curs.

%-----------------------------------------------------------------------------%

get_notmuch_config(Config, Key, Res, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, ["config", "get", Key],
        redirect_input("/dev/null"), no_redirect, redirect_stderr("/dev/null"),
        run_in_foreground, Command),
    call_system_capture_stdout(Command, no, Res0, !IO),
    (
        Res0 = ok(Value0),
        Value = string.strip(Value0),
        Res = ok(Value)
    ;
        Res0 = error(Error),
        Notmuch = command_prefix(shell_quoted(NotmuchString), _),
        string.append_list(["Error running ", NotmuchString, ": ",
            io.error_message(Error)], Message),
        Res = error(Message)
    ).

get_notmuch_config(Config, Section, Key, Res, !IO) :-
    get_notmuch_config(Config, quote_arg(Section ++ "." ++ Key), Res, !IO).

%-----------------------------------------------------------------------------%

run_notmuch(Config, Args, SuspendCurs, Parser, Result, !IO) :-
    run_notmuch(Config, Args, no_redirect, SuspendCurs, Parser, Result, !IO).

run_notmuch(Config, Args, RedirectStderr, SuspendCurs, Parser, Result, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, Args,
        redirect_input("/dev/null"), no_redirect, RedirectStderr,
        run_in_foreground, Command),
    promise_equivalent_solutions [Result, !:IO] (
        call_command_parse_json(Command, SuspendCurs, Parser, Result, !IO)
    ).

:- pred call_command_parse_json(string, suspend_curses,
    pred(json, T), maybe_error(T), io, io).
:- mode call_command_parse_json(in, in,
    in(pred(in, out) is det), out, di, uo) is cc_multi.

call_command_parse_json(Command, SuspendCurs, Parser, Result, !IO) :-
    (
        SuspendCurs = soft_suspend_curses,
        curs.soft_suspend(
            call_system_capture_stdout(Command, no), CommandResult, !IO)
    ;
        SuspendCurs = no_suspend_curses,
        call_system_capture_stdout(Command, no, CommandResult, !IO)
    ),
    (
        CommandResult = ok(String),
        parse_json(String, ParseResult),
        (
            ParseResult = ok(JSON),
            Parser(JSON, T),
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

parse_messages_list(CustomExcludeTags, JSON, Messages) :-
    ( JSON = list([List]) ->
        parse_inner_message_list(CustomExcludeTags, List, Messages)
    ; JSON = list([]) ->
        Messages = []
    ;
        notmuch_json_error
    ).

parse_top_message(JSON, Message) :-
    parse_message_details(no, JSON, [], Message).

:- pred parse_inner_message_list(maybe(set(tag))::in, json::in,
    list(message)::out) is det.

parse_inner_message_list(CustomExcludeTags, JSON, Messages) :-
    ( JSON = list(Array) ->
        list.map(parse_message(CustomExcludeTags), Array, Messagess),
        list.condense(Messagess, Messages)
    ;
        notmuch_json_error
    ).

:- pred parse_message(maybe(set(tag))::in, json::in, list(message)::out)
    is det.

parse_message(CustomExcludeTags, JSON, Messages) :-
    ( JSON = list([JSON1, JSON2]) ->
        parse_inner_message_list(CustomExcludeTags, JSON2, Replies),
        ( JSON1 = null ->
            Message = excluded_message(no, no, no, no, Replies)
        ;
            parse_message_details(CustomExcludeTags, JSON1, Replies, Message)
        ),
        % Completely hide excluded messages without replies.
        ( Message = excluded_message(_, _, _, _, []) ->
            Messages = []
        ;
            Messages = [Message]
        )
    ;
        notmuch_json_error
    ).

:- pred parse_message_details(maybe(set(tag))::in, json::in,
    list(message)::in, message::out) is det.

parse_message_details(CustomExcludeTags, JSON, Replies, Message) :-
    parse_message_for_recall(JSON, Message0),
    Message0 = message_for_recall(MessageId, Timestamp, Headers, TagSet),
    (
        JSON/"excluded" = bool(yes),
        really_exclude_message(CustomExcludeTags, TagSet)
    ->
        Message = excluded_message(yes(MessageId), yes(Timestamp),
            yes(Headers), yes(TagSet), Replies)
    ;
        parse_body(JSON, MessageId, Body),
        Message = message(MessageId, Timestamp, Headers, TagSet, Body,
            Replies)
    ).

:- pred really_exclude_message(maybe(set(tag))::in, set(tag)::in) is semidet.

really_exclude_message(CustomExcludeTags, MessageTags) :-
    (
        CustomExcludeTags = no
    ;
        CustomExcludeTags = yes(ExcludeTags),
        set.member(Tag, ExcludeTags),
        set.member(Tag, MessageTags)
    ).

parse_message_for_recall(JSON, Message) :-
    (
        JSON/"id" = unesc_string(Id),
        JSON/"timestamp" = Timestamp0,
        parse_timestamp(Timestamp0, Timestamp),
        JSON/"headers" = map(HeaderMap),
        map.foldl(parse_header, HeaderMap, init_headers, Headers),
        JSON/"tags" = list(TagsList),
        list.map(parse_tag, TagsList, Tags)
    ->
        MessageId = message_id(Id),
        TagSet = set.from_list(Tags),
        Message = message_for_recall(MessageId, Timestamp, Headers, TagSet)
    ;
        notmuch_json_error
    ).

:- pred parse_header(string::in, json::in, headers::in, headers::out) is semidet.

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

:- pred parse_body(json::in, message_id::in, list(part)::out) is det.

parse_body(JSON, MessageId, Body) :-
    ( JSON/"body" = list(BodyList) ->
        IsDecrypted = not_decrypted,
        list.map(parse_part(MessageId, IsDecrypted), BodyList, Body)
    ;
        notmuch_json_error
    ).

parse_part(MessageId, IsDecrypted0, JSON, Part) :-
    (
        % XXX notmuch/devel/schemata says id can be a string
        JSON/"id" = int(PartId),
        JSON/"content-type" = unesc_string(ContentTypeString)
    ->
        ContentType = make_mime_type(ContentTypeString),
        ( JSON/"content-disposition" = unesc_string(Disposition) ->
            MaybeContentDisposition = yes(content_disposition(Disposition))
        ;
            MaybeContentDisposition = no
        ),
        ( mime_type.is_multipart(ContentType) ->
            ( JSON/"content" = list(SubParts0) ->
                ( ContentType = mime_type.multipart_encrypted ->
                    % XXX check for encstatus even for other content-types?
                    ( JSON/"encstatus" = EncStatus ->
                        ( parse_encstatus(EncStatus, Encryption0) ->
                            Encryption = Encryption0
                        ;
                            notmuch_json_error
                        )
                    ;
                        Encryption = encrypted
                    ),
                    (
                        Encryption = decryption_good,
                        IsDecrypted = is_decrypted
                    ;
                        ( Encryption = encrypted
                        ; Encryption = decryption_bad
                        ; Encryption = not_encrypted % should not occur
                        ),
                        IsDecrypted = not_decrypted
                    )
                ;
                    Encryption = not_encrypted,
                    IsDecrypted = IsDecrypted0
                ),
                ( JSON/"sigstatus" = SigStatus ->
                    ( parse_sigstatus(SigStatus, Signatures0) ->
                        Signatures = Signatures0
                    ;
                        notmuch_json_error
                    )
                ;
                    Signatures = []
                ),
                list.map(parse_part(MessageId, IsDecrypted), SubParts0, SubParts),
                Content = subparts(Encryption, Signatures, SubParts),
                MaybeFilename = no,
                MaybeContentLength = no,
                MaybeContentTransferEncoding = no
            ;
                notmuch_json_error
            )
        ; ContentType = mime_type.message_rfc822 ->
            ( JSON/"content" = list(List) ->
                list.map(parse_encapsulated_message(MessageId, IsDecrypted0),
                    List, EncapMessages),
                Content = encapsulated_messages(EncapMessages),
                MaybeFilename = no,
                MaybeContentLength = no,
                MaybeContentTransferEncoding = no,
                IsDecrypted = IsDecrypted0
            ;
                notmuch_json_error
            )
        ;
            % Leaf part.
            ( JSON/"content" = unesc_string(ContentString) ->
                Content = text(ContentString)
            ;
                % "content" is unavailable for non-text parts.
                % We can those by running notmuch show --part=N id:NNN
                Content = unsupported
            ),
            ( JSON/"filename" = unesc_string(Filename) ->
                MaybeFilename = yes(filename(Filename))
            ;
                MaybeFilename = no
            ),
            /*
            ( JSON/"content-charset" = unesc_string(Charset) ->
                MaybeContentCharset = yes(content_charset(Charset))
            ;
                MaybeContentCharset = no
            ),
            */
            ( JSON/"content-length" = int(Length) ->
                MaybeContentLength = yes(content_length(Length))
            ;
                MaybeContentLength = no
            ),
            ( JSON/"content-transfer-encoding" = unesc_string(Encoding) ->
                MaybeContentTransferEncoding =
                    yes(content_transfer_encoding(Encoding))
            ;
                MaybeContentTransferEncoding = no
            ),
            IsDecrypted = IsDecrypted0
        ),
        Part = part(MessageId, yes(PartId), ContentType,
            MaybeContentDisposition, Content, MaybeFilename,
            MaybeContentLength, MaybeContentTransferEncoding, IsDecrypted)
    ;
        notmuch_json_error
    ).

:- pred parse_encstatus(json::in, encryption::out) is semidet.

parse_encstatus(JSON, Encryption) :-
    JSON = list([Obj]),
    Obj/"status" = unesc_string(Status),
    (
        Status = "good",
        Encryption = decryption_good
    ;
        Status = "bad",
        Encryption = decryption_bad
    ).

:- pred parse_sigstatus(json::in, list(signature)::out) is semidet.

parse_sigstatus(JSON, Signatures) :-
    JSON = list(Objs),
    map(parse_signature, Objs, Signatures).

:- pred parse_signature(json::in, signature::out) is semidet.

parse_signature(JSON, Signature) :-
    JSON/"status" = unesc_string(Status0),
    (
        Status0 = "none", % documented but not in the source code?
        SigStatus = none
    ;
        Status0 = "good",
        ( JSON/"fingerprint" = unesc_string(Fingerprint) ->
            MaybeFingerprint = yes(Fingerprint)
        ;
            MaybeFingerprint = no
        ),
        (
            Created0 = JSON/"created",
            parse_timestamp(Created0, Created)
        ->
            MaybeCreated = yes(Created)
        ;
            MaybeCreated = no
        ),
        (
            Expires0 = JSON/"expires",
            parse_timestamp(Expires0, Expires)
        ->
            MaybeExpires = yes(Expires)
        ;
            MaybeExpires = no
        ),
        ( JSON/"userid" = unesc_string(UserId) ->
            MaybeUserId = yes(UserId)
        ;
            MaybeUserId = no
        ),
        SigStatus = good(MaybeFingerprint, MaybeCreated, MaybeExpires,
            MaybeUserId)
    ;
        (
            Status0 = "bad",
            Status1 = bad
        ;
            Status0 = "error",
            Status1 = error
        ;
            Status0 = "unknown",
            Status1 = unknown
        ),
        ( JSON/"keyid" = unesc_string(KeyId) ->
            MaybeKeyId = yes(KeyId)
        ;
            MaybeKeyId = no
        ),
        SigStatus = not_good(Status1, MaybeKeyId)
    ),
    ( JSON/"errors" = int(Errors0) ->
        Errors = Errors0
    ;
        Errors = 0
    ),
    Signature = signature(SigStatus, Errors).

:- pred parse_encapsulated_message(message_id::in, maybe_decrypted::in,
    json::in, encapsulated_message::out) is det.

parse_encapsulated_message(MessageId, IsDecrypted0, JSON, EncapMessage) :-
    (
        JSON/"headers" = map(HeaderMap),
        map.foldl(parse_header, HeaderMap, init_headers, Headers),
        JSON/"body" = list(BodyList),
        list.map(parse_part(MessageId, IsDecrypted0), BodyList, Body)
    ->
        EncapMessage = encapsulated_message(Headers, Body)
    ;
        notmuch_json_error
    ).

%-----------------------------------------------------------------------------%

parse_threads_list(Json, Threads) :-
    ( Json = list(List) ->
        list.map(parse_thread, List, Threads)
    ;
        notmuch_json_error
    ).

:- pred parse_thread(json::in, thread::out) is det.

parse_thread(Json, Thread) :-
    (
        Json/"thread" = unesc_string(Id),
        Json/"timestamp" = Timestamp0,
        Json/"authors" = unesc_string(Authors),
        Json/"subject" = unesc_string(Subject),
        Json/"tags" = list(TagsList),
        Json/"matched" = int(Matched),
        Json/"total" = int(Total),
        parse_timestamp(Timestamp0, Timestamp),
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
        JSON = list(List),
        list.map(parse_message_id, List, MessageId0)
    ->
        MessageId = MessageId0
    ;
        notmuch_json_error
    ).

:- pred parse_message_id(json::in, message_id::out) is semidet.

parse_message_id(unesc_string(Id), message_id(Id)).

%-----------------------------------------------------------------------------%

parse_address_count_list(Json, AddressCounts) :-
    ( Json = list(List) ->
        list.map(parse_address_count, List, AddressCounts)
    ;
        notmuch_json_error
    ).

:- pred parse_address_count(json::in, pair(int, string)::out) is det.

parse_address_count(Json, Result) :-
    (
        Json/"count" = int(Count),
        Json/"name-addr" = unesc_string(NameAddr)
    ->
        Result = Count - NameAddr
    ;
        notmuch_json_error
    ).

%-----------------------------------------------------------------------------%

:- pred parse_timestamp(json::in, timestamp::out) is semidet.

parse_timestamp(Json, Timestamp) :-
    (
        Json = int(Int),
        % XXX At the time of writing, notmuch returns negative values for
        % timestamps beyond Y2038 even on platforms where time_t is 64-bits.
        Timestamp = timestamp(float(Int))
    ;
        Json = integer(Integer),
        % Convert directly to float in case int is 32-bits.
        % This case is currently unreachable in practice.
        Str = integer.to_string(Integer),
        string.to_float(Str, Float),
        Timestamp = timestamp(Float)
    ).

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
