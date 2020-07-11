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

% NOTE: these parser predicates may throw notmuch_json_errors
% which are intended to be caught by run_notmuch.

:- pred parse_thread_set(maybe(set(tag))::in, json::in, list(message)::out)
    is det.

:- pred parse_message(json::in, message::out) is det.

:- pred parse_message_for_recall(json::in, message_for_recall::out) is det.

:- pred parse_part(message_id::in, maybe_decrypted::in, json::in, part::out)
    is det.

:- pred parse_search_summary(json::in, list(thread)::out) is det.

:- pred parse_search_messages(json::in, list(message_id)::out) is det.

:- pred parse_address_count_list(json::in, list(pair(int, string))::out)
    is det.

:- pred parse_reply(json::in, reply_headers::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module integer.
:- import_module map.
:- import_module parsing_utils.
:- import_module string.
:- use_module exception.

:- import_module call_system.
:- import_module mime_type.
:- import_module process.
:- import_module time_util.

:- use_module curs.

:- type notmuch_json_error
    --->    notmuch_json_error(string).

%-----------------------------------------------------------------------------%

get_notmuch_config(Config, Key, Res, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, ["config", "get", Key],
        redirect_input("/dev/null"), no_redirect, redirect_stderr("/dev/null"),
        run_in_foreground, Command),
    call_system_capture_stdout(Command, environ([]), no, Res0, !IO),
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
            call_system_capture_stdout(Command, environ([]), no),
            CommandResult, !IO)
    ;
        SuspendCurs = no_suspend_curses,
        call_system_capture_stdout(Command, environ([]), no,
            CommandResult, !IO)
    ),
    (
        CommandResult = ok(String),
        parse_json(String, ParseResult),
        (
            ParseResult = ok(JSON),
            ( try []
                Parser(JSON, T)
            then
                Result = ok(T)
            catch notmuch_json_error(Error) ->
                Result = error(Error)
            )
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

    % notmuch/devel/schemata: threadid
    %
:- pred parse_thread_id(json::in, thread_id::out) is semidet.

parse_thread_id(Json, ThreadId) :-
    Json = string(Str0),
    Str = unescape(Str0),
    Str \= "", % just in case
    ThreadId = thread_id(Str).

    % notmuch/devel/schemata: messageid
    %
:- pred parse_message_id(json::in, message_id::out) is semidet.

parse_message_id(Json, MessageId) :-
    Json = string(Str0),
    Str = unescape(Str0),
    Str \= "", % just in case
    MessageId = message_id(Str).

    % notmuch/devel/schemata: unix_time
    % Number of seconds since the Epoch
    %
:- pred parse_unix_time(json::in, timestamp::out) is semidet.

parse_unix_time(Json, Timestamp) :-
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

    % notmuch/devel/schemata: thread_set
    % A top-level set of threads (do_show)
    % Returned by notmuch show without a --part argument
    %
parse_thread_set(CustomExcludeTags, JSON, Messages) :-
    ( JSON = list(List) ->
        list.map(parse_thread(CustomExcludeTags), List, Messages0),
        list.condense(Messages0, Messages)
    ;
        exception.throw(notmuch_json_error("parse_thread_set: expected list"))
    ).

    % notmuch/devel/schemata: thread
    % Top-level messages in a thread (show_messages)
    %
:- pred parse_thread(maybe(set(tag))::in, json::in, list(message)::out)
    is det.

parse_thread(CustomExcludeTags, JSON, Messages) :-
    ( JSON = list(List) ->
        list.map(parse_thread_node(CustomExcludeTags), List, Messages0),
        list.filter_map(maybe_is_yes, Messages0, Messages)
    ;
        exception.throw(notmuch_json_error("parse_thread: expected list"))
    ).

    % notmuch/devel/schemata: thread_node
    % A message and its replies (show_messages)
    %
:- pred parse_thread_node(maybe(set(tag))::in, json::in, maybe(message)::out)
    is det.

parse_thread_node(CustomExcludeTags, JSON, MaybeMessage) :-
    ( JSON = list([JSON1, JSON2]) ->
        ( JSON2 = list(List2) ->
            list.map(parse_thread_node(CustomExcludeTags), List2, Replies0),
            list.filter_map(maybe_is_yes, Replies0, Replies)
        ;
            exception.throw(notmuch_json_error(
                "parse_thread_node: expected replies list"))
        ),
        ( JSON1 = null ->
            Message = excluded_message(no, no, no, no, Replies)
        ;
            parse_message_maybe_exclude(CustomExcludeTags, JSON1, Replies,
                Message)
        ),
        % Completely hide excluded messages without replies.
        ( Message = excluded_message(_, _, _, _, []) ->
            MaybeMessage = no
        ;
            MaybeMessage = yes(Message)
        )
    ;
        exception.throw(notmuch_json_error(
            "parse_thread_node: expected list (2 elements)"))
    ).

    % notmuch/devel/schemata: message
    % A message (format_part_sprinter)
    %
parse_message(JSON, Message) :-
    parse_message_maybe_exclude(no, JSON, [], Message).

:- pred parse_message_maybe_exclude(maybe(set(tag))::in, json::in,
    list(message)::in, message::out) is det.

parse_message_maybe_exclude(CustomExcludeTags, JSON, Replies, Message) :-
    parse_message_for_recall(JSON, Message0),
    Message0 = message_for_recall(MessageId, Timestamp, Headers, TagSet),
    (
        % Not documented in notmuch/devel/schemata
        JSON/"excluded" = bool(yes),
        really_exclude_message(CustomExcludeTags, TagSet)
    ->
        Message = excluded_message(yes(MessageId), yes(Timestamp),
            yes(Headers), yes(TagSet), Replies)
    ;
        ( JSON/"body" = list([BodyObj]) ->
            IsDecrypted = not_decrypted,
            parse_part(MessageId, IsDecrypted, BodyObj, Body)
        ;
            exception.throw(notmuch_json_error(
                "parse_message_maybe_exclude: expected body"))
        ),
        Message = message(MessageId, Timestamp, Headers, TagSet, Body, Replies)
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

    % Main part of 'message' in notmuch/devel/schemata
    %
parse_message_for_recall(JSON, Message) :-
    (
        JSON/"id" = Id,
        parse_message_id(Id, MessageId),
        % match: bool,
        % filename: [string*],
        JSON/"timestamp" = Timestamp0,
        parse_unix_time(Timestamp0, Timestamp),
        % date_relative: string,
        JSON/"tags" = list(TagsList),
        list.map(parse_tag, TagsList, Tags),

        JSON/"headers" = map(HeaderMap),
        parse_headers(HeaderMap, Headers)
        % crypto: crypto,
        % body?: [part]    # omitted if --body=false
    ->
        TagSet = set.from_list(Tags),
        Message = message_for_recall(MessageId, Timestamp, Headers, TagSet)
    ;
        exception.throw(notmuch_json_error("parse_message_for_recall"))
    ).

    % notmuch/devel/schemata: headers
    % The headers of a message or part
    %
:- pred parse_headers(map(string, json)::in, headers::out) is semidet.

parse_headers(Map, Headers) :-
    map.foldl(parse_header, Map, init_headers, Headers).

:- pred parse_header(string::in, json::in, headers::in, headers::out) is semidet.

parse_header(Key, unesc_string(Value), !Headers) :-
    ( Key = "Subject" ->
        % notmuch should provide the decoded value.
        !Headers ^ h_subject := decoded_unstructured(Value)
    ; Key = "From" ->
        !Headers ^ h_from := header_value(Value)
    ; Key = "To" ->
        !Headers ^ h_to := header_value(Value)
    ; Key = "Cc" ->
        !Headers ^ h_cc := header_value(Value)
    ; Key = "Bcc" ->
        !Headers ^ h_bcc := header_value(Value)
    ; Key = "Reply-To" ->
        !Headers ^ h_replyto := header_value(Value)
    ; Key = "Date" ->
        !Headers ^ h_date := header_value(Value)

    % The following are not listed in notmuch/devel/schemata
    ; Key = "In-Reply-To" ->
        !Headers ^ h_inreplyto := header_value(Value)
    ; Key = "References" ->
        !Headers ^ h_references := header_value(Value)
    ;
        % Some other headers should be decoded_unstructured as well.
        Rest0 = !.Headers ^ h_rest,
        map.insert(Key, header_value(Value), Rest0, Rest),
        !Headers ^ h_rest := Rest
    ).

    % notmuch/devel/schemata: part
    % A MIME part (format_part_sprinter)
    %
parse_part(MessageId, IsDecrypted0, JSON, Part) :-
    (
        JSON/"id" = Id,
        parse_part_id(Id, PartId0)
    ->
        PartId = PartId0
    ;
        exception.throw(notmuch_json_error("parse_part: expected id"))
    ),
    ( JSON/"content-type" = unesc_string(ContentTypeString) ->
        ContentType = make_mime_type(ContentTypeString)
    ;
        exception.throw(notmuch_json_error(
            "parse_part: expected content-type"))
    ),
    ( JSON/"content-disposition" = unesc_string(Disposition) ->
        MaybeContentDisposition = yes(content_disposition(Disposition))
    ;
        MaybeContentDisposition = no
    ),
    % content-id?: string,

    ( mime_type.is_multipart(ContentType) ->
        ( JSON/"content" = list(SubParts0) ->
            ( ContentType = mime_type.multipart_encrypted ->
                % XXX notmuch/devel/schemata does not limit encstatus
                % to multipart/encrypted
                ( JSON/"encstatus" = EncStatus ->
                    ( parse_encstatus(EncStatus, Encryption0) ->
                        Encryption = Encryption0
                    ;
                        exception.throw(notmuch_json_error(
                            "parse_part: expected encstatus"))
                    )
                ;
                    Encryption = encrypted
                ),
                % XXX use message.crypto instead?
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
            % XXX notmuch/devel/schemata does not limit sigstatus to multipart
            ( JSON/"sigstatus" = SigStatus ->
                ( parse_sigstatus(SigStatus, Signatures0) ->
                    Signatures = Signatures0
                ;
                    exception.throw(notmuch_json_error(
                        "parse_part: expected sigstatus"))
                )
            ;
                Signatures = []
            ),
            list.map(parse_part(MessageId, IsDecrypted), SubParts0, SubParts),
            Content = subparts(Encryption, Signatures, SubParts),
            MaybeContentCharset = no,
            MaybeFilename = no,
            MaybeContentLength = no,
            MaybeContentTransferEncoding = no
        ;
            exception.throw(notmuch_json_error(
                "parse_part: expected content for multipart"))
        )
    ; ContentType = mime_type.message_rfc822 ->
        ( JSON/"content" = list([EncapMessageJson]) ->
            parse_message_rfc822_content(MessageId, IsDecrypted0,
                EncapMessageJson, EncapMessage),
            Content = encapsulated_message(EncapMessage),
            MaybeContentCharset = no,
            MaybeFilename = no,
            MaybeContentLength = no,
            MaybeContentTransferEncoding = no,
            IsDecrypted = IsDecrypted0
        ;
            exception.throw(notmuch_json_error(
                "parse_part: expected content for message/rfc822"))
        )
    ;
        % Leaf part.
        ( JSON/"filename" = unesc_string(Filename) ->
            MaybeFilename = yes(filename(Filename))
        ;
            MaybeFilename = no
        ),
        ( JSON/"content-charset" = unesc_string(Charset) ->
            MaybeContentCharset = yes(content_charset(Charset))
        ;
            MaybeContentCharset = no
        ),

        ( JSON/"content" = unesc_string(ContentString) ->
            Content = text(ContentString)
        ;
            % A leaf part's body content is optional, but may be included if
            % it can be correctly encoded as a string.
            Content = unsupported
        ),
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

    Part = part(MessageId, yes(PartId), ContentType, MaybeContentCharset,
        MaybeContentDisposition, Content, MaybeFilename,
        MaybeContentLength, MaybeContentTransferEncoding, IsDecrypted).

:- pred parse_part_id(json::in, part_id::out) is semidet.

parse_part_id(JSON, PartId) :-
    (
        JSON = int(Int),
        PartId = part_id(Int)
    ;
        JSON = string(Str),
        % notmuch/devel/schemata suggests part id can be a string,
        % but currently it's actually always an int.
        PartId = part_id_string(unescape(Str))
    ).

    % notmuch/devel/schemata: encstatus
    % Encryption status (format_part_sprinter)
    %
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

    % notmuch/devel/schemata: sigstatus
    % Signature status (format_part_sigstatus_sprinter)
    %
:- pred parse_sigstatus(json::in, list(signature)::out) is semidet.

parse_sigstatus(JSON, Signatures) :-
    JSON = list(Objs),
    map(parse_signature, Objs, Signatures).

    % notmuch/devel/schemata: signature
    %
:- pred parse_signature(json::in, signature::out) is semidet.

parse_signature(JSON, Signature) :-
    JSON/"status" = unesc_string(Status0),
    (
        Status0 = "none",
        % Was once documented though not used, undocumented in commit 9eacd7d3
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
            parse_unix_time(Created0, Created)
        ->
            MaybeCreated = yes(Created)
        ;
            MaybeCreated = no
        ),
        (
            Expires0 = JSON/"expires",
            parse_unix_time(Expires0, Expires)
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
    ( JSON/"errors" = SigErrorsObj ->
        ( SigErrorsObj = int(NumErrors) ->
            SigErrors = sig_errors_v3(NumErrors)
        ;
            parse_sig_errors_v4(SigErrorsObj, SigErrorsV4),
            SigErrors = sig_errors_v4(SigErrorsV4)
        ),
        MaybeSigErrors = yes(SigErrors)
    ;
        MaybeSigErrors = no
    ),
    Signature = signature(SigStatus, MaybeSigErrors).

    % notmuch/devel/schemata: sig_errors
    %
:- pred parse_sig_errors_v4(json::in, list(sig_error)::out) is semidet.

parse_sig_errors_v4(JSON, Errors) :-
    % Maybe should just accept any name we find?
    Flags = [
        "key-revoked",
        "key-expired",
        "sig-expired",
        "key-missing",
        "alt-unsupported",
        "crl-missing",
        "crl-too-old",
        "bad-policy",
        "sys-error",
        "tofu-conflict"
    ],
    map(parse_sig_error(JSON), Flags, MaybeErrors),
    filter_map(maybe_is_yes, MaybeErrors, Errors).

:- pred parse_sig_error(json::in, string::in, maybe(sig_error)::out)
    is semidet.

parse_sig_error(JSON, Name, MaybeError) :-
    ( JSON/Name = Value ->
        Value = bool(Bool),
        (
            Bool = yes,
            MaybeError = yes(sig_error(Name))
        ;
            Bool = no,
            MaybeError = no
        )
    ;
        MaybeError = no
    ).

    % The content of a 'part' when content-type is "message/rfc822"
    %
:- pred parse_message_rfc822_content(message_id::in, maybe_decrypted::in,
    json::in, encapsulated_message::out) is det.

parse_message_rfc822_content(MessageId, IsDecrypted0, JSON, EncapMessage) :-
    (
        JSON/"headers" = map(HeaderMap),
        parse_headers(HeaderMap, Headers0)
    ->
        Headers = Headers0
    ;
        exception.throw(notmuch_json_error(
            "parse_encapsulated_message: expected headers"))
    ),
    ( JSON/"body" = list([BodyObj]) ->
        parse_part(MessageId, IsDecrypted0, BodyObj, Body)
    ;
        exception.throw(notmuch_json_error(
            "parse_encapsulated_message: expected body"))
    ),
    EncapMessage = encapsulated_message(Headers, Body).

%-----------------------------------------------------------------------------%

    % notmuch/devel/schemata: notmuch search --output=summary
    %
parse_search_summary(Json, Threads) :-
    ( Json = list(List) ->
        list.map(parse_thread_summary, List, Threads)
    ;
        exception.throw(notmuch_json_error(
            "parse_search_summary: expected list"))
    ).

    % notmuch/devel/schemata: thread_summary
    %
:- pred parse_thread_summary(json::in, thread::out) is det.

parse_thread_summary(Json, Thread) :-
    (
        Json/"thread" = ThreadId0,
        parse_thread_id(ThreadId0, ThreadId),
        Json/"timestamp" = Timestamp0,
        parse_unix_time(Timestamp0, Timestamp),
        % date_relative: string,
        Json/"matched" = int(Matched),
        Json/"total" = int(Total),
        Json/"authors" = unesc_string(Authors),
        Json/"subject" = unesc_string(Subject),
        Json/"tags" = list(TagsList),
        list.map(parse_tag, TagsList, Tags)
    ->
        TagSet = set.from_list(Tags),
        Thread = thread(ThreadId, Timestamp, Authors, Subject, TagSet,
            Matched, Total)
    ;
        exception.throw(notmuch_json_error("parse_thread_summary"))
    ).

:- pred parse_tag(json::in, tag::out) is semidet.

parse_tag(Json, tag(Tag)) :-
    Json = unesc_string(Tag).

%-----------------------------------------------------------------------------%

    % notmuch/devel/schemata: notmuch search --output=messages
    %
parse_search_messages(JSON, MessageId) :-
    (
        JSON = list(List),
        list.map(parse_message_id, List, MessageId0)
    ->
        MessageId = MessageId0
    ;
        exception.throw(notmuch_json_error(
            "parse_search_messages: expected list"))
    ).

%-----------------------------------------------------------------------------%

    % Not documented in notmuch/devel/schemata
    %
parse_address_count_list(Json, AddressCounts) :-
    ( Json = list(List) ->
        list.map(parse_address_count, List, AddressCounts)
    ;
        exception.throw(notmuch_json_error("parse_address_count_list"))
    ).

    % Not documented in notmuch/devel/schemata
    %
:- pred parse_address_count(json::in, pair(int, string)::out) is det.

parse_address_count(Json, Result) :-
    (
        Json/"count" = int(Count),
        Json/"name-addr" = unesc_string(NameAddr)
    ->
        Result = Count - NameAddr
    ;
        exception.throw(notmuch_json_error("parse_address_count"))
    ).

%-----------------------------------------------------------------------------%

    % notmuch/devel/schemata: reply
    %
parse_reply(Json, Res) :-
    (
        Json/"reply-headers" = ReplyHeadersJson,
        parse_reply_headers(ReplyHeadersJson, ReplyHeaders)
        % original: message
    ->
        Res = ReplyHeaders
    ;
        exception.throw(notmuch_json_error(
            "parse_reply expected reply-headers"))
    ).

:- pred parse_reply_headers(json::in, reply_headers::out) is semidet.

parse_reply_headers(Json, ReplyHeaders) :-
    Json/"Subject" = unesc_string(Subject),
    Json/"From" = unesc_string(From),
    ( Json/"To" = ToJson ->
        ToJson = string(ToEscStr),
        MaybeTo = yes(unescape(ToEscStr))
    ;
        MaybeTo = no
    ),
    ( Json/"Cc" = CcJson ->
        CcJson = string(CcEscStr),
        MaybeCc = yes(unescape(CcEscStr))
    ;
        MaybeCc = no
    ),
    ( Json/"Bcc" = BccJson ->
        BccJson = string(BccEscStr),
        MaybeBcc = yes(unescape(BccEscStr))
    ;
        MaybeBcc = no
    ),
    Json/"In-reply-to" = unesc_string(InReplyTo),
    Json/"References" = unesc_string(References),
    ReplyHeaders = reply_headers(Subject, From, MaybeTo, MaybeCc, MaybeBcc,
        InReplyTo, References).

%-----------------------------------------------------------------------------%

:- func json / string = json is semidet.

map(Map) / Key = Value :-
    map.search(Map, Key, Value).

:- func unesc_string(string::out) = (json::in) is semidet.

unesc_string(unescape(EscString)) = string(EscString).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
