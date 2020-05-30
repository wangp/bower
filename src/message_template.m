% Bower - a frontend for the Notmuch email system
% Copyright (C) 2020 Peter Wang

:- module message_template.
:- interface.

:- import_module io.

:- import_module data.
:- import_module prog_config.

:- pred prepare_reply(prog_config::in, message::in(message),
    part_visibility_map::in, reply_headers::in, headers::out, string::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module solutions.
:- import_module string.

:- import_module copious_output.
:- import_module mime_type.

%-----------------------------------------------------------------------------%

prepare_reply(Config, Message, PartVisibilityMap, ReplyHeaders0, ReplyHeaders,
        ReplyBody, !IO) :-
    Message = message(_MessageId, _Timestamp, MessageHeaders, _Tags,
        Parts, _Replies),
    OrigDate = MessageHeaders ^ h_date,
    OrigFrom = MessageHeaders ^ h_from,
    ReplyHeaders0 = reply_headers(
        Subject,
        From,
        MaybeTo,
        MaybeCc,
        MaybeBcc,
        InReplyTo,
        References
    ),
    (
        MaybeTo = yes(To)
    ;
        MaybeTo = no,
        To = ""
    ),
    (
        MaybeCc = yes(Cc)
    ;
        MaybeCc = no,
        Cc = ""
    ),
    (
        MaybeBcc = yes(Bcc)
    ;
        MaybeBcc = no,
        Bcc = ""
    ),
    ReplyTo = "",
    ReplyDate = "",                     % filled in later
    ReplyHeaders = headers(
        header_value(ReplyDate),
        header_value(From),
        header_value(To),
        header_value(Cc),
        header_value(Bcc),
        decoded_unstructured(Subject),
        header_value(ReplyTo),
        header_value(InReplyTo),
        header_value(References),
        map.init
    ),
    % Efficiency could be better.
    make_attribution(OrigDate, OrigFrom, Attribution),
    list.foldl2(add_reply_part(Config, PartVisibilityMap), Parts,
        [], RevLines, !IO),
    list.reverse(RevLines, Lines),
    ReplyBody = string.join_list("\n", [Attribution | Lines]) ++ "\n".

:- pred make_attribution(header_value::in, header_value::in, string::out) is det.

make_attribution(Date, From, Line) :-
    string.format("On %s %s wrote:",
        [s(header_value_string(Date)), s(header_value_string(From))],
        Line).

:- pred add_reply_part(prog_config::in, part_visibility_map::in, part::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

add_reply_part(Config, PartVisibilityMap, Part, !RevLines, !IO) :-
    ContentType = Part ^ pt_content_type,
    PartContent = Part ^ pt_content,
    (
        PartContent = text(Text),
        ( ContentType = text_html ->
            add_reply_non_text_part(ContentType, !RevLines)
        ;
            add_reply_quoted_text(Text, !RevLines)
        )
    ;
        PartContent = subparts(_Encryption, _Signatures, SubParts),
        ( is_multipart(ContentType) ->
            (
                ContentType = multipart_alternative,
                reply_select_alternative(PartVisibilityMap, SubParts, SubPart)
            ->
                add_reply_part(Config, PartVisibilityMap, SubPart,
                    !RevLines, !IO)
            ;
                % multipart_alternative
                % multipart_mixed
                % multipart_related
                % multipart_signed
                % XXX untested: multipart_encrypted
                list.foldl2(add_reply_part(Config, PartVisibilityMap),
                    SubParts, !RevLines, !IO)
            )
        ;
            % Is this possible?
            add_reply_non_text_part(ContentType, !RevLines)
        )
    ;
        PartContent = encapsulated_messages(EncapMessages),
        list.foldl2(add_reply_encapsulated_message(Config, PartVisibilityMap),
            EncapMessages, !RevLines, !IO)
    ;
        PartContent = unsupported,
        add_reply_unsupported_part(Config, PartVisibilityMap, Part, !RevLines, !IO)
    ).

:- pred reply_select_alternative(part_visibility_map::in, list(part)::in,
    part::out) is semidet.

reply_select_alternative(PartVisibilityMap, Parts, Part) :-
    % If exactly one part visible then select that part.
    % If all parts are hidden then select the first part. This may be a bit
    % unintuitive if the user is expecting to not quote the part at all,
    % but at least should be more intuitive than quoting all alternatives.
    (
        solutions(has_one_visible_alternative(PartVisibilityMap, Parts),
            [VisiblePart])
    ->
        Part = VisiblePart
    ;
        all_true(part_has_visibility(PartVisibilityMap, part_hidden), Parts),
        Parts = [Part | _]
    ).

:- pred has_one_visible_alternative(part_visibility_map::in, list(part)::in,
    part::out) is nondet.

has_one_visible_alternative(PartVisibilityMap, Parts, Part) :-
    list.delete(Parts, Part, OtherParts),
    part_has_visibility(PartVisibilityMap, part_visible, Part),
    all_true(part_has_visibility(PartVisibilityMap, part_hidden), OtherParts).

:- pred part_has_visibility(part_visibility_map::in, part_visibility::in,
    part::in) is semidet.

part_has_visibility(Map, PartVisibility, Part) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    MaybePartId = yes(PartId),
    map.search(Map, message_part_id(MessageId, PartId), PartVisibility).

:- pred add_reply_encapsulated_message(prog_config::in,
    part_visibility_map::in, encapsulated_message::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

add_reply_encapsulated_message(Config, PartVisibilityMap, Message,
        !RevLines, !IO) :-
    % Follows notmuch-reply.c
    Message = encapsulated_message(Headers, Parts),
    Headers = headers(
        Date,
        From,
        To,
        Cc,
        _Bcc,
        Subject,
        _ReplyTo,
        _References,
        _InReplyTo,
        _Rest
    ),
    cons("", !RevLines),
    add_reply_encapsulated_message_header("From", From, !RevLines),
    maybe_add_reply_encapsulated_message_header("To", To, !RevLines),
    maybe_add_reply_encapsulated_message_header("Cc", Cc, !RevLines),
    add_reply_encapsulated_message_header("Subject", Subject, !RevLines),
    add_reply_encapsulated_message_header("Date", Date, !RevLines),
    cons(">", !RevLines),
    list.foldl2(add_reply_part(Config, PartVisibilityMap), Parts,
        !RevLines, !IO).

:- pred add_reply_encapsulated_message_header(string::in, header_value::in,
    list(string)::in, list(string)::out) is det.

add_reply_encapsulated_message_header(Name, Value, !RevLines) :-
    ValueStr = header_value_string(Value),
    Line = "> " ++ Name ++ ": " ++ ValueStr,
    cons(Line, !RevLines).

:- pred maybe_add_reply_encapsulated_message_header(string::in,
    header_value::in, list(string)::in, list(string)::out) is det.

maybe_add_reply_encapsulated_message_header(Name, Value, !RevLines) :-
    ValueStr = header_value_string(Value),
    ( ValueStr = "" ->
        true
    ;
        add_reply_encapsulated_message_header(Name, Value, !RevLines)
    ).


:- pred add_reply_quoted_text(string::in, list(string)::in, list(string)::out)
    is det.

add_reply_quoted_text(String, !RevLines) :-
    Lines0 = string.split_at_char('\n', String),
    ( split_last(Lines0, Lines1, "") ->
        Lines = Lines1
    ;
        Lines = Lines0
    ),
    list.foldl(add_reply_quoted_line, Lines, !RevLines).

:- pred add_reply_quoted_line(string::in, list(string)::in, list(string)::out)
    is det.

add_reply_quoted_line(Line, !RevLines) :-
    QLine = "> " ++ Line,
    cons(QLine, !RevLines).

:- pred add_reply_unsupported_part(prog_config::in, part_visibility_map::in,
    part::in, list(string)::in, list(string)::out, io::di, io::uo) is det.

add_reply_unsupported_part(Config, PartVisibilityMap, Part, !RevLines, !IO) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    ContentType = Part ^ pt_content_type,
    ( reply_ignore_unsupported_part(ContentType) ->
        true
    ;
        maybe_add_reply_blank_line(!RevLines),
        (
            part_has_visibility(PartVisibilityMap, part_visible, Part),
            MaybePartId = yes(PartId)
        ->
            ( get_text_filter_command(Config, ContentType, FilterCommand) ->
                MaybeFilterCommand = yes(FilterCommand)
            ;
                MaybeFilterCommand = no
            ),
            expand_part(Config, MessageId, PartId, MaybeFilterCommand,
                MaybeText, !IO),
            (
                MaybeText = ok(Text),
                add_reply_quoted_text(Text, !RevLines)
            ;
                MaybeText = error(_Error),
                add_reply_unsupported_part_stub(Part, !RevLines)
            )
        ;
            add_reply_unsupported_part_stub(Part, !RevLines)
        )
    ).

:- pred add_reply_unsupported_part_stub(part::in,
    list(string)::in, list(string)::out) is det.

add_reply_unsupported_part_stub(Part, !RevLines) :-
    ContentType = Part ^ pt_content_type,
    MaybeContentDisposition = Part ^ pt_content_disposition,
    MaybeFileName = Part ^ pt_filename,
    ( MaybeContentDisposition = yes(content_disposition("attachment")) ->
        add_reply_attachment_part(ContentType, MaybeFileName, !RevLines)
    ;
        add_reply_non_text_part(ContentType, !RevLines)
    ).

:- pred reply_ignore_unsupported_part(mime_type::in) is semidet.

reply_ignore_unsupported_part(application_pgp_encrypted).
reply_ignore_unsupported_part(application_pgp_signature).
reply_ignore_unsupported_part(application_pkcs7_mime).

:- pred add_reply_attachment_part(mime_type::in, maybe(filename)::in,
    list(string)::in, list(string)::out) is det.

add_reply_attachment_part(ContentType, MaybeFileName, !RevLines) :-
    (
        MaybeFileName = yes(filename(FileName)),
        FilenameSpc = FileName ++ " "
    ;
        MaybeFileName = no,
        FilenameSpc = ""
    ),
    string.format("Attachment: %s(%s)",
        [s(FilenameSpc), s(to_string(ContentType))], Line),
    cons(Line, !RevLines).

:- pred add_reply_non_text_part(mime_type::in,
    list(string)::in, list(string)::out) is det.

add_reply_non_text_part(ContentType, !RevLines) :-
    Line = "Non-text part: " ++ to_string(ContentType),
    cons(Line, !RevLines).

:- pred maybe_add_reply_blank_line(list(string)::in, list(string)::out) is det.

maybe_add_reply_blank_line(!RevLines) :-
    (
        !.RevLines = [Line | _],
        Line \= ""
    ->
        cons("", !RevLines)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
