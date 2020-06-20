% Bower - a frontend for the Notmuch email system
% Copyright (C) 2020 Peter Wang

:- module message_template.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.
:- import_module prog_config.

:- pred prepare_reply(prog_config::in, message::in(message),
    part_visibility_map::in, reply_headers::in, headers::out, string::out,
    io::di, io::uo) is det.

:- type maybe_quote_marker
    --->    yes_quote_marker
    ;       no_quote_marker.

:- pred render_part_to_text(prog_config::in, part_visibility_map::in,
    maybe_quote_marker::in, part::in, list(string)::out, io::di, io::uo)
    is det.

:- pred select_alternative_by_visibility(part_visibility_map::in,
    list(part)::in, part::out) is semidet.

:- pred part_has_visibility(part_visibility_map::in, part_visibility::in,
    part::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module maybe.
:- import_module solutions.
:- import_module string.

:- import_module copious_output.
:- import_module mime_type.
:- import_module string_util.

%-----------------------------------------------------------------------------%

prepare_reply(Config, OrigMessage, PartVisibilityMap, ReplyHeaders0,
        ReplyHeaders, ReplyBody, !IO) :-
    OrigMessage = message(_MessageId, _Timestamp, OrigMessageHeaders, _Tags,
        Parts, _Replies),
    OrigDate = OrigMessageHeaders ^ h_date,
    OrigFrom = OrigMessageHeaders ^ h_from,
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
    list.foldl2(add_part(Config, PartVisibilityMap, yes_quote_marker),
        Parts, [], RevLines, !IO),
    list.reverse(RevLines, Lines),
    ReplyBody = unlines([Attribution | Lines]).

:- pred make_attribution(header_value::in, header_value::in, string::out) is det.

make_attribution(Date, From, Line) :-
    string.format("On %s %s wrote:",
        [s(header_value_string(Date)), s(header_value_string(From))],
        Line).

%-----------------------------------------------------------------------------%

render_part_to_text(Config, PartVisibilityMap, MaybeQuoteMarker, Part, Lines,
        !IO) :-
    add_part(Config, PartVisibilityMap, MaybeQuoteMarker, Part, [], RevLines,
        !IO),
    list.reverse(RevLines, Lines).

:- pred add_part(prog_config::in, part_visibility_map::in,
    maybe_quote_marker::in, part::in, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

add_part(Config, PartVisibilityMap, MaybeQuoteMarker, Part, !RevLines, !IO) :-
    ContentType = Part ^ pt_content_type,
    PartContent = Part ^ pt_content,
    (
        PartContent = text(Text),
        % XXX is text_html possible?
        ( ContentType = text_html ->
            add_non_text_part(ContentType, !RevLines)
        ;
            add_quoted_text(MaybeQuoteMarker, Text, !RevLines)
        )
    ;
        PartContent = subparts(_Encryption, _Signatures, SubParts),
        ( is_multipart(ContentType) ->
            (
                ContentType = multipart_alternative,
                select_alternative_by_visibility(PartVisibilityMap, SubParts,
                    SubPart)
            ->
                add_part(Config, PartVisibilityMap, MaybeQuoteMarker, SubPart,
                    !RevLines, !IO)
            ;
                % multipart_alternative
                % multipart_mixed
                % multipart_related
                % multipart_signed
                % XXX untested: multipart_encrypted
                list.foldl2(
                    add_part(Config, PartVisibilityMap, MaybeQuoteMarker),
                    SubParts, !RevLines, !IO)
            )
        ;
            % Is this possible?
            add_non_text_part(ContentType, !RevLines)
        )
    ;
        PartContent = encapsulated_messages(EncapMessages),
        list.foldl2(
            add_encapsulated_message(Config, PartVisibilityMap,
                MaybeQuoteMarker),
            EncapMessages, !RevLines, !IO)
    ;
        PartContent = unsupported,
        add_unsupported_part(Config, PartVisibilityMap, MaybeQuoteMarker, Part,
            !RevLines, !IO)
    ).

:- pred add_encapsulated_message(prog_config::in, part_visibility_map::in,
    maybe_quote_marker::in, encapsulated_message::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

add_encapsulated_message(Config, PartVisibilityMap, MaybeQuoteMarker, Message,
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
    add_encapsulated_message_header("From", From, !RevLines),
    maybe_add_encapsulated_message_header("To", To, !RevLines),
    maybe_add_encapsulated_message_header("Cc", Cc, !RevLines),
    add_encapsulated_message_header("Subject", Subject, !RevLines),
    add_encapsulated_message_header("Date", Date, !RevLines),
    cons(">", !RevLines),
    list.foldl2(add_part(Config, PartVisibilityMap, MaybeQuoteMarker),
        Parts, !RevLines, !IO).

:- pred add_encapsulated_message_header(string::in, header_value::in,
    list(string)::in, list(string)::out) is det.

add_encapsulated_message_header(Name, Value, !RevLines) :-
    ValueStr = header_value_string(Value),
    Line = "> " ++ Name ++ ": " ++ ValueStr,
    cons(Line, !RevLines).

:- pred maybe_add_encapsulated_message_header(string::in,
    header_value::in, list(string)::in, list(string)::out) is det.

maybe_add_encapsulated_message_header(Name, Value, !RevLines) :-
    ValueStr = header_value_string(Value),
    ( ValueStr = "" ->
        true
    ;
        add_encapsulated_message_header(Name, Value, !RevLines)
    ).

:- pred add_quoted_text(maybe_quote_marker::in, string::in,
    list(string)::in, list(string)::out) is det.

add_quoted_text(MaybeQuoteMarker, String, !RevLines) :-
    Lines0 = string.split_at_char('\n', String),
    ( split_last(Lines0, Lines1, "") ->
        Lines = Lines1
    ;
        Lines = Lines0
    ),
    list.foldl(add_quoted_line(MaybeQuoteMarker), Lines, !RevLines).

:- pred add_quoted_line(maybe_quote_marker::in, string::in,
    list(string)::in, list(string)::out) is det.

add_quoted_line(MaybeQuoteMarker, Line, !RevLines) :-
    (
        MaybeQuoteMarker = yes_quote_marker,
        QLine = "> " ++ Line
    ;
        MaybeQuoteMarker = no_quote_marker,
        QLine = Line
    ),
    cons(QLine, !RevLines).

:- pred add_unsupported_part(prog_config::in, part_visibility_map::in,
    maybe_quote_marker::in, part::in, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

add_unsupported_part(Config, PartVisibilityMap, MaybeQuoteMarker, Part,
        !RevLines, !IO) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    ContentType = Part ^ pt_content_type,
    MaybeContentCharset = Part ^ pt_content_charset,
    ( ignore_unsupported_part(ContentType) ->
        true
    ;
        maybe_add_blank_line(!RevLines),
        (
            part_has_visibility(PartVisibilityMap, part_visible, Part),
            MaybePartId = yes(PartId)
        ->
            ( get_text_filter_command(Config, ContentType, FilterCommand) ->
                MaybeFilterCommand = yes(FilterCommand)
            ;
                MaybeFilterCommand = no
            ),
            expand_part(Config, MessageId, PartId, ContentType,
                MaybeContentCharset, MaybeFilterCommand, MaybeText, !IO),
            (
                MaybeText = ok(Text),
                add_quoted_text(MaybeQuoteMarker, Text, !RevLines)
            ;
                MaybeText = error(_Error),
                add_unsupported_part_stub(Part, !RevLines)
            )
        ;
            add_unsupported_part_stub(Part, !RevLines)
        )
    ).

:- pred ignore_unsupported_part(mime_type::in) is semidet.

ignore_unsupported_part(application_pgp_encrypted).
ignore_unsupported_part(application_pgp_signature).
ignore_unsupported_part(application_pkcs7_mime).

:- pred add_unsupported_part_stub(part::in,
    list(string)::in, list(string)::out) is det.

add_unsupported_part_stub(Part, !RevLines) :-
    ContentType = Part ^ pt_content_type,
    MaybeContentDisposition = Part ^ pt_content_disposition,
    MaybeFileName = Part ^ pt_filename,
    ( MaybeContentDisposition = yes(content_disposition("attachment")) ->
        add_attachment_part(ContentType, MaybeFileName, !RevLines)
    ;
        add_non_text_part(ContentType, !RevLines)
    ).

:- pred add_attachment_part(mime_type::in, maybe(filename)::in,
    list(string)::in, list(string)::out) is det.

add_attachment_part(ContentType, MaybeFileName, !RevLines) :-
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

:- pred add_non_text_part(mime_type::in,
    list(string)::in, list(string)::out) is det.

add_non_text_part(ContentType, !RevLines) :-
    Line = "Non-text part: " ++ to_string(ContentType),
    cons(Line, !RevLines).

:- pred maybe_add_blank_line(list(string)::in, list(string)::out) is det.

maybe_add_blank_line(!RevLines) :-
    (
        !.RevLines = [Line | _],
        Line \= ""
    ->
        cons("", !RevLines)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

select_alternative_by_visibility(PartVisibilityMap, Parts, Part) :-
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

part_has_visibility(Map, PartVisibility, Part) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    MaybePartId = yes(PartId),
    map.search(Map, message_part_id(MessageId, PartId), PartVisibility).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
