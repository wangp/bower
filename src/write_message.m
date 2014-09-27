% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module write_message.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module stream.

:- import_module data.
:- import_module prog_config.
:- import_module rfc5322.
:- import_module send_util.

%-----------------------------------------------------------------------------%

:- type message_spec
    --->    message_spec(list(header), message_type).

:- type header
    --->    header(
                field_name,
                field_body
            ).

:- type field_name
    --->    field_name(string).

:- type field_body
    --->    unstructured(header_value, write_header_options)
    ;       address_list(list(address), write_header_options)
    ;       references(header_value).

:- type message_type
    --->    plain(plain_body)
    ;       mime(mime_message).

:- type plain_body
    --->    plain_body(string).

:- type mime_message
    --->    mime_message(
                mime_version,
                mime_part
            ).

:- type mime_part
    --->    discrete(
                discrete_content_type,
                maybe(content_disposition),
                maybe(content_transfer_encoding),
                mime_part_body
            )
    ;       composite(
                composite_content_type,
                boundary,
                maybe(content_disposition),
                maybe(content_transfer_encoding),
                list(mime_part)
            ).

:- type mime_version
    --->    mime_version_1_0.

:- type discrete_content_type
    --->    text_plain(maybe(charset))
    ;       application_octet_stream
    ;       application_pgp_encrypted
    ;       content_type(string).

:- type composite_content_type
    --->    multipart_mixed
    ;       multipart_encrypted(protocol).

:- type charset
    --->    utf8.

:- type boundary
    --->    boundary(string).

:- type protocol
    --->    application_pgp_encrypted.

:- type content_disposition
    --->    inline
    ;       attachment(maybe(filename)).

:- type filename
    --->    filename(string).

:- type content_transfer_encoding
    --->    cte_8bit
    ;       cte_base64.

:- type mime_part_body
    --->    text(string)
    ;       external_base64(part). % requires base64 encoding

:- pred is_empty_field_body(field_body::in) is semidet.

:- pred is_empty_header_value(header_value::in) is semidet.

:- pred write_message(Stream::in, prog_config::in, message_spec::in,
    bool::in, maybe_error::out, io::di, io::uo) is det
    <= stream.writer(Stream, string, io).

:- pred write_message_type(Stream::in, prog_config::in, message_type::in,
    maybe_error::out, io::di, io::uo) is det
    <= stream.writer(Stream, string, io).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module stream.
:- import_module string.
:- import_module string.builder.

:- import_module call_system.
:- import_module quote_arg.
:- import_module rfc2045.
:- import_module rfc2231.

%-----------------------------------------------------------------------------%

is_empty_field_body(Body) :-
    require_complete_switch [Body]
    (
        Body = unstructured(Value, _),
        is_empty_header_value(Value)
    ;
        Body = address_list([], _)
    ;
        Body = references(Value),
        is_empty_header_value(Value)
    ).

is_empty_header_value(Value) :-
    require_complete_switch [Value]
    (
        Value = header_value("")
    ;
        Value = decoded_unstructured("")
    ).

%-----------------------------------------------------------------------------%

write_message(Stream, Config, Spec, AllowHeaderError, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        write_message_2(Stream, Config, Spec, AllowHeaderError, Res0, !IO)
      then
        Res = Res0
      catch_any Excp ->
        Res = error("Caught exception: " ++ string(Excp))
    ).

:- pred write_message_2(Stream::in, prog_config::in,
    message_spec::in, bool::in, maybe_error::out, io::di, io::uo) is det
    <= stream.writer(Stream, string, io).

write_message_2(Stream, Config, Spec, AllowHeaderError, Res, !IO) :-
    Spec = message_spec(Headers, MessageType),
    list.foldl2(build_header(string.builder.handle), Headers, ok, HeaderError,
        init, BuilderState),
    (
        AllowHeaderError = no,
        HeaderError = error(Error)
    ->
        Res = error(Error)
    ;
        HeaderString = to_string(BuilderState),
        put(Stream, HeaderString, !IO),
        % Do not write the blank line separating header and body yet.
        % MIME messages require more header fields.
        write_message_type_2(Stream, Config, MessageType, !IO),
        Res = ok
    ).

:- pred build_header(Stream::in, header::in, maybe_error::in, maybe_error::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

build_header(Stream, Header, !Error, !State) :-
    Header = header(field_name(Field), Body),
    (
        Body = unstructured(Value, Option),
        write_as_unstructured_header(Option, Stream, Field, Value, !State)
    ;
        Body = address_list(Addresses, Option),
        write_address_list_header(Option, Stream, Field, Addresses, !Error,
            !State)
    ;
        Body = references(Value),
        write_references_header(Stream, Field, Value, !State)
    ).

%-----------------------------------------------------------------------------%

write_message_type(Stream, Config, MessageType, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        write_message_type_2(Stream, Config, MessageType, !IO)
      then
        Res = ok
      catch_any Excp ->
        Res = error("Caught exception: " ++ string(Excp))
    ).

:- pred write_message_type_2(Stream::in, prog_config::in, message_type::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_message_type_2(Stream, Config, MessageType, !IO) :-
    (
        MessageType = plain(PlainBody),
        write_plain_body(Stream, PlainBody, !IO)
    ;
        MessageType = mime(MimeMessage),
        write_mime_message(Stream, Config, MimeMessage, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred write_plain_body(Stream::in, plain_body::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_plain_body(Stream, plain_body(Text), !IO) :-
    % Separate header and body.
    put(Stream, "\n", !IO),
    put(Stream, Text, !IO).

%-----------------------------------------------------------------------------%

:- pred write_mime_message(Stream::in, prog_config::in, mime_message::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_mime_message(Stream, Config, MimeMessage, !IO) :-
    MimeMessage = mime_message(MimeVersion, MimePart),
    write_mime_version(Stream, MimeVersion, !IO),
    write_mime_part(Stream, Config, MimePart, !IO).

:- pred write_mime_version(Stream::in, mime_version::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_mime_version(Stream, mime_version_1_0, !IO) :-
    put(Stream, "MIME-Version: 1.0\n", !IO).

:- pred write_mime_part(Stream::in, prog_config::in, mime_part::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_mime_part(Stream, Config, MimePart, !IO) :-
    (
        MimePart = discrete(ContentType, MaybeContentDisposition,
            MaybeTransferEncoding, Body),
        write_discrete_content_type(Stream, ContentType, !IO),
        fold_maybe(write_content_disposition(Stream),
            MaybeContentDisposition, !IO),
        fold_maybe(write_content_transfer_encoding(Stream),
            MaybeTransferEncoding, !IO),
        % Separate header and body.
        put(Stream, "\n", !IO),
        write_mime_part_body(Stream, Config, Body, !IO)
    ;
        MimePart = composite(ContentType, Boundary, MaybeContentDisposition,
            MaybeTransferEncoding, SubParts),
        write_composite_content_type(Stream, ContentType, Boundary, !IO),
        fold_maybe(write_content_disposition(Stream),
            MaybeContentDisposition, !IO),
        fold_maybe(write_content_transfer_encoding(Stream),
            MaybeTransferEncoding, !IO),
        % Separate header and body.
        put(Stream, "\n", !IO),
        list.foldl(write_mime_subpart(Stream, Config, Boundary), SubParts,
            !IO),
        write_mime_final_boundary(Stream, Boundary, !IO)
    ).

:- pred write_discrete_content_type(Stream::in, discrete_content_type::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_discrete_content_type(Stream, ContentType, !IO) :-
    (
        ContentType = text_plain(MaybeCharset),
        (
            MaybeCharset = yes(utf8),
            Value = "text/plain; charset=utf-8"
        ;
            MaybeCharset = no,
            Value = "text/plain"
        )
    ;
        ContentType = application_octet_stream,
        Value = "application/octet-stream"
    ;
        ContentType = application_pgp_encrypted,
        Value = "application/pgp-encrypted"
    ;
        ContentType = content_type(Value)
    ),
    put(Stream, "Content-Type: ", !IO),
    put(Stream, Value, !IO),
    put(Stream, "\n", !IO).

:- pred write_composite_content_type(Stream::in, composite_content_type::in,
    boundary::in, io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_composite_content_type(Stream, ContentType, boundary(Boundary), !IO) :-
    (
        ContentType = multipart_mixed,
        list.foldl(put(Stream),
            ["Content-Type: multipart/mixed; boundary=""", Boundary, """\n"],
            !IO)
    ;
        ContentType = multipart_encrypted(application_pgp_encrypted),
        list.foldl(put(Stream),
            ["Content-Type: multipart/encrypted; boundary=""",
            Boundary, """; protocol=""application/pgp-encrypted""\n"],
            !IO)
    ).

:- pred write_content_disposition(Stream::in, content_disposition::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_content_disposition(Stream, Disposition, !IO) :-
    (
        Disposition = inline,
        put(Stream, "Content-Disposition: inline\n", !IO)
    ;
        Disposition = attachment(MaybeFileName),
        put(Stream, "Content-Disposition: attachment", !IO),
        (
            MaybeFileName = yes(filename(FileName)),
            Attr = attribute("filename"),
            Value = quoted_string(make_quoted_string(FileName)),
            rfc2231.encode_parameter(Attr - Value, Param),
            parameter_to_string(Param, ParamString, Valid),
            (
                Valid = yes,
                put(Stream, "; ", !IO),
                put(Stream, ParamString, !IO)
            ;
                Valid = no
                % Shouldn't happen.
            )
        ;
            MaybeFileName = no
        ),
        put(Stream, "\n", !IO)
    ).

:- pred write_content_transfer_encoding(Stream::in,
    content_transfer_encoding::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_content_transfer_encoding(Stream, CTE, !IO) :-
    (
        CTE = cte_8bit,
        put(Stream, "Content-Transfer-Encoding: 8bit\n", !IO)
    ;
        CTE = cte_base64,
        put(Stream, "Content-Transfer-Encoding: base64\n", !IO)
    ).

:- pred write_mime_subpart(Stream::in, prog_config::in, boundary::in,
    mime_part::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_mime_subpart(Stream, Config, Boundary, Part, !IO) :-
    write_mime_part_boundary(Stream, Boundary, !IO),
    write_mime_part(Stream, Config, Part, !IO).

:- pred write_mime_part_boundary(Stream::in, boundary::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_mime_part_boundary(Stream, boundary(Boundary), !IO) :-
    put(Stream, "\n--", !IO),
    put(Stream, Boundary, !IO),
    put(Stream, "\n", !IO).

:- pred write_mime_final_boundary(Stream::in, boundary::in, io::di, io::uo)
    is det <= stream.writer(Stream, string, io).

write_mime_final_boundary(Stream, boundary(Boundary), !IO) :-
    put(Stream, "\n--", !IO),
    put(Stream, Boundary, !IO),
    put(Stream, "--\n", !IO).

:- pred write_mime_part_body(Stream::in, prog_config::in, mime_part_body::in,
    io::di, io::uo) is det <= stream.writer(Stream, string, io).

write_mime_part_body(Stream, Config, Body, !IO) :-
    (
        Body = text(Text),
        put(Stream, Text, !IO)
    ;
        Body = external_base64(Part),
        get_external_part_base64(Config, Part, Content, !IO),
        put(Stream, Content, !IO)
    ).

:- pred get_external_part_base64(prog_config::in, part::in, string::out,
    io::di, io::uo) is det.

get_external_part_base64(Config, Part, Content, !IO) :-
    Part = part(MessageId, MaybePartId, _, _, _, _, _),
    (
        MaybePartId = yes(PartId),
        get_notmuch_command(Config, Notmuch),
        make_quoted_command(Notmuch, [
            "show", "--format=raw", "--part=" ++ from_int(PartId),
            message_id_to_search_term(MessageId)
        ], redirect_input("/dev/null"), no_redirect, Command),
        call_system_capture_stdout(Command ++ " |base64", no, CallRes, !IO)
    ;
        MaybePartId = no,
        CallRes = error(io.make_io_error("no part id"))
    ),
    (
        CallRes = ok(Content)
    ;
        CallRes = error(Error),
        % XXX handle this gracefully
        unexpected($module, $pred, io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
