% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module write_message.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module stream.

:- import_module data.
:- import_module mime_type.
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
    ;       mime_v1(mime_part).

:- type plain_body
    --->    plain_body(
                string,
                maybe(string)   % optional signature
            ).

:- type mime_part
    --->    discrete(
                mime_type,
                maybe(string),  % charset
                maybe(write_content_disposition),
                maybe(write_content_transfer_encoding),
                mime_part_body
            )
    ;       composite(
                composite_content_type,
                boundary,
                maybe(write_content_disposition),
                maybe(write_content_transfer_encoding),
                list(mime_part)
            ).

:- type mime_version
    --->    mime_version_1_0.

:- type composite_content_type
    --->    multipart_mixed
    ;       multipart_alternative
    ;       multipart_encrypted(protocol)
    ;       multipart_signed(micalg, protocol).

:- type boundary
    --->    boundary(string).

:- type protocol
    --->    application_pgp_encrypted
    ;       application_pgp_signature.

:- type micalg
    --->    micalg(string). % pgp-*

:- type write_content_disposition
    --->    write_content_disposition(
                write_content_disposition_type,
                maybe(filename)
            ).

:- type write_content_disposition_type
    --->    inline
    ;       attachment.

:- type write_content_transfer_encoding
    --->    cte_8bit
    ;       cte_base64.

:- type mime_part_body
    --->    text(string)
    ;       base64(string) % already encoded
    ;       external(part).

:- pred is_empty_field_body(field_body::in) is semidet.

:- pred is_empty_header_value(header_value::in) is semidet.

:- typeclass writer(Stream)
    <= (stream.writer(Stream, string, io),
        stream.writer(Stream, char, io))
    where [].

:- instance writer(io.output_stream).

    % A reminder that the caller is responsible for suspending curses.
:- type i_paused_curses
    --->    i_paused_curses.

:- pred write_message(Stream::in, prog_config::in, message_spec::in,
    bool::in, i_paused_curses::in, maybe_error::out, io::di, io::uo) is det
    <= writer(Stream).

:- pred write_message_type(Stream::in, prog_config::in, message_type::in,
    i_paused_curses::in, maybe_error::out, io::di, io::uo) is det
    <= writer(Stream).

:- pred write_mime_part(Stream::in, prog_config::in, mime_part::in,
    i_paused_curses::in, maybe_error::out, io::di, io::uo) is det
    <= writer(Stream).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module string.
:- import_module string.builder.

:- import_module base64.
:- import_module call_system.
:- import_module process.
:- import_module quote_command.
:- import_module rfc2045.
:- import_module rfc2231.

:- instance writer(io.output_stream) where [].

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

write_message(Stream, Config, Spec, AllowHeaderError, PausedCurs, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        write_message_nocatch(Stream, Config, Spec, AllowHeaderError,
            PausedCurs, Res0, !IO)
      then
        Res = Res0
      catch_any Excp ->
        Res = error("Caught exception: " ++ string(Excp))
    ).

:- pred write_message_nocatch(Stream::in, prog_config::in,
    message_spec::in, bool::in, i_paused_curses::in, maybe_error::out,
    io::di, io::uo) is det <= writer(Stream).

write_message_nocatch(Stream, Config, Spec, AllowHeaderError, PausedCurs, Res,
        !IO) :-
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
        write_message_type_nocatch(Stream, Config, MessageType, PausedCurs,
            Res, !IO)
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

write_message_type(Stream, Config, MessageType, PausedCurs, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        write_message_type_nocatch(Stream, Config, MessageType, PausedCurs,
            Res0, !IO)
      then
        Res = Res0
      catch_any Excp ->
        Res = error("Caught exception: " ++ string(Excp))
    ).

:- pred write_message_type_nocatch(Stream::in, prog_config::in,
    message_type::in, i_paused_curses::in, maybe_error::out, io::di, io::uo)
    is det <= writer(Stream).

write_message_type_nocatch(Stream, Config, MessageType, PausedCurs, Res, !IO) :-
    (
        MessageType = plain(PlainBody),
        write_plain_body(Stream, PlainBody, !IO),
        Res = ok
    ;
        MessageType = mime_v1(MimePart),
        write_mime_version_1(Stream, !IO),
        write_mime_part_nocatch(Stream, Config, MimePart, PausedCurs, Res, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred write_plain_body(Stream::in, plain_body::in, io::di, io::uo)
    is det <= writer(Stream).

write_plain_body(Stream, plain_body(Text, MaybeSignature), !IO) :-
    % Separate header and body.
    put(Stream, "\n", !IO),
    put_ensure_terminated(Stream, Text, !IO),
    (
        MaybeSignature = yes(Signature),
        put(Stream, "\n", !IO),
        put_ensure_terminated(Stream, Signature, !IO)
    ;
        MaybeSignature = no
    ).

:- pred put_ensure_terminated(Stream::in, string::in, io::di, io::uo)
    is det <= writer(Stream).

put_ensure_terminated(Stream, String, !IO) :-
    ( String = "" ->
        true
    ;
        put(Stream, String, !IO),
        ( string.suffix(String, "\n") ->
            true
        ;
            put(Stream, "\n", !IO)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred write_mime_version_1(Stream::in, io::di, io::uo) is det
    <= writer(Stream).

write_mime_version_1(Stream, !IO) :-
    put(Stream, "MIME-Version: 1.0\n", !IO).

write_mime_part(Stream, Config, MimePart, PausedCurs, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        write_mime_part_nocatch(Stream, Config, MimePart, PausedCurs, Res0,
            !IO)
    then
        Res = Res0
    catch_any Excp ->
        Res = error("Caught exception: " ++ string(Excp))
    ).

:- pred write_mime_part_nocatch(Stream::in, prog_config::in, mime_part::in,
    i_paused_curses::in, maybe_error::out, io::di, io::uo) is det
    <= writer(Stream).

write_mime_part_nocatch(Stream, Config, MimePart, PausedCurs, Res, !IO) :-
    (
        MimePart = discrete(ContentType, MaybeCharset, MaybeContentDisposition,
            MaybeTransferEncoding, Body),
        write_discrete_content_type(Stream, ContentType, MaybeCharset, !IO),
        fold_maybe(write_content_disposition(Stream),
            MaybeContentDisposition, !IO),
        fold_maybe(write_content_transfer_encoding(Stream),
            MaybeTransferEncoding, !IO),
        % Separate header and body.
        put(Stream, "\n", !IO),
        (
            MaybeTransferEncoding = yes(TransferEncoding)
        ;
            MaybeTransferEncoding = no,
            TransferEncoding = cte_8bit
        ),
        write_mime_part_body(Stream, Config, TransferEncoding, Body,
            PausedCurs, Res, !IO)
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
        write_mime_subparts(Stream, Config, Boundary, PausedCurs, SubParts,
            Res0, !IO),
        (
            Res0 = ok,
            write_mime_final_boundary(Stream, Boundary, !IO),
            Res = ok
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred write_discrete_content_type(Stream::in, mime_type::in,
    maybe(string)::in, io::di, io::uo) is det <= writer(Stream).

write_discrete_content_type(Stream, ContentType, MaybeCharset, !IO) :-
    put(Stream, "Content-Type: ", !IO),
    put(Stream, mime_type.to_string(ContentType), !IO),
    ( ContentType = application_pgp_signature ->
        put(Stream, "; name=""signature.asc""", !IO)
    ;
        true
    ),
    (
        MaybeCharset = yes(Charset),
        put(Stream, "; charset=", !IO),
        put(Stream, Charset, !IO)
    ;
        MaybeCharset = no
    ),
    put(Stream, "\n", !IO).

:- pred write_composite_content_type(Stream::in, composite_content_type::in,
    boundary::in, io::di, io::uo) is det <= writer(Stream).

write_composite_content_type(Stream, ContentType, boundary(Boundary), !IO) :-
    (
        ContentType = multipart_mixed,
        list.foldl(put(Stream),
            ["Content-Type: multipart/mixed; boundary=""", Boundary, """\n"],
            !IO)
    ;
        ContentType = multipart_alternative,
        list.foldl(put(Stream),
            ["Content-Type: multipart/alternative",
            "; boundary=""", Boundary, """",
            "\n"],
            !IO)
    ;
        ContentType = multipart_encrypted(Protocol),
        list.foldl(put(Stream),
            ["Content-Type: multipart/encrypted",
            "; boundary=""", Boundary, """",
            "; protocol=""", protocol_string(Protocol), """",
            "\n"],
            !IO)
    ;
        ContentType = multipart_signed(micalg(MicAlg), Protocol),
        list.foldl(put(Stream),
            ["Content-Type: multipart/signed",
            "; boundary=""", Boundary, """",
            "; micalg=""", MicAlg, """",
            "; protocol=""", protocol_string(Protocol), """",
            "\n"], !IO)
    ).

:- func protocol_string(protocol) = string.

protocol_string(application_pgp_encrypted) = "application/pgp-encrypted".
protocol_string(application_pgp_signature) = "application/pgp-signature".

:- pred write_content_disposition(Stream::in, write_content_disposition::in,
    io::di, io::uo) is det <= writer(Stream).

write_content_disposition(Stream, Disposition, !IO) :-
    Disposition = write_content_disposition(DispositionType, MaybeFileName),
    (
        DispositionType = inline,
        put(Stream, "Content-Disposition: inline", !IO)
    ;
        DispositionType = attachment,
        put(Stream, "Content-Disposition: attachment", !IO)
    ),
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
    put(Stream, "\n", !IO).

:- pred write_content_transfer_encoding(Stream::in,
    write_content_transfer_encoding::in, io::di, io::uo) is det
    <= writer(Stream).

write_content_transfer_encoding(Stream, CTE, !IO) :-
    (
        CTE = cte_8bit,
        put(Stream, "Content-Transfer-Encoding: 8bit\n", !IO)
    ;
        CTE = cte_base64,
        put(Stream, "Content-Transfer-Encoding: base64\n", !IO)
    ).

:- pred write_mime_subparts(Stream::in, prog_config::in, boundary::in,
    i_paused_curses::in, list(mime_part)::in, maybe_error::out, io::di, io::uo)
    is det <= writer(Stream).

write_mime_subparts(Stream, Config, Boundary, PausedCurs, Parts, Res, !IO) :-
    (
        Parts = [],
        Res = ok
    ;
        Parts = [Part | TailParts],
        write_mime_subpart(Stream, Config, Boundary, PausedCurs, Part, Res0, !IO),
        (
            Res0 = ok,
            write_mime_subparts(Stream, Config, Boundary, PausedCurs,
                TailParts, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred write_mime_subpart(Stream::in, prog_config::in, boundary::in,
    i_paused_curses::in, mime_part::in, maybe_error::out, io::di, io::uo)
    is det <= writer(Stream).

write_mime_subpart(Stream, Config, Boundary, PausedCurs, Part, Res, !IO) :-
    write_mime_part_boundary(Stream, Boundary, !IO),
    write_mime_part_nocatch(Stream, Config, Part, PausedCurs, Res, !IO).

:- pred write_mime_part_boundary(Stream::in, boundary::in, io::di, io::uo)
    is det <= writer(Stream).

write_mime_part_boundary(Stream, boundary(Boundary), !IO) :-
    put(Stream, "\n--", !IO),
    put(Stream, Boundary, !IO),
    put(Stream, "\n", !IO).

:- pred write_mime_final_boundary(Stream::in, boundary::in, io::di, io::uo)
    is det <= writer(Stream).

write_mime_final_boundary(Stream, boundary(Boundary), !IO) :-
    put(Stream, "\n--", !IO),
    put(Stream, Boundary, !IO),
    put(Stream, "--\n", !IO).

:- pred write_mime_part_body(Stream::in, prog_config::in,
    write_content_transfer_encoding::in, mime_part_body::in,
    i_paused_curses::in, maybe_error::out, io::di, io::uo) is det
    <= writer(Stream).

write_mime_part_body(Stream, Config, TransferEncoding, Body, PausedCurs, Res,
        !IO) :-
    (
        TransferEncoding = cte_8bit,
        (
            Body = text(EncodedBody),
            put(Stream, EncodedBody, !IO),
            Res = ok
        ;
            Body = base64(_),
            Res = error("write_mime_part_body: expected text, got base64")
        ;
            Body = external(_),
            Res = error("write_mime_part_body: expected text, got external")
        )
    ;
        TransferEncoding = cte_base64,
        (
            Body = text(Text),
            base64.encode_wrap(Text, 0, length(Text), Stream, !IO),
            put(Stream, '\n', !IO),
            Res = ok
        ;
            Body = base64(EncodedBody),
            put(Stream, EncodedBody, !IO),
            Res = ok
        ;
            Body = external(Part),
            get_external_part_base64(Config, Part, PausedCurs, Res0, !IO),
            (
                Res0 = ok(EncodedBody),
                put(Stream, EncodedBody, !IO),
                Res = ok
            ;
                Res0 = error(Error),
                Res = error(Error)
            )
        )
    ).

:- pred get_external_part_base64(prog_config::in, part::in,
    i_paused_curses::in, maybe_error(string)::out, io::di, io::uo) is det.

get_external_part_base64(Config, Part, PausedCurs, Res, !IO) :-
    Part = part(MessageId, MaybePartId, _, _, _, _, _, _, _, IsDecrypted),
    (
        MaybePartId = yes(PartId),
        get_notmuch_command(Config, Notmuch),
        make_quoted_command(Notmuch, [
            "show", "--format=raw",
            decrypt_arg(IsDecrypted), % should not happen yet
            part_id_to_part_option(PartId),
            "--", message_id_to_search_term(MessageId)
        ], redirect_input("/dev/null"), no_redirect, Command),
        % Decryption may invoke pinentry-curses.
        PausedCurs = i_paused_curses,
        call_system_capture_stdout(Command ++ " |base64", environ([]), no,
            CallRes, !IO)
    ;
        MaybePartId = no,
        CallRes = error(io.make_io_error("no part id"))
    ),
    (
        CallRes = ok(Content),
        Res = ok(Content)
    ;
        CallRes = error(Error0),
        Error = "Failed to base64 encode attachment: " ++ io.error_message(Error0),
        Res = error(Error)
    ).

:- func decrypt_arg(maybe_decrypted) = string.

decrypt_arg(is_decrypted) = "--decrypt".
decrypt_arg(not_decrypted) = "--decrypt=false".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
