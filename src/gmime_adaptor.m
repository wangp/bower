% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gmime_adaptor.
:- interface.

:- import_module io.

:- import_module data.
:- import_module gmime.

:- pred message_to_part(g_mime_message::in, part::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

message_to_part(Message, Part, !IO) :-
    get_message_id(Message, MessageId, !IO),
    get_mime_part(Message, Object, !IO),
    object_to_part(message_id(MessageId), Object, Part, !IO).

:- pred object_to_part(message_id::in, g_mime_object::in, part::out,
    io::di, io::uo) is det.

object_to_part(MessageId, Object, Part, !IO) :-
    is_part(Object, IsPart, !IO),
    (
        IsPart = yes(GPart),
        part_to_part(MessageId, Object, GPart, Part, !IO)
    ;
        IsPart = no,
        is_multipart(Object, IsMultipart, !IO),
        (
            IsMultipart = yes(Multipart),
            multipart_to_part(MessageId, Object, Multipart, Part, !IO)
        ;
            IsMultipart = no,
            % TODO
            % is_message_part(Object, IsMessagePart, !IO),
            % is_message(Object, IsMessage, !IO)
            Part = part(MessageId, no, default_content_type, unsupported,
                no, no, no)
        )
    ).

:- func default_content_type = string.

default_content_type = "application/octet-stream".

%-----------------------------------------------------------------------------%

:- pred part_to_part(message_id::in, g_mime_object::in, g_mime_part::in,
    part::out, io::di, io::uo) is det.

part_to_part(MessageId, Object, GPart, Part, !IO) :-
    MaybePartId = no,
    get_content_type(Object, MaybeContentType, !IO),
    (
        MaybeContentType = yes(ContentType0),
        content_type_to_string(ContentType0, ContentType, !IO),
        content_type_is_type(ContentType0, "text", "*", IsText, !IO),
        (
            IsText = yes,
            get_text_content(GPart, Content, !IO),
            MaybeContentLength = no
        ;
            IsText = no,
            Content = unsupported,
            get_content_length(GPart, MaybeContentLength, !IO)
        )
    ;
        MaybeContentType = no,
        ContentType = default_content_type,
        Content = unsupported,
        get_content_length(GPart, MaybeContentLength, !IO)
    ),
    get_filename(GPart, MaybeFileName, !IO),
    get_content_encoding(GPart, Encoding, !IO),
    content_encoding(Encoding, MaybeEncoding),
    Part = part(MessageId, MaybePartId, ContentType, Content, MaybeFileName,
        MaybeEncoding, MaybeContentLength).

:- pred get_text_content(g_mime_part::in, part_content::out, io::di, io::uo)
    is det.

get_text_content(GPart, Content, !IO) :-
    get_content_object(GPart, ResDataWrapper, !IO),
    (
        ResDataWrapper = yes(DataWrapper),
        stream_mem_new(MemStream, !IO),
        % XXX charset filter to UTF-8?
        % XXX CRLF->LF filter?
        write_to_stream(DataWrapper, MemStream, ResWrite, !IO),
        (
            ResWrite = ok(_NumWritten),
            copy_to_string(MemStream, String, !IO),
            Content = text(String)
        ;
            ResWrite = error(_WriteError),
            Content = unsupported
        ),
        stream_unref(MemStream, !IO)
    ;
        ResDataWrapper = no,
        Content = unsupported
    ).

:- pred get_content_length(g_mime_part::in, maybe(int)::out, io::di, io::uo)
    is det.

get_content_length(GPart, MaybeContentLength, !IO) :-
    get_content_object(GPart, ResDataWrapper, !IO),
    (
        ResDataWrapper = yes(DataWrapper),
        get_stream(DataWrapper, DataStream, !IO),
        stream_length(DataStream, MaybeContentLength, !IO)
    ;
        ResDataWrapper = no,
        MaybeContentLength = no
    ).

%-----------------------------------------------------------------------------%

:- pred multipart_to_part(message_id::in, g_mime_object::in,
    g_mime_multipart::in, part::out, io::di, io::uo) is det.

multipart_to_part(MessageId, Object, Multipart, Part, !IO) :-
    get_count(Multipart, Count, !IO),
    list.map_foldl(multipart_to_part_2(MessageId, Multipart),
        0 .. Count - 1, SubParts, !IO),
    MaybePartId = no,
    get_content_type_string(Object, MaybeContentType, !IO),
    (
        MaybeContentType = yes(ContentType)
    ;
        MaybeContentType = no,
        ContentType = default_content_type
    ),
    Content = subparts(SubParts),
    MaybeFileName = no,
    MaybeEncoding = no,
    MaybeContentLength = no,
    Part = part(MessageId, MaybePartId, ContentType, Content, MaybeFileName,
        MaybeEncoding, MaybeContentLength).

:- pred multipart_to_part_2(message_id::in, g_mime_multipart::in, int::in,
    part::out, io::di, io::uo) is det.

multipart_to_part_2(MessageId, Multipart, Index, Part, !IO) :-
    get_part(Multipart, Index, MaybeObject, !IO),
    (
        MaybeObject = yes(Object),
        object_to_part(MessageId, Object, Part, !IO)
    ;
        MaybeObject = no,
        unexpected($module, $pred, "get_part failed")
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
