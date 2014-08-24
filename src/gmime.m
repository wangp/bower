% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gmime.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- pred g_mime_init(io::di, io::uo) is det.

:- type g_mime_content_encoding
    --->    default
    ;       encoding_7bit
    ;       encoding_8bit
    ;       binary
    ;       base64
    ;       quotedprintable
    ;       uuencode.

:- pred content_encoding(g_mime_content_encoding::in, maybe(string)::out)
    is det.

% Streams

:- type g_mime_stream(T).

:- type stream_mem.
:- type stream_filter.
:- type stream_unknown.

:- pred stream_mem_new(g_mime_stream(stream_mem)::out, io::di, io::uo) is det.

:- pred copy_to_string(g_mime_stream(stream_mem)::in, string::uo,
    io::di, io::uo) is det.

:- pred stream_filter_new(g_mime_stream(T)::in,
    g_mime_stream(stream_filter)::out, io::di, io::uo) is det.

:- pred stream_filter_add(g_mime_stream(T)::in, g_mime_filter::in,
    io::di, io::uo) is det.

:- pred stream_unref(g_mime_stream(T)::in, io::di, io::uo) is det.

:- pred seek_start(g_mime_stream(T)::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred stream_length(g_mime_stream(T)::in, maybe(int)::out, io::di, io::uo)
    is det.

% Stream Filters

:- type g_mime_filter.

:- type encode_or_decode
    --->    decode
    ;       encode.

:- pred filter_basic_new(g_mime_content_encoding::in, encode_or_decode::in,
    g_mime_filter::out, io::di, io::uo) is det.

:- pred filter_charset_new(string::in, string::in, maybe(g_mime_filter)::out,
    io::di, io::uo) is det.

:- pred filter_unref(g_mime_filter::in, io::di, io::uo) is det.

% Data Wrappers

:- type g_mime_data_wrapper.

:- pred get_stream(g_mime_data_wrapper::in, g_mime_stream(stream_unknown)::out,
    io::di, io::uo) is det.

:- pred write_to_stream(g_mime_data_wrapper::in, g_mime_stream(T)::in,
    maybe_error(int)::out, io::di, io::uo) is det.

% Message and MIME Headers

:- type g_mime_content_type.

:- pred content_type_to_string(g_mime_content_type::in, string::out,
    io::di, io::uo) is det.

:- pred content_type_is_type(g_mime_content_type::in, string::in, string::in,
    bool::out, io::di, io::uo) is det.

% MIME Messages and Parts

:- type g_mime_object.

:- pred get_content_type(g_mime_object::in, maybe(g_mime_content_type)::out,
    io::di, io::uo) is det.

:- pred get_content_type_string(g_mime_object::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred get_content_type_parameter(g_mime_object::in, string::in,
    maybe(string)::out, io::di, io::uo) is det.

:- type g_mime_message.

:- pred message_unref(g_mime_message::in, io::di, io::uo) is det.

:- pred get_message_id(g_mime_message::in, string::out, io::di, io::uo) is det.

:- pred get_mime_part(g_mime_message::in, g_mime_object::out, io::di, io::uo)
    is det.

:- type g_mime_part.

:- pred is_part(g_mime_object::in, maybe(g_mime_part)::out, io::di, io::uo)
    is det.

:- pred get_content_encoding(g_mime_part::in, g_mime_content_encoding::out,
    io::di, io::uo) is det.

:- pred get_filename(g_mime_part::in, maybe(string)::out, io::di, io::uo)
    is det.

:- pred get_content_object(g_mime_part::in, maybe(g_mime_data_wrapper)::out,
    io::di, io::uo) is det.

:- type g_mime_multipart.

:- pred is_multipart(g_mime_object::in, maybe(g_mime_multipart)::out,
    io::di, io::uo) is det.

:- pred get_count(g_mime_multipart::in, int::out, io::di, io::uo) is det.

:- pred get_part(g_mime_multipart::in, int::in, maybe(g_mime_object)::out,
    io::di, io::uo) is det.

:- pred get_subpart_from_content_id(g_mime_multipart::in, string::in,
    maybe(g_mime_object)::out, io::di, io::uo) is det.

:- type g_mime_message_part.

:- pred is_message_part(g_mime_object::in, maybe(g_mime_message_part)::out,
    io::di, io::uo) is det.

% Parsing Messages and MIME Parts

:- type g_mime_parser.

:- pred parser_new_with_stream(g_mime_stream(T)::in, g_mime_parser::out,
    io::di, io::uo) is det.

:- pred parser_unref(g_mime_parser::in, io::di, io::uo) is det.

:- pred construct_message(g_mime_parser::in, maybe(g_mime_message)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <gmime/gmime.h>
").

:- type stream_mem      ---> stream_mem.
:- type stream_filter   ---> stream_filter.
:- type stream_unknown  ---> stream_unknown.

:- pragma foreign_type("C", g_mime_stream(T), "GMimeStream *").

:- pragma foreign_type("C", g_mime_filter, "GMimeFilter *").

:- pragma foreign_type("C", g_mime_data_wrapper, "GMimeDataWrapper *").

:- pragma foreign_type("C", g_mime_content_type, "GMimeContentType *").

:- pragma foreign_type("C", g_mime_object, "GMimeObject *").

:- pragma foreign_type("C", g_mime_message, "GMimeMessage *").

:- pragma foreign_type("C", g_mime_part, "GMimePart *").

:- pragma foreign_type("C", g_mime_multipart, "GMimeMultipart *").

:- pragma foreign_type("C", g_mime_message_part, "GMimeMessagePart *").

:- pragma foreign_type("C", g_mime_parser, "GMimeParser *").

:- pragma foreign_enum("C", g_mime_content_encoding/0,
[
    default - "GMIME_CONTENT_ENCODING_DEFAULT",
    encoding_7bit - "GMIME_CONTENT_ENCODING_7BIT",
    encoding_8bit - "GMIME_CONTENT_ENCODING_8BIT",
    binary - "GMIME_CONTENT_ENCODING_BINARY",
    base64 - "GMIME_CONTENT_ENCODING_BASE64",
    quotedprintable - "GMIME_CONTENT_ENCODING_QUOTEDPRINTABLE",
    uuencode - "GMIME_CONTENT_ENCODING_UUENCODE"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    g_mime_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    g_mime_init(GMIME_ENABLE_RFC2047_WORKAROUNDS);
").

%-----------------------------------------------------------------------------%

content_encoding(default, no).
content_encoding(encoding_7bit, yes("7bit")).
content_encoding(encoding_8bit, yes("8bit")).
content_encoding(binary, yes("binary")).
content_encoding(base64, yes("base64")).
content_encoding(quotedprintable, yes("quoted-printable")).
content_encoding(uuencode, yes("x-uuencode")).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    stream_mem_new(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = g_mime_stream_mem_new();
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    copy_to_string(Stream::in, String::uo, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    GByteArray *byte_array;
    const char *data;

    byte_array = g_mime_stream_mem_get_byte_array(GMIME_STREAM_MEM(Stream));
    data = (const char *) byte_array->data;

    MR_allocate_aligned_string_msg(String, byte_array->len, MR_ALLOC_ID);
    memcpy(String, data, byte_array->len);
    String[byte_array->len] = '\\0';
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    stream_filter_new(Source::in, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = g_mime_stream_filter_new(Source);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    stream_filter_add(Stream::in, Filter::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    g_mime_stream_filter_add(GMIME_STREAM_FILTER(Stream), Filter);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    stream_unref(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    /* Implies g_mime_stream_close. */
    g_object_unref(Stream);
").

%-----------------------------------------------------------------------------%

seek_start(Stream, Res, !IO) :-
    seek_start_2(Stream, Ok, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error("seek error")
    ).

:- pred seek_start_2(g_mime_stream(T)::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    seek_start_2(Stream::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    gint64 r;

    r = g_mime_stream_seek(Stream, 0, SEEK_SET);
    if (r == 0) {
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

stream_length(Stream, Res, !IO) :-
    stream_length_2(Stream, Length, !IO),
    ( Length >= 0 ->
        Res = yes(Length)
    ;
        Res = no
    ).

:- pred stream_length_2(g_mime_stream(T)::in, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    stream_length_2(Stream::in, Length::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Length = g_mime_stream_length(Stream);
").

%-----------------------------------------------------------------------------%

filter_basic_new(Encoding, EncodeOrDecode, Filter, !IO) :-
    (
        EncodeOrDecode = encode,
        Encode = yes
    ;
        EncodeOrDecode = decode,
        Encode = no
    ),
    filter_basic_new_2(Encoding, Encode, Filter, !IO).

:- pred filter_basic_new_2(g_mime_content_encoding::in, bool::in,
    g_mime_filter::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    filter_basic_new_2(Encoding::in, Encode::in, Filter::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Filter = g_mime_filter_basic_new(Encoding, (Encode ? TRUE : FALSE));
").

%-----------------------------------------------------------------------------%

filter_charset_new(FromCharSet, ToCharSet, Res, !IO) :-
    filter_charset_new_2(FromCharSet, ToCharSet, Ok, Filter, !IO),
    (
        Ok = yes,
        Res = yes(Filter)
    ;
        Ok = no,
        Res = no
    ).

:- pred filter_charset_new_2(string::in, string::in, bool::out,
    g_mime_filter::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    filter_charset_new_2(FromCharSet::in, ToCharSet::in, Ok::out, Filter::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Filter = g_mime_filter_charset_new(FromCharSet, ToCharSet);
    Ok = (Filter != NULL) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    filter_unref(Filter::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    g_object_unref(Filter);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_stream(DataWrapper::in, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = g_mime_data_wrapper_get_stream(DataWrapper);
").

%-----------------------------------------------------------------------------%

write_to_stream(DataWrapper, Stream, Res, !IO) :-
    write_to_stream_2(DataWrapper, Stream, NumWritten, !IO),
    ( NumWritten = -1 ->
        Res = error("g_mime_data_wrapper_write_to_stream error")
    ;
        Res = ok(NumWritten)
    ).

:- pred write_to_stream_2(g_mime_data_wrapper::in, g_mime_stream(T)::in,
    int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_to_stream_2(DataWrapper::in, Stream::in, NumWritten::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    NumWritten = g_mime_data_wrapper_write_to_stream(DataWrapper, Stream);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    content_type_to_string(ContentType::in, String::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char *s = g_mime_content_type_to_string(ContentType);
    MR_make_aligned_string_copy_msg(String, s, MR_ALLOC_ID);
    g_free(s);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    content_type_is_type(ContentType::in, Type::in, SubType::in,
        Bool::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Bool = g_mime_content_type_is_type(ContentType, Type, SubType)
            ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

get_content_type(Object, Res, !IO) :-
    get_content_type_2(Object, Ok, ContentType, !IO),
    (
        Ok = yes,
        Res = yes(ContentType)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_content_type_2(g_mime_object::in, bool::out,
    g_mime_content_type::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_content_type_2(Object::in, Ok::out, ContentType::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ContentType = g_mime_object_get_content_type(Object);
    Ok = (ContentType != NULL) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

get_content_type_string(Object, Res, !IO) :-
    get_content_type(Object, MaybeContentType, !IO),
    (
        MaybeContentType = yes(ContentType),
        content_type_to_string(ContentType, String, !IO),
        Res = yes(String)
    ;
        MaybeContentType = no,
        Res = no
    ).

%-----------------------------------------------------------------------------%

get_content_type_parameter(Object, Name, Res, !IO) :-
    get_content_type_parameter_2(Object, Name, Ok, Value, !IO),
    (
        Ok = yes,
        Res = yes(Value)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_content_type_parameter_2(g_mime_object::in, string::in, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_content_type_parameter_2(Object::in, Name::in, Ok::out, Value::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const char *s = g_mime_object_get_content_type_parameter(Object, Name);
    if (s != NULL) {
        Ok = MR_YES;
        MR_make_aligned_string_copy_msg(Value, s, MR_ALLOC_ID);
    } else {
        Ok = MR_NO;
        Value = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    message_unref(Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    g_object_unref(Message);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_message_id(Message::in, MessageId::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const char *s = g_mime_message_get_message_id(Message);
    if (s != NULL) {
        MR_make_aligned_string_copy_msg(MessageId, s, MR_ALLOC_ID);
    } else {
        MessageId = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_mime_part(Message::in, Object::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Object = g_mime_message_get_mime_part(Message);
").

%-----------------------------------------------------------------------------%

% :- pragma foreign_proc("C",
%     g_mime_part_unref(Part::in, _IO0::di, _IO::uo),
%     [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
% "
%     g_object_unref(Part);
% ").

%-----------------------------------------------------------------------------%

is_part(Object, Res, !IO) :-
    is_part_2(Object, Ok, Part, !IO),
    (
        Ok = yes,
        Res = yes(Part)
    ;
        Ok = no,
        Res = no
    ).

:- pred is_part_2(g_mime_object::in, bool::out, g_mime_part::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    is_part_2(Object::in, Ok::out, Part::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (GMIME_IS_PART(Object)) {
        Ok = MR_YES;
        Part = GMIME_PART(Object);
    } else {
        Ok = MR_NO;
        Part = NULL;
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_content_encoding(Part::in, Encoding::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Encoding = g_mime_part_get_content_encoding(Part);
").

%-----------------------------------------------------------------------------%

get_filename(Part, Res, !IO) :-
    get_filename_2(Part, Ok, FileName, !IO),
    (
        Ok = yes,
        Res = yes(FileName)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_filename_2(g_mime_part::in, bool::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    get_filename_2(Part::in, Ok::out, FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const char *s = g_mime_part_get_filename(Part);
    if (s != NULL) {
        Ok = MR_YES;
        MR_make_aligned_string_copy_msg(FileName, s, MR_ALLOC_ID);
    } else {
        Ok = MR_NO;
        FileName = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

get_content_object(Part, Res, !IO) :-
    get_content_object_2(Part, Ok, DataWrapper, !IO),
    (
        Ok = yes,
        Res = yes(DataWrapper)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_content_object_2(g_mime_part::in, bool::out,
    g_mime_data_wrapper::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_content_object_2(Part::in, Ok::out, DataWrapper::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    DataWrapper = g_mime_part_get_content_object(Part);
    if (DataWrapper != NULL) {
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

is_multipart(Object, Res, !IO) :-
    is_multipart_2(Object, Ok, Multipart, !IO),
    (
        Ok = yes,
        Res = yes(Multipart)
    ;
        Ok = no,
        Res = no
    ).

:- pred is_multipart_2(g_mime_object::in, bool::out, g_mime_multipart::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    is_multipart_2(Object::in, Ok::out, Multipart::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (GMIME_IS_MULTIPART(Object)) {
        Ok = MR_YES;
        Multipart = GMIME_MULTIPART(Object);
    } else {
        Ok = MR_NO;
        Multipart = NULL;
    }
").

%-----------------------------------------------------------------------------%

% :- pragma foreign_proc("C",
%     g_mime_multipart_unref(Multipart::in, _IO0::di, _IO::uo),
%     [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
% "
%     g_object_unref(Multipart);
% ").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_count(Multipart::in, Count::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Count = g_mime_multipart_get_count(Multipart);
").

%-----------------------------------------------------------------------------%

get_part(Multipart, Index, Res, !IO) :-
    get_part_2(Multipart, Index, Ok, Object, !IO),
    (
        Ok = yes,
        Res = yes(Object)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_part_2(g_mime_multipart::in, int::in, bool::out,
    g_mime_object::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_part_2(Multipart::in, Index::in, Ok::out, Object::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Object = g_mime_multipart_get_part(Multipart, Index);
    if (Object != NULL) {
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

get_subpart_from_content_id(Multipart, ContentId, Res, !IO) :-
    get_subpart_from_content_id_2(Multipart, ContentId, Ok, Object, !IO),
    (
        Ok = yes,
        Res = yes(Object)
    ;
        Ok = no,
        Res = no
    ).

:- pred get_subpart_from_content_id_2(g_mime_multipart::in, string::in,
    bool::out, g_mime_object::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_subpart_from_content_id_2(Multipart::in, ContentId::in, Ok::out,
        Object::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Object = g_mime_multipart_get_subpart_from_content_id(Multipart, ContentId);
    if (Object != NULL) {
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

is_message_part(Object, Res, !IO) :-
    is_message_part_2(Object, Ok, MessagePart, !IO),
    (
        Ok = yes,
        Res = yes(MessagePart)
    ;
        Ok = no,
        Res = no
    ).

:- pred is_message_part_2(g_mime_object::in, bool::out,
    g_mime_message_part::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    is_message_part_2(Object::in, Ok::out, MessagePart::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (GMIME_IS_MESSAGE_PART(Object)) {
        Ok = MR_YES;
        MessagePart = GMIME_MESSAGE_PART(Object);
    } else {
        Ok = MR_NO;
        MessagePart = NULL;
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    parser_new_with_stream(Stream::in, Parser::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Parser = g_mime_parser_new_with_stream(Stream);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    parser_unref(Parser::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    g_object_unref(Parser);
").

%-----------------------------------------------------------------------------%

construct_message(Parser, Res, !IO) :-
    construct_message_2(Parser, Ok, Message, !IO),
    (
        Ok = yes,
        Res = yes(Message)
    ;
        Ok = no,
        Res = no
    ).

:- pred construct_message_2(g_mime_parser::in, bool::out, g_mime_message::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    construct_message_2(Parser::in, Ok::out, Message::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Message = g_mime_parser_construct_message(Parser);
    if (Message != NULL) {
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
