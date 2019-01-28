% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module data.
:- interface.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module mime_type.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type thread
    --->    thread(
                t_id        :: thread_id,
                t_timestamp :: timestamp,
                t_authors   :: string,
                t_subject   :: string,
                t_tags      :: set(tag),
                t_matched   :: int,
                t_total     :: int
            ).

:- type thread_id
    --->    thread_id(string).

:- type message
    --->    message(
                m_id            :: message_id,
                m_timestamp     :: timestamp,
                m_headers       :: headers,
                m_tags          :: set(tag),
                m_body          :: list(part),
                m_replies       :: list(message)
            )
    ;       excluded_message(
                % Although we will usually have information about excluded
                % messages now, we often want to act as if they do not exist,
                % hence the use of a separate constructor.
                em_id           :: maybe(message_id),
                em_timestamp    :: maybe(timestamp),
                em_headers      :: maybe(headers),
                em_tags         :: maybe(set(tag)),
                em_replies      :: list(message)
            ).

:- inst message
    --->    message(ground, ground, ground, ground, ground, ground).

:- type message_for_recall
    --->    message_for_recall(
                mr_id           :: message_id,
                mr_timestamp    :: timestamp,
                mr_headers      :: headers,
                mr_tags         :: set(tag)
            ).

:- type message_id
    --->    message_id(string).

:- type headers
    --->    headers(
                % Technically, header fields.
                h_date          :: header_value,
                h_from          :: header_value,
                h_to            :: header_value,
                h_cc            :: header_value,
                h_bcc           :: header_value,
                h_subject       :: header_value,
                h_replyto       :: header_value,
                h_references    :: header_value,
                h_inreplyto     :: header_value,
                % XXX should use a distinct type for header field names
                % for they are case-insensitive
                h_rest          :: map(string, header_value)
            ).

:- type header_value
    --->    header_value(string)
            % Most header values.
    ;       decoded_unstructured(string).
            % An unstructured field that may contain RFC 2047 encoded-words,
            % which we keep in decoded form.

:- type tag
    --->    tag(string).

:- type filename
    --->    filename(string).

:- type content_disposition
    --->    content_disposition(string).

:- type content_length
    --->    content_length(int).

:- type content_transfer_encoding
    --->    content_transfer_encoding(string).

:- type part
    --->    part(
                pt_msgid                :: message_id,
                pt_part                 :: maybe(int), % XXX use bespoke type
                pt_content_type         :: mime_type,
                pt_content_disposition  :: maybe(content_disposition),
                pt_content              :: part_content,
                pt_filename             :: maybe(filename),
                pt_content_length       :: maybe(content_length),
                pt_content_transfer_encoding :: maybe(content_transfer_encoding),
                pt_decrypted            :: maybe_decrypted
            ).

:- type part_content
    --->    text(string)
    ;       subparts(encryption, list(signature), list(part))
    ;       encapsulated_messages(list(encapsulated_message))
    ;       unsupported.

:- type encryption
    --->    not_encrypted
    ;       encrypted
    ;       decryption_good
    ;       decryption_bad.

:- type signature
    --->    signature(
                signature_status :: signature_status,
                signature_errors :: int
            ).

:- type signature_status
    --->    none
    ;       good(
                fingerprint     :: maybe(string),
                created         :: maybe(timestamp),
                expires         :: maybe(timestamp),
                userid          :: maybe(string)
            )
    ;       not_good(
                not_good_status :: signature_not_good_status,
                not_good_keyid  :: maybe(string)
            ).

:- type signature_not_good_status
    --->    bad
    ;       error
    ;       unknown.

:- type encapsulated_message
    --->    encapsulated_message(
                encap_headers   :: headers,
                encap_body      :: list(part)
            ).

:- type maybe_decrypted
    --->    not_decrypted
    ;       is_decrypted.

%-----------------------------------------------------------------------------%

:- func thread_id_to_search_term(thread_id) = string.

:- func message_id_to_search_term(message_id) = string.

:- func init_headers = headers.

:- pred empty_header_value(header_value::in) is semidet.

:- func header_value_string(header_value) = string.

:- pred tag_to_string(tag::in, string::out) is det.

:- pred non_excluded_message(message).
:- mode non_excluded_message(ground >> message) is semidet.

:- func get_maybe_message_id(message) = maybe(message_id).

:- func get_timestamp_or_zero(message) = timestamp.

:- func get_maybe_subject(message) = maybe(header_value).

:- func get_replies(message) = list(message).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

thread_id_to_search_term(thread_id(Id)) = "thread:" ++ Id.

message_id_to_search_term(message_id(Id)) = "id:" ++ Id.

init_headers = Headers :-
    Empty = header_value(""),
    Headers = headers(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
        Empty, map.init).

empty_header_value(header_value("")).
empty_header_value(decoded_unstructured("")).

header_value_string(header_value(S)) = S.
header_value_string(decoded_unstructured(S)) = S.

tag_to_string(tag(String), String).

non_excluded_message(Message) :-
    require_complete_switch [Message]
    (
        Message = message(_, _, _, _, _, _)
    ;
        Message = excluded_message(_, _, _, _, _),
        fail
    ).

get_maybe_message_id(message(Id, _, _, _, _, _)) = yes(Id).
get_maybe_message_id(excluded_message(MaybeId, _, _, _, _)) = MaybeId.

get_timestamp_or_zero(Message) = Timestamp :-
    (
        Message = message(_, Timestamp, _, _, _, _)
    ;
        Message = excluded_message(_, MaybeTimestamp, _, _, _),
        (
            MaybeTimestamp = yes(Timestamp)
        ;
            MaybeTimestamp = no,
            Timestamp = timestamp(0.0)
        )
    ).

get_maybe_subject(Message) = MaybeSubject :-
    (
        Message = message(_, _, Headers, _, _, _),
        MaybeSubject = yes(Headers ^ h_subject)
    ;
        Message = excluded_message(_, _, MaybeHeaders, _, _),
        (
            MaybeHeaders = yes(Headers),
            MaybeSubject = yes(Headers ^ h_subject)
        ;
            MaybeHeaders = no,
            MaybeSubject = no
        )
    ).

get_replies(message(_, _, _, _, _, Replies)) = Replies.
get_replies(excluded_message(_, _, _, _, Replies)) = Replies.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
