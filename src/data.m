% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module data.
:- interface.

:- import_module bool.
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
                m_id        :: message_id,
                m_timestamp :: timestamp,
                m_headers   :: headers,
                m_tags      :: set(tag),
                m_body      :: list(part),
                m_replies   :: list(message)
            )
    ;       excluded_message(
                em_replies  :: list(message)
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

:- type part
    --->    part(
                pt_msgid        :: message_id,
                pt_part         :: maybe(int),
                pt_type         :: mime_type,
                pt_content      :: part_content,
                pt_filename     :: maybe(string),
                pt_encoding     :: maybe(string),
                pt_content_len  :: maybe(int),
                pt_decrypted    :: bool
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
                em_headers      :: headers,
                em_body         :: list(part)
            ).

%-----------------------------------------------------------------------------%

:- func thread_id_to_search_term(thread_id) = string.

:- func message_id_to_search_term(message_id) = string.

:- func init_headers = headers.

:- pred empty_header_value(header_value::in) is semidet.

:- func header_value_string(header_value) = string.

:- pred tag_to_string(tag::in, string::out) is det.

:- func get_maybe_message_id(message) = maybe(message_id).

:- func get_timestamp_fallback(message) = timestamp.

:- func get_subject_fallback(message) = header_value.

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

get_maybe_message_id(message(Id, _, _, _, _, _)) = yes(Id).
get_maybe_message_id(excluded_message(_)) = no.

get_timestamp_fallback(message(_, Timestamp, _, _, _, _)) = Timestamp.
get_timestamp_fallback(excluded_message(_)) = timestamp(0.0).

get_subject_fallback(message(_, _, Headers, _, _, _)) = Headers ^ h_subject.
get_subject_fallback(excluded_message(_)) = header_value("(excluded message)").

get_replies(message(_, _, _, _, _, Replies)) = Replies.
get_replies(excluded_message(Replies)) = Replies.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
