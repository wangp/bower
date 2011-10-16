%-----------------------------------------------------------------------------%

:- module data.
:- interface.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type thread
    --->    thread(
                t_id        :: thread_id,
                t_timestamp :: int,
                t_authors   :: string,
                t_subject   :: string,
                t_tags      :: list(string),
                t_matched   :: int,
                t_total     :: int
            ).

:- type thread_id
    --->    thread_id(string).

:- type message
    --->    message(
                m_id        :: message_id,
                m_timestamp :: int,
                m_headers   :: headers,
                m_tags      :: list(string),
                m_body      :: list(part),
                m_replies   :: list(message)
            ).

:- type message_id
    --->    message_id(string).

:- type headers
    --->    headers(
                % Technically, header fields.
                h_date          :: string,
                h_from          :: string,
                h_to            :: string,
                h_cc            :: string,
                h_bcc           :: string,
                h_subject       :: string,
                h_replyto       :: string,
                h_references    :: string,
                h_inreplyto     :: string,
                h_rest          :: map(string, string)
            ).

:- type part
    --->    part(
                pt_msgid        :: message_id,
                pt_part         :: int,
                pt_type         :: string,
                pt_content      :: part_content,
                pt_filename     :: maybe(string)
            ).

:- type part_content
    --->    text(string)
    ;       subparts(list(part))
    ;       unsupported.

%-----------------------------------------------------------------------------%

:- func thread_id_to_search_term(thread_id) = string.

:- func message_id_to_search_term(message_id) = string.

:- func init_headers = headers.

:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module string.

%-----------------------------------------------------------------------------%

thread_id_to_search_term(thread_id(Id)) = "thread:" ++ Id.

message_id_to_search_term(message_id(Id)) = "id:" ++ Id.

init_headers = headers("", "", "", "", "", "", "", "", "", map.init).

snoc(X, C, snoc(C, X)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
