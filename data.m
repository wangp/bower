%-----------------------------------------------------------------------------%

:- module data.
:- interface.

:- import_module cord.
:- import_module list.
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
                m_date      :: string,
                m_from      :: string,
                m_subject   :: string,
                m_to        :: string,
                m_cc        :: string,
                m_reply_to  :: string,
                m_tags      :: list(string),
                m_body      :: cord(content),
                m_replies   :: list(message)
            ).

:- type message_id
    --->    message_id(string).

:- type content
    --->    content(
                c_msgid     :: message_id,
                c_part      :: int,
                c_type      :: string,
                c_content   :: maybe(string),
                c_filename  :: maybe(string)
            ).

%-----------------------------------------------------------------------------%

:- func thread_id_to_search_term(thread_id) = string.

:- func message_id_to_search_term(message_id) = string.

:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

thread_id_to_search_term(thread_id(Id)) = "thread:" ++ Id.

message_id_to_search_term(message_id(Id)) = "id:" ++ Id.

snoc(X, C, snoc(C, X)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
