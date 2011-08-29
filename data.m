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
                m_subject   :: string,
                m_from      :: string,
                m_to        :: string,
                m_date      :: string,
                m_tags      :: list(string),
                m_body      :: cord(content),
                m_replies   :: list(message)
            ).

:- type message_id
    --->    message_id(string).

:- type content
    --->    content(
                c_id        :: int,
                c_type      :: string,
                c_content   :: maybe(string),
                c_filename  :: maybe(string)
            ).

:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

snoc(X, C, snoc(C, X)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
