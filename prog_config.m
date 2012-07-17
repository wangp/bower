% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prog_config.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred get_notmuch_prefix(string::out, io::di, io::uo) is det.

:- pred get_notmuch_deliver_prefix(string::out, io::di, io::uo) is det.

:- pred get_editor_command(string::out, io::di, io::uo) is det.

:- pred get_sendmail_command(string::out, io::di, io::uo) is det.

:- pred get_maybe_post_sendmail_command(maybe(string)::out, io::di, io::uo)
    is det.

:- pred get_html_dump_command(string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module string.

:- import_module config.
:- import_module xdg.

%-----------------------------------------------------------------------------%

:- mutable(prog_config_var, maybe(config), no, ground,
    [untrailed, attach_to_io_state]).

:- pred get_prog_config(config::out, io::di, io::uo) is det.

get_prog_config(Config, !IO) :-
    get_prog_config_var(MaybeConfig, !IO),
    (
        MaybeConfig = yes(Config)
    ;
        MaybeConfig = no,
        search_config_file(config_filename, MaybeConfigFile, !IO),
        some [!Config] (
            (
                MaybeConfigFile = yes(ConfigFile),
                load_config_file(ConfigFile, LoadRes, !IO),
                (
                    LoadRes = ok(!:Config)
                ;
                    LoadRes = error(_),
                    !:Config = init_config
                )
            ;
                MaybeConfigFile = no,
                !:Config = init_config
            ),
            Config = !.Config
        ),
        set_prog_config_var(yes(Config), !IO)
    ).

%-----------------------------------------------------------------------------%

get_notmuch_prefix(Notmuch, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "notmuch", Value) ->
        Notmuch = Value ++ " "
    ;
        Notmuch = default_notmuch_prefix
    ).

get_notmuch_deliver_prefix(NotmuchDeliver, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "notmuch_deliver", Value) ->
        NotmuchDeliver = Value ++ " "
    ;
        NotmuchDeliver = default_notmuch_deliver_prefix
    ).

get_editor_command(Command, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "editor", Value) ->
        Command = Value
    ;
        io.get_environment_var("EDITOR", MaybeEditor, !IO),
        (
            MaybeEditor = yes(Command)
        ;
            MaybeEditor = no,
            Command = default_editor_command
        )
    ).

get_sendmail_command(Command, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "sendmail", Value) ->
        Command = Value
    ;
        Command = default_sendmail_command
    ).

get_maybe_post_sendmail_command(MaybeCommand, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "post_sendmail", Value) ->
        MaybeCommand = yes(Value)
    ;
        MaybeCommand = no
    ).

get_html_dump_command(Command, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "html_dump", Value) ->
        Command = Value
    ;
        Command = default_html_dump_command
    ).

%-----------------------------------------------------------------------------%

:- func config_filename = string.

config_filename = "bower/bower.conf".

:- func default_notmuch_prefix = string.

default_notmuch_prefix = "notmuch ". % trailing space

:- func default_notmuch_deliver_prefix = string.

default_notmuch_deliver_prefix = "notmuch-deliver ". % trailing space

:- func default_editor_command = string.

default_editor_command = "vi".

:- func default_sendmail_command = string.

default_sendmail_command = "/usr/bin/sendmail -oi -oem".

:- func default_html_dump_command = string.

default_html_dump_command = "lynx -dump -force-html -stdin".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
