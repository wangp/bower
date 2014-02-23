% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prog_config.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred load_prog_config(maybe_error::out, io::di, io::uo) is det.

:- pred check_sendmail_command(maybe_error::out, io::di, io::uo) is det.

:- pred get_notmuch_prefix(string::out, io::di, io::uo) is det.

:- pred get_notmuch_deliver_prefix(string::out, io::di, io::uo) is det.

:- pred get_editor_command(string::out, io::di, io::uo) is det.

:- type sendmail_option
    --->    sendmail_no_read_recipients
    ;       sendmail_read_recipients. % '-t' option

:- pred get_sendmail_command(sendmail_option::in, string::out, io::di, io::uo)
    is det.

:- pred get_maybe_post_sendmail_command(maybe(string)::out, io::di, io::uo)
    is det.

:- pred get_maybe_html_dump_command(maybe(string)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module string.

:- import_module config.
:- import_module xdg.

%-----------------------------------------------------------------------------%

:- mutable(prog_config, config, init_config, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

load_prog_config(Res, !IO) :-
    search_config_file(config_filename, MaybeConfigFile, !IO),
    (
        MaybeConfigFile = yes(ConfigFile),
        load_config_file(ConfigFile, LoadRes, !IO),
        (
            LoadRes = ok(Config),
            set_prog_config(Config, !IO),
            Res = ok
        ;
            LoadRes = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MaybeConfigFile = no,
        Res = ok
    ).

%-----------------------------------------------------------------------------%

check_sendmail_command(Res, !IO) :-
    get_sendmail_command(sendmail_no_read_recipients, Command, !IO),
    (
        ( string.suffix(Command, " -t")
        ; string.sub_string_search(Command, " -t ", _)
        )
    ->
        Res = error("Please remove -t option from command.sendmail:  " ++ Command)
    ;
        Res = ok
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

get_sendmail_command(Option, Command, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "sendmail", Value) ->
        Command0 = Value
    ;
        Command0 = default_sendmail_command
    ),
    (
        Option = sendmail_no_read_recipients,
        Command = Command0
    ;
        Option = sendmail_read_recipients,
        Command = Command0 ++ " -t"
    ).

get_maybe_post_sendmail_command(MaybeCommand, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "post_sendmail", Value) ->
        MaybeCommand = yes(Value)
    ;
        MaybeCommand = no
    ).

get_maybe_html_dump_command(MaybeCommand, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "html_dump", Value) ->
        ( Value = "" ->
            MaybeCommand = no
        ;
            MaybeCommand = yes(Value)
        )
    ;
        MaybeCommand = yes(default_html_dump_command)
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

default_html_dump_command = "lynx -dump -force-html -stdin -display-charset=utf-8".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
