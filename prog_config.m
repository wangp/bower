%-----------------------------------------------------------------------------%

:- module prog_config.
:- interface.

:- import_module io.

:- pred get_sendmail_command(string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module maybe.

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
        (
            MaybeConfigFile = yes(ConfigFile),
            load_config_file(ConfigFile, LoadRes, !IO),
            (
                LoadRes = ok(Config)
            ;
                LoadRes = error(_),
                Config = init_config
            )
        ;
            MaybeConfigFile = no,
            Config = init_config
        ),
        set_prog_config_var(yes(Config), !IO)
    ).

%-----------------------------------------------------------------------------%

get_sendmail_command(Command, !IO) :-
    get_prog_config(Config, !IO),
    ( search_config(Config, "command", "sendmail", Value) ->
        Command = Value
    ;
        Command = default_sendmail_command
    ).

%-----------------------------------------------------------------------------%

:- func config_filename = string.

config_filename = "bower/bower.conf".

:- func default_sendmail_command = string.

default_sendmail_command = "/usr/bin/sendmail -oi -oem".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
