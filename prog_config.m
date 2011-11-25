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

get_sendmail_command(Command, !IO) :-
    search_config_file(config_filename, MaybeConfigFile, !IO),
    (
        MaybeConfigFile = yes(ConfigFile),
        load_config_file(ConfigFile, LoadRes, !IO),
        (
            LoadRes = ok(Config),
            search_default(Config, "command", "sendmail", Command,
                default_sendmail_command)
        ;
            LoadRes = error(_),
            Command = default_sendmail_command
        )
    ;
        MaybeConfigFile = no,
        Command = default_sendmail_command
    ).

:- pred search_default(config::in, section::in, string::in, string::out,
    string::in) is det.

search_default(Config, Section, Key, Value, Default) :-
    (
        map.search(Config, Section, SectionMap),
        map.search(SectionMap, Key, ValuePrime)
    ->
        Value = ValuePrime
    ;
        Value = Default
    ).

:- func config_filename = string.

config_filename = "bower/bower.conf".

:- func default_sendmail_command = string.

default_sendmail_command = "/usr/bin/sendmail -oi -oem".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
