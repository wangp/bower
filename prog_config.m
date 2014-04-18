% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prog_config.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module color.

%-----------------------------------------------------------------------------%

:- type prog_config.

:- pred load_prog_config(maybe_error(prog_config)::out, io::di, io::uo) is det.

:- pred check_sendmail_command(prog_config::in, maybe_error::out) is det.

:- pred get_notmuch_prefix(prog_config::in, string::out) is det.

:- pred get_notmuch_deliver_prefix(prog_config::in, string::out) is det.

:- pred get_editor_command(prog_config::in, string::out) is det.

:- type sendmail_option
    --->    sendmail_no_read_recipients
    ;       sendmail_read_recipients. % '-t' option

:- pred get_sendmail_command(prog_config::in, sendmail_option::in,
    string::out) is det.

:- pred get_maybe_post_sendmail_command(prog_config::in, maybe(string)::out)
    is det.

:- pred get_maybe_html_dump_command(prog_config::in, maybe(string)::out)
    is det.

:- pred get_open_part_command(prog_config::in, string::out) is det.

:- pred get_open_url_command(prog_config::in, string::out) is det.

:- func generic_attrs(prog_config) = generic_attrs.
:- func status_attrs(prog_config) = status_attrs.
:- func pager_attrs(prog_config) = pager_attrs.
:- func index_attrs(prog_config) = index_attrs.
:- func thread_attrs(prog_config) = thread_attrs.
:- func compose_attrs(prog_config) = compose_attrs.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module string.

:- import_module config.
:- import_module xdg.

:- type prog_config
    --->    prog_config(
                notmuch         :: string,
                notmuch_deliver :: string,
                editor          :: string,
                sendmail        :: string,
                post_sendmail   :: maybe(string),
                html_dump       :: maybe(string),
                open_part       :: string,
                open_url        :: string,
                colors          :: colors
            ).

%-----------------------------------------------------------------------------%

load_prog_config(Res, !IO) :-
    search_config_file(config_filename, MaybeConfigFile, !IO),
    (
        MaybeConfigFile = yes(ConfigFile),
        load_config_file(ConfigFile, LoadRes, !IO),
        (
            LoadRes = ok(Config),
            make_prog_config(Config, ProgConfig, !IO),
            Res = ok(ProgConfig)
        ;
            LoadRes = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MaybeConfigFile = no,
        make_prog_config(init_config, ProgConfig, !IO),
        Res = ok(ProgConfig)
    ).

:- pred make_prog_config(config::in, prog_config::out, io::di, io::uo) is det.

make_prog_config(Config, ProgConfig, !IO) :-
    ( search_config(Config, "command", "notmuch", Notmuch0) ->
        Notmuch = Notmuch0
    ;
        Notmuch = default_notmuch_command
    ),

    ( search_config(Config, "command", "notmuch_deliver", NotmuchDeliver0) ->
        NotmuchDeliver = NotmuchDeliver0
    ;
        NotmuchDeliver = default_notmuch_deliver_command
    ),

    ( search_config(Config, "command", "editor", Editor0) ->
        Editor = Editor0
    ;
        io.get_environment_var("EDITOR", MaybeEditor, !IO),
        (
            MaybeEditor = yes(Editor)
        ;
            MaybeEditor = no,
            Editor = default_editor_command
        )
    ),

    ( search_config(Config, "command", "sendmail", Sendmail0) ->
        Sendmail = Sendmail0
    ;
        Sendmail = default_sendmail_command
    ),

    ( search_config(Config, "command", "post_sendmail", PostSendmail) ->
        MaybePostSendmail = yes(PostSendmail)
    ;
        MaybePostSendmail = no
    ),

    ( search_config(Config, "command", "html_dump", HtmlDump) ->
        ( HtmlDump = "" ->
            MaybeHtmlDump = no
        ;
            MaybeHtmlDump = yes(HtmlDump)
        )
    ;
        MaybeHtmlDump = yes(default_html_dump_command)
    ),

    ( search_config(Config, "command", "open_part", OpenPart0) ->
        OpenPart = OpenPart0
    ;
        OpenPart = default_open_part_command
    ),

    ( search_config(Config, "command", "open_url", OpenUrl0) ->
        OpenUrl = OpenUrl0
    ;
        OpenUrl = default_open_url_command
    ),

    make_colors(Config, Colors),

    ProgConfig ^ notmuch = Notmuch,
    ProgConfig ^ notmuch_deliver = NotmuchDeliver,
    ProgConfig ^ editor = Editor,
    ProgConfig ^ sendmail = Sendmail,
    ProgConfig ^ post_sendmail = MaybePostSendmail,
    ProgConfig ^ html_dump = MaybeHtmlDump,
    ProgConfig ^ open_part = OpenPart,
    ProgConfig ^ open_url = OpenUrl,
    ProgConfig ^ colors = Colors.

%-----------------------------------------------------------------------------%

check_sendmail_command(Config, Res) :-
    get_sendmail_command(Config, sendmail_no_read_recipients, Command),
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

get_notmuch_prefix(Config, Notmuch) :-
    Notmuch = Config ^ notmuch ++ " ".

get_notmuch_deliver_prefix(Config, NotmuchDeliver) :-
    NotmuchDeliver = Config ^ notmuch_deliver ++ " ".

get_editor_command(Config, Command) :-
    Command = Config ^ editor.

get_sendmail_command(Config, Option, Command) :-
    Command0 = Config ^ sendmail,
    (
        Option = sendmail_no_read_recipients,
        Command = Command0
    ;
        Option = sendmail_read_recipients,
        Command = Command0 ++ " -t"
    ).

get_maybe_post_sendmail_command(Config, MaybeCommand) :-
    MaybeCommand = Config ^ post_sendmail.

get_maybe_html_dump_command(Config, MaybeCommand) :-
    MaybeCommand = Config ^ html_dump.

get_open_part_command(Config, Command) :-
    Command = Config ^ open_part.

get_open_url_command(Config, Command) :-
    Command = Config ^ open_url.

%-----------------------------------------------------------------------------%

generic_attrs(Config) = Config ^ colors ^ generic_attrs.
status_attrs(Config) = Config ^ colors ^ status_attrs.
pager_attrs(Config) = Config ^ colors ^ pager_attrs.
index_attrs(Config) = Config ^ colors ^ index_attrs.
thread_attrs(Config) = Config ^ colors ^ thread_attrs.
compose_attrs(Config) = Config ^ colors ^ compose_attrs.

%-----------------------------------------------------------------------------%

:- func config_filename = string.

config_filename = "bower/bower.conf".

:- func default_notmuch_command = string.

default_notmuch_command = "notmuch".

:- func default_notmuch_deliver_command = string.

default_notmuch_deliver_command = "notmuch-deliver".

:- func default_editor_command = string.

default_editor_command = "vi".

:- func default_sendmail_command = string.

default_sendmail_command = "/usr/bin/sendmail -oi -oem".

:- func default_html_dump_command = string.

default_html_dump_command = "lynx -dump -force-html -stdin -display-charset=utf-8".

:- func default_open_part_command = string.

default_open_part_command = "xdg-open&".

:- func default_open_url_command = string.

default_open_url_command = "xdg-open&".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
