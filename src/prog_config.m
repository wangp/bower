% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prog_config.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module color.
:- import_module quote_arg.
:- import_module shell_word.

%-----------------------------------------------------------------------------%

:- type prog_config.

:- type load_prog_config_result
    --->    ok(prog_config)
    ;       errors(list(string)).

:- type sendmail_option
    --->    sendmail_read_recipients. % '-t' option

:- type post_sendmail
    --->    default
    ;       nothing
    ;       command(command_prefix).

%-----------------------------------------------------------------------------%

:- pred load_prog_config(load_prog_config_result::out, io::di, io::uo)
    is cc_multi.

:- pred get_notmuch_command(prog_config::in, command_prefix::out) is det.

:- pred get_editor_command(prog_config::in, command_prefix::out) is det.

:- pred get_sendmail_command(prog_config::in, sendmail_option::in,
    command_prefix::out) is det.

:- pred get_post_sendmail_action(prog_config::in, post_sendmail::out) is det.

:- pred get_maybe_html_dump_command(prog_config::in,
    maybe(command_prefix)::out) is det.

:- pred get_open_part_command(prog_config::in, string::out) is det.

:- pred get_open_url_command(prog_config::in, string::out) is det.

:- pred get_poll_period_secs(prog_config::in, maybe(int)::out) is det.

:- func generic_attrs(prog_config) = generic_attrs.
:- func status_attrs(prog_config) = status_attrs.
:- func pager_attrs(prog_config) = pager_attrs.
:- func index_attrs(prog_config) = index_attrs.
:- func thread_attrs(prog_config) = thread_attrs.
:- func compose_attrs(prog_config) = compose_attrs.

%-----------------------------------------------------------------------------%

    % Exported for open part / URL commands.
:- pred detect_ssh(list(word)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module parsing_utils.
:- import_module string.

:- import_module config.
:- import_module quote_arg.
:- import_module xdg.

:- type prog_config
    --->    prog_config(
                notmuch         :: command_prefix,
                editor          :: command_prefix,
                sendmail        :: command_prefix,
                post_sendmail   :: post_sendmail,
                html_dump       :: maybe(command_prefix),
                open_part       :: string, % not shell-quoted
                open_url        :: string, % not shell-quoted
                poll_period_secs :: maybe(int),
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
            load_prog_config_2(Config, Res, !IO)
        ;
            LoadRes = error(Error),
            Res = errors([io.error_message(Error)])
        )
    ;
        MaybeConfigFile = no,
        load_prog_config_2(init_config, Res, !IO)
    ).

:- pred load_prog_config_2(config::in, load_prog_config_result::out,
    io::di, io::uo) is cc_multi.

load_prog_config_2(Config, Res, !IO) :-
    make_prog_config(Config, ProgConfig, [], RevErrors, !IO),
    (
        RevErrors = [],
        Res = ok(ProgConfig)
    ;
        RevErrors = [_ | _],
        Res = errors(reverse(RevErrors))
    ).

:- pred make_prog_config(config::in, prog_config::out,
    list(string)::in, list(string)::out, io::di, io::uo) is cc_multi.

make_prog_config(Config, ProgConfig, !Errors, !IO) :-
    ( search_config(Config, "command", "notmuch", Notmuch0) ->
        parse_command(Notmuch0, Notmuch, !Errors)
    ;
        Notmuch = default_notmuch_command
    ),

    ( search_config(Config, "command", "notmuch_deliver", _) ->
        cons("notmuch_deliver is no longer supported; " ++
            "please update to notmuch 0.19 or later.", !Errors)
    ;
        true
    ),

    ( search_config(Config, "command", "editor", Editor0) ->
        parse_command(Editor0, Editor, !Errors)
    ;
        io.get_environment_var("EDITOR", MaybeEditor, !IO),
        (
            MaybeEditor = yes(Editor0),
            parse_command(Editor0, Editor, !Errors)
        ;
            MaybeEditor = no,
            Editor = default_editor_command
        )
    ),

    ( search_config(Config, "command", "sendmail", Sendmail0) ->
        parse_command(Sendmail0, Sendmail, !Errors),
        check_sendmail_command(Sendmail, !Errors)
    ;
        Sendmail = default_sendmail_command
    ),

    ( search_config(Config, "command", "post_sendmail", PostSendmail0) ->
        ( PostSendmail0 = "" ->
            PostSendmail = nothing
        ;
            parse_command(PostSendmail0, PostSendmail1, !Errors),
            PostSendmail = command(PostSendmail1)
        )
    ;
        PostSendmail = default
    ),

    ( search_config(Config, "command", "html_dump", HtmlDump0) ->
        ( HtmlDump0 = "" ->
            MaybeHtmlDump = no
        ;
            parse_command(HtmlDump0, HtmlDump1, !Errors),
            MaybeHtmlDump = yes(HtmlDump1)
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

    ( search_config(Config, "index", "poll_period_secs", Value) ->
        check_poll_period_secs(Value, PollPeriodSecs, !Errors)
    ;
        PollPeriodSecs = default_poll_period_secs
    ),

    make_colors(Config, Colors),

    ProgConfig ^ notmuch = Notmuch,
    ProgConfig ^ editor = Editor,
    ProgConfig ^ sendmail = Sendmail,
    ProgConfig ^ post_sendmail = PostSendmail,
    ProgConfig ^ html_dump = MaybeHtmlDump,
    ProgConfig ^ open_part = OpenPart,
    ProgConfig ^ open_url = OpenUrl,
    ProgConfig ^ poll_period_secs = PollPeriodSecs,
    ProgConfig ^ colors = Colors.

:- pred parse_command(string::in, command_prefix::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_command(S0, command_prefix(shell_quoted(S), QuoteTimes), !Errors) :-
    shell_word.split(S0, ParseResult),
    (
        ParseResult = ok(Words),
        Args = list.map(word_string, Words),
        S = string.join_list(" ", list.map(quote_arg, Args)),
        QuoteTimes = ( detect_ssh(Words) -> quote_twice ; quote_once )
    ;
        (
            ParseResult = error(yes(Message), _Line, _Column)
        ;
            ParseResult = error(no, _Line, _Column),
            Message = "parse error"
        ),
        Error = string.format("%s in value: %s", [s(Message), s(S0)]),
        cons(Error, !Errors),
        S = "false", % dummy
        QuoteTimes = quote_once
    ).

detect_ssh([FirstWord | _]) :-
    FirstWord = word(Segments),
    list.member(unquoted(String), Segments),
    string.sub_string_search(String, "ssh", _).

:- pred check_sendmail_command(command_prefix::in,
    list(string)::in, list(string)::out) is det.

check_sendmail_command(command_prefix(shell_quoted(Command), _), !Errors) :-
    (
        ( string.suffix(Command, " -t")
        ; string.sub_string_search(Command, " -t ", _)
        )
    ->
        Error = "sendmail command contains -t option: " ++ Command,
        cons(Error, !Errors)
    ;
        true
    ).

:- pred check_poll_period_secs(string::in, maybe(int)::out,
    list(string)::in, list(string)::out) is det.

check_poll_period_secs(Value, PollPeriodSecs, !Errors) :-
    ( string.to_lower(Value, "off") ->
        PollPeriodSecs = no
    ; string.to_int(Value, Int), Int > 0 ->
        PollPeriodSecs = yes(Int)
    ;
        Error = "poll_period_secs invalid: " ++ Value,
        cons(Error, !Errors),
        PollPeriodSecs = default_poll_period_secs
    ).

%-----------------------------------------------------------------------------%

get_notmuch_command(Config, Notmuch) :-
    Notmuch = Config ^ notmuch.

get_editor_command(Config, Editor) :-
    Editor = Config ^ editor.

get_sendmail_command(Config, sendmail_read_recipients, Command) :-
    Command0 = Config ^ sendmail,
    Command0 = command_prefix(shell_quoted(Cmd), QuoteTimes),
    Command = command_prefix(shell_quoted(Cmd ++ " -t"), QuoteTimes).

get_post_sendmail_action(Config, Action) :-
    Action = Config ^ post_sendmail.

get_maybe_html_dump_command(Config, MaybeCommand) :-
    MaybeCommand = Config ^ html_dump.

get_open_part_command(Config, Command) :-
    Command = Config ^ open_part.

get_open_url_command(Config, Command) :-
    Command = Config ^ open_url.

get_poll_period_secs(Config, PollPeriodSecs) :-
    PollPeriodSecs = Config ^ poll_period_secs.

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

:- func default_notmuch_command = command_prefix.

default_notmuch_command =
    command_prefix(shell_quoted("notmuch"), quote_once).

:- func default_editor_command = command_prefix.

default_editor_command =
    command_prefix(shell_quoted("vi"), quote_once).

:- func default_sendmail_command = command_prefix.

default_sendmail_command =
    command_prefix(shell_quoted("/usr/bin/sendmail -oi -oem"), quote_once).

:- func default_html_dump_command = command_prefix.

default_html_dump_command = command_prefix(shell_quoted(Lynx), quote_once) :-
    Lynx = "lynx -dump -force-html -stdin -display-charset=utf-8".

:- func default_open_part_command = string.

default_open_part_command = "xdg-open&".

:- func default_open_url_command = string.

default_open_url_command = "xdg-open&".

:- func default_poll_period_secs = maybe(int).

default_poll_period_secs = yes(60).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
