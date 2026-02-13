% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prog_config.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module color.
:- import_module data.
:- import_module mime_type.
:- import_module notmuch_config.
:- import_module quote_command.
:- import_module rfc5322.
:- import_module shell_word.
:- import_module uri.

%-----------------------------------------------------------------------------%

:- type prog_config.

:- type load_prog_config_result
    --->    ok(prog_config, notmuch_config)
    ;       errors(list(string)).

:- type account.

:- type post_sendmail
    --->    default
    ;       nothing
    ;       command(command_prefix).

:- type thread_ordering
    --->    thread_ordering_threaded
    ;       thread_ordering_flat.

:- type use_alt_html_filter
    --->    alt_html_filter_never
    ;       alt_html_filter_off
    ;       alt_html_filter_on
    ;       alt_html_filter_always.

:- type default_max_threads
    --->    no_max_threads
    ;       max_threads(int).

%-----------------------------------------------------------------------------%

:- pred load_prog_config(load_prog_config_result::out, io::di, io::uo)
    is cc_multi.

:- pred get_notmuch_command(prog_config::in, command_prefix::out) is det.

:- pred get_editor_command(prog_config::in, command_prefix::out) is det.

:- pred get_open_part_command(prog_config::in, list(string)::out) is det.

:- pred get_open_url_command(prog_config::in, list(string)::out) is det.

:- pred get_pipe_id_command(prog_config::in, list(string)::out) is det.

:- pred get_alt_html_filter_command(prog_config::in,
    maybe(command_prefix)::out) is det.

:- pred get_use_alt_html_filter(prog_config::in, use_alt_html_filter::out)
    is det.

:- pred get_poll_notify_command(prog_config::in, maybe(command_prefix)::out)
    is det.

:- pred get_poll_period_secs(prog_config::in, maybe(int)::out) is det.

:- pred get_auto_refresh_inactive_secs(prog_config::in, maybe(int)::out)
    is det.

:- pred get_wrap_width(prog_config::in, int::in, int::out) is det.

:- pred get_thread_ordering(prog_config::in, thread_ordering::out) is det.

:- pred get_default_save_directory(prog_config::in, string::out) is det.

:- pred get_default_max_threads(prog_config::in, default_max_threads::out)
    is det.

:- pred get_text_filter_command(prog_config::in, mime_type::in,
    command_prefix::out) is semidet.

:- pred get_maybe_signature_file(prog_config::in, maybe(string)::out) is det.

:- pred get_encrypt_by_default(prog_config::in, bool::out) is det.

:- pred get_sign_by_default(prog_config::in, bool::out) is det.

:- pred get_decrypt_by_default(prog_config::in, bool::out) is det.

:- pred get_verify_by_default(prog_config::in, bool::out) is det.

:- pred get_all_accounts(prog_config::in, list(account)::out) is det.

:- pred get_default_account(prog_config::in, maybe(account)::out) is det.

:- pred get_some_matching_account(prog_config::in, address_list::in,
    maybe(account)::out) is det.

:- pred get_matching_account(prog_config::in, address::in, account::out)
    is semidet.

:- pred get_account_name(account::in, string::out) is det.

:- pred get_from_address(account::in, mailbox::out) is det.

:- pred get_from_address_as_string(account::in, string::out) is det.

:- pred get_sendmail_command(account::in, command_prefix::out) is det.

:- pred get_post_sendmail_action(account::in, post_sendmail::out) is det.

:- pred get_autocrypt_keydata(account::in, maybe(string)::out) is det.

:- pred get_exclude_tags(prog_config::in, set(tag)::out) is det.

:- func generic_attrs(prog_config) = generic_attrs.
:- func status_attrs(prog_config) = status_attrs.
:- func pager_attrs(prog_config) = pager_attrs.
:- func index_attrs(prog_config) = index_attrs.
:- func thread_attrs(prog_config) = thread_attrs.
:- func compose_attrs(prog_config) = compose_attrs.

:- pred get_url_regex(prog_config::in, url_regex::out) is det.

%-----------------------------------------------------------------------------%

    % Exported for open part / URL commands.
:- pred detect_ssh(list(shell_token)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module parsing_utils.
:- import_module string.

:- import_module base64.
:- import_module config.
:- import_module path_expand.
:- import_module rfc5322.parser.
:- import_module rfc5322.writer.
:- import_module xdg.

:- type prog_config
    --->    prog_config(
                % XXX reorganise this type

                % [command]
                notmuch         :: command_prefix,
                editor          :: command_prefix,
                text_filters    :: map(mime_type, command_prefix),
                open_part       :: list(string), % not shell-quoted
                open_url        :: list(string), % not shell-quoted
                pipe_id         :: list(string), % not shell-quoted
                alt_html_filter :: maybe(command_prefix),
                use_alt_html_filter :: use_alt_html_filter,
                poll_notify     :: maybe(command_prefix),

                % [ui]
                poll_period_secs :: maybe(int),
                auto_refresh_inactive_secs :: maybe(int),
                wrap_width      :: maybe(int),
                thread_ordering :: thread_ordering,
                default_save_directory :: string,
                default_max_threads :: default_max_threads,

                % [compose]
                maybe_signature_file  :: maybe(string), % absolute path

                % [crypto]
                encrypt_by_default :: bool,
                sign_by_default :: bool,
                decrypt_by_default :: bool,
                verify_by_default :: bool,

                % [account.*]
                accounts        :: list(account),
                default_account :: maybe(account),

                % from notmuch config
                exclude_tags    :: set(tag),

                % [color]
                colors          :: colors,

                % not actually configuration but kept here for convenience
                url_regex       :: url_regex
            ).

:- type account == account(mailbox).

:- type account(From)
    --->    account(
                account_name        :: string,
                from_address        :: From,
                default             :: default_setting,
                sendmail            :: command_prefix,
                post_sendmail       :: post_sendmail,
                autocrypt_keydata   :: maybe(string)
            ).

:- type default_setting
    --->    unset
    ;       no
    ;       yes.

%-----------------------------------------------------------------------------%

load_prog_config(Res, !IO) :-
    get_environment_var("BOWER_CONFIG", MaybeEnv, !IO),
    (
        MaybeEnv = yes(EnvValue),
        MaybeConfigFile = yes(EnvValue)
    ;
        MaybeEnv = no,
        search_config_file(config_filename, MaybeConfigFile, !IO)
    ),
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
    get_home_dir(Home, !IO),
    make_prog_config(Home, Config, ProgConfig, NotmuchConfig,
        [], RevErrors, !IO),
    (
        RevErrors = [],
        Res = ok(ProgConfig, NotmuchConfig)
    ;
        RevErrors = [_ | _],
        Res = errors(reverse(RevErrors))
    ).

:- pred make_prog_config(home::in, config::in, prog_config::out,
    notmuch_config::out, list(string)::in, list(string)::out, io::di, io::uo)
    is cc_multi.

make_prog_config(Home, Config, ProgConfig, NotmuchConfig, !Errors, !IO) :-
    ( search_config(Config, "command", "notmuch", Notmuch0) ->
        parse_command(Home, Notmuch0, Notmuch, !Errors)
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
        parse_command(Home, Editor0, Editor, !Errors)
    ;
        io.get_environment_var("EDITOR", MaybeEditor, !IO),
        (
            MaybeEditor = yes(Editor0),
            parse_command(Home, Editor0, Editor, !Errors)
        ;
            MaybeEditor = no,
            Editor = default_editor_command
        )
    ),

    ( search_config(Config, "command", "open_part", OpenPart0) ->
        parse_multi_commands(OpenPart0, OpenPart, !Errors)
    ;
        OpenPart = [default_open_part_command]
    ),

    ( search_config(Config, "command", "open_url", OpenUrl0) ->
        parse_multi_commands(OpenUrl0, OpenUrl, !Errors)
    ;
        OpenUrl = [default_open_url_command]
    ),

    ( search_config(Config, "command", "pipe_id", PipeId0) ->
        parse_multi_commands(PipeId0, PipeId, !Errors)
    ;
        PipeId = [default_pipe_id_command]
    ),

    ( search_config(Config, "command", "alt_html_filter", AltHtmlFilter0) ->
        ( AltHtmlFilter0 \= "" ->
            parse_command(Home, AltHtmlFilter0, AltHtmlFilter1, !Errors),
            AltHtmlFilter = yes(AltHtmlFilter1)
        ;
            AltHtmlFilter = no
        )
    ;
        AltHtmlFilter = yes(default_alt_html_filter_command)
    ),

    (
        search_config(Config, "command", "use_alt_html_filter",
            UseAltHtmlFilter0),
        UseAltHtmlFilter1 = string.to_lower(UseAltHtmlFilter0),
        (
            UseAltHtmlFilter1 = "never",
            UseAltHtmlFilter = alt_html_filter_never
        ;
            UseAltHtmlFilter1 = "manual",
            UseAltHtmlFilter = alt_html_filter_off
        ;
            UseAltHtmlFilter1 = "default",
            UseAltHtmlFilter = alt_html_filter_on
        ;
            UseAltHtmlFilter1 = "always",
            UseAltHtmlFilter = alt_html_filter_always
        )
    ;
        UseAltHtmlFilter = alt_html_filter_off
    ),

    (
        search_config(Config, "command", "poll_notify", PollNotify0),
        PollNotify0 \= ""
    ->
        parse_command(Home, PollNotify0, PollNotify1, !Errors),
        PollNotify = yes(PollNotify1)
    ;
        PollNotify = no
    ),

    ( search_config(Config, "ui", "poll_period_secs", PollSecs0) ->
        check_poll_period_secs(PollSecs0, PollSecs, !Errors)
    ; search_config(Config, "index", "poll_period_secs", PollSecs0) ->
        % For backwards compatibility.
        check_poll_period_secs(PollSecs0, PollSecs, !Errors)
    ;
        PollSecs = default_poll_period_secs
    ),

    (
        search_config(Config, "ui", "auto_refresh_inactive_secs",
            AutoRefreshInactiveSecs0)
    ->
        check_auto_refresh_inactive_secs(AutoRefreshInactiveSecs0,
            AutoRefreshInactiveSecs, !Errors)
    ;
        AutoRefreshInactiveSecs = default_auto_refresh_inactive_secs
    ),

    (
        search_config(Config, "ui", "wrap_width", WrapWidth0),
        string.to_int(WrapWidth0, WrapWidthInt),
        WrapWidthInt > 0
    ->
        WrapWidth = yes(WrapWidthInt)
    ;
        WrapWidth = no
    ),

    (
        search_config(Config, "ui", "thread_ordering", Ordering0),
        Ordering0 = "flat"
    ->
        Ordering = thread_ordering_flat
    ;
        Ordering = thread_ordering_threaded
    ),

    ( search_config(Config, "ui", "default_save_directory", DefaultSaveDir0) ->
        % Empty string is okay.
        DefaultSaveDir = DefaultSaveDir0
    ;
        DefaultSaveDir = ""
    ),

    ( search_config(Config, "ui", "default_max_threads", DefaultMaxThreads0) ->
        ( DefaultMaxThreads0 = "none" ->
            DefaultMaxThreads = no_max_threads
        ;
            string.to_int(DefaultMaxThreads0, DefaultMaxThreadsInt),
            DefaultMaxThreadsInt > 0
        ->
            DefaultMaxThreads = max_threads(DefaultMaxThreadsInt)
        ;
            cons("default_max_threads invalid: " ++ DefaultMaxThreads0,
                !Errors),
            DefaultMaxThreads = max_threads(300)
        )
    ;
        DefaultMaxThreads = max_threads(300)
    ),

    some [!Filters] (
        !:Filters = map.init,
        % For backwards compatibility.
        (
            search_config(Config, "command", "html_dump", HtmlDump0),
            HtmlDump0 \= ""
        ->
            parse_command(Home, HtmlDump0, HtmlDump, !Errors),
            set(text_html, HtmlDump, !Filters)
        ;
            true
        ),
        ( search(Config, "filter", FilterSection) ->
            parse_filters(Home, FilterSection, !Filters, !Errors)
        ;
            true
        ),
        Filters = !.Filters
    ),

    ( search_config(Config, "compose", "signature_file", SignatureFile0) ->
        expand_tilde_home(Home, SignatureFile0, SignatureFile),
        ( dir.path_name_is_absolute(SignatureFile) ->
            MaybeSignatureFile = yes(SignatureFile)
        ;
            cons("signature_file must specify an absolute path: " ++
                SignatureFile0, !Errors),
            MaybeSignatureFile = no
        )
    ;
        MaybeSignatureFile = no
    ),

    ( search_config(Config, "crypto", "encrypt_by_default", Encrypt0) ->
        ( to_bool(Encrypt0, Encrypt1) ->
            EncryptByDefault = Encrypt1
        ;
            cons("encrypt_by_default invalid: " ++ Encrypt0, !Errors),
            EncryptByDefault = no
        )
    ;
        EncryptByDefault = no
    ),

    ( search_config(Config, "crypto", "sign_by_default", Sign0) ->
        ( to_bool(Sign0, Sign1) ->
            SignByDefault = Sign1
        ;
            cons("sign_by_default invalid: " ++ Sign0, !Errors),
            SignByDefault = no
        )
    ;
        SignByDefault = no
    ),

    ( search_config(Config, "crypto", "decrypt_by_default", Decrypt0) ->
        ( to_bool(Decrypt0, Decrypt1) ->
            DecryptByDefault = Decrypt1
        ;
            cons("decrypt_by_default invalid: " ++ Decrypt0, !Errors),
            DecryptByDefault = no
        )
    ;
        DecryptByDefault = no
    ),

    ( search_config(Config, "crypto", "verify_by_default", Verify0) ->
        ( to_bool(Verify0, Verify1) ->
            VerifyByDefault = Verify1
        ;
            Error = "verify_by_default invalid: " ++ Verify0,
            cons(Error, !Errors),
            VerifyByDefault = no
        )
    ;
        VerifyByDefault = no
    ),

    get_notmuch_config(Notmuch, ResNotmuchConfig, !IO),
    (
        ResNotmuchConfig = ok(NotmuchConfig)
    ;
        ResNotmuchConfig = error(NotmuchConfigError),
        cons(error_message(NotmuchConfigError), !Errors),
        NotmuchConfig = empty_notmuch_config
    ),

    parse_accounts(Home, Config, Accounts0, AccountErrors),
    (
        AccountErrors = [],
        fill_default_mailbox(NotmuchConfig, Accounts0, Accounts, !Errors),
        % XXX check for duplicate from_address in accounts
        pick_default_account(Accounts, DefaultAccount, !Errors)
    ;
        AccountErrors = [_ | _],
        Accounts = [],
        DefaultAccount = no,
        append(AccountErrors, !Errors)
    ),

    get_notmuch_search_exclude_tags(NotmuchConfig, ExcludeTags),

    make_colors(Config, Colors),

    init_url_regex(URLReg),

    ProgConfig ^ notmuch = Notmuch,
    ProgConfig ^ editor = Editor,
    ProgConfig ^ open_part = OpenPart,
    ProgConfig ^ open_url = OpenUrl,
    ProgConfig ^ pipe_id = PipeId,
    ProgConfig ^ alt_html_filter = AltHtmlFilter,
    ProgConfig ^ use_alt_html_filter = UseAltHtmlFilter,
    ProgConfig ^ poll_notify = PollNotify,
    ProgConfig ^ poll_period_secs = PollSecs,
    ProgConfig ^ auto_refresh_inactive_secs = AutoRefreshInactiveSecs,
    ProgConfig ^ wrap_width = WrapWidth,
    ProgConfig ^ thread_ordering = Ordering,
    ProgConfig ^ default_save_directory = DefaultSaveDir,
    ProgConfig ^ default_max_threads = DefaultMaxThreads,
    ProgConfig ^ text_filters = Filters,
    ProgConfig ^ maybe_signature_file = MaybeSignatureFile,
    ProgConfig ^ encrypt_by_default = EncryptByDefault,
    ProgConfig ^ sign_by_default = SignByDefault,
    ProgConfig ^ decrypt_by_default = DecryptByDefault,
    ProgConfig ^ verify_by_default = VerifyByDefault,
    ProgConfig ^ accounts = Accounts,
    ProgConfig ^ default_account = DefaultAccount,
    ProgConfig ^ exclude_tags = ExcludeTags,
    ProgConfig ^ colors = Colors,
    ProgConfig ^ url_regex = URLReg.

%-----------------------------------------------------------------------------%

:- pred parse_command(home::in, string::in, command_prefix::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_command(Home, String, CommandPrefix, !Errors) :-
    do_parse_command(Home, String, Res),
    (
        Res = ok(CommandPrefix)
    ;
        Res = error(Error),
        cons(Error, !Errors),
        % dummy
        CommandPrefix = command_prefix(shell_quoted("false"), quote_once)
    ).

:- pred do_parse_command(home::in, string::in,
    maybe_error(command_prefix)::out) is cc_multi.

do_parse_command(Home, S0, Res) :-
    shell_word.tokenise(S0, ParseResult),
    (
        ParseResult = ok(Tokens0),
        expand_tilde_home_in_shell_tokens(Home, Tokens0, Tokens),
        serialise_quote_all(Tokens, QuotedCommandStr),
        QuoteTimes = ( detect_ssh(Tokens) -> quote_twice ; quote_once ),
        Res = ok(command_prefix(shell_quoted(QuotedCommandStr), QuoteTimes))
    ;
        (
            ParseResult = error(yes(Message), _Line, _Column)
        ;
            ParseResult = error(no, _Line, _Column),
            Message = "parse error"
        ),
        Error = string.format("%s in value: %s", [s(Message), s(S0)]),
        Res = error(Error)
    ).

detect_ssh([FirstWord | _]) :-
    FirstWord = word(Segments),
    list.member(unquoted(String), Segments),
    string.sub_string_search(String, "ssh", _).

%-----------------------------------------------------------------------------%

:- pred parse_multi_commands(string::in, list(string)::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_multi_commands(Str, Commands, !Errors) :-
    shell_word.tokenise(Str, ParseResult),
    (
        ParseResult = ok(Tokens0),
        list.map(break_token_at_semicolons, Tokens0, Tokens),
        reconstruct_commands(condense(Tokens), Commands)
    ;
        (
            ParseResult = error(yes(Message), _Line, _Column)
        ;
            ParseResult = error(no, _Line, _Column),
            Message = "parse error"
        ),
        Error = string.format("%s in value: %s", [s(Message), s(Str)]),
        cons(Error, !Errors),
        Commands = []
    ).

:- pred break_token_at_semicolons(shell_token::in, list(shell_token)::out)
    is det.

break_token_at_semicolons(Token0, Tokens) :-
    require_complete_switch [Token0]
    (
        ( Token0 = whitespace
        ; Token0 = word(_)
        ),
        Tokens = [Token0]
    ;
        Token0 = gmeta(Str0),
        string.to_char_list(Str0, Cs0),
        break_gmeta_at_semicolons(Cs0, Tokens)
    ).

:- pred break_gmeta_at_semicolons(list(char)::in, list(shell_token)::out)
    is det.

break_gmeta_at_semicolons([], []).
break_gmeta_at_semicolons([C | Cs], Tokens) :-
    ( if C = (';') then
        Token = gmeta_semicolon,
        TailCs = Cs
    else
        take_while(is_not(';'), Cs, TakeCs, TailCs),
        Token = gmeta(string.from_char_list([C | TakeCs]))
    ),
    break_gmeta_at_semicolons(TailCs, TailTokens),
    Tokens = [Token | TailTokens].

:- pred reconstruct_commands(list(shell_token)::in, list(string)::out) is det.

reconstruct_commands(Tokens0, Commands) :-
    (
        Tokens0 = [],
        Commands = []
    ;
        Tokens0 = [_ | _],
        take_while(is_not(gmeta_semicolon), Tokens0, CommandTokens, Tokens1),
        serialise_as_is(trim_whitespace(CommandTokens), Command),
        (
            Tokens1 = [],
            TailCommands = []
        ;
            Tokens1 = [_ | Tokens2],
            reconstruct_commands(Tokens2, TailCommands)
        ),
        Commands = [Command | TailCommands]
    ).

:- func gmeta_semicolon = shell_token.

gmeta_semicolon = gmeta(";").

:- pred is_not(T::in, T::in) is semidet.

is_not(X, Y) :-
    X \= Y.

%-----------------------------------------------------------------------------%

:- pred parse_filters(home::in, map(string, string)::in,
    map(mime_type, command_prefix)::in, map(mime_type, command_prefix)::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_filters(Home, Section, !Filters, !Errors) :-
    map.foldl2(parse_filter(Home), Section, !Filters, !Errors).

:- pred parse_filter(home::in, string::in, string::in,
    map(mime_type, command_prefix)::in, map(mime_type, command_prefix)::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_filter(Home, Name, Value, !Filters, !Errors) :-
    ( parse_mime_type(Name, MimeType, _Type, _SubType) ->
        ( Value = "" ->
            true
        ;
            do_parse_command(Home, Value, ResParse),
            (
                ResParse = ok(CommandPrefix),
                set(MimeType, CommandPrefix, !Filters)
            ;
                ResParse = error(Error),
                cons(Error, !Errors)
            )
        )
    ;
        Errors = "invalid media type to filter: " ++ Name,
        cons(Errors, !Errors)
    ).

%-----------------------------------------------------------------------------%

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

:- pred check_auto_refresh_inactive_secs(string::in, maybe(int)::out,
    list(string)::in, list(string)::out) is det.

check_auto_refresh_inactive_secs(Value, AutoRefreshSecs, !Errors) :-
    ( string.to_lower(Value, "off") ->
        AutoRefreshSecs = no
    ; string.to_int(Value, Int), Int > 0 ->
        AutoRefreshSecs = yes(Int)
    ;
        Error = "auto_refresh_inactive_secs invalid: " ++ Value,
        cons(Error, !Errors),
        AutoRefreshSecs = default_auto_refresh_inactive_secs
    ).

%-----------------------------------------------------------------------------%

:- pred parse_accounts(home::in, config::in,
    list(account(maybe(mailbox)))::out, list(string)::out) is cc_multi.

parse_accounts(Home, Config, Accounts, !:Errors) :-
    map.foldl2(parse_if_account_section(Home), Config,
        [], Accounts0, [], !:Errors),
    (
        Accounts0 = [],
        parse_compat_account(Config, Home, Account, !Errors),
        Accounts = [Account]
    ;
        Accounts0 = [_ | _],
        list.sort(Accounts0, Accounts) % sort by name
    ).

:- pred parse_if_account_section(home::in, section::in, map(string, string)::in,
    list(account(maybe(mailbox)))::in, list(account(maybe(mailbox)))::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_if_account_section(Home, SectionName, SectionMap, !Accounts, !Errors) :-
    ( is_account_section(SectionName, Name) ->
        parse_account(Home, Name, SectionMap, Account, !Errors),
        cons(Account, !Accounts)
    ;
        true
    ).

:- pred is_account_section(string::in, string::out) is semidet.

is_account_section(SectionName, Name) :-
    string.remove_prefix("account.", SectionName, Name).

:- pred parse_account(home::in, string::in, map(string, string)::in,
    account(maybe(mailbox))::out, list(string)::in, list(string)::out)
    is cc_multi.

parse_account(Home, Name, Section, Account, !Errors) :-
    ( map.search(Section, "from_address", FromString) ->
        (
            parse_address(backslash_quote_all, FromString, FromAddress),
            FromAddress = mailbox(Mailbox),
            Mailbox = mailbox(_, _)
        ->
            MaybeFrom = yes(Mailbox)
        ;
            cons("from_address invalid: " ++ FromString, !Errors),
            MaybeFrom = no
        )
    ;
        MaybeFrom = no
    ),

    ( map.search(Section, "default", Default0) ->
        ( to_bool(Default0, Bool) ->
            (
                Bool = yes,
                Default = yes
            ;
                Bool = no,
                Default = no
            )
        ;
            Error = "fallback invalid: " ++ Default0,
            cons(Error, !Errors),
            Default = unset
        )
    ;
        Default = unset
    ),

    parse_account_rest(Home, Name, Section, MaybeFrom, Default, Account,
        !Errors).

:- pred parse_compat_account(config::in, home::in,
    account(maybe(mailbox))::out, list(string)::in, list(string)::out)
    is cc_multi.

parse_compat_account(Config, Home, Account, !Errors) :-
    ( map.search(Config, "command", Section0) ->
        Section = Section0
    ;
        Section = map.init
    ),
    MaybeFrom = no,
    Default = unset,
    parse_account_rest(Home, "default", Section, MaybeFrom, Default, Account,
        !Errors).

:- pred parse_account_rest(home::in, string::in, map(string, string)::in,
    maybe(mailbox)::in, default_setting::in, account(maybe(mailbox))::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_account_rest(Home, Name, Section, MaybeFrom, Default, Account,
        !Errors) :-
    ( map.search(Section, "sendmail", Sendmail0) ->
        parse_command(Home, Sendmail0, Sendmail, !Errors),
        check_sendmail_command(Sendmail, !Errors)
    ;
        Sendmail = default_sendmail_command
    ),

    ( map.search(Section, "post_sendmail", PostSendmail0) ->
        ( PostSendmail0 = "" ->
            PostSendmail = nothing
        ;
            parse_command(Home, PostSendmail0, PostSendmail1, !Errors),
            PostSendmail = command(PostSendmail1)
        )
    ;
        PostSendmail = default
    ),

    ( map.search(Section, "autocrypt_keydata", Keydata), Keydata \= "" ->
        validate_autocrypt_keydata(Keydata, !Errors),
        MaybeKeydata = yes(Keydata)
    ;
        MaybeKeydata = no
    ),

    Account = account(Name, MaybeFrom, Default, Sendmail, PostSendmail,
        MaybeKeydata).

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

:- pred validate_autocrypt_keydata(string::in,
    list(string)::in, list(string)::out) is det.

validate_autocrypt_keydata(Keydata, !Errors) :-
    ( all_match(is_base64_char, rstrip_pred(is_equals_sign, Keydata)) ->
        true
    ;
        Error = "invalid value for autocrypt_keydata: " ++ Keydata,
        cons(Error, !Errors)
    ).
    % XXX Instead of just checking if the characters are allowed to occur
    % in base64, it would be even better to use the base64 module to check
    % that Keydata can actually be decoded.

:- pred is_equals_sign(char::in) is semidet.

is_equals_sign('=').

:- pred fill_default_mailbox(notmuch_config::in,
    list(account(maybe(mailbox)))::in, list(account)::out,
    list(string)::in, list(string)::out) is det.

fill_default_mailbox(Config, Accounts0, Accounts, !Errors) :-
    (
        member(SomeAccount, Accounts0),
        SomeAccount ^ from_address = no
    ->
        get_notmuch_from_address(Config, MaybeDefault),
        (
            MaybeDefault = yes(Default)
        ;
            MaybeDefault = no,
            cons("could not derive default from_address from .notmuch-config",
                !Errors),
            Default = bad_mailbox("") % dummy
        )
    ;
        Default = bad_mailbox("") % dummy
    ),
    map(set_default_from_address(Default), Accounts0, Accounts).

:- pred get_notmuch_from_address(notmuch_config::in, maybe(mailbox)::out)
    is det.

get_notmuch_from_address(Config, Res) :-
    ( search(Config, "user", "name", Name0) ->
        Name = Name0
    ;
        Name = ""
    ),
    ( search(Config, "user", "primary_email", Email0) ->
        Email = Email0
    ;
        Email = ""
    ),
    String = string.append_list([Name, " <", Email, ">"]),
    (
        parse_address(backslash_quote_all, String, Address),
        Address = mailbox(Mailbox),
        Mailbox = mailbox(_, _)
    ->
        Res = yes(Mailbox)
    ;
        Res = no
    ).

:- pred set_default_from_address(mailbox::in, account(maybe(mailbox))::in,
    account::out) is det.

set_default_from_address(DefaultFrom, Account0, Account) :-
    Account0 = account(Name, MaybeFrom, Default, Sendmail, PostSendmail,
        Keydata),
    (
        MaybeFrom = yes(From)
    ;
        MaybeFrom = no,
        From = DefaultFrom
    ),
    Account = account(Name, From, Default, Sendmail, PostSendmail, Keydata).

:- pred pick_default_account(list(account)::in, maybe(account)::out,
    list(string)::in, list(string)::out) is det.

pick_default_account(Accounts, DefaultAccount, !Errors) :-
    list.filter(is_default_account, Accounts, DefaultAccounts),
    (
        DefaultAccounts = [],
        (
            Accounts = [Account],
            Account ^ default = unset
        ->
            DefaultAccount = yes(Account)
        ;
            DefaultAccount = no
        )
    ;
        DefaultAccounts = [Account],
        DefaultAccount = yes(Account)
    ;
        DefaultAccounts = [_, _ | _],
        DefaultAccount = no,
        cons("multiple fallback accounts defined", !Errors)
    ).

:- pred is_default_account(account::in) is semidet.

is_default_account(Account) :-
    Account ^ default = yes.

%-----------------------------------------------------------------------------%

:- pred get_notmuch_search_exclude_tags(notmuch_config::in, set(tag)::out)
    is det.

get_notmuch_search_exclude_tags(Config, ExcludeTags) :-
    ( search(Config, "search", "exclude_tags", Value) ->
        Words = string.words_separator(unify(';'), Value),
        Tags = list.map(func(Name) = tag(Name), Words),
        ExcludeTags = set.from_list(Tags)
    ;
        ExcludeTags = set.init
    ).

%-----------------------------------------------------------------------------%

:- pred to_bool(string::in, bool::out) is semidet.

to_bool(String, Bool) :-
    string.to_lower(String, Lower),
    (
        ( Lower = "true"
        ; Lower = "yes"
        ; Lower = "1"
        ),
        Bool = yes
    ;
        ( Lower = "false"
        ; Lower = "no"
        ; Lower = "0"
        ),
        Bool = no
    ).

%-----------------------------------------------------------------------------%

get_notmuch_command(Config, Notmuch) :-
    Notmuch = Config ^ notmuch.

get_editor_command(Config, Editor) :-
    Editor = Config ^ editor.

get_open_part_command(Config, Command) :-
    Command = Config ^ open_part.

get_open_url_command(Config, Command) :-
    Command = Config ^ open_url.

get_pipe_id_command(Config, Command) :-
    Command = Config ^ pipe_id.

get_alt_html_filter_command(Config, Command) :-
    Command = Config ^ alt_html_filter.

get_use_alt_html_filter(Config, UseAltHtmlFilter) :-
    UseAltHtmlFilter = Config ^ use_alt_html_filter.

get_poll_notify_command(Config, Command) :-
    Command = Config ^ poll_notify.

get_poll_period_secs(Config, PollPeriodSecs) :-
    PollPeriodSecs = Config ^ poll_period_secs.

get_auto_refresh_inactive_secs(Config, AutoRefreshInactiveSecs) :-
    AutoRefreshInactiveSecs = Config ^ auto_refresh_inactive_secs.

get_wrap_width(Config, Cols, WrapWidth) :-
    MaybeWrapWidth = Config ^ wrap_width,
    (
        MaybeWrapWidth = yes(WrapWidth0),
        WrapWidth = min(WrapWidth0, Cols)
    ;
        MaybeWrapWidth = no,
        WrapWidth = Cols
    ).

get_thread_ordering(Config, Ordering) :-
    Ordering = Config ^ thread_ordering.

get_default_save_directory(Config, DefaultSaveDir) :-
    DefaultSaveDir = Config ^ default_save_directory.

get_default_max_threads(Config, DefaultMaxThreads) :-
    DefaultMaxThreads = Config ^ default_max_threads.

get_text_filter_command(Config, MimeType, Command) :-
    Filters = Config ^ text_filters,
    search(Filters, MimeType, Command).

get_maybe_signature_file(Config, MaybeSignatureFile) :-
    MaybeSignatureFile = Config ^ maybe_signature_file.

get_encrypt_by_default(Config, EncryptByDefault) :-
    EncryptByDefault = Config ^ encrypt_by_default.

get_sign_by_default(Config, SignByDefault) :-
    SignByDefault = Config ^ sign_by_default.

get_decrypt_by_default(Config, DecryptByDefault) :-
    DecryptByDefault = Config ^ decrypt_by_default.

get_verify_by_default(Config, VerifyByDefault) :-
    VerifyByDefault = Config ^ verify_by_default.

%-----------------------------------------------------------------------------%

get_all_accounts(Config, Config ^ accounts).

get_default_account(Config, Config ^ default_account).

get_some_matching_account(Config, Addresses, MaybeAccount) :-
    (
        Addresses = [],
        MaybeAccount = Config ^ default_account
    ;
        Addresses = [Addr | Addrs],
        ( get_matching_account(Config, Addr, Account) ->
            MaybeAccount = yes(Account)
        ;
            get_some_matching_account(Config, Addrs, MaybeAccount)
        )
    ).

get_matching_account(Config, Address, Account) :-
    Accounts = Config ^ accounts,
    find_first_match(match_account(Address), Accounts, Account).

:- pred match_account(address::in, account::in) is semidet.

match_account(Address, Account) :-
    require_complete_switch [Address]
    (
        Address = mailbox(MailboxA)
    ;
        Address = group(_, Mailboxes),
        member(MailboxA, Mailboxes)
    ),
    Account ^ from_address = MailboxB,
    MailboxA = mailbox(_, AddrSpecA),
    MailboxB = mailbox(_, AddrSpecB),
    AddrSpecA = AddrSpecB.

get_account_name(Account, Account ^ account_name).

get_from_address(Account, Account ^ from_address).

get_from_address_as_string(Account, String) :-
    mailbox_to_string(no_encoding, Account ^ from_address, String, _Ok).

get_sendmail_command(Account, Command) :-
    Command = Account ^ sendmail.

get_post_sendmail_action(Account, Action) :-
    Action = Account ^ post_sendmail.

get_autocrypt_keydata(Account, Account ^ autocrypt_keydata).

%-----------------------------------------------------------------------------%

get_exclude_tags(Config, ExcludeTags) :-
    ExcludeTags = Config ^ exclude_tags.

%-----------------------------------------------------------------------------%

generic_attrs(Config) = Config ^ colors ^ generic_attrs.
status_attrs(Config) = Config ^ colors ^ status_attrs.
pager_attrs(Config) = Config ^ colors ^ pager_attrs.
index_attrs(Config) = Config ^ colors ^ index_attrs.
thread_attrs(Config) = Config ^ colors ^ thread_attrs.
compose_attrs(Config) = Config ^ colors ^ compose_attrs.

%-----------------------------------------------------------------------------%

get_url_regex(Config, URLReg) :-
    URLReg = Config ^ url_regex.

%-----------------------------------------------------------------------------%

:- func config_filename = string.

config_filename = "bower/bower.conf".

:- func default_notmuch_command = command_prefix.

default_notmuch_command =
    command_prefix(shell_quoted("notmuch"), quote_once).

:- func default_editor_command = command_prefix.

default_editor_command =
    command_prefix(shell_quoted("vi"), quote_once).

:- func default_open_part_command = string.

default_open_part_command = "xdg-open&".

:- func default_open_url_command = string.

default_open_url_command = "xdg-open&".

:- func default_pipe_id_command = string.

default_pipe_id_command = "xclip".

:- func default_alt_html_filter_command = command_prefix.

default_alt_html_filter_command =
    command_prefix(shell_quoted("pandoc -f markdown -t html"), quote_once).

:- func default_poll_period_secs = maybe(int).

default_poll_period_secs = yes(60).

:- func default_auto_refresh_inactive_secs = maybe(int).

default_auto_refresh_inactive_secs = no.

:- func default_sendmail_command = command_prefix.

default_sendmail_command =
    command_prefix(shell_quoted("/usr/bin/sendmail -oi -oem"), quote_once).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
