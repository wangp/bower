% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module xdg.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred search_config_file(string::in, maybe(string)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

search_config_file(FileName, Res, !IO) :-
    get_config_home(MaybeConfigHome, !IO),
    (
        MaybeConfigHome = yes(ConfigHome),
        search_config_file_loop([ConfigHome], FileName, Res0, !IO)
    ;
        MaybeConfigHome = no,
        Res0 = no
    ),
    (
        Res0 = yes(_),
        Res = Res0
    ;
        Res0 = no,
        get_config_dirs(ConfigDirs, !IO),
        search_config_file_loop(ConfigDirs, FileName, Res, !IO)
    ).

:- pred search_config_file_loop(list(string)::in, string::in,
    maybe(string)::out, io::di, io::uo) is det.

search_config_file_loop([], _, no, !IO).
search_config_file_loop([Dir | Dirs], FileName, Res, !IO) :-
    io.check_file_accessibility(Dir / FileName, [read], Res0, !IO),
    (
        Res0 = ok,
        Res = yes(Dir / FileName)
    ;
        Res0 = error(_),
        search_config_file_loop(Dirs, FileName, Res, !IO)
    ).

:- pred get_config_home(maybe(string)::out, io::di, io::uo) is det.

get_config_home(ConfigHome, !IO) :-
    get_environment_var("XDG_CONFIG_HOME", MaybeEnv, !IO),
    (
        MaybeEnv = yes(EnvValue),
        EnvValue \= ""
    ->
        ConfigHome = yes(EnvValue)
    ;
        get_environment_var("HOME", MaybeHome, !IO),
        (
            MaybeHome = yes(Home),
            ConfigHome = yes(Home / ".config")
        ;
            MaybeHome = no,
            % XXX could try getpwuid?
            ConfigHome = no
        )
    ).

:- pred get_config_dirs(list(string)::out, io::di, io::uo) is det.

get_config_dirs(ConfigDirs, !IO) :-
    get_environment_var("XDG_CONFIG_DIRS", MaybeEnv, !IO),
    (
        MaybeEnv = yes(EnvValue),
        EnvValue \= ""
    ->
        ConfigDirs = string.split_at_char(':', EnvValue)
    ;
        ConfigDirs = ["/etc/xdg"]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
