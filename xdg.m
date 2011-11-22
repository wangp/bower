%-----------------------------------------------------------------------------%

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

search_config_file(FileName, MaybeFound, !IO) :-
    get_config_home(ConfigHome, !IO),
    get_config_dirs(SearchDirs0, !IO),
    SearchDirs = [ConfigHome | SearchDirs0],
    search_config_file_2(SearchDirs, FileName, MaybeFound, !IO).

:- pred search_config_file_2(list(string)::in, string::in, maybe(string)::out,
    io::di, io::uo) is det.

search_config_file_2([], _, no, !IO).
search_config_file_2([Dir | Dirs], FileName, MaybeFound, !IO) :-
    io.check_file_accessibility(Dir / FileName, [read], Res, !IO),
    (
        Res = ok,
        MaybeFound = yes(Dir / FileName)
    ;
        Res = error(_),
        search_config_file_2(Dirs, FileName, MaybeFound, !IO)
    ).

:- pred get_config_home(string::out, io::di, io::uo) is det.

get_config_home(ConfigHome, !IO) :-
    get_environment_var("XDG_CONFIG_HOME", MaybeEnv, !IO),
    (
        MaybeEnv = yes(Env),
        Env \= ""
    ->
        ConfigHome = Env
    ;
        get_environment_var("HOME", MaybeHome, !IO),
        (
            MaybeHome = yes(Home),
            ConfigHome = Home / ".config"
        ;
            MaybeHome = no,
            % XXX what can you do?
            ConfigHome = ""
        )
    ).

:- pred get_config_dirs(list(string)::out, io::di, io::uo) is det.

get_config_dirs(ConfigDirs, !IO) :-
    get_environment_var("XDG_CONFIG_DIRS", MaybeEnv, !IO),
    (
        MaybeEnv = yes(Env),
        Env \= ""
    ->
        ConfigDirs = string.split_at_char(':', Env)
    ;
        ConfigDirs = ["/etc/xdg"]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
