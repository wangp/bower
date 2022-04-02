% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module path_expand.
:- interface.

:- import_module io.

:- type home
    --->    home(string)
    ;       no_home.

:- pred get_home_dir(home::out, io::di, io::uo) is det.

:- pred expand_tilde_home(home::in, string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

get_home_dir(MaybeHome, !IO) :-
    io.get_environment_var("HOME", MaybeEnvValue, !IO),
    (
        MaybeEnvValue = yes(EnvValue),
        % Sanity check.
        string.prefix(EnvValue, "/")
    ->
        MaybeHome = home(EnvValue)
    ;
        MaybeHome = no_home
    ).

expand_tilde_home(MaybeHome, String0, String) :-
    (
        MaybeHome = home(HomeDir),
        % We don't support ~USERNAME prefix.
        ( string.remove_prefix("~/", String0, String1) ->
            String = HomeDir ++ "/" ++ String1
        ; String0 = "~" ->
            String = HomeDir
        ;
            String = String0
        )
    ;
        MaybeHome = no_home,
        String = String0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
