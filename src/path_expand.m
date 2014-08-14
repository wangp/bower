% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module path_expand.
:- interface.

:- import_module io.

:- type home
    --->    home(string).

:- pred get_home_dir(home::out, io::di, io::uo) is det.

:- pred expand_tilde_home(home::in, string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

get_home_dir(home(Home), !IO) :-
    io.get_environment_var("HOME", MaybeHome, !IO),
    (
        MaybeHome = yes(Home)
    ;
        MaybeHome = no,
        % Not really, but not reachable in practice.
        Home = "~"
    ).

expand_tilde_home(home(HomeDir), String0, String) :-
    (
        string.remove_prefix("~", String0, String1),
        (
            string.unsafe_index_next(String1, 0, _, NextChar)
        =>
            NextChar = ('/')
        )
    ->
        String = HomeDir ++ "/" ++ String1
    ;
        String = String0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
