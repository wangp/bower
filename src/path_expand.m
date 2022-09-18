% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module path_expand.
:- interface.

:- import_module io.
:- import_module list.

:- import_module shell_word.

:- type home
    --->    home(string)
    ;       no_home.

:- pred get_home_dir(home::out, io::di, io::uo) is det.

:- pred expand_tilde_home(home::in, string::in, string::out) is det.

:- pred expand_tilde_home_in_shell_tokens(home::in,
    list(shell_token)::in, list(shell_token)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
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

%-----------------------------------------------------------------------------%

expand_tilde_home(MaybeHome, String0, String) :-
    FollowedByWordBoundary = yes, % assume nothing follows String0
    expand_tilde_home_follow(MaybeHome, String0, FollowedByWordBoundary,
        String).

:- pred expand_tilde_home_follow(home::in, string::in, bool::in, string::out)
    is det.

expand_tilde_home_follow(MaybeHome, String0, FollowedByWordBoundary, String) :-
    (
        MaybeHome = home(HomeDir),
        % We don't support ~USERNAME prefix.
        ( string.remove_prefix("~/", String0, String1) ->
            String = HomeDir ++ "/" ++ String1
        ; String0 = "~", FollowedByWordBoundary = yes ->
            String = HomeDir
        ;
            String = String0
        )
    ;
        MaybeHome = no_home,
        String = String0
    ).

%-----------------------------------------------------------------------------%

expand_tilde_home_in_shell_tokens(Home, Tokens0, Tokens) :-
    list.map(expand_tilde_home_in_shell_token(Home), Tokens0, Tokens).

:- pred expand_tilde_home_in_shell_token(home::in,
    shell_token::in, shell_token::out) is det.

expand_tilde_home_in_shell_token(Home, Token0, Token) :-
    (
        ( Token0 = whitespace
        ; Token0 = gmeta(_)
        ),
        Token = Token0
    ;
        Token0 = word(Segments0),
        expand_tilde_home_in_shell_word(Home, Segments0, Segments),
        Token = word(Segments)
    ).

:- pred expand_tilde_home_in_shell_word(home::in,
    list(shell_word_segment)::in, list(shell_word_segment)::out) is det.

expand_tilde_home_in_shell_word(Home, Segments0, Segments) :-
    (
        Segments0 = [unquoted(Str0) | Tail]
    ->
        % ~ should expand
        % ~/x should expand
        % ~"x" should not expand
        (
            Tail = [],
            FollowedByWordBoundary = yes
        ;
            Tail = [_ | _],
            FollowedByWordBoundary = no
        ),
        expand_tilde_home_follow(Home, Str0, FollowedByWordBoundary, Str),
        Segments = [unquoted(Str) | Tail]
    ;
        Segments = Segments0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
