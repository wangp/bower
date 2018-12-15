% Bower - a frontend for the Notmuch email system
% Copyright (C) 2018 Peter Wang

:- module notmuch_config.
:- interface.

:- import_module io.

:- import_module prog_config.

:- type notmuch_config.

:- func empty_notmuch_config = notmuch_config.

:- pred get_notmuch_config(prog_config::in, io.res(notmuch_config)::out,
    io::di, io::uo) is det.

:- pred contains(notmuch_config::in, string::in, string::in) is semidet.

:- pred search(notmuch_config::in, string::in, string::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module call_system.
:- import_module quote_arg.

:- type notmuch_config == list(string).     % section.item=value

%-----------------------------------------------------------------------------%

empty_notmuch_config = [].

%-----------------------------------------------------------------------------%

get_notmuch_config(Config, Res, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, ["config", "list"],
        redirect_input("/dev/null"), no_redirect, Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
    (
        CallRes = ok(ItemsString),
        % The empty string following the final newline is not an item.
        ItemsList = string.words_separator(unify('\n'), ItemsString),
        Res = ok(ItemsList)
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

%---------------------------------------------------------------------------%

contains(Config, Section, ItemName) :-
    Prefix = Section ++ "." ++ ItemName ++ "=",
    contains_loop(Config, Prefix).

:- pred contains_loop(notmuch_config::in, string::in) is semidet.

contains_loop([Item | Items], Prefix) :-
    ( if string.prefix(Item, Prefix) then
        true
    else
        contains_loop(Items, Prefix)
    ).

%---------------------------------------------------------------------------%

search(Config, Section, ItemName, Value) :-
    Prefix = Section ++ "." ++ ItemName ++ "=",
    search_loop(Config, Prefix, Value).

:- pred search_loop(notmuch_config::in, string::in, string::out) is semidet.

search_loop([Item | Items], Prefix, Value) :-
    ( if string.remove_prefix(Prefix, Item, Suffix) then
        Value = Suffix
    else
        search_loop(Items, Prefix, Value)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
