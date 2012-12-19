% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module addressbook.
:- interface.

:- import_module io.

:- import_module screen.

%-----------------------------------------------------------------------------%

:- func addressbook_section = string.

:- pred expand_aliases(string::in, string::out, io::di, io::uo) is det.

:- pred prompt_addressbook_add(screen::in, string::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module callout.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module text_entry.

%-----------------------------------------------------------------------------%

addressbook_section = "bower:addressbook".

%-----------------------------------------------------------------------------%

:- pred is_alias_char(char::in) is semidet.

is_alias_char(C) :- char.is_alnum_or_underscore(C).
is_alias_char('-').
is_alias_char('+').
is_alias_char('.').

%-----------------------------------------------------------------------------%

expand_aliases(String0, String, !IO) :-
    Words0 = string.split_at_string(", ", String0),
    list.map_foldl(expand_aliases_2, Words0, Words1, !IO),
    ( Words0 = Words1 ->
        String = String0
    ;
        String = string.join_list(", ", Words1)
    ).

:- pred expand_aliases_2(string::in, string::out, io::di, io::uo) is det.

expand_aliases_2(Word0, Expansion, !IO) :-
    Word1 = string.strip(Word0),
    ( string.all_match(is_alias_char, Word1) ->
        Key = addressbook_section ++ "." ++ Word1,
        get_notmuch_config(Key, MaybeValue, !IO),
        (
            MaybeValue = ok(Expansion)
        ;
            MaybeValue = error(_),
            Expansion = Word0
        )
    ;
        Expansion = Word0
    ).

%-----------------------------------------------------------------------------%

prompt_addressbook_add(Screen, Address0, !IO) :-
    History0 = init_history,
    text_entry_initial(Screen, "Address: ", History0, Address0,
        complete_none, ReturnAddress, !IO),
    (
        ReturnAddress = yes(Address),
        ( Address = "" ->
            true
        ;
            prompt_addressbook_add_2(Screen, Address, !IO)
        )
    ;
        ReturnAddress = no
    ).

:- pred prompt_addressbook_add_2(screen::in, string::in,
    io::di, io::uo) is det.

prompt_addressbook_add_2(Screen, Address, !IO) :-
    History0 = init_history,
    text_entry_initial(Screen, "Alias as: ", History0, suggest_alias(Address),
        complete_config_key(addressbook_section), ReturnAlias, !IO),
    (
        ReturnAlias = yes(Alias),
        ( Alias = "" ->
            true
        ; string.all_match(is_alias_char, Alias) ->
            do_addressbook_add(Alias, Address, Res, !IO),
            (
                Res = ok,
                update_message_immed(Screen, set_info("Alias added."), !IO)
            ;
                Res = error(Error),
                update_message_immed(Screen, set_warning(Error), !IO)
            )
        ;
            update_message_immed(Screen, set_warning("Invalid alias."), !IO)
        )
    ;
        ReturnAlias = no
    ).

:- func suggest_alias(string) = string.

suggest_alias(Address) = Alias :-
    ( string.sub_string_search(Address, "<", Index) ->
        string.between(Address, Index + 1, length(Address), SubString),
        string.to_char_list(SubString, Chars0)
    ;
        string.to_char_list(Address, Chars0)
    ),
    list.takewhile(is_alias_char, Chars0, Chars, _),
    string.from_char_list(Chars, Alias).

:- pred do_addressbook_add(string::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

do_addressbook_add(Alias, Address, Res, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    Key = addressbook_section ++ "." ++ Alias,
    args_to_quoted_command(["config", "set", Key, Address], Command),
    io.call_system(Notmuch ++ Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("notmuch returned exit status %d",
                [i(ExitStatus)], Warning),
            Res = error(Warning)
        )
    ;
        CallRes = error(Error),
        string.append_list(["Error running ", Notmuch, ": ",
            io.error_message(Error)], Warning),
        Res = error(Warning)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
