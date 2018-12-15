% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module addressbook.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module notmuch_config.
:- import_module prog_config.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- func addressbook_section = string.

:- pred search_addressbook(notmuch_config::in, string::in, string::out)
    is semidet.

:- pred search_notmuch_address(prog_config::in, string::in, list(string)::out,
    io::di, io::uo) is det.

:- pred search_notmuch_address_top(prog_config::in, string::in,
    maybe(string)::out, io::di, io::uo) is det.

:- pred prompt_addressbook_add(prog_config::in, screen::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module string.

:- import_module callout.
:- import_module list_util.
:- import_module quote_arg.
:- import_module text_entry.    % XXX cyclic

%-----------------------------------------------------------------------------%

addressbook_section = "bower:addressbook".

%-----------------------------------------------------------------------------%

:- pred is_alias_char(char::in) is semidet.

is_alias_char(C) :-
    ( char.is_alnum_or_underscore(C)
    ; C = ('-')
    ; C = ('+')
    ; C = ('.')
    ;
        % Allow all non-ASCII.  I suppose we should check for Unicode
        % whitespace but it should not matter.
        char.to_int(C, Int),
        Int > 0x7f
    ).

%-----------------------------------------------------------------------------%

search_addressbook(NotmuchConfig, Alias, Expansion) :-
    string.all_match(is_alias_char, Alias),
    search(NotmuchConfig, addressbook_section, Alias, Expansion).

%-----------------------------------------------------------------------------%

search_notmuch_address(Config, SearchString, NameAddrs, !IO) :-
    ( string.prefix(SearchString, "/") ->
        % Avoid notmuch-address interpreting the string as an incomplete regex.
        NameAddrs = []
    ;
        run_notmuch(Config,
            [
                "address", "--format=json", "--output=sender", "--output=count",
                "--deduplicate=address", "date:1y..now",
                "from:" ++ SearchString
            ],
            no_suspend_curses,
            parse_address_count_list, Res, !IO),
        (
            Res = ok(Pairs0),
            sort(descending, Pairs0, Pairs),
            map(snd, Pairs, NameAddrs)
        ;
            Res = error(_),
            NameAddrs = []
        )
    ).

search_notmuch_address_top(Config, SearchString, MaybeFound, !IO) :-
    search_notmuch_address(Config, SearchString, NameAddrs, !IO),
    (
        NameAddrs = [Top | _],
        MaybeFound = yes(Top)
    ;
        NameAddrs = [],
        MaybeFound = no
    ).

:- pred descending(T::in, T::in, comparison_result::out) is det.

descending(A, B, R) :-
    compare(R, B, A).

%-----------------------------------------------------------------------------%

prompt_addressbook_add(Config, Screen, Address0, !IO) :-
    History0 = init_history,
    text_entry_initial(Screen, "Address: ", History0, Address0, complete_none,
        ReturnAddress, !IO),
    (
        ReturnAddress = yes(Address),
        ( Address = "" ->
            true
        ;
            prompt_addressbook_add_2(Config, Screen, Address, !IO)
        )
    ;
        ReturnAddress = no
    ).

:- pred prompt_addressbook_add_2(prog_config::in, screen::in, string::in,
    io::di, io::uo) is det.

prompt_addressbook_add_2(Config, Screen, Address, !IO) :-
    History0 = init_history,
    text_entry_initial(Screen, "Alias as: ", History0, suggest_alias(Address),
        complete_config_key(Config, addressbook_section), ReturnAlias, !IO),
    (
        ReturnAlias = yes(Alias),
        ( Alias = "" ->
            true
        ; string.all_match(is_alias_char, Alias) ->
            do_addressbook_add(Config, Alias, Address, Res, !IO),
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
    list_util.take_while(is_alias_char, Chars0, Chars, _),
    string.from_char_list(Chars, Alias).

:- pred do_addressbook_add(prog_config::in, string::in, string::in,
    maybe_error::out, io::di, io::uo) is det.

do_addressbook_add(Config, Alias, Address, Res, !IO) :-
    get_notmuch_command(Config, Notmuch),
    Key = addressbook_section ++ "." ++ Alias,
    make_quoted_command(Notmuch, ["config", "set", Key, Address],
        redirect_input("/dev/null"), redirect_output("/dev/null"), Command),
    io.call_system(Command, CallRes, !IO),
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
        Notmuch = command_prefix(shell_quoted(NotmuchString), _),
        string.append_list(["Error running ", NotmuchString, ": ",
            io.error_message(Error)], Warning),
        Res = error(Warning)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
