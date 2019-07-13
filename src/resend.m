% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module resend.
:- interface.

:- import_module io.

:- import_module data.
:- import_module prog_config.
:- import_module screen.
:- import_module text_entry.

%-----------------------------------------------------------------------------%

:- pred handle_resend(prog_config::in, screen::in, message_id::in,
    message_update::out, history::in, history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module compose.
:- import_module make_temp.
:- import_module quote_arg.
:- import_module rfc5322.
:- import_module rfc5322.parser.
:- import_module send_util.

:- use_module curs.

:- type sent
    --->    sent
    ;       not_sent.

%-----------------------------------------------------------------------------%

handle_resend(Config, Screen, MessageId, Message, !ToHistory, !IO) :-
    prompt_from_account(Config, Screen, FromRes, !IO),
    (
        FromRes = ok(From, FromAddresses, Account),
        prompt_to(Config, Screen, ToRes, !ToHistory, !IO),
        (
            ToRes = ok(To, ToAddresses),
            confirm_resend(Screen, From, To, Confirmation, !IO),
            (
                Confirmation = yes,
                create_temp_message_file_and_resend(Config, Screen, MessageId,
                    Account, FromAddresses, ToAddresses, Message, !IO)
            ;
                Confirmation = no,
                Message = set_info("Message not resent.")
            )
        ;
            ToRes = invalid_address,
            Message = set_warning("Invalid To address, message not resent.")
        ;
            ToRes = cancel,
            Message = set_info("Message not resent.")
        )
    ;
        FromRes = no_account,
        Message = set_warning("No matching account, message not resent.")
    ;
        FromRes = invalid_address,
        Message = set_warning("Invalid From address, message not resent.")
    ;
        FromRes = cancel,
        Message = set_info("Message not resent.")
    ).

%-----------------------------------------------------------------------------%

:- type prompt_from_result
    --->    ok(string, address_list, account)
    ;       no_account
    ;       invalid_address
    ;       cancel.

:- pred prompt_from_account(prog_config::in, screen::in,
    prompt_from_result::out, io::di, io::uo) is det.

prompt_from_account(Config, Screen, Res, !IO) :-
    make_from_history(Config, History0, Initial),
    text_entry_initial(Screen, "From: ", History0, Initial,
        complete_address(Config), MaybeFrom, !IO),
    (
        MaybeFrom = yes(From0),
        From0 \= ""
    ->
        parse_and_expand_addresses_string(Config, backslash_quote_meta_chars,
            From0, From, FromAddresses, FromValid, !IO),
        (
            FromValid = yes,
            get_some_matching_account(Config, FromAddresses, MaybeAccount),
            (
                MaybeAccount = yes(Account),
                Res = ok(From, FromAddresses, Account)
            ;
                MaybeAccount = no,
                Res = no_account
            )
        ;
            FromValid = no,
            Res = invalid_address
        )
    ;
        Res = cancel
    ).

:- pred make_from_history(prog_config::in, history::out, string::out) is det.

make_from_history(Config, History, Initial) :-
    get_all_accounts(Config, Accounts),
    get_default_account(Config, MaybeDefault),
    (
        MaybeDefault = yes(DefaultAccount),
        get_from_address_as_string(DefaultAccount, Initial)
    ;
        MaybeDefault = no,
        (
            Accounts = [],
            Initial = ""
        ;
            Accounts = [Account | _],
            get_from_address_as_string(Account, Initial)
        )
    ),
    map(get_from_address_as_string, Accounts, Strings0),
    ( delete_first(Strings0, Initial, Strings) ->
        History = init_history_list(Strings)
    ;
        History = init_history_list(Strings0)
    ).

%-----------------------------------------------------------------------------%

:- type prompt_to_result
    --->    ok(string, address_list)
    ;       invalid_address
    ;       cancel.

:- pred prompt_to(prog_config::in, screen::in, prompt_to_result::out,
    history::in, history::out, io::di, io::uo) is det.

prompt_to(Config, Screen, Res, !ToHistory, !IO) :-
    text_entry_initial(Screen, "Resend message to: ", !.ToHistory, "",
        complete_address(Config), MaybeTo, !IO),
    (
        MaybeTo = yes(To0),
        To0 \= ""
    ->
        add_history_nodup(To0, !ToHistory),
        parse_and_expand_addresses_string(Config, backslash_quote_meta_chars,
            To0, To, ToAddresses, ToValid, !IO),
        (
            ToValid = yes,
            Res = ok(To, ToAddresses)
        ;
            ToValid = no,
            Res = invalid_address
        )
    ;
        Res = cancel
    ).

%-----------------------------------------------------------------------------%

:- pred confirm_resend(screen::in, string::in, string::in, bool::out,
    io::di, io::uo) is det.

confirm_resend(Screen, From, To, Confirmation, !IO) :-
    Prompt = "Resend from " ++ From ++ " to " ++ To ++ "? (Y/n) ",
    update_message_immed(Screen, set_prompt(Prompt), !IO),
    get_keycode_blocking(Code, !IO),
    ( Code = char('Y') ->
        Confirmation = yes
    ;
        Confirmation = no
    ).

%-----------------------------------------------------------------------------%

:- pred create_temp_message_file_and_resend(prog_config::in, screen::in,
    message_id::in, account::in, address_list::in, address_list::in,
    message_update::out, io::di, io::uo) is det.

create_temp_message_file_and_resend(Config, Screen, MessageId, Account,
        FromAddress, ToAddresses, MessageUpdate, !IO) :-
    write_resent_headers(Config, FromAddress, ToAddresses, ResWrite, !IO),
    (
        ResWrite = ok(FileName),
        get_notmuch_command(Config, Notmuch),
        make_quoted_command(Notmuch, [
            "show", "--format=raw", "--",
            message_id_to_search_term(MessageId)
        ], no_redirect, redirect_append(FileName), Command),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                resend_with_progress(Screen, Account, FileName, MessageUpdate,
                    !IO)
            ;
                string.format("notmuch returned exit status %d",
                    [i(ExitStatus)], Msg),
                MessageUpdate = set_warning(Msg)
            )
        ;
            CallRes = error(Error),
            string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
            MessageUpdate = set_warning(Warning)
        ),
        io.remove_file(FileName, _, !IO)
    ;
        ResWrite = error(Error),
        MessageUpdate = set_warning(Error)
    ).

:- pred write_resent_headers(prog_config::in, address_list::in,
    address_list::in, maybe_error(string)::out, io::di, io::uo) is det.

write_resent_headers(Config, FromAddress, ToAddresses, Res, !IO) :-
    make_temp_suffix("", ResTemp, !IO),
    (
        ResTemp = ok(FileName),
        io.open_output(FileName, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            promise_equivalent_solutions [Res, !:IO]
            (
              try [io(!IO)] (
                generate_resent_headers(Config, Stream, FromAddress, ToAddresses,
                    !IO),
                io.close_output(Stream, !IO)
              )
              then
                Res = ok(FileName)
              catch_any Excp ->
                io.remove_file(FileName, _, !IO),
                Res = error("exception occurred: " ++ string(Excp))
            )
        ;
            ResOpen = error(Error),
            Res = error("error opening " ++ FileName ++ ": " ++
                io.error_message(Error))
        )
    ;
        ResTemp = error(Error),
        Res = error("error opening temporary file: " ++ Error)
    ).

:- pred generate_resent_headers(prog_config::in, io.output_stream::in,
    address_list::in, address_list::in, io::di, io::uo) is det.

generate_resent_headers(Config, Stream, FromAddress, ToAddresses, !IO) :-
    get_message_id_right_part(Config, RightPart),
    generate_date_msg_id(RightPart, Date, ResentMessageId, !IO),

    write_address_list_header(rfc2047_encoding, Stream,
        "Resent-From", FromAddress, ok, _FromError, !IO),
    write_as_unstructured_header(no_encoding, Stream,
        "Resent-Date", Date, !IO),
    write_as_unstructured_header(no_encoding, Stream,
        "Resent-Message-ID", ResentMessageId, !IO),
    % The To addresses were checked before confirmation.
    write_address_list_header(rfc2047_encoding, Stream,
        "Resent-To", ToAddresses, ok, _ToError, !IO).

%-----------------------------------------------------------------------------%

:- pred resend_with_progress(screen::in, account::in, string::in,
    message_update::out, io::di, io::uo) is det.

resend_with_progress(Screen, Account, Filename, MessageUpdate, !IO) :-
    update_message_immed(Screen, set_info("Sending message..."), !IO),
    call_send_mail(Account, Filename, SendRes, !IO),
    (
        SendRes = ok,
        MessageUpdate = set_info("Message resent.")
    ;
        SendRes = error(Error),
        MessageUpdate = set_info(Error)
    ).

:- pred call_send_mail(account::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

call_send_mail(Account, Filename, Res, !IO) :-
    % Sendmail-compatible programs should extract the recipient from Resent-To
    % header when passed the "-t" option.
    get_sendmail_command(Account, sendmail_read_recipients, Sendmail),
    make_quoted_command(Sendmail, [], redirect_input(Filename), no_redirect,
        Command),
    % e.g. msmtp 'passwordeval' option may invoke pinentry-curses.
    curs.soft_suspend(io.call_system(Command), ResSend, !IO),
    (
        ResSend = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = string.format("%s: returned with exit status %d",
                [s(Command), i(ExitStatus)]),
            Res = error(Msg)
        )
    ;
        ResSend = error(Error),
        Msg = Command ++ ": " ++ io.error_message(Error),
        Res = error(Msg)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
