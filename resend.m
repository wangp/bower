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

:- import_module addressbook.
:- import_module compose.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module rfc5322.
:- import_module send_util.

:- type sent
    --->    sent
    ;       not_sent.

%-----------------------------------------------------------------------------%

handle_resend(Config, Screen, MessageId, MessageUpdate, !ToHistory, !IO) :-
    text_entry_initial(Screen, "Resend message to: ", !.ToHistory, "",
        complete_config_key(Config, addressbook_section), MaybeTo, !IO),
    (
        MaybeTo = yes(To0),
        To0 \= ""
    ->
        add_history_nodup(To0, !ToHistory),
        parse_and_expand_addresses_string(Config, To0, To, ToAddresses,
            ToValid, !IO),
        (
            ToValid = yes,
            confirm_resend(Screen, To, Confirmation, !IO),
            (
                Confirmation = yes,
                create_temp_message_file_and_resend(Config, Screen, MessageId,
                    ToAddresses, MessageUpdate, !IO)
            ;
                Confirmation = no,
                MessageUpdate = set_info("Message not resent.")
            )
        ;
            ToValid = no,
            MessageUpdate = set_warning("Invalid address. Message not resent.")
        )
    ;
        MessageUpdate = set_info("Message not resent.")
    ).

:- pred confirm_resend(screen::in, string::in, bool::out, io::di, io::uo)
    is det.

confirm_resend(Screen, To, Confirmation, !IO) :-
    Prompt = "Resend message to " ++ To ++ "? (Y/n) ",
    update_message_immed(Screen, set_prompt(Prompt), !IO),
    get_keycode_blocking(Code, !IO),
    ( Code = char('Y') ->
        Confirmation = yes
    ;
        Confirmation = no
    ).

%-----------------------------------------------------------------------------%

:- pred create_temp_message_file_and_resend(prog_config::in, screen::in,
    message_id::in, address_list::in, message_update::out, io::di, io::uo)
    is det.

create_temp_message_file_and_resend(Config, Screen, MessageId, ToAddresses,
        MessageUpdate, !IO) :-
    get_from_address(Config, FromAddress, !IO),
    write_resent_headers(FromAddress, ToAddresses, ResWrite, !IO),
    (
        ResWrite = ok(FileName),
        get_notmuch_command(Config, Notmuch),
        args_to_quoted_command(Notmuch, [
            "show", "--format=raw", "--",
            message_id_to_search_term(MessageId)
        ], no, redirect_append(FileName), Command),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                resend_with_progress(Config, Screen, FileName, MessageUpdate,
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

:- pred write_resent_headers(address::in, address_list::in,
    maybe_error(string)::out, io::di, io::uo) is det.

write_resent_headers(FromAddress, ToAddresses, Res, !IO) :-
    io.make_temp(FileName, !IO),
    io.open_output(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        promise_equivalent_solutions [Res, !:IO]
        (
          try [io(!IO)] (
            generate_resent_headers(Stream, FromAddress, ToAddresses, !IO),
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
    ).

:- pred generate_resent_headers(io.output_stream::in, address::in,
    address_list::in, io::di, io::uo) is det.

generate_resent_headers(Stream, FromAddress, ToAddresses, !IO) :-
    generate_date_msg_id(Date, ResentMessageId, !IO),

    % We assume the From address is ok.
    write_address_list_header(rfc2047_encoding, Stream,
        "Resent-From", [FromAddress], ok, _FromError, !IO),
    write_as_unstructured_header(no_encoding, Stream,
        "Resent-Date", Date, !IO),
    write_as_unstructured_header(no_encoding, Stream,
        "Resent-Message-ID", ResentMessageId, !IO),
    % The To addresses were checked before confirmation.
    write_address_list_header(rfc2047_encoding, Stream,
        "Resent-To", ToAddresses, ok, _ToError, !IO).

%-----------------------------------------------------------------------------%

:- pred resend_with_progress(prog_config::in, screen::in, string::in,
    message_update::out, io::di, io::uo) is det.

resend_with_progress(Config, Screen, Filename, MessageUpdate, !IO) :-
    update_message_immed(Screen, set_info("Sending message..."), !IO),
    call_send_mail(Config, Filename, SendRes, !IO),
    (
        SendRes = ok,
        MessageUpdate = set_info("Message resent.")
    ;
        SendRes = error(Error),
        MessageUpdate = set_info(Error)
    ).

:- pred call_send_mail(prog_config::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

call_send_mail(Config, Filename, Res, !IO) :-
    % Sendmail-compatible programs should extract the recipient from Resent-To
    % header when passed the "-t" option.
    get_sendmail_command(Config, sendmail_read_recipients,
        shell_quoted(Sendmail)),
    Command = string.join_list(" ", [Sendmail, " < ", quote_arg(Filename)]),
    io.call_system(Command, ResSend, !IO),
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
        Msg = Sendmail ++ ": " ++ io.error_message(Error),
        Res = error(Msg)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
