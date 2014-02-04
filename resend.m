% Bower - a frontend for the Notmuch email system
% Copyright (C) 2013 Peter Wang

:- module resend.
:- interface.

:- import_module io.

:- import_module data.
:- import_module screen.
:- import_module text_entry.

%-----------------------------------------------------------------------------%

:- pred handle_resend(screen::in, message_id::in, message_update::out,
    history::in, history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module addressbook.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module send_util.

:- type to_addr
    --->    to_addr(string).

:- type sent
    --->    sent
    ;       not_sent.

%-----------------------------------------------------------------------------%

handle_resend(Screen, MessageId, MessageUpdate, !ToHistory, !IO) :-
    text_entry_initial(Screen, "Resend message to: ", !.ToHistory, "",
        complete_config_key(addressbook_section), MaybeTo, !IO),
    (
        MaybeTo = yes(To0),
        To0 \= ""
    ->
        add_history_nodup(To0, !ToHistory),
        expand_aliases(To0, To, !IO),
        ToAddr = to_addr(To),
        confirm_resend(Screen, ToAddr, Confirmation, !IO),
        (
            Confirmation = yes,
            create_temp_message_file_and_resend(Screen, MessageId, ToAddr,
                MessageUpdate, !IO)
        ;
            Confirmation = no,
            MessageUpdate = set_info("Message not resent.")
        )
    ;
        MessageUpdate = set_info("Message not resent.")
    ).

:- pred confirm_resend(screen::in, to_addr::in, bool::out, io::di, io::uo)
    is det.

confirm_resend(Screen, to_addr(To), Confirmation, !IO) :-
    Prompt = "Resend message to " ++ To ++ "? (Y/n) ",
    update_message_immed(Screen, set_prompt(Prompt), !IO),
    get_keycode_blocking(Code, !IO),
    ( Code = char('Y') ->
        Confirmation = yes
    ;
        Confirmation = no
    ).

%-----------------------------------------------------------------------------%

:- pred create_temp_message_file_and_resend(screen::in, message_id::in,
    to_addr::in, message_update::out, io::di, io::uo) is det.

create_temp_message_file_and_resend(Screen, MessageId, ToAddr, MessageUpdate,
        !IO) :-
    io.make_temp(Filename, !IO),
    generate_resent_headers(Filename, ToAddr, ResHeaders, !IO),
    (
        ResHeaders = ok,
        args_to_quoted_command([
            "show", "--format=raw", "--",
            message_id_to_search_term(MessageId)
        ], no, redirect_append(Filename), Command),
        get_notmuch_prefix(Notmuch, !IO),
        io.call_system(Notmuch ++ Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                resend_with_progress(Screen, Filename, MessageUpdate, !IO)
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
        )
    ;
        ResHeaders = error(Error),
        MessageUpdate = set_warning(Error)
    ),
    io.remove_file(Filename, _, !IO).

:- pred generate_resent_headers(string::in, to_addr::in, maybe_error::out,
    io::di, io::uo) is det.

generate_resent_headers(FileName, to_addr(To), Res, !IO) :-
    get_from(From, !IO),
    generate_date_msg_id(Date, MessageId, !IO),
    io.open_output(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        write_address_list_header(Stream, "Resent-From", From, !IO),
        write_unstructured_header(Stream, "Resent-Date", Date, !IO),
        write_unstructured_header(Stream, "Resent-Message-ID", MessageId, !IO),
        write_address_list_header(Stream, "Resent-To", To, !IO),
        io.close_output(Stream, !IO),
        Res = ok
    ;
        ResOpen = error(Error),
        Res = error("error opening " ++ FileName ++ ": " ++
            io.error_message(Error))
    ).

:- pred resend_with_progress(screen::in, string::in, message_update::out,
    io::di, io::uo) is det.

resend_with_progress(Screen, Filename, MessageUpdate, !IO) :-
    update_message_immed(Screen, set_info("Sending message..."), !IO),
    call_send_mail(Filename, SendRes, !IO),
    (
        SendRes = ok,
        MessageUpdate = set_info("Message resent.")
    ;
        SendRes = error(Error),
        MessageUpdate = set_info(Error)
    ).

:- pred call_send_mail(string::in, maybe_error::out, io::di, io::uo) is det.

call_send_mail(Filename, Res, !IO) :-
    % Sendmail-compatible programs should extract the recipient from Resent-To
    % header when passed the "-t" option.
    get_sendmail_command(sendmail_read_recipients, Sendmail, !IO),
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
