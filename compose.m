%-----------------------------------------------------------------------------%

:- module compose.
:- interface.

:- import_module io.

:- import_module data.
:- import_module screen.

:- type reply_kind
    --->    direct_reply
    ;       group_reply
    ;       list_reply.

:- pred start_compose(screen::in, io::di, io::uo) is det.

:- pred start_reply(screen::in, message::in, reply_kind::in,
    io::di, io::uo) is det.

:- pred continue_postponed(screen::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module time.

:- import_module callout.
:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module message_file.
:- import_module pager.
:- import_module popen.
:- import_module quote_arg.
:- import_module text_entry.
:- import_module sys_util.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- mutable(msgid_counter, int, 0, ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

start_compose(Screen, !IO) :-
    get_from(From, !IO),
    text_entry(Screen, "To: ", MaybeTo, !IO),
    (
        MaybeTo = yes(To),
        text_entry(Screen, "Subject: ", MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            Cc = "",
            Bcc = "",
            ReplyTo = "",
            References = "",
            InReplyTo = "",
            Headers = headers(From, To, Cc, Bcc, Subject, ReplyTo, References,
                InReplyTo, map.init),
            Body = "",
            MaybeOldDraft = no,
            create_edit_stage(Screen, Headers, Body, MaybeOldDraft, !IO)
        ;
            MaybeSubject = no
        )
    ;
        MaybeTo = no
    ).

:- pred get_from(string::out, io::di, io::uo) is det.

get_from(From, !IO) :-
    get_notmuch_config("user.name", ResName, !IO),
    (
        ResName = ok(Name),
        get_notmuch_config("user.primary_email", ResEmail, !IO),
        (
            ResEmail = ok(Email),
            From = string.append_list([Name, " <", Email, ">"])
        ;
            ResEmail = error(_),
            From = Name
        )
    ;
        ResName = error(_),
        From = ""
    ).

%-----------------------------------------------------------------------------%

start_reply(Screen, Message, ReplyKind, !IO) :-
    Message ^ m_id = MessageId,
    Args = ["notmuch", "reply", message_id_to_search_term(MessageId)],
    args_to_quoted_command(Args, Command),
    popen(Command, CommandResult, !IO),
    (
        CommandResult = ok(String),
        read_headers_from_string(String, 0, init_headers, Headers0, Body),
        (
            ReplyKind = direct_reply,
            OrigFrom = Message ^ m_from,
            OrigReplyTo = Message ^ m_reply_to,
            set_headers_for_direct_reply(OrigFrom, OrigReplyTo,
                Headers0, Headers)
        ;
            ReplyKind = group_reply,
            set_headers_for_group_reply(Headers0, Headers)
        ;
            ReplyKind = list_reply,
            OrigFrom = Message ^ m_from,
            set_headers_for_list_reply(OrigFrom, Headers0, Headers)
        ),
        MaybeOldDraft = no,
        create_edit_stage(Screen, Headers, Body, MaybeOldDraft, !IO)
    ;
        CommandResult = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        update_message(Screen, set_warning(Warning), !IO)
    ).

:- pred set_headers_for_direct_reply(string::in, string::in,
    headers::in, headers::out) is det.

set_headers_for_direct_reply(OrigFrom, OrigReplyTo, !Headers) :-
    ( OrigReplyTo \= "" ->
        !Headers ^ h_to := OrigReplyTo
    ; OrigFrom \= "" ->
        !Headers ^ h_to := OrigFrom
    ;
        true
    ),
    !Headers ^ h_cc := "".

:- pred set_headers_for_group_reply(headers::in, headers::out) is det.

set_headers_for_group_reply(!Headers) :-
    % Move all but the first To address down to Cc.  This acts more like the
    % behaviour I am used to from Mutt.

    To0 = !.Headers ^ h_to,
    Cc0 = !.Headers ^ h_cc,
    string.split_at_string(", ", To0) = ToList0,
    ( Cc0 = "" ->
        CcList0 = []
    ;
        CcList0 = string.split_at_string(", ", Cc0)
    ),
    ( ToList0 = [To | ToList1] ->
        CcList = ToList1 ++ CcList0,
        Cc = string.join_list(", ", CcList),
        !Headers ^ h_to := To,
        !Headers ^ h_cc := Cc
    ;
        true
    ).

:- pred set_headers_for_list_reply(string::in, headers::in, headers::out)
    is det.

set_headers_for_list_reply(OrigFrom, !Headers) :-
    To0 = !.Headers ^ h_to,
    string.split_at_string(", ", To0) = ToList0,
    (
        ToList0 = [_, _ | _],
        list.delete_first(ToList0, OrigFrom, ToList)
    ->
        string.join_list(", ", ToList) = To,
        !Headers ^ h_to := To
    ;
        true
    ).

%-----------------------------------------------------------------------------%

continue_postponed(Screen, Filename, !IO) :-
    parse_message_file(Filename, ResParse, !IO),
    (
        ResParse = ok(Headers - Body),
        create_edit_stage(Screen, Headers, Body, yes(Filename), !IO)
    ;
        ResParse = error(Error),
        io.error_message(Error, Msg),
        update_message(Screen, set_warning(Msg), !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred create_edit_stage(screen::in, headers::in, string::in,
    maybe(string)::in, io::di, io::uo) is det.

create_edit_stage(Screen, Headers0, Body0, MaybeOldDraft, !IO) :-
    Sending = no,
    WriteReferences = no,
    create_temp_message_file(Headers0, Body0, Sending, WriteReferences,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_editor(Screen, Filename, ResEdit, !IO),
        (
            ResEdit = yes,
            parse_message_file(Filename, ResParse, !IO),
            (
                ResParse = ok(Headers1 - Body),
                io.remove_file(Filename, _, !IO),
                update_references(Headers0, Headers1, Headers),
                Cols = Screen ^ cols,
                setup_pager_for_staging(Cols, Body, PagerInfo),
                staging_screen(Screen, Headers, Body, MaybeOldDraft,
                    PagerInfo, !IO)
            ;
                ResParse = error(Error),
                io.error_message(Error, Msg),
                update_message(Screen, set_warning(Msg), !IO)
            )
        ;
            ResEdit = no
        )
    ;
        ResFilename = error(Error),
        Message = io.error_message(Error),
        update_message(Screen, set_warning(Message), !IO)
    ).

:- pred call_editor(screen::in, string::in, bool::out, io::di, io::uo) is det.

call_editor(Screen, Filename, Res, !IO) :-
    io.get_environment_var("EDITOR", MaybeEditor, !IO),
    (
        MaybeEditor = yes(Editor)
    ;
        MaybeEditor = no,
        Editor = "vi"
    ),
    curs.def_prog_mode(!IO),
    curs.stop(!IO),
    ( string.sub_string_search(Editor, "vim", _) ->
        Args = [Editor, "+set ft=mail", Filename]
    ;
        Args = [Editor, Filename]
    ),
    args_to_quoted_command(Args, Command),
    io.call_system(Command, CallRes, !IO),
    curs.refresh(!IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = yes
        ;
            string.format("%s returned exit status %d",
                [s(Editor), i(ExitStatus)], Warning),
            update_message(Screen, set_warning(Warning), !IO),
            Res = no
        )
    ;
        CallRes = error(Error),
        string.append_list(["Error running ", Editor, ": ",
            io.error_message(Error)], Warning),
        update_message(Screen, set_warning(Warning), !IO),
        Res = no
    ).

:- pred update_references(headers::in, headers::in, headers::out) is det.

update_references(Headers0, !Headers) :-
    % If user doesn't touch the In-Reply-To field then copy the References
    % field.  Otherwise leave References as-is (likely blank).
    InReplyTo0 = Headers0 ^ h_inreplyto,
    InReplyTo1 = !.Headers ^ h_inreplyto,
    ( InReplyTo0 = InReplyTo1 ->
        References0 = Headers0 ^ h_references,
        !Headers ^ h_references := References0
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred staging_screen(screen::in, headers::in, string::in,
    maybe(string)::in, pager_info::in, io::di, io::uo) is det.

staging_screen(Screen, Headers, Body, MaybeOldDraft, !.PagerInfo, !IO) :-
    draw_pager(Screen, !.PagerInfo, !IO),
    draw_staging_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'e' ->
        create_edit_stage(Screen, Headers, Body, MaybeOldDraft, !IO)
    ; Char = 'p' ->
        postpone(Screen, Headers, Body, Res, !IO),
        (
            Res = yes,
            maybe_remove_draft(MaybeOldDraft, !IO)
        ;
            Res = no,
            staging_screen(Screen, Headers, Body, MaybeOldDraft, !.PagerInfo,
                !IO)
        )
    ; Char = 'Y' ->
        send_mail(Screen, Headers, Body, Res, !IO),
        (
            Res = yes,
            maybe_remove_draft(MaybeOldDraft, !IO)
        ;
            Res = no,
            staging_screen(Screen, Headers, Body, MaybeOldDraft, !.PagerInfo,
                !IO)
        )
    ; Char = 'A' ->
        (
            MaybeOldDraft = yes(_),
            maybe_remove_draft(MaybeOldDraft, !IO),
            update_message(Screen, set_info("Postponed message deleted."), !IO)
        ;
            MaybeOldDraft = no,
            update_message(Screen, set_info("Mail not sent."), !IO)
        )
    ;
        pager_input(Screen, Char, _Action, MessageUpdate, !PagerInfo),
        update_message(Screen, MessageUpdate, !IO),
        staging_screen(Screen, Headers, Body, MaybeOldDraft, !.PagerInfo, !IO)
    ).

:- pred draw_staging_bar(screen::in, io::di, io::uo) is det.

draw_staging_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- ", !IO),
    Msg = "Compose: (e) edit, (p) postpone, (Y) send, (A) abandon. ",
    my_addstr_fixed(Panel, Cols - 3, Msg, '-', !IO).

%-----------------------------------------------------------------------------%

:- pred postpone(screen::in, headers::in, string::in, bool::out,
    io::di, io::uo) is det.

postpone(Screen, Headers, Body, Res, !IO) :-
    % XXX write the message to the draft file directly
    Sending = no,
    WriteReferences = yes,
    create_temp_message_file(Headers, Body, Sending, WriteReferences,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        add_draft(Filename, DraftRes, !IO),
        io.remove_file(Filename, _, !IO),
        (
            DraftRes = ok,
            update_message(Screen, set_info("Message postponed."), !IO),
            Res = yes
        ;
            DraftRes = error(Error),
            Msg = io.error_message(Error),
            update_message(Screen, set_warning(Msg), !IO),
            Res = no
        )
    ;
        ResFilename = error(Error),
        update_message(Screen, set_warning(io.error_message(Error)), !IO),
        Res = no
    ).

:- pred maybe_remove_draft(maybe(string)::in, io::di, io::uo) is det.

maybe_remove_draft(no, !IO).
maybe_remove_draft(yes(FileName), !IO) :-
    io.remove_file(FileName, _, !IO).

%-----------------------------------------------------------------------------%

:- pred send_mail(screen::in, headers::in, string::in, bool::out,
    io::di, io::uo) is det.

send_mail(Screen, Headers, Body, Res, !IO) :-
    Sending = yes,
    WriteReferences = yes,
    create_temp_message_file(Headers, Body, Sending, WriteReferences,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_send_mail(Screen, Filename, Res, !IO),
        io.remove_file(Filename, _, !IO)
    ;
        ResFilename = error(Error),
        Message = io.error_message(Error),
        update_message(Screen, set_warning(Message), !IO),
        Res = no
    ).

:- pred call_send_mail(screen::in, string::in, bool::out, io::di, io::uo)
    is det.

call_send_mail(Screen, Filename, Res, !IO) :-
    update_message(Screen, set_info("Sending message..."), !IO),
    panel.update_panels(!IO),
    args_to_quoted_command(["helper-send", Filename], Command),
    io.call_system(Command, ResSend, !IO),
    (
        ResSend = ok(ExitStatus),
        ( ExitStatus = 0 ->
            update_message(Screen, set_info("Mail sent."), !IO),
            Res = yes
        ;
            Msg = string.format("helper-send returned with exit status %d",
                [i(ExitStatus)]),
            update_message(Screen, set_warning(Msg), !IO),
            Res = no
        )
    ;
        ResSend = error(Error),
        Msg = "helper-send: " ++ io.error_message(Error),
        update_message(Screen, set_warning(Msg), !IO),
        Res = no
    ).

%-----------------------------------------------------------------------------%

:- pred create_temp_message_file(headers::in, string::in, bool::in, bool::in,
    io.res(string)::out, io::di, io::uo) is det.

create_temp_message_file(Headers, Body, Sending, WriteReferences, Res, !IO) :-
    io.make_temp(Filename, !IO),
    io.open_output(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        Headers = headers(From, To, Cc, Bcc, Subject, ReplyTo, References,
            InReplyTo, Rest),
        (
            Sending = yes,
            generate_date_msg_id(Date, MessageId, !IO),
            write_header(Stream, "Date", Date, !IO),
            write_header(Stream, "Message-ID", MessageId, !IO),
            Write = write_header_opt(Stream)
        ;
            Sending = no,
            Write = write_header(Stream)
        ),
        Write("From", From, !IO),
        Write("To", To, !IO),
        Write("Cc", Cc, !IO),
        Write("Bcc", Bcc, !IO),
        Write("Subject", Subject, !IO),
        Write("Reply-To", ReplyTo, !IO),
        (
            WriteReferences = yes,
            Write("References", References, !IO)
        ;
            WriteReferences = no
        ),
        Write("In-Reply-To", InReplyTo, !IO),
        map.foldl(Write, Rest, !IO),
        io.nl(Stream, !IO),
        io.write_string(Stream, Body, !IO),
        io.close_output(Stream, !IO),
        Res = ok(Filename)
    ;
        ResOpen = error(_Error),
        Message = "Error writing temporary file " ++ Filename,
        Res = error(io.make_io_error(Message))
    ).

:- pred generate_date_msg_id(string::out, string::out, io::di, io::uo) is det.

generate_date_msg_id(Date, MessageId, !IO) :-
    time(Time, !IO),
    TM = localtime(Time),
    Year = 1900 + TM ^ tm_year,
    Month = 1 + TM ^ tm_mon,
    Day = TM ^ tm_mday,
    Hour = TM ^ tm_hour,
    Min = TM ^ tm_min,
    Sec = TM ^ tm_sec,
    Wday = TM ^ tm_wday,
    (
        weekday_short_name(Wday, WdayName0),
        month_short_name(Month, MonthName0)
    ->
        WdayName = WdayName0,
        MonthName = MonthName0
    ;
        unexpected($module, $pred, "bad weekday or month")
    ),
    get_timezone(Time, Tz0),
    Tz = Tz0 // 60,
    TzHour = Tz // 60,
    TzMin = abs(Tz) mod 60,
    Date = string.format("%s, %d %s %d %02d:%02d:%02d %+03d%02d",
        [s(WdayName), i(Day), s(MonthName), i(Year), i(Hour), i(Min), i(Sec),
        i(TzHour), i(TzMin)]),

    % This emulates the Message-ID generated by Mutt.

    get_msgid_counter(Counter0, !IO),
    char.det_from_int(65 + Counter0, Char),
    Counter = (Counter0 + 1) mod 26,
    set_msgid_counter(Counter, !IO),

    get_pid(Pid, !IO),
    get_hostname(HostName, !IO),
    get_domainname(DomainName, !IO),
    MessageId = string.format("<%04d%02d%02d%02d%02d%02d.G%c%d@%s.%s>",
        [i(Year), i(Month), i(Day), i(Hour), i(Min), i(Sec),
        c(Char), i(Pid), s(HostName), s(DomainName)]).

:- pred write_header(io.output_stream::in, string::in, string::in,
    io::di, io::uo) is det.

write_header(Stream, Header, Value, !IO) :-
    io.write_string(Stream, Header, !IO),
    io.write_string(Stream, ": ", !IO),
    io.write_string(Stream, Value, !IO),
    io.nl(Stream, !IO).

:- pred write_header_opt(io.output_stream::in, string::in, string::in,
    io::di, io::uo) is det.

write_header_opt(Stream, Name, Value, !IO) :-
    ( Value = "" ->
        true
    ;
        write_header(Stream, Name, Value, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
