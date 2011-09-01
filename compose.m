%-----------------------------------------------------------------------------%

:- module compose.
:- interface.

:- import_module io.

:- import_module screen.

:- pred start_compose(screen::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module pager.
:- import_module popen.
:- import_module quote_arg.
:- import_module text_entry.

:- type headers
    --->    headers(
                % Technically, header fields.
                h_from          :: string,
                h_to            :: string,
                h_cc            :: string,
                h_bcc           :: string,
                h_subject       :: string,
                h_reply_to      :: string,
                h_rest          :: map(string, string)
            ).

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
            Headers = headers(From, To, Cc, Bcc, Subject, ReplyTo, map.init),
            Body = "",
            create_edit_stage(Screen, Headers, Body, !IO)
        ;
            MaybeSubject = no
        )
    ;
        MaybeTo = no
    ).

:- pred get_from(string::out, io::di, io::uo) is det.

get_from(From, !IO) :-
    popen("notmuch config get user.name", ResName, !IO),
    (
        ResName = ok(Name0),
        Name = string.strip(Name0),
        popen("notmuch config get user.primary_email", ResEmail, !IO),
        (
            ResEmail = ok(Email0),
            Email = string.strip(Email0),
            From = string.append_list([Name, " <", Email, ">"])
        ;
            ResEmail = error(_),
            From = Name
        )
    ;
        ResName = error(_),
        From = ""
    ).

:- pred create_edit_stage(screen::in, headers::in, string::in, io::di, io::uo)
    is det.

create_edit_stage(Screen, Headers0, Body0, !IO) :-
    create_temp_message_file(Headers0, Body0, ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_editor(Screen, Filename, ResEdit, !IO),
        (
            ResEdit = yes,
            parse_message_file(Filename, ResParse, !IO),
            (
                ResParse = ok(Headers - Body),
                io.remove_file(Filename, _, !IO),
                Cols = Screen ^ cols,
                setup_pager_for_staging(Cols, Body, PagerInfo),
                staging_screen(Screen, Headers, Body, PagerInfo, !IO)
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

%-----------------------------------------------------------------------------%

:- pred staging_screen(screen::in, headers::in, string::in, pager_info::in,
    io::di, io::uo) is det.

staging_screen(Screen, Headers, Body, !.PagerInfo, !IO) :-
    draw_pager(Screen, !.PagerInfo, !IO),
    draw_staging_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'e' ->
        create_edit_stage(Screen, Headers, Body, !IO)
    ; Char = 'p' ->
        postpone(Screen, Headers, Body, Res, !IO),
        (
            Res = yes
        ;
            Res = no,
            staging_screen(Screen, Headers, Body, !.PagerInfo, !IO)
        )
    ; Char = 'Y' ->
        send_mail(Screen, Headers, Body, Res, !IO),
        (
            Res = yes
        ;
            Res = no,
            staging_screen(Screen, Headers, Body, !.PagerInfo, !IO)
        )
    ; Char = 'A' ->
        update_message(Screen, set_info("Mail not sent."), !IO)
    ;
        pager_input(Screen, Char, _Action, MessageUpdate, !PagerInfo),
        update_message(Screen, MessageUpdate, !IO),
        staging_screen(Screen, Headers, Body, !.PagerInfo, !IO)
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
    popen("notmuch config get database.path", ResDbPath, !IO),
    (
        ResDbPath = ok(DbPath0),
        DbPath = string.strip(DbPath0),
        % XXX write the message to the draft file directly
        create_temp_message_file(Headers, Body, ResFilename, !IO),
        (
            ResFilename = ok(Filename),
            add_draft(DbPath, Filename, DraftRes, !IO),
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
        )
    ;
        ResDbPath = error(Error),
        Msg = "Unable to get database path: " ++ io.error_message(Error),
        update_message(Screen, set_warning(Msg), !IO),
        Res = no
    ).

%-----------------------------------------------------------------------------%

:- pred send_mail(screen::in, headers::in, string::in, bool::out,
    io::di, io::uo) is det.

send_mail(Screen, Headers, Body, Res, !IO) :-
    create_temp_message_file(Headers, Body, ResFilename, !IO),
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

:- pred create_temp_message_file(headers::in, string::in, io.res(string)::out,
    io::di, io::uo) is det.

create_temp_message_file(Headers, Body, Res, !IO) :-
    io.make_temp(Filename, !IO),
    io.open_output(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        Headers = headers(From, To, Cc, Bcc, Subject, ReplyTo, Rest),
        % XXX add Date, MessageID
        write_header(Stream, "From", From, !IO),
        write_header(Stream, "To", To, !IO),
        write_header(Stream, "Cc", Cc, !IO),
        write_header(Stream, "Bcc", Bcc, !IO),
        write_header(Stream, "Subject", Subject, !IO),
        write_header(Stream, "Reply-To", ReplyTo, !IO),
        map.foldl(write_header(Stream), Rest, !IO),
        io.nl(Stream, !IO),
        io.write_string(Stream, Body, !IO),
        io.close_output(Stream, !IO),
        Res = ok(Filename)
    ;
        ResOpen = error(_Error),
        Message = "Error writing temporary file " ++ Filename,
        Res = error(io.make_io_error(Message))
    ).

:- pred write_header(io.output_stream::in, string::in, string::in,
    io::di, io::uo) is det.

write_header(Stream, Header, Value, !IO) :-
    io.write_string(Stream, Header, !IO),
    io.write_string(Stream, ": ", !IO),
    io.write_string(Stream, Value, !IO),
    io.nl(Stream, !IO).

:- pred parse_message_file(string::in, io.res(pair(headers, string))::out,
    io::di, io::uo) is det.

parse_message_file(Filename, Res, !IO) :-
    io.open_input(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        Headers0 = headers("", "", "", "", "", "", map.init),
        read_headers(Stream, ResHeaders, MaybeBodyLine, Headers0, Headers,
            !IO),
        (
            ResHeaders = ok,
            read_file_as_string(Stream, ResBody, !IO),
            (
                ResBody = ok(Body0),
                (
                    MaybeBodyLine = yes(BodyLine),
                    Body = BodyLine ++ Body0
                ;
                    MaybeBodyLine = no,
                    Body = Body0
                ),
                Res = ok(Headers - Body)
            ;
                ResBody = error(_, Error),
                Res = error(Error)
            )
        ;
            ResHeaders = error(Error),
            Res = error(Error)
        ),
        io.close_input(Stream, !IO)
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

    % XXX implement proper parser
:- pred read_headers(io.input_stream::in, io.res::out, maybe(string)::out,
    headers::in, headers::out, io::di, io::uo) is det.

read_headers(Stream, Res, MaybeBodyLine, !Headers, !IO) :-
    io.read_line_as_string(Stream, ResRead, !IO),
    (
        ResRead = ok(Line),
        ( Line = "\n" ->
            Res = ok,
            MaybeBodyLine = no
        ; string.sub_string_search(Line, ":", Colon) ->
            End = string.count_code_units(Line),
            string.between(Line, 0, Colon, Name0),
            string.between(Line, Colon + 1, End, Value0),
            Name = string.strip(Name0),
            Value = string.strip(Value0),
            ( read_standard_header(Name, Value, !Headers) ->
                true
            ;
                Rest0 = !.Headers ^ h_rest,
                map.set(Name, Value, Rest0, Rest),
                !Headers ^ h_rest := Rest
            ),
            read_headers(Stream, Res, MaybeBodyLine, !Headers, !IO)
        ;
            Res = ok,
            MaybeBodyLine = yes(Line)
        )
    ;
        ResRead = eof,
        Res = ok,
        MaybeBodyLine = no
    ;
        ResRead = error(Error),
        Res = error(Error),
        MaybeBodyLine = no
    ).

:- pred read_standard_header(string::in, string::in,
    headers::in, headers::out) is semidet.

read_standard_header(Name, Value, !Headers) :-
    ( strcase_equal(Name, "From") ->
        !Headers ^ h_from := Value
    ; strcase_equal(Name, "To") ->
        !Headers ^ h_to := Value
    ; strcase_equal(Name, "Cc") ->
        !Headers ^ h_cc := Value
    ; strcase_equal(Name, "Bcc") ->
        !Headers ^ h_bcc := Value
    ; strcase_equal(Name, "Subject") ->
        !Headers ^ h_subject := Value
    ; strcase_equal(Name, "Reply-To") ->
        !Headers ^ h_reply_to := Value
    ; strcase_equal(Name, "Date") ->
        % Ignore it.
        true
    ; strcase_equal(Name, "Message-ID") ->
        % Ignore it.
        true
    ;
        fail
    ).

:- pred strcase_equal(string::in, string::in) is semidet.

:- pragma foreign_proc("C",
    strcase_equal(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasecmp(SA, SB) == 0);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
