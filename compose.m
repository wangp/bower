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
:- import_module maybe.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.
:- import_module pager.
:- import_module popen.
:- import_module quote_arg.
:- import_module text_entry.

%-----------------------------------------------------------------------------%

start_compose(Screen, !IO) :-
    get_from(From, !IO),
    text_entry(Screen, "To: ", MaybeTo, !IO),
    (
        MaybeTo = yes(To),
        text_entry(Screen, "Subject: ", MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            new_message_template(From, To, Subject, ResTemplate, !IO),
            (
                ResTemplate = ok(Filename),
                edit_then_stage(Screen, Filename, !IO),
                io.remove_file(Filename, _, !IO)
            ;
                ResTemplate = error(Error),
                io.error_message(Error, Warning),
                update_message(Screen, set_warning(Warning), !IO)
            )
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

:- pred new_message_template(string::in, string::in, string::in,
    io.res(string)::out, io::di, io::uo) is det.

new_message_template(From, To, Subject, Res, !IO) :-
    io.make_temp(Filename, !IO),
    io.open_output(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        list.foldl(io.write_string(Stream), [
            "From: ", From, "\n",
            "To: ", To, "\n",
            "Cc:\n",
            "Bcc:\n",
            "Subject: ", Subject, "\n",
            "Reply-To:\n",
            "\n"
        ], !IO),
        io.close_output(Stream, !IO),
        Res = ok(Filename)
    ;
        ResOpen = error(_),
        Message = "Error writing temporary file " ++ Filename,
        Res = error(io.make_io_error(Message))
    ).

:- pred edit_then_stage(screen::in, string::in, io::di, io::uo) is det.

edit_then_stage(Screen, Filename, !IO) :-
    call_editor(Screen, Filename, ResEdit, !IO),
    (
        ResEdit = yes,
        read_file_as_string(Filename, ResRead, !IO),
        (
            ResRead = ok(String),
            Cols = Screen ^ cols,
            setup_pager_for_staging(Cols, String, PagerInfo),
            staging_screen(Screen, Filename, PagerInfo, !IO)
        ;
            ResRead = error(Error),
            io.error_message(Error, Msg),
            update_message(Screen, set_warning(Msg), !IO)
        )
    ;
        ResEdit = no
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

:- pred read_file_as_string(string::in, io.res(string)::out,
    io::di, io::uo) is det.

read_file_as_string(Filename, Res, !IO) :-
    io.open_input(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        io.read_file_as_string(Stream, ResRead, !IO),
        io.close_input(Stream, !IO),
        (
            ResRead = ok(String),
            Res = ok(String)
        ;
            ResRead = error(_, Error),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- pred staging_screen(screen::in, string::in, pager_info::in,
    io::di, io::uo) is det.

staging_screen(Screen, Filename, !.PagerInfo, !IO) :-
    draw_pager(Screen, !.PagerInfo, !IO),
    draw_staging_bar(Screen, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'e' ->
        edit_then_stage(Screen, Filename, !IO)
    ; Char = 'Y' ->
        call_send_mail(Screen, Filename, Res, !IO),
        (
            Res = yes
        ;
            Res = no,
            staging_screen(Screen, Filename, !.PagerInfo, !IO)
        )
    ; Char = 'A' ->
        update_message(Screen, set_info("Mail not sent."), !IO)
    ;
        pager_input(Screen, Char, _Action, MessageUpdate, !PagerInfo),
        update_message(Screen, MessageUpdate, !IO),
        staging_screen(Screen, Filename, !.PagerInfo, !IO)
    ).

:- pred draw_staging_bar(screen::in, io::di, io::uo) is det.

draw_staging_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- ", !IO),
    Msg = "Compose: (e) edit, (Y) send, (A) abandon. ",
    my_addstr_fixed(Panel, Cols - 3, Msg, '-', !IO).

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
% vim: ft=mercury ts=4 sts=4 sw=4 et
