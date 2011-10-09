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

:- pred continue_postponed(screen::in, message::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module random.
:- import_module require.
:- import_module string.
:- import_module time.

:- import_module callout.
:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module message_file.
:- import_module mime_type.
:- import_module pager.
:- import_module popen.
:- import_module quote_arg.
:- import_module scrollable.
:- import_module text_entry.
:- import_module string_util.
:- import_module sys_util.
:- import_module time_util.

:- type header_type
    --->    from
    ;       to
    ;       cc
    ;       bcc
    ;       subject
    ;       replyto.

:- type staging_info
    --->    staging_info(
                si_headers      :: headers,
                si_text         :: string,
                si_old_msgid    :: maybe(message_id)
            ).

:- type attach_info == scrollable(attachment).

:- type attachment
    --->    old_attachment(part)
    ;       new_text_attachment(
                txt_type        :: string,
                txt_content     :: string,
                txt_filename    :: string
            )
    ;       new_binary_attachment(
                bin_type        :: string,
                bin_base64      :: string,
                bin_filename    :: string
            ).

:- instance scrollable.line(attachment) where [
    pred(draw_line/5) is draw_attachment_line
].

%-----------------------------------------------------------------------------%

:- mutable(msgid_counter, int, 0, ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

start_compose(Screen, !IO) :-
    get_from(From, !IO),
    text_entry(Screen, "To: ", init_history, MaybeTo, !IO),
    (
        MaybeTo = yes(To),
        text_entry(Screen, "Subject: ", init_history, MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            some [!Headers] (
                !:Headers = init_headers,
                !Headers ^ h_from := From,
                !Headers ^ h_to := To,
                !Headers ^ h_subject := Subject,
                Headers = !.Headers
            ),
            Text = "",
            Attachments = [],
            MaybeOldDraft = no,
            create_edit_stage(Screen, Headers, Text, Attachments,
                MaybeOldDraft, !IO)
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
        read_headers_from_string(String, 0, init_headers, Headers0, Text),
        (
            ReplyKind = direct_reply,
            OrigFrom = Message ^ m_headers ^ h_from,
            OrigReplyTo = Message ^ m_headers ^ h_replyto,
            set_headers_for_direct_reply(OrigFrom, OrigReplyTo,
                Headers0, Headers)
        ;
            ReplyKind = group_reply,
            set_headers_for_group_reply(Headers0, Headers)
        ;
            ReplyKind = list_reply,
            OrigFrom = Message ^ m_headers ^ h_from,
            set_headers_for_list_reply(OrigFrom, Headers0, Headers)
        ),
        Attachments = [],
        MaybeOldDraft = no,
        create_edit_stage(Screen, Headers, Text, Attachments, MaybeOldDraft,
            !IO)
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

continue_postponed(Screen, Message, !IO) :-
    MessageId = Message ^ m_id,
    Headers0 = Message ^ m_headers,
    Body0 = cord.list(Message ^ m_body),
    first_text_part(Body0, Text, AttachmentParts),
    list.map(to_old_attachment, AttachmentParts, Attachments),

    % XXX notmuch show --format=json does not return References and In-Reply-To
    % so we parse them from the raw output.
    args_to_quoted_command([
        "notmuch", "show", "--format=raw", "--",
        message_id_to_search_term(MessageId)
    ], Command),
    popen(Command, CallRes, !IO),
    (
        CallRes = ok(String),
        read_headers_from_string(String, 0, init_headers, HeadersB, _Body),
        some [!Headers] (
            !:Headers = Headers0,
            !Headers ^ h_replyto := (HeadersB ^ h_replyto),
            !Headers ^ h_references := (HeadersB ^ h_references),
            !Headers ^ h_inreplyto := (HeadersB ^ h_inreplyto),
            Headers = !.Headers
        ),
        create_edit_stage(Screen, Headers, Text, Attachments, yes(MessageId), !IO)
    ;
        CallRes = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        update_message(Screen, set_warning(Warning), !IO)
    ).

:- pred first_text_part(list(part)::in, string::out, list(part)::out)
    is det.

first_text_part([], "", []).
first_text_part([Part | Parts], Text, AttachmentParts) :-
    MaybeContent = Part ^ pt_content,
    (
        MaybeContent = yes(Text),
        AttachmentParts = Parts
    ;
        MaybeContent = no,
        first_text_part(Parts, Text, AttachmentParts0),
        AttachmentParts = [Part | AttachmentParts0]
    ).

:- pred to_old_attachment(part::in, attachment::out) is det.

to_old_attachment(Part, old_attachment(Part)).

%-----------------------------------------------------------------------------%

:- pred create_edit_stage(screen::in, headers::in, string::in,
    list(attachment)::in, maybe(message_id)::in, io::di, io::uo) is det.

create_edit_stage(Screen, Headers0, Text0, Attachments, MaybeOldDraft, !IO) :-
    create_temp_message_file(Headers0, Text0, Attachments, prepare_edit,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_editor(Screen, Filename, ResEdit, !IO),
        (
            ResEdit = yes,
            parse_message_file(Filename, ResParse, !IO),
            (
                ResParse = ok(Headers1 - Text),
                io.remove_file(Filename, _, !IO),
                update_references(Headers0, Headers1, Headers),
                StagingInfo = staging_info(Headers, Text, MaybeOldDraft),
                AttachInfo = scrollable.init_with_cursor(Attachments, 0),
                Cols = Screen ^ cols,
                setup_pager_for_staging(Cols, Text, PagerInfo),
                staging_screen(Screen, StagingInfo, AttachInfo, PagerInfo, !IO)
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

:- pred staging_screen(screen::in, staging_info::in, attach_info::in,
    pager_info::in, io::di, io::uo) is det.

staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO) :-
    !.StagingInfo = staging_info(Headers, Text, MaybeOldDraft),
    split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels),
    draw_header_lines(HeaderPanels, Headers, !IO),
    scrollable.draw(AttachmentPanels, !.AttachInfo, !IO),
    draw_attachments_label(AttachmentPanels, !IO),
    draw_sep_bar(Screen, MaybeSepPanel, !IO),
    draw_pager_lines(PagerPanels, !.PagerInfo, !IO),
    draw_staging_bar(Screen, !IO),
    panel.update_panels(!IO),
    NumAttachmentRows = list.length(AttachmentPanels),
    get_char(Char, !IO),
    ( Char = 'e' ->
        Attachments = get_lines_list(!.AttachInfo),
        create_edit_stage(Screen, Headers, Text, Attachments, MaybeOldDraft,
            !IO)
    ; Char = 'f' ->
        edit_header(Screen, from, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 't' ->
        edit_header(Screen, to, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'c' ->
        edit_header(Screen, cc, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'b' ->
        edit_header(Screen, bcc, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 's' ->
        edit_header(Screen, subject, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'r' ->
        edit_header(Screen, replyto, !StagingInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'j' ->
        scroll_attachments(Screen, NumAttachmentRows, 1, !AttachInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'k' ->
        scroll_attachments(Screen, NumAttachmentRows, -1, !AttachInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'a' ->
        add_attachment(Screen, NumAttachmentRows, !AttachInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'd' ->
        delete_attachment(Screen, !AttachInfo, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ; Char = 'p' ->
        Attachments = get_lines_list(!.AttachInfo),
        postpone(Screen, Headers, Text, Attachments, Res, !IO),
        (
            Res = yes,
            maybe_remove_draft(!.StagingInfo, !IO)
        ;
            Res = no,
            staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
        )
    ; Char = 'Y' ->
        Attachments = get_lines_list(!.AttachInfo),
        send_mail(Screen, Headers, Text, Attachments, Res, !IO),
        (
            Res = yes,
            tag_replied_message(Screen, Headers, !IO),
            maybe_remove_draft(!.StagingInfo, !IO)
        ;
            Res = no,
            staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
        )
    ; Char = 'Q' ->
        % XXX prompt to abandon
        (
            MaybeOldDraft = yes(_),
            maybe_remove_draft(!.StagingInfo, !IO),
            update_message(Screen, set_info("Postponed message deleted."), !IO)
        ;
            MaybeOldDraft = no,
            update_message(Screen, set_info("Mail not sent."), !IO)
        )
    ;
        pager_input(Screen, Char, _Action, MessageUpdate, !PagerInfo),
        update_message(Screen, MessageUpdate, !IO),
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred edit_header(screen::in, header_type::in,
    staging_info::in, staging_info::out, io::di, io::uo) is det.

edit_header(Screen, HeaderType, !StagingInfo, !IO) :-
    Headers0 = !.StagingInfo ^ si_headers,
    get_header(HeaderType, Headers0, Prompt, Initial),
    text_entry_initial(Screen, Prompt, init_history, Initial, Return, !IO),
    (
        Return = yes(Final),
        set_header(HeaderType, Final, Headers0, Headers),
        !StagingInfo ^ si_headers := Headers
    ;
        Return = no
    ).

:- pred get_header(header_type::in, headers::in, string::out, string::out)
    is det.

get_header(from,    H, "From: ",     H ^ h_from).
get_header(to,      H, "To: ",       H ^ h_to).
get_header(cc,      H, "Cc: ",       H ^ h_cc).
get_header(bcc,     H, "Bcc: ",      H ^ h_bcc).
get_header(subject, H, "Subject: ",  H ^ h_subject).
get_header(replyto, H, "Reply-To: ", H ^ h_replyto).

:- pred set_header(header_type::in, string::in, headers::in, headers::out)
    is det.

set_header(from,    Value, H, H ^ h_from := Value).
set_header(to,      Value, H, H ^ h_to := Value).
set_header(cc,      Value, H, H ^ h_cc := Value).
set_header(bcc,     Value, H, H ^ h_bcc := Value).
set_header(subject, Value, H, H ^ h_subject := Value).
set_header(replyto, Value, H, H ^ h_replyto := Value).

%-----------------------------------------------------------------------------%

:- pred scroll_attachments(screen::in, int::in, int::in,
    attach_info::in, attach_info::out, io::di, io::uo) is det.

scroll_attachments(Screen, NumRows, Delta, !AttachInfo, !IO) :-
    scrollable.move_cursor(NumRows, Delta, HitLimit, !AttachInfo),
    (
        HitLimit = yes,
        ( Delta < 0 ->
            MessageUpdate = set_warning("You are on the first entry.")
        ;
            MessageUpdate = set_warning("You are on the last entry.")
        )
    ;
        HitLimit = no,
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred add_attachment(screen::in, int::in, attach_info::in, attach_info::out,
    io::di, io::uo) is det.

add_attachment(Screen, NumRows, !AttachInfo, !IO) :-
    text_entry(Screen, "Attach file: ", init_history, Return, !IO),
    (
        Return = yes(FileName),
        do_attach_file(FileName, NumRows, MessageUpdate, !AttachInfo, !IO)
    ;
        Return = no,
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_attach_file(string::in, int::in, message_update::out,
    attach_info::in, attach_info::out, io::di, io::uo) is det.

do_attach_file(FileName, NumRows, MessageUpdate, !AttachInfo, !IO) :-
    lookup_mime_type(FileName, ResMimeType, !IO),
    (
        ResMimeType = ok(MimeType),
        ( dir.basename(FileName, BaseName0) ->
            BaseName = BaseName0
        ;
            BaseName = FileName
        ),
        MimeType = mime_type(Type, Charset),
        ( Charset = "binary" ->
            do_attach_binary_file(FileName, BaseName, Type, NumRows,
                MessageUpdate, !AttachInfo, !IO)
        ; acceptable_charset(Charset) ->
            do_attach_text_file(FileName, BaseName, Type, NumRows,
                MessageUpdate, !AttachInfo, !IO)
        ;
            MessageUpdate = set_warning(
                "Only ASCII and UTF-8 text files supported yet.")
        )
    ;
        ResMimeType = error(Error),
        Msg = io.error_message(Error),
        MessageUpdate = set_warning(Msg)
    ).

:- pred acceptable_charset(string::in) is semidet.

acceptable_charset(Charset) :-
    ( strcase_equal(Charset, "us-ascii")
    ; strcase_equal(Charset, "utf-8")
    ).

:- pred do_attach_text_file(string::in, string::in, string::in, int::in,
    message_update::out, attach_info::in, attach_info::out, io::di, io::uo)
    is det.

do_attach_text_file(FileName, BaseName, Type, NumRows, MessageUpdate,
        !AttachInfo, !IO) :-
    io.open_input(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Input),
        io.read_file_as_string(Input, ResRead, !IO),
        io.close_input(Input, !IO),
        (
            ResRead = ok(Content),
            NewAttachment = new_text_attachment(Type, Content, BaseName),
            append_attachment(NewAttachment, NumRows, !AttachInfo),
            MessageUpdate = clear_message
        ;
            ResRead = error(_, Error),
            string.format("Error reading %s: %s",
                [s(FileName), s(io.error_message(Error))], Msg),
            MessageUpdate = set_warning(Msg)
        )
    ;
        ResOpen = error(Error),
        string.format("Error opening %s: %s",
            [s(FileName), s(io.error_message(Error))], Msg),
        MessageUpdate = set_warning(Msg)
    ).

:- pred do_attach_binary_file(string::in, string::in, string::in, int::in,
    message_update::out, attach_info::in, attach_info::out, io::di, io::uo)
    is det.

do_attach_binary_file(FileName, BaseName, Type, NumRows, MessageUpdate,
        !AttachInfo, !IO) :-
    args_to_quoted_command(["base64", FileName], Command),
    popen(Command, CallRes, !IO),
    (
        CallRes = ok(Content),
        NewAttachment = new_binary_attachment(Type, Content, BaseName),
        append_attachment(NewAttachment, NumRows, !AttachInfo),
        MessageUpdate = clear_message
    ;
        CallRes = error(Error),
        Msg = io.error_message(Error),
        MessageUpdate = set_warning(Msg)
    ).

:- pred append_attachment(attachment::in, int::in,
    attach_info::in, attach_info::out) is det.

append_attachment(NewAttachment, NumRows, !AttachInfo) :-
    scrollable.append_line(NewAttachment, !AttachInfo),
    NumLines = get_num_lines(!.AttachInfo),
    Cursor = NumLines - 1,
    scrollable.set_cursor_visible(Cursor, NumRows, !AttachInfo).

:- pred delete_attachment(screen::in, attach_info::in, attach_info::out,
    io::di, io::uo) is det.

delete_attachment(Screen, !AttachInfo, !IO) :-
    ( scrollable.delete_cursor_line(!AttachInfo) ->
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("There are no attachments to delete.")
    ),
    update_message(Screen, MessageUpdate, !IO).

%-----------------------------------------------------------------------------%

:- pred split_panels(screen::in, list(panel)::out, list(panel)::out,
    maybe(panel)::out, list(panel)::out) is det.

split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels) :-
    Panels0 = Screen ^ main_panels,
    list.split_upto(6, Panels0, HeaderPanels, Panels1),
    list.split_upto(3, Panels1, AttachmentPanels, Panels2),
    (
        Panels2 = [SepPanel | PagerPanels],
        MaybeSepPanel = yes(SepPanel)
    ;
        Panels2 = [],
        MaybeSepPanel = no,
        PagerPanels = []
    ).

:- pred draw_header_lines(list(panel)::in, headers::in, io::di, io::uo) is det.

draw_header_lines(!.Panels, Headers, !IO) :-
    draw_header_line(!Panels, "    From", Headers ^ h_from, !IO),
    draw_header_line(!Panels, "      To", Headers ^ h_to, !IO),
    draw_header_line(!Panels, "      Cc", Headers ^ h_cc, !IO),
    draw_header_line(!Panels, "     Bcc", Headers ^ h_bcc, !IO),
    draw_header_line(!Panels, " Subject", Headers ^ h_subject, !IO),
    draw_header_line(!Panels, "Reply-To", Headers ^ h_replyto, !IO),
    !.Panels = _.

:- pred draw_header_line(list(panel)::in, list(panel)::out,
    string::in, string::in, io::di, io::uo) is det.

draw_header_line([], [], _, _, !IO).
draw_header_line([Panel | Panels], Panels, FieldName, Value, !IO) :-
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(red, black) + bold, !IO),
    my_addstr(Panel, FieldName, !IO),
    my_addstr(Panel, ": ", !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, Value, !IO).

:- pred draw_attachment_line(panel::in, attachment::in, bool::in,
    io::di, io::uo) is det.

draw_attachment_line(Panel, Attachment, IsCursor, !IO) :-
    (
        Attachment = old_attachment(Part),
        Type = Part ^ pt_type,
        MaybeFilename = Part ^ pt_filename,
        (
            MaybeFilename = yes(Filename)
        ;
            MaybeFilename = no,
            Filename = "(no filename)"
        )
    ;
        Attachment = new_text_attachment(Type, _, Filename)
    ;
        Attachment = new_binary_attachment(Type, _, Filename)
    ),
    panel.erase(Panel, !IO),
    panel.move(Panel, 0, 13, !IO),
    (
        IsCursor = yes,
        panel.attr_set(Panel, reverse, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, normal, !IO)
    ),
    my_addstr(Panel, Filename, !IO),
    my_addstr(Panel, " (", !IO),
    my_addstr(Panel, Type, !IO),
    my_addstr(Panel, ")", !IO).

:- pred draw_attachments_label(list(panel)::in, io::di, io::uo) is det.

draw_attachments_label([], !IO).
draw_attachments_label([Panel | _], !IO) :-
    panel.move(Panel, 0, 0, !IO),
    panel.attr_set(Panel, fg_bg(red, black) + bold, !IO),
    my_addstr(Panel, "Attachments: ", !IO).

:- pred draw_sep_bar(screen::in, maybe(panel)::in, io::di, io::uo) is det.

draw_sep_bar(_, no, !IO).
draw_sep_bar(Screen, yes(Panel), !IO) :-
    Cols = Screen ^ cols,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- (ftcbsr) edit fields; (a) attach, (d) detach", !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

:- pred draw_staging_bar(screen::in, io::di, io::uo) is det.

draw_staging_bar(Screen, !IO) :-
    Cols = Screen ^ cols,
    Panel = Screen ^ bar_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- ", !IO),
    Msg = "Compose: (e) edit, (p) postpone, (Y) send, (Q) abandon. ",
    my_addstr_fixed(Panel, Cols - 3, Msg, '-', !IO).

%-----------------------------------------------------------------------------%

:- pred postpone(screen::in, headers::in, string::in, list(attachment)::in,
    bool::out, io::di, io::uo) is det.

postpone(Screen, Headers, Text, Attachments, Res, !IO) :-
    create_temp_message_file(Headers, Text, Attachments, prepare_postpone,
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

:- pred maybe_remove_draft(staging_info::in, io::di, io::uo) is det.

maybe_remove_draft(StagingInfo, !IO) :-
    MaybeOldDraft = StagingInfo ^ si_old_msgid,
    (
        MaybeOldDraft = yes(MessageId),
        tag_messages(["+deleted"], [MessageId], _Res, !IO)
    ;
        MaybeOldDraft = no
    ).

%-----------------------------------------------------------------------------%

:- pred send_mail(screen::in, headers::in, string::in, list(attachment)::in,
    bool::out, io::di, io::uo) is det.

send_mail(Screen, Headers, Text, Attachments, Res, !IO) :-
    create_temp_message_file(Headers, Text, Attachments, prepare_send,
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
            add_sent(Filename, ResAdd, !IO),
            (
                ResAdd = ok,
                update_message(Screen, set_info("Mail sent."), !IO),
                Res = yes
            ;
                ResAdd = error(Error),
                Msg = "Mail sent, but " ++ io.error_message(Error),
                update_message(Screen, set_warning(Msg), !IO),
                Res = no
            )
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

:- pred tag_replied_message(screen::in, headers::in, io::di, io::uo) is det.

tag_replied_message(Screen, Headers, !IO) :-
    InReplyTo0 = Headers ^ h_inreplyto,
    (
        string.index(InReplyTo0, 0, '<'),
        Length = string.count_codepoints(InReplyTo0),
        string.codepoint_offset(InReplyTo0, Length - 1, LastPos),
        string.index(InReplyTo0, LastPos, '>')
    ->
        string.between(InReplyTo0, 1, LastPos, Id),
        MessageId = message_id(Id),
        tag_messages(["+replied"], [MessageId], TagRes, !IO),
        (
            TagRes = ok
        ;
            TagRes = error(Error),
            Msg = io.error_message(Error),
            update_message(Screen, set_warning(Msg), !IO)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- type prepare_temp
    --->    prepare_send
    ;       prepare_edit
    ;       prepare_postpone.

:- type mime
    --->    mime_single_part
    ;       mime_multipart(string). % boundary

:- pred create_temp_message_file(headers::in, string::in, list(attachment)::in,
    prepare_temp::in, io.res(string)::out, io::di, io::uo) is det.

create_temp_message_file(Headers, Text, Attachments, Prepare, Res, !IO) :-
    io.make_temp(Filename, !IO),
    io.open_output(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        Headers = headers(_Date, From, To, Cc, Bcc, Subject, ReplyTo,
            References, InReplyTo, RestHeaders),
        % XXX detect charset
        Charset = "utf-8",
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            )
        ->
            (
                Attachments = [],
                % This is only really necessary if the body is non-ASCII.
                MIME = yes(mime_single_part)
            ;
                Attachments = [_ | _],
                generate_boundary(BoundaryA, !IO),
                MIME = yes(mime_multipart(BoundaryA))
            )
        ;
            MIME = no
        ),
        (
            Prepare = prepare_send,
            generate_date_msg_id(Date, MessageId, !IO),
            write_header(Stream, "Date", Date, !IO),
            write_header(Stream, "Message-ID", MessageId, !IO),
            Write = write_header_opt(Stream)
        ;
            ( Prepare = prepare_edit
            ; Prepare = prepare_postpone
            ),
            Write = write_header(Stream)
        ),
        Write("From", From, !IO),
        Write("To", To, !IO),
        Write("Cc", Cc, !IO),
        Write("Bcc", Bcc, !IO),
        Write("Subject", Subject, !IO),
        Write("Reply-To", ReplyTo, !IO),
        Write("In-Reply-To", InReplyTo, !IO),
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            Write("References", References, !IO)
        ;
            Prepare = prepare_edit
        ),
        map.foldl(Write, RestHeaders, !IO),
        (
            MIME = no
        ;
            MIME = yes(_),
            write_mime_version(Stream, !IO),
            (
                MIME = yes(mime_single_part),
                write_content_type(Stream, "text/plain", yes(Charset), !IO)
            ;
                MIME = yes(mime_multipart(BoundaryB)),
                write_content_type_multipart_mixed(Stream, BoundaryB, !IO)
            ),
            write_content_disposition_inline(Stream, !IO),
            write_content_transfer_encoding(Stream, "8bit", !IO)
        ),

        % End header fields.
        io.nl(Stream, !IO),

        % Begin body.
        (
            MIME = no,
            io.write_string(Stream, Text, !IO)
        ;
            MIME = yes(mime_single_part),
            io.write_string(Stream, Text, !IO)
        ;
            MIME = yes(mime_multipart(BoundaryC)),
            write_mime_part_boundary(Stream, BoundaryC, !IO),
            write_mime_part_text(Stream, Charset, Text, !IO),
            list.foldl(write_mime_part_attachment(Stream, BoundaryC),
                Attachments, !IO),
            write_mime_final_boundary(Stream, BoundaryC, !IO)
        ),

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
    % XXX quote if non-ASCII
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

:- pred write_mime_version(io.output_stream::in, io::di, io::uo) is det.

write_mime_version(Stream, !IO) :-
    io.write_string(Stream, "MIME-Version: 1.0\n", !IO).

:- pred write_content_type(io.output_stream::in, string::in,
    maybe(string)::in, io::di, io::uo) is det.

write_content_type(Stream, Type, MaybeCharset, !IO) :-
    io.write_string(Stream, "Content-Type: ", !IO),
    io.write_string(Stream, Type, !IO),
    (
        MaybeCharset = yes(Charset),
        io.write_string(Stream, "; charset=", !IO),
        io.write_string(Stream, Charset, !IO)
    ;
        MaybeCharset = no
    ),
    io.write_string(Stream, "\n", !IO).

:- pred write_content_type_multipart_mixed(io.output_stream::in, string::in,
    io::di, io::uo) is det.

write_content_type_multipart_mixed(Stream, Boundary, !IO) :-
    io.write_string(Stream, "Content-Type: multipart/mixed; boundary=""", !IO),
    io.write_string(Stream, Boundary, !IO),
    io.write_string(Stream, """\n", !IO).

:- pred write_content_disposition_inline(io.output_stream::in,
    io::di, io::uo) is det.

write_content_disposition_inline(Stream, !IO) :-
    io.write_string(Stream, "Content-Disposition: inline\n", !IO).

:- pred write_content_disposition_attachment(io.output_stream::in,
    maybe(string)::in, io::di, io::uo) is det.

write_content_disposition_attachment(Stream, MaybeFileName, !IO) :-
    io.write_string(Stream, "Content-Disposition: attachment", !IO),
    (
        MaybeFileName = yes(FileName),
        % XXX quote if non-ASCII
        io.write_string(Stream, "; filename=""", !IO),
        io.write_string(Stream, FileName, !IO),
        io.write_string(Stream, """", !IO)
    ;
        MaybeFileName = no
    ),
    io.nl(Stream, !IO).

:- pred write_content_transfer_encoding(io.output_stream::in,
    string::in, io::di, io::uo) is det.

write_content_transfer_encoding(Stream, CTE, !IO) :-
    io.write_string(Stream, "Content-Transfer-Encoding: ", !IO),
    io.write_string(Stream, CTE, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred write_mime_part_boundary(io.output_stream::in, string::in,
    io::di, io::uo) is det.

write_mime_part_boundary(Stream, Boundary, !IO) :-
    io.write_string(Stream, "\n--", !IO),
    io.write_string(Stream, Boundary, !IO),
    io.nl(Stream, !IO).

:- pred write_mime_final_boundary(io.output_stream::in, string::in,
    io::di, io::uo) is det.

write_mime_final_boundary(Stream, Boundary, !IO) :-
    io.write_string(Stream, "\n--", !IO),
    io.write_string(Stream, Boundary, !IO),
    io.write_string(Stream, "--\n", !IO).

:- pred write_mime_part_text(io.output_stream::in, string::in, string::in,
    io::di, io::uo) is det.

write_mime_part_text(Stream, Charset, Text, !IO) :-
    write_content_type(Stream, "text/plain", yes(Charset), !IO),
    write_content_disposition_inline(Stream, !IO),
    write_content_transfer_encoding(Stream, "8bit", !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, Text, !IO).

:- pred write_mime_part_attachment(io.output_stream::in, string::in,
    attachment::in, io::di, io::uo) is det.

write_mime_part_attachment(Stream, Boundary, Attachment, !IO) :-
    (
        Attachment = old_attachment(Part),
        Type = Part ^ pt_type,
        MaybeContent = Part ^ pt_content,
        (
            MaybeContent = yes(Content),
            CTE = "8bit"
        ;
            MaybeContent = no,
            CTE = "base64",
            get_non_text_part_base64(Part, Content, !IO)
        ),
        MaybeFileName = Part ^ pt_filename
    ;
        Attachment = new_text_attachment(Type, Content, FileName),
        MaybeFileName = yes(FileName),
        CTE = "8bit"
    ;
        Attachment = new_binary_attachment(Type, Content, FileName),
        MaybeFileName = yes(FileName),
        CTE = "base64"
    ),

    write_mime_part_boundary(Stream, Boundary, !IO),
    ( CTE = "base64" ->
        write_content_type(Stream, Type, no, !IO)
    ;
        % XXX detect charset
        Charset = "utf-8",
        write_content_type(Stream, Type, yes(Charset), !IO)
    ),
    write_content_disposition_attachment(Stream, MaybeFileName, !IO),
    write_content_transfer_encoding(Stream, CTE, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, Content, !IO).

:- pred get_non_text_part_base64(part::in, string::out, io::di, io::uo) is det.

get_non_text_part_base64(Part, Content, !IO) :-
    Part = part(MessageId, PartId, _, _, _),
    args_to_quoted_command([
        "notmuch", "show", "--format=raw", "--part=" ++ from_int(PartId),
        message_id_to_search_term(MessageId)
    ], Command),
    popen(Command ++ " |base64", CallRes, !IO),
    (
        CallRes = ok(Content)
    ;
        CallRes = error(Error),
        % XXX handle this gracefully
        unexpected($module, $pred, io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%

:- pred generate_boundary(string::out, io::di, io::uo) is det.

generate_boundary(Boundary, !IO) :-
    % This emulates the boundaries generated by Mutt.
    time(Time, !IO),
    time_to_int(Time, Seed),
    random.init(Seed, RS),
    Len = 16,
    list.map_foldl(generate_boundary_char, 1 .. Len, Chars, RS, _RS),
    string.from_char_list(Chars, Boundary).

:- pred generate_boundary_char(int::in, char::out,
    random.supply::mdi, random.supply::muo) is det.

generate_boundary_char(_, Char, !RS) :-
    random.random(0, 64, Index, !RS),
    string.unsafe_index(base64_chars, Index, Char).

:- func base64_chars = string.

base64_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
