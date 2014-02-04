% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module compose.
:- interface.

:- import_module io.

:- import_module data.
:- import_module screen.
:- import_module text_entry.

:- type reply_kind
    --->    direct_reply
    ;       group_reply
    ;       list_reply.

:- type sent
    --->    sent
    ;       not_sent.

:- pred start_compose(screen::in, screen_transition(sent)::out,
    history::in, history::out, history::in, history::out, io::di, io::uo)
    is det.

:- pred start_reply(screen::in, message::in, reply_kind::in,
    screen_transition(sent)::out, io::di, io::uo) is det.

:- pred start_reply_to_message_id(screen::in, message_id::in, reply_kind::in,
    screen_transition(sent)::out, io::di, io::uo) is det.

:- pred continue_postponed(screen::in, message::in,
    screen_transition(sent)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module addressbook.
:- import_module callout.
:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module message_file.
:- import_module mime_type.
:- import_module pager.
:- import_module path_expand.
:- import_module call_system.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module scrollable.
:- import_module send_util.
:- import_module string_util.
:- import_module tags.

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
                si_old_msgid    :: maybe(message_id),
                si_attach_hist  :: history
            ).

:- type attach_info == scrollable(attachment).

:- type attachment
    --->    old_attachment(part)
    ;       new_attachment(
                att_type        :: string,
                att_content     :: attachment_content,
                att_filename    :: string,
                att_size        :: int
            ).

:- type attachment_content
    --->    text(string)
    ;       binary_base64(string).

:- type staging_screen_action
    --->    continue
    ;       resize
    ;       edit
    ;       leave(sent, message_update).

:- type call_res
    --->    ok
    ;       error(string).

:- instance scrollable.line(attachment) where [
    pred(draw_line/6) is draw_attachment_line
].

%-----------------------------------------------------------------------------%

start_compose(Screen, Transition, !ToHistory, !SubjectHistory, !IO) :-
    get_from(From, !IO),
    text_entry_initial(Screen, "To: ", !.ToHistory, "",
        complete_config_key(addressbook_section), MaybeTo, !IO),
    (
        MaybeTo = yes(To),
        add_history_nodup(To, !ToHistory),
        text_entry_initial(Screen, "Subject: ", !.SubjectHistory, "",
            complete_none, MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            add_history_nodup(Subject, !SubjectHistory),
            expand_aliases(To, ExpandTo, !IO),
            some [!Headers] (
                !:Headers = init_headers,
                !Headers ^ h_from := From,
                !Headers ^ h_to := ExpandTo,
                !Headers ^ h_subject := Subject,
                Headers = !.Headers
            ),
            Text = "",
            Attachments = [],
            MaybeOldDraft = no,
            create_edit_stage(Screen, Headers, Text, Attachments,
                MaybeOldDraft, Transition, !IO)
        ;
            MaybeSubject = no,
            Transition = screen_transition(not_sent, no_change)
        )
    ;
        MaybeTo = no,
        Transition = screen_transition(not_sent, no_change)
    ).

%-----------------------------------------------------------------------------%

start_reply(Screen, Message, ReplyKind, Transition, !IO) :-
    Message ^ m_id = MessageId,
    Args = ["reply", message_id_to_search_term(MessageId)],
    args_to_quoted_command(Args, Command),
    get_notmuch_prefix(Notmuch, !IO),
    call_system_capture_stdout(Notmuch ++ Command, CommandResult, !IO),
    (
        CommandResult = ok(String),
        parse_message(String, Headers0, Text),
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
            Transition, !IO)
    ;
        CommandResult = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        Transition = screen_transition(not_sent, set_warning(Warning))
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

start_reply_to_message_id(Screen, MessageId, ReplyKind, Sent, !IO) :-
    run_notmuch([
        "show", "--format=json", "--part=0", "--",
        message_id_to_search_term(MessageId)
    ], parse_top_message, Res, !IO),
    (
        Res = ok(Message),
        start_reply(Screen, Message, ReplyKind, Sent, !IO)
    ;
        Res = error(Error),
        unexpected($module, $pred, Error)
    ).

%-----------------------------------------------------------------------------%

continue_postponed(Screen, Message, Transition, !IO) :-
    MessageId = Message ^ m_id,
    Headers0 = Message ^ m_headers,
    Body0 = Message ^ m_body,
    first_text_part(Body0, Text, AttachmentParts),
    list.map(to_old_attachment, AttachmentParts, Attachments),

    % XXX notmuch show --format=json does not return References and In-Reply-To
    % so we parse them from the raw output.
    args_to_quoted_command([
        "show", "--format=raw", "--", message_id_to_search_term(MessageId)
    ], Command),
    get_notmuch_prefix(Notmuch, !IO),
    call_system_capture_stdout(Notmuch ++ Command, CallRes, !IO),
    (
        CallRes = ok(String),
        parse_message(String, HeadersB, _Body),
        some [!Headers] (
            !:Headers = Headers0,
            !Headers ^ h_replyto := (HeadersB ^ h_replyto),
            !Headers ^ h_references := (HeadersB ^ h_references),
            !Headers ^ h_inreplyto := (HeadersB ^ h_inreplyto),
            Headers = !.Headers
        ),
        create_edit_stage(Screen, Headers, Text, Attachments, yes(MessageId),
            Transition, !IO)
    ;
        CallRes = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        Transition = screen_transition(not_sent, set_warning(Warning))
    ).

:- pred first_text_part(list(part)::in, string::out, list(part)::out)
    is det.

first_text_part([], "", []).
first_text_part([Part | Parts], Text, AttachmentParts) :-
    PartContent = Part ^ pt_content,
    (
        PartContent = text(Text),
        AttachmentParts = Parts
    ;
        PartContent = subparts(SubParts),
        first_text_part(SubParts, Text, AttachmentParts)
    ;
        ( PartContent = encapsulated_messages(_)
        ; PartContent = unsupported
        ),
        first_text_part(Parts, Text, AttachmentParts0),
        AttachmentParts = [Part | AttachmentParts0]
    ).

:- pred to_old_attachment(part::in, attachment::out) is det.

to_old_attachment(Part, old_attachment(Part)).

%-----------------------------------------------------------------------------%

:- pred create_edit_stage(screen::in, headers::in, string::in,
    list(attachment)::in, maybe(message_id)::in, screen_transition(sent)::out,
    io::di, io::uo) is det.

create_edit_stage(Screen, Headers0, Text0, Attachments, MaybeOldDraft,
        Transition, !IO) :-
    create_temp_message_file(Headers0, Text0, Attachments, prepare_edit,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_editor(Filename, ResEdit, !IO),
        (
            ResEdit = ok,
            parse_message_file(Filename, ResParse, !IO),
            (
                ResParse = ok(Headers1 - Text),
                io.remove_file(Filename, _, !IO),
                update_references(Headers0, Headers1, Headers2),
                expand_aliases_after_edit(Headers2, Headers, !IO),
                StagingInfo = staging_info(Headers, Text, MaybeOldDraft,
                    init_history),
                AttachInfo = scrollable.init_with_cursor(Attachments),
                get_cols(Screen, Cols),
                setup_pager_for_staging(Cols, Text, new_pager, PagerInfo),
                staging_screen(Screen, StagingInfo, AttachInfo, PagerInfo,
                    Transition, !IO)
            ;
                ResParse = error(Error),
                io.error_message(Error, Msg),
                Transition = screen_transition(not_sent, set_warning(Msg))
            )
        ;
            ResEdit = error(Msg),
            Transition = screen_transition(not_sent, set_warning(Msg))
        )
    ;
        ResFilename = error(Error),
        Transition = screen_transition(not_sent, set_warning(Error))
    ).

:- pred call_editor(string::in, call_res::out, io::di, io::uo) is det.

call_editor(Filename, Res, !IO) :-
    get_editor_command(Editor, !IO),
    curs.def_prog_mode(!IO),
    curs.stop(!IO),
    io.call_system(Editor ++ " " ++ quote_arg(Filename), CallRes, !IO),
    curs.reset_prog_mode(!IO),
    curs.refresh(!IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("%s returned exit status %d",
                [s(Editor), i(ExitStatus)], Warning),
            Res = error(Warning)
        )
    ;
        CallRes = error(Error),
        string.append_list(["Error running ", Editor, ": ",
            io.error_message(Error)], Warning),
        Res = error(Warning)
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

:- pred expand_aliases_after_edit(headers::in, headers::out, io::di, io::uo)
    is det.

expand_aliases_after_edit(!Headers, !IO) :-
    To0 = !.Headers ^ h_to,
    Cc0 = !.Headers ^ h_cc,
    Bcc0 = !.Headers ^ h_bcc,
    expand_aliases(To0, To, !IO),
    expand_aliases(Cc0, Cc, !IO),
    expand_aliases(Bcc0, Bcc, !IO),
    !Headers ^ h_to := To,
    !Headers ^ h_cc := Cc,
    !Headers ^ h_bcc := Bcc.

%-----------------------------------------------------------------------------%

:- pred staging_screen(screen::in, staging_info::in, attach_info::in,
    pager_info::in, screen_transition(sent)::out, io::di, io::uo) is det.

staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, Transition,
        !IO) :-
    !.StagingInfo = staging_info(Headers, Text, MaybeOldDraft, _AttachHistory),
    split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels),
    draw_header_lines(HeaderPanels, Headers, !IO),
    scrollable.draw(AttachmentPanels, !.AttachInfo, !IO),
    draw_attachments_label(AttachmentPanels, !IO),
    draw_sep_bar(Screen, MaybeSepPanel, !IO),
    draw_pager_lines(PagerPanels, !.PagerInfo, !IO),
    draw_staging_bar(Screen, !.StagingInfo, !IO),
    panel.update_panels(!IO),
    NumAttachmentRows = list.length(AttachmentPanels),
    NumPagerRows = list.length(PagerPanels),
    get_keycode_blocking(KeyCode, !IO),
    ( KeyCode = char('e') ->
        Action = edit
    ; KeyCode = char('f') ->
        edit_header(Screen, from, !StagingInfo, !IO),
        Action = continue
    ; KeyCode = char('t') ->
        edit_header(Screen, to, !StagingInfo, !IO),
        Action = continue
    ; KeyCode = char('c') ->
        edit_header(Screen, cc, !StagingInfo, !IO),
        Action = continue
    ; KeyCode = char('b') ->
        edit_header(Screen, bcc, !StagingInfo, !IO),
        Action = continue
    ; KeyCode = char('s') ->
        edit_header(Screen, subject, !StagingInfo, !IO),
        Action = continue
    ; KeyCode = char('r') ->
        edit_header(Screen, replyto, !StagingInfo, !IO),
        Action = continue
    ;
        ( KeyCode = char('j')
        ; KeyCode = code(key_down)
        )
    ->
        scroll_attachments(Screen, NumAttachmentRows, 1, !AttachInfo, !IO),
        Action = continue
    ;
        ( KeyCode = char('k')
        ; KeyCode = code(key_up)
        )
    ->
        scroll_attachments(Screen, NumAttachmentRows, -1, !AttachInfo, !IO),
        Action = continue
    ; KeyCode = char('a') ->
        add_attachment(Screen, NumAttachmentRows, !StagingInfo, !AttachInfo,
            !IO),
        Action = continue
    ; KeyCode = char('d') ->
        delete_attachment(Screen, !AttachInfo, !IO),
        Action = continue
    ; KeyCode = char('T') ->
        edit_attachment_type(Screen, !AttachInfo, !IO),
        Action = continue
    ; KeyCode = char('p') ->
        Attachments = get_lines_list(!.AttachInfo),
        postpone(Screen, Headers, Text, Attachments, Res, PostponeMsg, !IO),
        (
            Res = yes,
            maybe_remove_draft(!.StagingInfo, !IO),
            Action = leave(not_sent, PostponeMsg)
        ;
            Res = no,
            update_message(Screen, PostponeMsg, !IO),
            Action = continue
        )
    ; KeyCode = char('Y') ->
        Attachments = get_lines_list(!.AttachInfo),
        send_mail(Screen, Headers, Text, Attachments, Sent0, MessageUpdate0,
            !IO),
        (
            Sent0 = sent,
            tag_replied_message(Headers, TagRes, !IO),
            maybe_remove_draft(!.StagingInfo, !IO),
            (
                TagRes = ok,
                MessageUpdate = MessageUpdate0
            ;
                TagRes = error(TagError),
                MessageUpdate = set_warning(TagError)
            ),
            Action = leave(sent, MessageUpdate)
        ;
            Sent0 = not_sent,
            update_message(Screen, MessageUpdate0, !IO),
            Action = continue
        )
    ; KeyCode = char('D') ->
        % XXX prompt to discard
        (
            MaybeOldDraft = yes(_),
            Message = "Message discarded (older postponed message kept).",
            Action = leave(not_sent, set_info(Message))
        ;
            MaybeOldDraft = no,
            Message = "Not editing a postponed message.",
            update_message(Screen, set_warning(Message), !IO),
            Action = continue
        )
    ; KeyCode = char('Q') ->
        % XXX prompt to abandon
        (
            MaybeOldDraft = yes(_),
            maybe_remove_draft(!.StagingInfo, !IO),
            Message = set_info("Postponed message deleted."),
            Action = leave(not_sent, Message)
        ;
            MaybeOldDraft = no,
            Message = set_info("Mail not sent."),
            Action = leave(not_sent, Message)
        )
    ; KeyCode = code(key_resize) ->
        Action = resize
    ;
        pager_input(NumPagerRows, KeyCode, _Action, MessageUpdate, !PagerInfo),
        update_message(Screen, MessageUpdate, !IO),
        Action = continue
    ),
    (
        Action = continue,
        staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo,
            Transition, !IO)
    ;
        Action = resize,
        resize_staging_screen(Screen, NewScreen, !.StagingInfo, !PagerInfo,
            !IO),
        staging_screen(NewScreen, !.StagingInfo, !.AttachInfo, !.PagerInfo,
            Transition, !IO)
    ;
        Action = edit,
        EditAttachments = get_lines_list(!.AttachInfo),
        create_edit_stage(Screen, Headers, Text, EditAttachments,
            MaybeOldDraft, Transition, !IO)
    ;
        Action = leave(Sent, TransitionMessage),
        Transition = screen_transition(Sent, TransitionMessage)
    ).

:- pred resize_staging_screen(screen::in, screen::out, staging_info::in,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

resize_staging_screen(Screen0, Screen, StagingInfo, PagerInfo0, PagerInfo,
        !IO) :-
    replace_screen_for_resize(Screen0, Screen, !IO),
    get_cols(Screen, Cols),
    split_panels(Screen, _HeaderPanels, _AttachmentPanels, _MaybeSepPanel,
        PagerPanels),
    NumPagerRows = list.length(PagerPanels),
    Text = StagingInfo ^ si_text,
    setup_pager_for_staging(Cols, Text,
        retain_pager_pos(PagerInfo0, NumPagerRows), PagerInfo).

%-----------------------------------------------------------------------------%

:- pred edit_header(screen::in, header_type::in,
    staging_info::in, staging_info::out, io::di, io::uo) is det.

edit_header(Screen, HeaderType, !StagingInfo, !IO) :-
    Headers0 = !.StagingInfo ^ si_headers,
    get_header(HeaderType, Headers0, Prompt, Initial, ExpandAddresses),
    (
        ExpandAddresses = yes,
        Completion = complete_config_key(addressbook_section)
    ;
        ExpandAddresses = no,
        Completion = complete_none
    ),
    text_entry_initial(Screen, Prompt, init_history, Initial, Completion,
        Return, !IO),
    (
        Return = yes(Value0),
        (
            ExpandAddresses = yes,
            expand_aliases(Value0, Value, !IO)
        ;
            ExpandAddresses = no,
            Value = Value0
        ),
        set_header(HeaderType, Value, Headers0, Headers),
        !StagingInfo ^ si_headers := Headers
    ;
        Return = no
    ).

:- pred get_header(header_type::in, headers::in, string::out, string::out,
    bool::out) is det.

get_header(from,    H, "From: ",     H ^ h_from,    no).
get_header(to,      H, "To: ",       H ^ h_to,      yes).
get_header(cc,      H, "Cc: ",       H ^ h_cc,      yes).
get_header(bcc,     H, "Bcc: ",      H ^ h_bcc,     yes).
get_header(subject, H, "Subject: ",  H ^ h_subject, no).
get_header(replyto, H, "Reply-To: ", H ^ h_replyto, no).

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

:- pred add_attachment(screen::in, int::in,
    staging_info::in, staging_info::out, attach_info::in, attach_info::out,
    io::di, io::uo) is det.

add_attachment(Screen, NumRows, !StagingInfo, !AttachInfo, !IO) :-
    AttachHistory0 = !.StagingInfo ^ si_attach_hist,
    get_home_dir(Home, !IO),
    text_entry(Screen, "Attach file: ", AttachHistory0, complete_path(Home),
        Return, !IO),
    (
        Return = yes(FileName0),
        FileName0 \= ""
    ->
        add_history_nodup(FileName0, AttachHistory0, AttachHistory),
        !StagingInfo ^ si_attach_hist := AttachHistory,
        expand_tilde_home(Home, FileName0, FileName),
        do_attach_file(FileName, NumRows, MessageUpdate, !AttachInfo, !IO)
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred do_attach_file(string::in, int::in, message_update::out,
    attach_info::in, attach_info::out, io::di, io::uo) is det.

do_attach_file(FileName, NumRows, MessageUpdate, !AttachInfo, !IO) :-
    FollowSymLinks = yes,
    io.file_type(FollowSymLinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            ( FileType = regular_file
            ; FileType = unknown
            ),
            io.check_file_accessibility(FileName, [read], ResAccess, !IO),
            (
                ResAccess = ok,
                do_attach_file_2(FileName, NumRows, MessageUpdate,
                    !AttachInfo, !IO)
            ;
                ResAccess = error(Error),
                MessageUpdate = set_warning(io.error_message(Error))
            )
        ;
            ( FileType = directory
            ; FileType = symbolic_link
            ; FileType = named_pipe
            ; FileType = socket
            ; FileType = character_device
            ; FileType = block_device
            ; FileType = message_queue
            ; FileType = semaphore
            ; FileType = shared_memory
            ),
            MessageUpdate = set_warning("Not a regular file.")
        )
    ;
        ResFileType = error(Error),
        MessageUpdate = set_warning(io.error_message(Error))
    ).

:- pred do_attach_file_2(string::in, int::in, message_update::out,
    attach_info::in, attach_info::out, io::di, io::uo) is det.

do_attach_file_2(FileName, NumRows, MessageUpdate, !AttachInfo, !IO) :-
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
            string.length(Content, Size),
            NewAttachment = new_attachment(Type, text(Content), BaseName,
                Size),
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
    call_system_capture_stdout(Command, CallRes, !IO),
    (
        CallRes = ok(Content),
        string.length(Content, Size),
        NewAttachment = new_attachment(Type, binary_base64(Content), BaseName,
            Size),
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

:- pred edit_attachment_type(screen::in, attach_info::in, attach_info::out,
    io::di, io::uo) is det.

edit_attachment_type(Screen, !AttachInfo, !IO) :-
    ( scrollable.get_cursor_line(!.AttachInfo, _Line, Attachment0) ->
        (
            Attachment0 = new_attachment(Type0, Content, FileName, Size),
            % Supply some useful media types.
            History0 = init_history,
            add_history_nodup("application/octet-stream", History0, History1),
            add_history_nodup("text/plain", History1, History),
            text_entry_initial(Screen, "Media type: ", History, Type0,
                complete_none, Return, !IO),
            (
                Return = yes(Type),
                Type \= ""
            ->
                ( accept_media_type(Type) ->
                    Attachment = new_attachment(Type, Content, FileName, Size),
                    scrollable.set_cursor_line(Attachment, !AttachInfo),
                    MessageUpdate = clear_message
                ;
                    Msg = "Refusing to set media type: " ++ Type,
                    MessageUpdate = set_warning(Msg)
                )
            ;
                MessageUpdate = clear_message
            )
        ;
            Attachment0 = old_attachment(_),
            Msg = "Modifying type of old attachments is not yet supported.",
            MessageUpdate = set_warning(Msg)
        )
    ;
        MessageUpdate = set_warning("There are no attachments.")
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred accept_media_type(string::in) is semidet.

accept_media_type(String) :-
    [Type, SubType] = string.split_at_char('/', String),
    string.to_lower(Type, LowerType),
    ( LowerType = "application"
    ; LowerType = "audio"
    ; LowerType = "image"
   %; LowerType = "message"
    ; LowerType = "model"
   %; LowerType = "multipart"
    ; LowerType = "text"
    ; LowerType = "video"
    ),
    SubType \= "",
    string.all_match(token_char, SubType).

% RFC 2616, Section 2.2 Basic Rules.

:- pred token_char(char::in) is semidet.

token_char(C) :-
    char.to_int(C, Int),
    31 < Int, Int < 127,    % not CTL
    not separator_char(C).

:- pred separator_char(char::in) is semidet.

separator_char('(').
separator_char(')').
separator_char('<').
separator_char('>').
separator_char('@').
separator_char(',').
separator_char(';').
separator_char(':').
separator_char('\\').
separator_char('"').
separator_char('/').
separator_char('[').
separator_char(']').
separator_char('?').
separator_char('=').
separator_char('{').
separator_char('}').
separator_char(' ').
separator_char('\t').

%-----------------------------------------------------------------------------%

:- pred split_panels(screen::in, list(panel)::out, list(panel)::out,
    maybe(panel)::out, list(panel)::out) is det.

split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels) :-
    get_main_panels(Screen, Panels0),
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
    panel.attr_set(Panel, fg_bg(red, default) + bold, !IO),
    my_addstr(Panel, FieldName, !IO),
    my_addstr(Panel, ": ", !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, Value, !IO).

:- pred draw_attachment_line(panel::in, attachment::in, int::in, bool::in,
    io::di, io::uo) is det.

draw_attachment_line(Panel, Attachment, LineNr, IsCursor, !IO) :-
    (
        Attachment = old_attachment(Part),
        Type = Part ^ pt_type,
        MaybeFilename = Part ^ pt_filename,
        (
            MaybeFilename = yes(Filename)
        ;
            MaybeFilename = no,
            Filename = "(no filename)"
        ),
        Size = -1
    ;
        Attachment = new_attachment(Type, _, Filename, Size)
    ),
    panel.erase(Panel, !IO),
    panel.move(Panel, 0, 10, !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, format("%d. ", [i(LineNr + 1)]), !IO),
    (
        IsCursor = yes,
        panel.attr_set(Panel, reverse, !IO)
    ;
        IsCursor = no,
        panel.attr_set(Panel, normal, !IO)
    ),
    my_addstr(Panel, Filename, !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, " (", !IO),
    my_addstr(Panel, Type, !IO),
    my_addstr(Panel, ")", !IO),
    ( Size >= 1024 * 1024 ->
        SizeM = float(Size) / (1024.0 * 1024.0),
        my_addstr(Panel, format(" %.1f MiB", [f(SizeM)]), !IO)
    ; Size >= 1024 ->
        SizeK = float(Size) / 1024.0,
        my_addstr(Panel, format(" %.1f KiB", [f(SizeK)]), !IO)
    ; Size >= 0 ->
        my_addstr(Panel, format(" %d bytes", [i(Size)]), !IO)
    ;
        true
    ).

:- pred draw_attachments_label(list(panel)::in, io::di, io::uo) is det.

draw_attachments_label([], !IO).
draw_attachments_label([Panel | _], !IO) :-
    panel.move(Panel, 0, 0, !IO),
    panel.attr_set(Panel, fg_bg(red, default) + bold, !IO),
    my_addstr(Panel, "  Attach: ", !IO).

:- pred draw_sep_bar(screen::in, maybe(panel)::in, io::di, io::uo) is det.

draw_sep_bar(_, no, !IO).
draw_sep_bar(Screen, yes(Panel), !IO) :-
    get_cols(Screen, Cols),
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- (ftcbsr) edit fields; (a) attach, (d) detach, "
        ++ "(T) edit attachment type ", !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

:- pred draw_staging_bar(screen::in, staging_info::in, io::di, io::uo) is det.

draw_staging_bar(Screen, StagingInfo, !IO) :-
    MaybeOldDraft = StagingInfo ^ si_old_msgid,
    get_cols(Screen, Cols),
    get_bar_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "-- ", !IO),
    (
        MaybeOldDraft = yes(_),
        Msg = "Compose: (e) edit, (p) postpone, (Y) send, " ++
            "(D) discard new changes, (Q) abandon."
    ;
        MaybeOldDraft = no,
        Msg = "Compose: (e) edit, (p) postpone, (Y) send, (Q) abandon."
    ),
    my_addstr_fixed(Panel, Cols - 3, Msg, '-', !IO).

%-----------------------------------------------------------------------------%

:- pred postpone(screen::in, headers::in, string::in, list(attachment)::in,
    bool::out, message_update::out, io::di, io::uo) is det.

postpone(Screen, Headers, Text, Attachments, Res, MessageUpdate, !IO) :-
    create_temp_message_file(Headers, Text, Attachments, prepare_postpone,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        update_message_immed(Screen, set_info("Postponing message..."), !IO),
        add_draft(Filename, DraftRes, !IO),
        io.remove_file(Filename, _, !IO),
        (
            DraftRes = ok,
            MessageUpdate = set_info("Message postponed."),
            Res = yes
        ;
            DraftRes = error(Error),
            MessageUpdate = set_warning(Error),
            Res = no
        )
    ;
        ResFilename = error(Error),
        MessageUpdate = set_warning(Error),
        Res = no
    ).

:- pred maybe_remove_draft(staging_info::in, io::di, io::uo) is det.

maybe_remove_draft(StagingInfo, !IO) :-
    MaybeOldDraft = StagingInfo ^ si_old_msgid,
    (
        MaybeOldDraft = yes(MessageId),
        tag_messages([tag_delta("+deleted")], [MessageId], _Res, !IO)
    ;
        MaybeOldDraft = no
    ).

%-----------------------------------------------------------------------------%

:- pred send_mail(screen::in, headers::in, string::in, list(attachment)::in,
    sent::out, message_update::out, io::di, io::uo) is det.

send_mail(Screen, Headers, Text, Attachments, Res, MessageUpdate, !IO) :-
    create_temp_message_file(Headers, Text, Attachments, prepare_send,
        ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        update_message_immed(Screen, set_info("Sending message..."), !IO),
        call_send_mail(Filename, SendRes, !IO),
        io.remove_file(Filename, _, !IO),
        (
            SendRes = ok,
            MessageUpdate = set_info("Mail sent."),
            Res = sent
        ;
            SendRes = error(Message),
            MessageUpdate = set_warning(Message),
            Res = not_sent
        )
    ;
        ResFilename = error(Error),
        MessageUpdate = set_warning(Error),
        Res = not_sent
    ).

:- pred call_send_mail(string::in, call_res::out, io::di, io::uo) is det.

call_send_mail(Filename, Res, !IO) :-
    get_sendmail_command(sendmail_read_recipients, Command, !IO),
    io.call_system(Command ++ " < " ++ quote_arg(Filename), ResSend, !IO),
    (
        ResSend = ok(ExitStatus),
        ( ExitStatus = 0 ->
            call_post_sendmail_command(Filename, ResAfter, !IO),
            (
                ResAfter = ok,
                Res = ok
            ;
                ResAfter = error(Error),
                Res = error("Mail sent, but " ++ Error)
            )
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

:- pred call_post_sendmail_command(string::in, maybe_error::out,
    io::di, io::uo) is det.

call_post_sendmail_command(Filename, Res, !IO) :-
    get_maybe_post_sendmail_command(MaybeCommand, !IO),
    (
        MaybeCommand = yes(Command),
        ( Command = "" ->
            Res = ok
        ;
            io.call_system(Command ++ " < " ++ quote_arg(Filename),
                ResCall, !IO),
            (
                ResCall = ok(ExitStatus),
                ( ExitStatus = 0 ->
                    Res = ok
                ;
                    Msg = string.format("%s: returned with exit status %d",
                        [s(Command), i(ExitStatus)]),
                    Res = error(Msg)
                )
            ;
                ResCall = error(Error),
                Res = error(io.error_message(Error))
            )
        )
    ;
        MaybeCommand = no,
        % Default behaviour.
        add_sent(Filename, Res, !IO)
    ).

:- pred tag_replied_message(headers::in, maybe_error::out, io::di, io::uo)
    is det.

tag_replied_message(Headers, Res, !IO) :-
    InReplyTo0 = Headers ^ h_inreplyto,
    (
        string.index(InReplyTo0, 0, '<'),
        Length = string.count_codepoints(InReplyTo0),
        string.codepoint_offset(InReplyTo0, Length - 1, LastPos),
        string.index(InReplyTo0, LastPos, '>')
    ->
        string.between(InReplyTo0, 1, LastPos, Id),
        MessageId = message_id(Id),
        tag_messages([tag_delta("+replied"), tag_delta("-unread")],
            [MessageId], Res, !IO)
    ;
        Res = ok
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
    prepare_temp::in, maybe_error(string)::out, io::di, io::uo) is det.

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
            write_unstructured_header(Stream, "Date", Date, !IO),
            write_unstructured_header(Stream, "Message-ID", MessageId, !IO),
            WriteUnstruc = skip_if_empty(write_unstructured_header(Stream)),
            WriteAddrs = skip_if_empty(write_address_list_header(Stream)),
            WriteRefs = skip_if_empty(write_references_header(Stream))
        ;
            Prepare = prepare_postpone,
            generate_date_msg_id(Date, _MessageId, !IO),
            write_unstructured_header(Stream, "Date", Date, !IO),
            WriteUnstruc = write_unstructured_header(Stream),
            WriteAddrs = write_address_list_header(Stream),
            WriteRefs = write_references_header(Stream)
        ;
            Prepare = prepare_edit,
            WriteUnstruc = write_unstructured_header(Stream),
            WriteAddrs = write_address_list_header(Stream),
            WriteRefs = write_references_header(Stream)
        ),
        WriteAddrs("From", From, !IO),
        WriteAddrs("To", To, !IO),
        WriteAddrs("Cc", Cc, !IO),
        WriteAddrs("Bcc", Bcc, !IO),
        WriteUnstruc("Subject", Subject, !IO),
        WriteAddrs("Reply-To", ReplyTo, !IO),
        WriteRefs("In-Reply-To", InReplyTo, !IO),
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            WriteRefs("References", References, !IO)
        ;
            Prepare = prepare_edit
        ),
        map.foldl(WriteUnstruc, RestHeaders, !IO),
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
        Res = error(Message)
    ).

:- pred skip_if_empty(pred(string, string, io, io)::in(pred(in, in, di, uo) is det),
    string::in, string::in, io::di, io::uo) is det.

skip_if_empty(Pred, Field, Value, !IO) :-
    ( Value = "" ->
        true
    ;
        Pred(Field, Value, !IO)
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
        Content = Part ^ pt_content,
        (
            Content = text(ContentString),
            CTE = "8bit"
        ;
            Content = unsupported,
            CTE = "base64",
            get_non_text_part_base64(Part, ContentString, !IO)
        ;
            Content = subparts(_),
            unexpected($module, $pred, "nested part")
        ;
            Content = encapsulated_messages(_),
            unexpected($module, $pred, "encapsulated_messages")
        ),
        MaybeFileName = Part ^ pt_filename
    ;
        Attachment = new_attachment(Type, Content, FileName, _Size),
        MaybeFileName = yes(FileName),
        (
            Content = text(ContentString),
            CTE = "8bit"
        ;
            Content = binary_base64(ContentString),
            CTE = "base64"
        )
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
    io.write_string(Stream, ContentString, !IO).

:- pred get_non_text_part_base64(part::in, string::out, io::di, io::uo) is det.

get_non_text_part_base64(Part, Content, !IO) :-
    Part = part(MessageId, PartId, _, _, _, _, _),
    args_to_quoted_command([
        "show", "--format=raw", "--part=" ++ from_int(PartId),
        message_id_to_search_term(MessageId)
    ], Command),
    get_notmuch_prefix(Notmuch, !IO),
    call_system_capture_stdout(Notmuch ++ Command ++ " |base64", CallRes, !IO),
    (
        CallRes = ok(Content)
    ;
        CallRes = error(Error),
        % XXX handle this gracefully
        unexpected($module, $pred, io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
