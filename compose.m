% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module compose.
:- interface.

:- import_module bool.
:- import_module io.

:- import_module data.
:- import_module prog_config.
:- import_module rfc5322.
:- import_module screen.
:- import_module text_entry.

:- type reply_kind
    --->    direct_reply
    ;       group_reply
    ;       list_reply.

:- type sent
    --->    sent
    ;       not_sent.

:- pred start_compose(prog_config::in, screen::in,
    screen_transition(sent)::out, history::in, history::out,
    history::in, history::out, io::di, io::uo) is det.

:- pred start_reply(prog_config::in, screen::in, message::in, reply_kind::in,
    screen_transition(sent)::out, io::di, io::uo) is det.

:- pred start_reply_to_message_id(prog_config::in, screen::in, message_id::in,
    reply_kind::in, screen_transition(sent)::out, io::di, io::uo) is det.

:- pred continue_postponed(prog_config::in, screen::in, message::in,
    screen_transition(sent)::out, io::di, io::uo) is det.

    % Exported for resend.
    %
:- pred parse_and_expand_addresses_string(prog_config::in, string::in,
    string::out, address_list::out, bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module stream.
:- import_module string.
:- import_module string.builder.

:- import_module addressbook.
:- import_module call_system.
:- import_module callout.
:- import_module color.
:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module message_file.
:- import_module mime_type.
:- import_module pager.
:- import_module path_expand.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module rfc2045.
:- import_module rfc5322.parser.
:- import_module rfc5322.writer.
:- import_module scrollable.
:- import_module send_util.
:- import_module string_util.
:- import_module tags.
:- use_module rfc2231.

:- type header_type
    --->    from
    ;       to
    ;       cc
    ;       bcc
    ;       subject
    ;       replyto.

:- type staging_info
    --->    staging_info(
                si_config       :: prog_config,
                si_headers      :: headers,
                si_parsed_hdrs  :: parsed_headers,
                si_text         :: string,
                si_old_msgid    :: maybe(message_id),
                si_attach_hist  :: history
            ).

:- type parsed_headers
    --->    parsed_headers(
                ph_from         :: address_list,
                ph_to           :: address_list,
                ph_cc           :: address_list,
                ph_bcc          :: address_list,
                ph_replyto      :: address_list
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

%-----------------------------------------------------------------------------%

start_compose(Config, Screen, Transition, !ToHistory, !SubjectHistory, !IO) :-
    get_from_address(Config, FromAddress, !IO),
    text_entry_initial(Screen, "To: ", !.ToHistory, "",
        complete_config_key(Config, addressbook_section), MaybeTo, !IO),
    (
        MaybeTo = yes(To),
        add_history_nodup(To, !ToHistory),
        text_entry_initial(Screen, "Subject: ", !.SubjectHistory, "",
            complete_none, MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            add_history_nodup(Subject, !SubjectHistory),
            address_to_string(no_encoding, FromAddress, From, _FromValid),
            expand_aliases(Config, To, ExpandTo, !IO),
            some [!Headers] (
                !:Headers = init_headers,
                !Headers ^ h_from := header_value(From),
                !Headers ^ h_to := header_value(ExpandTo),
                !Headers ^ h_subject := decoded_unstructured(Subject),
                Headers = !.Headers
            ),
            Text = "",
            Attachments = [],
            MaybeOldDraft = no,
            create_edit_stage(Config, Screen, Headers, Text, Attachments,
                MaybeOldDraft, Transition, !IO)
        ;
            MaybeSubject = no,
            Transition = screen_transition(not_sent, no_change)
        )
    ;
        MaybeTo = no,
        Transition = screen_transition(not_sent, no_change)
    ).

:- pred expand_aliases(prog_config::in, string::in, string::out,
    io::di, io::uo) is det.

expand_aliases(Config, Input, Output, !IO) :-
    parse_and_expand_addresses_string(Config, Input, Output, _Addresses,
        _Valid, !IO).

%-----------------------------------------------------------------------------%

start_reply(Config, Screen, Message, ReplyKind, Transition, !IO) :-
    get_notmuch_command(Config, Notmuch),
    Message ^ m_id = MessageId,
    make_quoted_command(Notmuch, [
        "reply", reply_to_arg(ReplyKind), "--",
        message_id_to_search_term(MessageId)
    ], Command),
    call_system_capture_stdout(Command, no, CommandResult, !IO),
    (
        CommandResult = ok(String),
        parse_message(String, Headers0, Text),
        (
            ReplyKind = direct_reply,
            Headers = Headers0
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
        create_edit_stage(Config, Screen, Headers, Text, Attachments,
            MaybeOldDraft, Transition, !IO)
    ;
        CommandResult = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        Transition = screen_transition(not_sent, set_warning(Warning))
    ).

:- func reply_to_arg(reply_kind) = string.

reply_to_arg(direct_reply) = "--reply-to=sender".
reply_to_arg(group_reply) = "--reply-to=all".
reply_to_arg(list_reply) = "--reply-to=all".

:- pred set_headers_for_group_reply(headers::in, headers::out) is det.

set_headers_for_group_reply(!Headers) :-
    % Move all but the first To address down to Cc.  This acts more like the
    % behaviour I am used to from Mutt.
    % XXX do proper address list parsing

    To0 = header_value_string(!.Headers ^ h_to),
    Cc0 = header_value_string(!.Headers ^ h_cc),
    parse_address_list(To0, ToList0),
    parse_address_list(Cc0, CcList0),
    (
        ToList0 = [ToHead | ToTail],
        ToHead = mailbox(_)
    ->
        ToList = [ToHead],
        CcList = ToTail ++ CcList0,
        address_list_to_string(no_encoding, ToList, To, _ToValid),
        address_list_to_string(no_encoding, CcList, Cc, _CcValid),
        !Headers ^ h_to := header_value(To),
        !Headers ^ h_cc := header_value(Cc)
    ;
        true
    ).

:- pred set_headers_for_list_reply(header_value::in, headers::in, headers::out)
    is det.

set_headers_for_list_reply(OrigFrom, !Headers) :-
    % Remove OrigFrom if it appears in the To header, and it is not the only
    % address in To.  This acts a bit like the list reply function from Mutt
    % without knowing which addresses are list addresses.

    To0 = header_value_string(!.Headers ^ h_to),
    parse_address_list(header_value_string(OrigFrom), FromList),
    parse_address_list(To0, ToList0),
    (
        FromList = [mailbox(FromMailbox)],
        FromMailbox = mailbox(_, FromAddrSpec),
        list.negated_filter(similar_mailbox(FromAddrSpec), ToList0, ToList),
        ToList = [_ | _]
    ->
        address_list_to_string(no_encoding, ToList, To, _ToValid),
        !Headers ^ h_to := header_value(To)
    ;
        true
    ).

:- pred similar_mailbox(addr_spec::in, address::in) is semidet.

similar_mailbox(AddrSpec, OtherAddress) :-
    OtherAddress = mailbox(mailbox(_DisplayName, AddrSpec)).

%-----------------------------------------------------------------------------%

start_reply_to_message_id(Config, Screen, MessageId, ReplyKind, Sent, !IO) :-
    run_notmuch(Config, [
        "show", "--format=json", "--part=0", "--",
        message_id_to_search_term(MessageId)
    ], parse_top_message, Res, !IO),
    (
        Res = ok(Message),
        start_reply(Config, Screen, Message, ReplyKind, Sent, !IO)
    ;
        Res = error(Error),
        unexpected($module, $pred, Error)
    ).

%-----------------------------------------------------------------------------%

continue_postponed(Config, Screen, Message, Transition, !IO) :-
    MessageId = Message ^ m_id,
    Headers0 = Message ^ m_headers,
    Body0 = Message ^ m_body,
    first_text_part(Body0, Text, AttachmentParts),
    list.map(to_old_attachment, AttachmentParts, Attachments),

    % XXX notmuch show --format=json does not return References and In-Reply-To
    % so we parse them from the raw output.
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, [
        "show", "--format=raw", "--", message_id_to_search_term(MessageId)
    ], Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
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
        create_edit_stage(Config, Screen, Headers, Text, Attachments,
            yes(MessageId), Transition, !IO)
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
        ; PartContent = unsupported_inline
        ),
        first_text_part(Parts, Text, AttachmentParts0),
        AttachmentParts = [Part | AttachmentParts0]
    ).

:- pred to_old_attachment(part::in, attachment::out) is det.

to_old_attachment(Part, old_attachment(Part)).

%-----------------------------------------------------------------------------%

:- pred create_edit_stage(prog_config::in, screen::in, headers::in, string::in,
    list(attachment)::in, maybe(message_id)::in, screen_transition(sent)::out,
    io::di, io::uo) is det.

create_edit_stage(Config, Screen, Headers0, Text0, Attachments, MaybeOldDraft,
        Transition, !IO) :-
    make_parsed_headers(Headers0, ParsedHeaders0),
    create_temp_message_file(Config, prepare_edit, Headers0, ParsedHeaders0,
        Text0, Attachments, ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        call_editor(Config, Filename, ResEdit, !IO),
        (
            ResEdit = ok,
            parse_message_file(Filename, ResParse, !IO),
            (
                ResParse = ok(Headers1 - Text),
                io.remove_file(Filename, _, !IO),
                update_references(Headers0, Headers1, Headers2),
                enter_staging(Config, Screen, Headers2, Text, Attachments,
                    MaybeOldDraft, Transition, !IO)
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

:- pred make_parsed_headers(headers::in, parsed_headers::out) is det.

make_parsed_headers(Headers, Parsed) :-
    Headers = headers(_Date, From, To, Cc, Bcc, _Subject, ReplyTo,
        _References, _InReplyTo, _Rest),

    % [RFC 6854] allows group syntax in From - saves us work.
    parse_address_list(header_value_string(From), ParsedFrom),
    parse_address_list(header_value_string(To), ParsedTo),
    parse_address_list(header_value_string(Cc), ParsedCc),
    parse_address_list(header_value_string(Bcc), ParsedBcc),
    parse_address_list(header_value_string(ReplyTo), ParsedReplyTo),

    Parsed = parsed_headers(ParsedFrom, ParsedTo, ParsedCc, ParsedBcc,
        ParsedReplyTo).

:- pred call_editor(prog_config::in, string::in, call_res::out, io::di, io::uo)
    is det.

call_editor(Config, Filename, Res, !IO) :-
    get_editor_command(Config, Editor),
    make_quoted_command(Editor, [Filename], Command),
    curs.def_prog_mode(!IO),
    curs.stop(!IO),
    io.call_system(Command, CallRes, !IO),
    curs.reset_prog_mode(!IO),
    curs.refresh(!IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            string.format("%s returned exit status %d",
                [s(Command), i(ExitStatus)], Warning),
            Res = error(Warning)
        )
    ;
        CallRes = error(Error),
        string.append_list(["Error running ", Command, ": ",
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

%-----------------------------------------------------------------------------%

:- pred enter_staging(prog_config::in, screen::in, headers::in, string::in,
    list(attachment)::in, maybe(message_id)::in, screen_transition(sent)::out,
    io::di, io::uo) is det.

enter_staging(Config, Screen, Headers0, Text, Attachments, MaybeOldDraft,
        Transition, !IO) :-
    parse_and_expand_headers(Config, Headers0, Headers, Parsed, !IO),
    StagingInfo = staging_info(Config, Headers, Parsed, Text, MaybeOldDraft,
        init_history),
    AttachInfo = scrollable.init_with_cursor(Attachments),
    get_cols(Screen, Cols),
    setup_pager_for_staging(Config, Cols, Text, new_pager, PagerInfo),
    staging_screen(Screen, StagingInfo, AttachInfo, PagerInfo, Transition,
        !IO).

:- pred parse_and_expand_headers(prog_config::in, headers::in, headers::out,
    parsed_headers::out, io::di, io::uo) is det.

parse_and_expand_headers(Config, Headers0, Headers, Parsed, !IO) :-
    Headers0 = headers(Date, From0, To0, Cc0, Bcc0, Subject, ReplyTo0,
        References, InReplyTo, Rest),

    % [RFC 6854] allows group syntax in From - saves us work.
    parse_and_expand_addresses(Config, From0, From, ParsedFrom, !IO),
    parse_and_expand_addresses(Config, To0, To, ParsedTo, !IO),
    parse_and_expand_addresses(Config, Cc0, Cc, ParsedCc, !IO),
    parse_and_expand_addresses(Config, Bcc0, Bcc, ParsedBcc, !IO),
    parse_and_expand_addresses(Config, ReplyTo0, ReplyTo, ParsedReplyTo, !IO),

    Headers = headers(Date, From, To, Cc, Bcc, Subject, ReplyTo,
        References, InReplyTo, Rest),
    Parsed = parsed_headers(ParsedFrom, ParsedTo, ParsedCc, ParsedBcc,
        ParsedReplyTo).

:- pred parse_and_expand_addresses(prog_config::in, header_value::in,
    header_value::out, address_list::out, io::di, io::uo) is det.

parse_and_expand_addresses(Config, Input, header_value(Output), Addresses, !IO)
        :-
    parse_and_expand_addresses_string(Config, header_value_string(Input),
        Output, Addresses, _Valid, !IO).

parse_and_expand_addresses_string(Config, Input, Output, Addresses, Valid, !IO)
        :-
    parse_address_list(Input, Addresses0),
    list.map_foldl(maybe_expand_address(Config), Addresses0, Addresses, !IO),
    address_list_to_string(no_encoding, Addresses, Output, Valid).

:- pred maybe_expand_address(prog_config::in, address::in, address::out,
    io::di, io::uo) is det.

maybe_expand_address(Config, Address0, Address, !IO) :-
    (
        Address0 = mailbox(Mailbox0),
        maybe_expand_mailbox(Config, Mailbox0, Mailbox, !IO),
        Address = mailbox(Mailbox)
    ;
        Address0 = group(DisplayName, Mailboxes0),
        list.map_foldl(maybe_expand_mailbox(Config), Mailboxes0, Mailboxes,
            !IO),
        Address = group(DisplayName, Mailboxes)
    ).

:- pred maybe_expand_mailbox(prog_config::in, mailbox::in, mailbox::out,
    io::di, io::uo) is det.

maybe_expand_mailbox(Config, Mailbox0, Mailbox, !IO) :-
    (
        Mailbox0 = mailbox(_, _),
        Mailbox = Mailbox0
    ;
        Mailbox0 = bad_mailbox(PotentialAlias),
        search_addressbook(Config, PotentialAlias, MaybeFound, !IO),
        (
            MaybeFound = yes(Expansion),
            parse_address(Expansion, mailbox(Mailbox1))
            % Can't expand to a group or multiple mailboxes yet.
        ->
            Mailbox = Mailbox1
        ;
            Mailbox = Mailbox0
        )
    ).

%-----------------------------------------------------------------------------%

:- pred staging_screen(screen::in, staging_info::in, attach_info::in,
    pager_info::in, screen_transition(sent)::out, io::di, io::uo) is det.

staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, Transition,
        !IO) :-
    !.StagingInfo = staging_info(Config, Headers, ParsedHeaders, Text,
        MaybeOldDraft, _AttachHistory),
    Attrs = compose_attrs(Config),
    PagerAttrs = pager_attrs(Config),

    split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels),
    draw_header_lines(HeaderPanels, Attrs, Headers, ParsedHeaders, !IO),
    scrollable.draw(draw_attachment_line(Attrs), AttachmentPanels,
        !.AttachInfo, !IO),
    draw_attachments_label(Attrs, AttachmentPanels, !IO),
    draw_sep_bar(Attrs, Screen, MaybeSepPanel, !IO),
    draw_pager_lines(PagerAttrs, PagerPanels, !.PagerInfo, !IO),
    draw_staging_bar(Attrs, Screen, !.StagingInfo, !IO),
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
        postpone(Config, Screen, Headers, ParsedHeaders, Text, Attachments,
            Res, PostponeMsg, !IO),
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
        send_mail(Config, Screen, Headers, ParsedHeaders, Text, Attachments,
            Sent0, MessageUpdate0, !IO),
        (
            Sent0 = sent,
            tag_replied_message(Config, Headers, TagRes, !IO),
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
        % XXX make this tail-recursive in hlc
        create_edit_stage(Config, Screen, Headers, Text, EditAttachments,
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
    Config = StagingInfo ^ si_config,
    Text = StagingInfo ^ si_text,
    setup_pager_for_staging(Config, Cols, Text,
        retain_pager_pos(PagerInfo0, NumPagerRows), PagerInfo).

%-----------------------------------------------------------------------------%

:- pred edit_header(screen::in, header_type::in,
    staging_info::in, staging_info::out, io::di, io::uo) is det.

edit_header(Screen, HeaderType, !StagingInfo, !IO) :-
    Config = !.StagingInfo ^ si_config,
    Headers0 = !.StagingInfo ^ si_headers,
    get_header(HeaderType, Headers0, Prompt, Initial, CompleteAddressbook),
    InitialString = header_value_string(Initial),
    (
        CompleteAddressbook = yes,
        Completion = complete_config_key(Config, addressbook_section)
    ;
        CompleteAddressbook = no,
        Completion = complete_none
    ),
    text_entry_full(Screen, Prompt, init_history, InitialString, Completion,
        no, Return, !IO),
    (
        Return = yes(ReturnString),
        (
            Initial = header_value(_),
            Value = header_value(ReturnString)
        ;
            Initial = decoded_unstructured(_),
            Value = decoded_unstructured(ReturnString)
        ),
        ParsedHeaders0 = !.StagingInfo ^ si_parsed_hdrs,
        update_header(Config, HeaderType, Value, Headers0, Headers,
            ParsedHeaders0, ParsedHeaders, !IO),
        !StagingInfo ^ si_headers := Headers,
        !StagingInfo ^ si_parsed_hdrs := ParsedHeaders
    ;
        Return = no
    ).

:- pred get_header(header_type::in, headers::in, string::out,
    header_value::out, bool::out) is det.

get_header(from,    H, "From: ",     H ^ h_from,    yes).
get_header(to,      H, "To: ",       H ^ h_to,      yes).
get_header(cc,      H, "Cc: ",       H ^ h_cc,      yes).
get_header(bcc,     H, "Bcc: ",      H ^ h_bcc,     yes).
get_header(subject, H, "Subject: ",  H ^ h_subject, no).
get_header(replyto, H, "Reply-To: ", H ^ h_replyto, yes).

:- pred update_header(prog_config::in, header_type::in, header_value::in,
    headers::in, headers::out, parsed_headers::in, parsed_headers::out,
    io::di, io::uo) is det.

update_header(Config, HeaderType, Input, !Headers, !Parsed, !IO) :-
    (
        HeaderType = from,
        parse_and_expand_addresses(Config, Input, Output, Parsed, !IO),
        !Headers ^ h_from := Output,
        !Parsed ^ ph_from := Parsed
    ;
        HeaderType = to,
        parse_and_expand_addresses(Config, Input, Output, Parsed, !IO),
        !Headers ^ h_to := Output,
        !Parsed ^ ph_to := Parsed
    ;
        HeaderType = cc,
        parse_and_expand_addresses(Config, Input, Output, Parsed, !IO),
        !Headers ^ h_cc := Output,
        !Parsed ^ ph_cc := Parsed
    ;
        HeaderType = bcc,
        parse_and_expand_addresses(Config, Input, Output, Parsed, !IO),
        !Headers ^ h_bcc := Output,
        !Parsed ^ ph_bcc := Parsed
    ;
        HeaderType = replyto,
        parse_and_expand_addresses(Config, Input, Output, Parsed, !IO),
        !Headers ^ h_replyto := Output,
        !Parsed ^ ph_replyto := Parsed
    ;
        HeaderType = subject,
        !Headers ^ h_subject := Input
    ).

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
    make_quoted_command(base64_command, [FileName], Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
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

:- func base64_command = command_prefix.

base64_command = command_prefix(shell_quoted("base64"), quote_once).

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
            text_entry_full(Screen, "Media type: ", History, Type0,
                complete_none, no, Return, !IO),
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

:- pred draw_header_lines(list(panel)::in, compose_attrs::in, headers::in,
    parsed_headers::in, io::di, io::uo) is det.

draw_header_lines(!.Panels, Attrs, Headers, Parsed, !IO) :-
    hdr(!Panels, Attrs, "    From", draw_addresses, Parsed ^ ph_from, !IO),
    hdr(!Panels, Attrs, "      To", draw_addresses, Parsed ^ ph_to, !IO),
    hdr(!Panels, Attrs, "      Cc", draw_addresses, Parsed ^ ph_cc, !IO),
    hdr(!Panels, Attrs, "     Bcc", draw_addresses, Parsed ^ ph_bcc, !IO),
    hdr(!Panels, Attrs, " Subject", draw_unstruct, Headers ^ h_subject, !IO),
    hdr(!Panels, Attrs, "Reply-To", draw_addresses, Parsed ^ ph_replyto, !IO),
    !.Panels = _.

:- pred hdr(list(panel), list(panel), compose_attrs, string,
    pred(compose_attrs, panel, T, io, io), T, io, io).
:- mode hdr(in, out, in, in,
    in(pred(in, in, in, di, uo) is det), in, di, uo) is det.

hdr(Panels0, Panels, Attrs, FieldName, DrawValue, Value, !IO) :-
    (
        Panels0 = [],
        Panels = []
    ;
        Panels0 = [Panel | Panels],
        draw_header_field(Attrs, Panel, FieldName, !IO),
        DrawValue(Attrs, Panel, Value, !IO)
    ).

:- pred draw_header_field(compose_attrs::in, panel::in, string::in,
    io::di, io::uo) is det.

draw_header_field(Attrs, Panel, FieldName, !IO) :-
    panel.erase(Panel, !IO),
    draw(Panel, Attrs ^ c_generic ^ field_name, FieldName, !IO),
    draw(Panel, ": ", !IO).

:- pred draw_list(pred(compose_attrs, panel, T, io, io), compose_attrs, panel,
    list(T), io, io).
:- mode draw_list(in(pred(in, in, in, di, uo) is det), in, in,
    in, di, uo) is det.

draw_list(_Pred, _Attrs, _Panel, [], !IO).
draw_list(Pred, Attrs, Panel, [H | T], !IO) :-
    Pred(Attrs, Panel, H, !IO),
    (
        T = []
    ;
        T = [_ | _],
        draw(Panel, Attrs ^ c_generic ^ field_body, ", ", !IO),
        draw_list(Pred, Attrs, Panel, T, !IO)
    ).

:- pred draw_addresses(compose_attrs::in, panel::in, list(address)::in,
    io::di, io::uo) is det.

draw_addresses(Attrs, Panel, Addresses, !IO) :-
    draw_list(draw_address, Attrs, Panel, Addresses, !IO).

:- pred draw_address(compose_attrs::in, panel::in, address::in, io::di, io::uo)
    is det.

draw_address(Attrs, Panel, Address, !IO) :-
    (
        Address = mailbox(Mailbox),
        draw_mailbox(Attrs, Panel, Mailbox, !IO)
    ;
        Address = group(DisplayName, Mailboxes),
        draw_display_name(Attrs, Panel, DisplayName, !IO),
        Attr = Attrs ^ c_generic ^ field_body,
        draw(Panel, Attr, ": ", !IO),
        draw_list(draw_mailbox, Attrs, Panel, Mailboxes, !IO),
        draw(Panel, Attr, ";", !IO)
    ).

:- pred draw_display_name(compose_attrs::in, panel::in, display_name::in,
    io::di, io::uo) is det.

draw_display_name(Attrs, Panel, DisplayName, !IO) :-
    display_name_to_string(no_encoding, DisplayName, String, Valid),
    (
        Valid = yes,
        Attr = Attrs ^ c_generic ^ field_body
    ;
        Valid = no,
        Attr = Attrs ^ c_invalid
    ),
    draw(Panel, Attr, String, !IO).

:- pred draw_mailbox(compose_attrs::in, panel::in, mailbox::in, io::di, io::uo)
    is det.

draw_mailbox(Attrs, Panel, Mailbox, !IO) :-
    (
        Mailbox = mailbox(yes(DisplayName), AddrSpec),
        draw_display_name(Attrs, Panel, DisplayName, !IO),
        Attr = Attrs ^ c_generic ^ field_body,
        draw(Panel, Attr, " <", !IO),
        draw_addr_spec(Attrs, Panel, AddrSpec, !IO),
        draw(Panel, Attr, ">", !IO)
    ;
        Mailbox = mailbox(no, AddrSpec),
        draw_addr_spec(Attrs, Panel, AddrSpec, !IO)
    ;
        Mailbox = bad_mailbox(String),
        draw(Panel, Attrs ^ c_invalid, String, !IO)
    ).

:- pred draw_addr_spec(compose_attrs::in, panel::in, addr_spec::in,
    io::di, io::uo) is det.

draw_addr_spec(Attrs, Panel, AddrSpec, !IO) :-
    addr_spec_to_string(AddrSpec, String, Valid),
    (
        Valid = yes,
        Attr = Attrs ^ c_address
    ;
        Valid = no,
        Attr = Attrs ^ c_invalid
    ),
    draw(Panel, Attr, String, !IO).

:- pred draw_unstruct(compose_attrs::in, panel::in, header_value::in,
    io::di, io::uo) is det.

draw_unstruct(Attrs, Panel, Value, !IO) :-
    String = header_value_string(Value),
    draw(Panel, Attrs ^ c_generic ^ field_body, String, !IO).

:- pred draw_attachment_line(compose_attrs::in, panel::in, attachment::in,
    int::in, bool::in, io::di, io::uo) is det.

draw_attachment_line(Attrs, Panel, Attachment, LineNr, IsCursor, !IO) :-
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
    Attr = Attrs ^ c_generic ^ field_body,
    draw(Panel, Attr, format("%d. ", [i(LineNr + 1)]), !IO),
    (
        IsCursor = yes,
        FilenameAttr = Attr + reverse
    ;
        IsCursor = no,
        FilenameAttr = Attr
    ),
    draw(Panel, FilenameAttr, Filename, !IO),
    draw(Panel, Attr, " (" ++ Type ++ ")", !IO),
    ( Size >= 1024 * 1024 ->
        SizeM = float(Size) / (1024.0 * 1024.0),
        draw(Panel, format(" %.1f MiB", [f(SizeM)]), !IO)
    ; Size >= 1024 ->
        SizeK = float(Size) / 1024.0,
        draw(Panel, format(" %.1f KiB", [f(SizeK)]), !IO)
    ; Size >= 0 ->
        draw(Panel, format(" %d bytes", [i(Size)]), !IO)
    ;
        true
    ).

:- pred draw_attachments_label(compose_attrs::in, list(panel)::in,
    io::di, io::uo) is det.

draw_attachments_label(_Attrs, [], !IO).
draw_attachments_label(Attrs, [Panel | _], !IO) :-
    panel.move(Panel, 0, 0, !IO),
    draw(Panel, Attrs ^ c_generic ^ field_name, "  Attach: ", !IO).

:- pred draw_sep_bar(compose_attrs::in, screen::in, maybe(panel)::in,
    io::di, io::uo) is det.

draw_sep_bar(_, _, no, !IO).
draw_sep_bar(Attrs, Screen, yes(Panel), !IO) :-
    Attr = Attrs ^ c_status ^ bar,
    get_cols(Screen, Cols),
    panel.erase(Panel, !IO),
    draw(Panel, Attr, "-- (ftcbsr) edit fields; (a) attach, (d) detach, "
        ++ "(T) edit attachment type ", !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

:- pred draw_staging_bar(compose_attrs::in, screen::in, staging_info::in,
    io::di, io::uo) is det.

draw_staging_bar(Attrs, Screen, StagingInfo, !IO) :-
    MaybeOldDraft = StagingInfo ^ si_old_msgid,
    get_cols(Screen, Cols),
    get_bar_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    draw(Panel, Attrs ^ c_status ^ bar, "-- ", !IO),
    (
        MaybeOldDraft = yes(_),
        Msg = "Compose: (e) edit, (p) postpone, (Y) send, " ++
            "(D) discard new changes, (Q) abandon."
    ;
        MaybeOldDraft = no,
        Msg = "Compose: (e) edit, (p) postpone, (Y) send, (Q) abandon."
    ),
    draw_fixed(Panel, Cols - 3, Msg, '-', !IO).

%-----------------------------------------------------------------------------%

:- pred postpone(prog_config::in, screen::in, headers::in, parsed_headers::in,
    string::in, list(attachment)::in, bool::out, message_update::out,
    io::di, io::uo) is det.

postpone(Config, Screen, Headers, ParsedHeaders, Text, Attachments, Res,
        MessageUpdate, !IO) :-
    create_temp_message_file(Config, prepare_postpone, Headers, ParsedHeaders,
        Text, Attachments, ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        update_message_immed(Screen, set_info("Postponing message..."), !IO),
        add_draft(Config, Filename, DraftRes, !IO),
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
    Config = StagingInfo ^ si_config,
    MaybeOldDraft = StagingInfo ^ si_old_msgid,
    (
        MaybeOldDraft = yes(MessageId),
        tag_messages(Config, [tag_delta("+deleted")], [MessageId], _Res, !IO)
    ;
        MaybeOldDraft = no
    ).

%-----------------------------------------------------------------------------%

:- pred send_mail(prog_config::in, screen::in, headers::in, parsed_headers::in,
    string::in, list(attachment)::in, sent::out, message_update::out,
    io::di, io::uo) is det.

send_mail(Config, Screen, Headers, ParsedHeaders, Text, Attachments, Res,
        MessageUpdate, !IO) :-
    create_temp_message_file(Config, prepare_send, Headers, ParsedHeaders,
        Text, Attachments, ResFilename, !IO),
    (
        ResFilename = ok(Filename),
        update_message_immed(Screen, set_info("Sending message..."), !IO),
        call_send_mail(Config, Filename, SendRes, !IO),
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

:- pred call_send_mail(prog_config::in, string::in, call_res::out,
    io::di, io::uo) is det.

call_send_mail(Config, Filename, Res, !IO) :-
    get_sendmail_command(Config, sendmail_read_recipients, Sendmail),
    make_quoted_command(Sendmail, [], redirect_input(Filename), no_redirect,
        Command),
    io.call_system(Command, ResSend, !IO),
    (
        ResSend = ok(ExitStatus),
        ( ExitStatus = 0 ->
            do_post_sendmail(Config, Filename, ResAfter, !IO),
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

:- pred do_post_sendmail(prog_config::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

do_post_sendmail(Config, Filename, Res, !IO) :-
    get_post_sendmail_action(Config, Action),
    (
        Action = default,
        % Default behaviour.
        add_sent(Config, Filename, Res, !IO)
    ;
        Action = nothing,
        Res = ok
    ;
        Action = command(CommandPrefix),
        make_quoted_command(CommandPrefix, [], redirect_input(Filename),
            no_redirect, Command),
        io.call_system(Command, ResCall, !IO),
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
    ).

:- pred tag_replied_message(prog_config::in, headers::in, maybe_error::out,
    io::di, io::uo) is det.

tag_replied_message(Config, Headers, Res, !IO) :-
    InReplyTo0 = header_value_string(Headers ^ h_inreplyto),
    (
        % XXX could improve this
        string.index(InReplyTo0, 0, '<'),
        Length = string.count_codepoints(InReplyTo0),
        string.codepoint_offset(InReplyTo0, Length - 1, LastPos),
        string.index(InReplyTo0, LastPos, '>')
    ->
        string.between(InReplyTo0, 1, LastPos, Id),
        MessageId = message_id(Id),
        tag_messages(Config, [tag_delta("+replied"), tag_delta("-unread")],
            [MessageId], Res, !IO)
    ;
        Res = ok
    ).

%-----------------------------------------------------------------------------%

:- type prepare_temp
    --->    prepare_send
    ;       prepare_edit
    ;       prepare_postpone.

:- pred create_temp_message_file(prog_config::in, prepare_temp::in,
    headers::in, parsed_headers::in, string::in, list(attachment)::in,
    maybe_error(string)::out, io::di, io::uo) is det.

create_temp_message_file(Config, Prepare, Headers, ParsedHeaders, Text,
        Attachments, Res, !IO) :-
    some [!State] (
        !:State = init,
        generate_date_msg_id(Date, MessageId, !IO),
        write_most_headers(string.builder.handle, Prepare, Headers,
            ParsedHeaders, Date, MessageId, HeaderError, !State),
        (
            HeaderError = error(Error),
            stop_at_header_error(Prepare) = yes
        ->
            Res = error(Error)
        ;
            HeaderString = to_string(!.State),
            maybe_mime(Prepare, Attachments, MaybeMIME, !IO),
            write_temp_message_file(Config, HeaderString, MaybeMIME, Text,
                Attachments, Res, !IO)
        )
    ).

:- func stop_at_header_error(prepare_temp) = bool.

stop_at_header_error(prepare_send) = yes.
stop_at_header_error(prepare_edit) = no.
stop_at_header_error(prepare_postpone) = no.

:- pred write_temp_message_file(prog_config::in, string::in, maybe(mime)::in,
    string::in, list(attachment)::in, maybe_error(string)::out, io::di, io::uo)
    is det.

write_temp_message_file(Config, HeaderString, MaybeMIME, Text, Attachments,
        Res, !IO) :-
    io.make_temp(Filename, !IO),
    io.open_output(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        promise_equivalent_solutions [Res, !:IO] (
          try [io(!IO)] (
            write_temp_message_file_2(Stream, Config, HeaderString, MaybeMIME,
                Text, Attachments, !IO),
            io.close_output(Stream, !IO)
          )
          then
            Res = ok(Filename)
          catch_any Excp ->
            io.remove_file(Filename, _, !IO),
            Res = error("Caught exception: " ++ string(Excp))
        )
    ;
        ResOpen = error(_Error),
        Message = "Error writing temporary file " ++ Filename,
        Res = error(Message)
    ).

:- pred write_temp_message_file_2(io.output_stream::in, prog_config::in,
    string::in, maybe(mime)::in, string::in, list(attachment)::in, io::di,
    io::uo) is det.

write_temp_message_file_2(Stream, Config, HeaderString, MaybeMIME, Text,
        Attachments, !IO) :-
    % XXX detect charset
    Charset = "utf-8",
    io.write_string(Stream, HeaderString, !IO),
    write_mime_headers(Stream, MaybeMIME, Charset, !IO),

    % End header fields.
    io.nl(Stream, !IO),

    % Begin body.
    (
        MaybeMIME = no,
        io.write_string(Stream, Text, !IO)
    ;
        MaybeMIME = yes(mime_single_part),
        io.write_string(Stream, Text, !IO)
    ;
        MaybeMIME = yes(mime_multipart(Boundary)),
        write_mime_part_boundary(Stream, Boundary, !IO),
        write_mime_part_text(Stream, Charset, Text, !IO),
        list.foldl(write_mime_part_attachment(Stream, Config, Boundary),
            Attachments, !IO),
        write_mime_final_boundary(Stream, Boundary, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred write_most_headers(Stream::in, prepare_temp::in, headers::in,
    parsed_headers::in, header_value::in, header_value::in, maybe_error::out,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_most_headers(Stream, Prepare, Headers, ParsedHeaders, Date, MessageId,
        !:Error, !State) :-
    Headers = headers(_Date, _From, _To, _Cc, _Bcc, Subject, _ReplyTo,
        References, InReplyTo, RestHeaders),
    ParsedHeaders = parsed_headers(From, To, Cc, Bcc, ReplyTo),
    (
        Prepare = prepare_send,
        write_as_unstructured_header(no_encoding, Stream,
            "Date", Date, !State),
        write_as_unstructured_header(no_encoding, Stream,
            "Message-ID", MessageId, !State)
    ;
        Prepare = prepare_postpone,
        write_as_unstructured_header(no_encoding, Stream,
            "Date", Date, !State)
    ;
        Prepare = prepare_edit
    ),
    (
        ( Prepare = prepare_send
        ; Prepare = prepare_postpone
        ),
        WriteAsUnstructured = skip_if_empty_header_value(
            write_as_unstructured_header(rfc2047_encoding)),
        WriteAddrs = skip_if_empty_list(
            write_address_list_header(rfc2047_encoding)),
        WriteRefs = skip_if_empty_header_value(write_references_header)
    ;
        Prepare = prepare_edit,
        WriteAsUnstructured = write_as_unstructured_header(no_encoding),
        WriteAddrs = write_address_list_header(no_encoding),
        WriteRefs = write_references_header
    ),
    !:Error = ok,
    WriteAddrs(Stream, "From", From, !Error, !State),
    WriteAddrs(Stream, "To", To, !Error, !State),
    WriteAddrs(Stream, "Cc", Cc, !Error, !State),
    WriteAddrs(Stream, "Bcc", Bcc, !Error, !State),
    WriteAsUnstructured(Stream, "Subject", Subject, !State),
    WriteAddrs(Stream, "Reply-To", ReplyTo, !Error, !State),
    WriteRefs(Stream, "In-Reply-To", InReplyTo, !State),
    (
        ( Prepare = prepare_send
        ; Prepare = prepare_postpone
        ),
        WriteRefs(Stream, "References", References, !State)
    ;
        Prepare = prepare_edit
    ),
    map.foldl(pred(K::in, V::in, IO0::di, IO::uo) is det :-
        WriteAsUnstructured(Stream, K, V, IO0, IO), RestHeaders, !State).

:- pred skip_if_empty_header_value(pred(Stream, string, header_value,
    State, State), Stream, string, header_value, State, State)
    is det <= stream.writer(Stream, string, State).
:- mode skip_if_empty_header_value(in(pred(in, in, in, di, uo) is det),
    in, in, in, di, uo) is det.

skip_if_empty_header_value(Pred, Stream, Field, Value, !IO) :-
    ( empty_header_value(Value) ->
        true
    ;
        Pred(Stream, Field, Value, !IO)
    ).

:- pred skip_if_empty_list(pred(Stream, string, list(T), U, U, State, State),
    Stream, string, list(T), U, U, State, State)
    is det <= stream.writer(Stream, string, State).
:- mode skip_if_empty_list(in(pred(in, in, in, in, out, di, uo) is det),
    in, in, in, in, out, di, uo) is det.

skip_if_empty_list(Pred, Stream, Field, Value, !Acc, !State) :-
    (
        Value = []
    ;
        Value = [_ | _],
        Pred(Stream, Field, Value, !Acc, !State)
    ).

%-----------------------------------------------------------------------------%

:- type mime
    --->    mime_single_part
    ;       mime_multipart(string). % boundary

:- pred maybe_mime(prepare_temp::in, list(attachment)::in, maybe(mime)::out,
    io::di, io::uo) is det.

maybe_mime(Prepare, Attachments, MaybeMIME, !IO) :-
    (
        ( Prepare = prepare_send
        ; Prepare = prepare_postpone
        ),
        (
            Attachments = [],
            % This is only really necessary if the body is non-ASCII.
            MaybeMIME = yes(mime_single_part)
        ;
            Attachments = [_ | _],
            generate_boundary(Boundary, !IO),
            MaybeMIME = yes(mime_multipart(Boundary))
        )
    ;
        Prepare = prepare_edit,
        MaybeMIME = no
    ).

:- pred write_mime_headers(io.output_stream::in, maybe(mime)::in, string::in,
    io::di, io::uo) is det.

write_mime_headers(_Stream, no, _Charset, !IO).
write_mime_headers(Stream, yes(MIME), Charset, !IO) :-
    write_mime_version(Stream, !IO),
    (
        MIME = mime_single_part,
        write_content_type(Stream, "text/plain", yes(Charset), !IO)
    ;
        MIME = mime_multipart(Boundary),
        write_content_type_multipart_mixed(Stream, Boundary, !IO)
    ),
    write_content_disposition_inline(Stream, !IO),
    write_content_transfer_encoding(Stream, "8bit", !IO).

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
        Attr = attribute("filename"),
        Value = quoted_string(make_quoted_string(FileName)),
        rfc2231.encode_parameter(Attr - Value, Param),
        parameter_to_string(Param, ParamString, Valid),
        (
            Valid = yes,
            io.write_string(Stream, "; ", !IO),
            io.write_string(Stream, ParamString, !IO)
        ;
            Valid = no
            % Shouldn't happen.
        )
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

:- pred write_mime_part_attachment(io.output_stream::in, prog_config::in,
    string::in, attachment::in, io::di, io::uo) is det.

write_mime_part_attachment(Stream, Config, Boundary, Attachment, !IO) :-
    (
        Attachment = old_attachment(Part),
        Type = Part ^ pt_type,
        Content = Part ^ pt_content,
        (
            Content = text(ContentString),
            CTE = "8bit"
        ;
            ( Content = unsupported
            ; Content = unsupported_inline
            ),
            % text/html parts are automatically forced to unsupported_inline in
            % parse_part.  We don't actually support Content-Disposition:
            % inline attachments yet.
            CTE = "base64",
            get_non_text_part_base64(Config, Part, ContentString, !IO)
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

:- pred get_non_text_part_base64(prog_config::in, part::in, string::out,
    io::di, io::uo) is det.

get_non_text_part_base64(Config, Part, Content, !IO) :-
    Part = part(MessageId, PartId, _, _, _, _, _),
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, [
        "show", "--format=raw", "--part=" ++ from_int(PartId),
        message_id_to_search_term(MessageId)
    ], Command),
    call_system_capture_stdout(Command ++ " |base64", no, CallRes,
        !IO),
    (
        CallRes = ok(Content)
    ;
        CallRes = error(Error),
        % XXX handle this gracefully
        unexpected($module, $pred, io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
