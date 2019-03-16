% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module compose.
:- interface.

:- import_module bool.
:- import_module io.

:- import_module crypto.
:- import_module data.
:- import_module prog_config.
:- import_module rfc5322.
:- import_module rfc5322.parser.
:- import_module screen.
:- import_module text_entry.

:- type reply_kind
    --->    direct_reply
    ;       group_reply
    ;       list_reply.

:- type sent
    --->    sent
    ;       not_sent.

:- pred start_compose(prog_config::in, crypto::in, screen::in,
    screen_transition(sent)::out, history::in, history::out,
    history::in, history::out, io::di, io::uo) is det.

:- pred start_reply(prog_config::in, crypto::in, screen::in,
    message::in(message), reply_kind::in, screen_transition(sent)::out,
    io::di, io::uo) is det.

:- pred start_reply_to_message_id(prog_config::in, crypto::in, screen::in,
    message_id::in, reply_kind::in, screen_transition(sent)::out,
    io::di, io::uo) is det.

:- pred start_forward(prog_config::in, crypto::in, screen::in,
    message::in(message), screen_transition(sent)::out, io::di, io::uo) is det.

:- type continue_base
    --->    postponed_message
    ;       arbitrary_message.

:- pred continue_from_message(prog_config::in, crypto::in, screen::in,
    continue_base::in, message::in(message), screen_transition(sent)::out,
    io::di, io::uo) is det.

    % Exported for resend.
    %
:- pred parse_and_expand_addresses_string(prog_config::in, quote_opt::in,
    string::in, string::out, address_list::out, bool::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module dir.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module addressbook.
:- import_module call_system.
:- import_module callout.
:- import_module color.
:- import_module curs.
:- import_module curs.panel.
:- import_module detect_mime_type.
:- import_module forward.
:- import_module gpgme.
:- import_module gpgme.key.
:- import_module maildir.
:- import_module make_temp.
:- import_module message_file.
:- import_module mime_type.
:- import_module notmuch_config.
:- import_module pager.
:- import_module path_expand.
:- import_module quote_arg.
:- import_module rfc2047.
:- import_module rfc2047.decoder.
:- import_module rfc5322.writer.
:- import_module rfc6068.
:- import_module scrollable.
:- import_module send_util.
:- import_module size_util.
:- import_module splitmix64.
:- import_module string_util.
:- import_module tags.
:- import_module time_util.
:- import_module write_message.

:- include_module compose.crypto.
:- import_module compose.crypto.

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
                si_account      :: maybe(account),
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
                att_type        :: mime_type,
                att_content     :: attachment_content,
                att_filename    :: string,
                att_size        :: int
            ).

:- type attachment_content
    --->    text(string)
    ;       base64_encoded(string).

:- type staging_screen_action
    --->    continue
    ;       resize
    ;       edit
    ;       leave(sent, message_update).

:- type call_res
    --->    ok
    ;       error(string).

%-----------------------------------------------------------------------------%

start_compose(Config, Crypto, Screen, Transition, !ToHistory, !SubjectHistory,
        !IO) :-
    text_entry_initial(Screen, "To: ", !.ToHistory, "",
        complete_address(Config), MaybeInput, !IO),
    (
        MaybeInput = yes(Input),
        add_history_nodup(Input, !ToHistory),
        ( is_mailto_uri(Input) ->
            ( extract_mailto(Input, Headers, Body) ->
                start_compose_2(Config, Crypto, Screen, Headers, Body,
                    Transition, !SubjectHistory, !IO)
            ;
                Message = set_warning("Could not parse mailto URI."),
                Transition = screen_transition(not_sent, Message)
            )
        ;
            expand_aliases(Config, backslash_quote_meta_chars, Input, To, !IO),
            Headers0 = init_headers,
            Headers = Headers0 ^ h_to := header_value(To),
            Body = "",
            start_compose_2(Config, Crypto, Screen, Headers, Body, Transition,
                !SubjectHistory, !IO)
        )
    ;
        MaybeInput = no,
        Transition = screen_transition(not_sent, no_change)
    ).

:- pred start_compose_2(prog_config::in, crypto::in, screen::in, headers::in,
    string::in, screen_transition(sent)::out, history::in, history::out,
    io::di, io::uo) is det.

start_compose_2(Config, Crypto, Screen, !.Headers, Body, Transition,
        !SubjectHistory, !IO) :-
    Subject0 = header_value_string(!.Headers ^ h_subject),
    ( Subject0 = "" ->
        text_entry_initial(Screen, "Subject: ", !.SubjectHistory, "",
            complete_none, MaybeSubject, !IO),
        (
            MaybeSubject = yes(Subject),
            add_history_nodup(Subject, !SubjectHistory),
            !Headers ^ h_subject := decoded_unstructured(Subject),
            start_compose_3(Config, Crypto, Screen, !.Headers, Body,
                Transition, !IO)
        ;
            MaybeSubject = no,
            Transition = screen_transition(not_sent, no_change)
        )
    ;
        start_compose_3(Config, Crypto, Screen, !.Headers, Body, Transition,
            !IO)
    ).

:- pred start_compose_3(prog_config::in, crypto::in, screen::in, headers::in,
    string::in, screen_transition(sent)::out, io::di, io::uo) is det.

start_compose_3(Config, Crypto, Screen, !.Headers, Body, Transition, !IO) :-
    get_default_account(Config, MaybeAccount),
    (
        MaybeAccount = yes(Account),
        get_from_address_as_string(Account, From)
    ;
        MaybeAccount = no,
        From = ""
    ),
    !Headers ^ h_from := header_value(From),
    Attachments = [],
    MaybeOldDraft = no,
    create_edit_stage(Config, Crypto, Screen, !.Headers, Body, Attachments,
        MaybeOldDraft, no, no, Transition, !IO).

%-----------------------------------------------------------------------------%

:- pred extract_mailto(string::in, headers::out, string::out) is semidet.

extract_mailto(Input, !:Headers, Body) :-
    parse_mailto_uri(Input, Params),
    require_det
    (
        lookup_header_field(Params, "To", To),
        lookup_header_field(Params, "Cc", Cc),
        lookup_header_field(Params, "Bcc", Bcc),
        lookup_header_field(Params, "Subject", Subject0),
        lookup_header_field(Params, "Reply-To", ReplyTo),
        lookup_header_field(Params, "In-Reply-To", InReplyTo),
        lookup_header_field(Params, "body", Body0),
        rfc2047.decoder.decode_unstructured(Subject0, Subject),
        replace_crlf(Body0, Body),

        !:Headers = init_headers,
        !Headers ^ h_to := header_value(To),
        !Headers ^ h_cc := header_value(Cc),
        !Headers ^ h_bcc := header_value(Bcc),
        !Headers ^ h_subject := decoded_unstructured(Subject),
        !Headers ^ h_replyto := header_value(ReplyTo),
        !Headers ^ h_inreplyto := header_value(InReplyTo)
    ).

:- pred lookup_header_field(assoc_list(hfname, hfvalue)::in, string::in,
    string::out) is det.

lookup_header_field([], _, "").
lookup_header_field([K0 - V0 | T], K, V) :-
    ( strcase_equal(K0, K) ->
        V = V0
    ;
        lookup_header_field(T, K, V)
    ).

:- pred replace_crlf(string::in, string::out) is det.

replace_crlf(S0, S) :-
    string.replace_all(S0, "\r\n", "\n", S).

%-----------------------------------------------------------------------------%

:- pred expand_aliases(prog_config::in, quote_opt::in, string::in, string::out,
    io::di, io::uo) is det.

expand_aliases(Config, QuoteOpt, Input, Output, !IO) :-
    parse_and_expand_addresses_string(Config, QuoteOpt, Input, Output,
        _Addresses, _Valid, !IO).

%-----------------------------------------------------------------------------%

start_reply(Config, Crypto, Screen, Message, ReplyKind, Transition, !IO) :-
    get_notmuch_command(Config, Notmuch),
    Message ^ m_id = MessageId,
    WasEncrypted = contains(Message ^ m_tags, encrypted_tag),
    make_quoted_command(Notmuch, [
        "reply", reply_to_arg(ReplyKind), decrypt_arg(WasEncrypted),
        "--", message_id_to_search_term(MessageId)
    ], redirect_input("/dev/null"), no_redirect, Command),
    % Decryption may invoke pinentry-curses.
    curs.soft_suspend(
        call_system_capture_stdout(Command, no), CommandResult, !IO),
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
        SignInit = no,
        create_edit_stage(Config, Crypto, Screen, Headers, Text, Attachments,
            MaybeOldDraft, WasEncrypted, SignInit, Transition, !IO)
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

:- func decrypt_arg(bool) = string.

decrypt_arg(yes) = "--decrypt".
decrypt_arg(no) = "--decrypt=false".

:- pred set_headers_for_group_reply(headers::in, headers::out) is det.

set_headers_for_group_reply(!Headers) :-
    % Move all but the first To address down to Cc.  This acts more like the
    % behaviour I am used to from Mutt.
    % XXX do proper address list parsing

    To0 = header_value_string(!.Headers ^ h_to),
    Cc0 = header_value_string(!.Headers ^ h_cc),
    Opt = backslash_quote_all,
    parse_address_list(Opt, To0, ToList0),
    parse_address_list(Opt, Cc0, CcList0),
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
    Opt = backslash_quote_all,
    parse_address_list(Opt, header_value_string(OrigFrom), FromList),
    parse_address_list(Opt, To0, ToList0),
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

:- func contains(set(T), T) = bool.

contains(Set, X) = pred_to_bool(contains(Set, X)).

%-----------------------------------------------------------------------------%

start_reply_to_message_id(Config, Crypto, Screen, MessageId, ReplyKind,
        Transition, !IO) :-
    run_notmuch(Config,
        [
            "show", "--format=json", "--part=0", "--",
            message_id_to_search_term(MessageId)
        ],
        no_suspend_curses,
        parse_top_message, Res, !IO),
    (
        Res = ok(Message),
        (
            Message = message(_, _, _, _, _, _),
            start_reply(Config, Crypto, Screen, Message, ReplyKind, Transition,
                !IO)
        ;
            Message = excluded_message(_, _, _, _, _),
            Warning = "Excluded message.",
            Transition = screen_transition(not_sent, set_warning(Warning))
        )
    ;
        Res = error(Error),
        unexpected($module, $pred, Error)
    ).

%-----------------------------------------------------------------------------%

start_forward(Config, Crypto, Screen, Message, Transition, !IO) :-
    get_default_account(Config, MaybeAccount),
    (
        MaybeAccount = yes(Account),
        get_from_address_as_string(Account, From)
    ;
        MaybeAccount = no,
        From = ""
    ),
    prepare_forward_message(Message, From, Headers, Body, AttachmentParts),
    list.map(to_old_attachment, AttachmentParts, Attachments),
    MaybeOldDraft = no,
    WasEncrypted = contains(Message ^ m_tags, encrypted_tag),
    DraftSign = no,
    create_edit_stage(Config, Crypto, Screen, Headers, Body, Attachments,
        MaybeOldDraft, WasEncrypted, DraftSign, Transition, !IO).

%-----------------------------------------------------------------------------%

continue_from_message(Config, Crypto, Screen, ContinueBase, Message,
        Transition, !IO) :-
    MessageId = Message ^ m_id,
    Headers0 = Message ^ m_headers,
    Tags0 = Message ^ m_tags,
    Body0 = Message ^ m_body,
    select_body_text_and_attachments(Body0, Text, AttachmentParts),
    list.map(to_old_attachment, AttachmentParts, Attachments),
    WasEncrypted = contains(Tags0, encrypted_tag),
    DraftSign = contains(Tags0, draft_sign_tag),

    % XXX notmuch show --format=json does not return References and In-Reply-To
    % so we parse them from the raw output.
    % XXX decryption is not yet supported for --format=raw
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, [
        "show", "--format=raw", decrypt_arg(WasEncrypted),
        "--", message_id_to_search_term(MessageId)
    ], redirect_input("/dev/null"), no_redirect, Command),
    % Decryption may invoke pinentry-curses.
    curs.soft_suspend(call_system_capture_stdout(Command, no), CallRes, !IO),
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
        (
            ContinueBase = postponed_message,
            MaybeOldDraft = yes(MessageId)
        ;
            ContinueBase = arbitrary_message,
            MaybeOldDraft = no
        ),
        create_edit_stage(Config, Crypto, Screen, Headers, Text, Attachments,
            MaybeOldDraft, WasEncrypted, DraftSign, Transition, !IO)
    ;
        CallRes = error(Error),
        string.append_list(["Error running notmuch: ",
            io.error_message(Error)], Warning),
        Transition = screen_transition(not_sent, set_warning(Warning))
    ).

:- pred to_old_attachment(part::in, attachment::out) is det.

to_old_attachment(Part, old_attachment(Part)).

%-----------------------------------------------------------------------------%

:- pred create_edit_stage(prog_config::in, crypto::in, screen::in, headers::in,
    string::in, list(attachment)::in, maybe(message_id)::in, bool::in,
    bool::in, screen_transition(sent)::out, io::di, io::uo) is det.

create_edit_stage(Config, Crypto, Screen, Headers0, Text0, Attachments,
        MaybeOldDraft, EncryptInit, SignInit, Transition, !IO) :-
    get_encrypt_by_default(Config, EncryptByDefault),
    get_sign_by_default(Config, SignByDefault),
    CryptoInfo0 = init_crypto_info(Crypto,
        EncryptByDefault `or` EncryptInit,
        SignByDefault `or` SignInit),
    create_edit_stage_2(Config, Screen, Headers0, Text0, Attachments,
        MaybeOldDraft, Transition, CryptoInfo0, CryptoInfo, !IO),
    unref_keys(CryptoInfo, !IO).

:- pred create_edit_stage_2(prog_config::in, screen::in,
    headers::in, string::in, list(attachment)::in, maybe(message_id)::in,
    screen_transition(sent)::out, crypto_info::in, crypto_info::out,
    io::di, io::uo) is det.

create_edit_stage_2(Config, Screen, Headers0, Text0, Attachments,
        MaybeOldDraft, Transition, !CryptoInfo, !IO) :-
    make_parsed_headers(Headers0, ParsedHeaders0),
    create_temp_message_file(Config, prepare_edit, Headers0,
        ParsedHeaders0, Text0, Attachments, !.CryptoInfo, ResFilename,
        _MaybeWarning, !IO),
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
                    MaybeOldDraft, Transition, !CryptoInfo, !IO)
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
    Opt = backslash_quote_all,
    parse_address_list(Opt, header_value_string(From), ParsedFrom),
    parse_address_list(Opt, header_value_string(To), ParsedTo),
    parse_address_list(Opt, header_value_string(Cc), ParsedCc),
    parse_address_list(Opt, header_value_string(Bcc), ParsedBcc),
    parse_address_list(Opt, header_value_string(ReplyTo), ParsedReplyTo),

    Parsed = parsed_headers(ParsedFrom, ParsedTo, ParsedCc, ParsedBcc,
        ParsedReplyTo).

:- pred call_editor(prog_config::in, string::in, call_res::out, io::di, io::uo)
    is det.

call_editor(Config, Filename, Res, !IO) :-
    get_editor_command(Config, Editor),
    make_quoted_command(Editor, [Filename], no_redirect, no_redirect, Command),
    curs.suspend(io.call_system(Command), CallRes, !IO),
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
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

enter_staging(Config, Screen, Headers0, Text, Attachments, MaybeOldDraft,
        Transition, !CryptoInfo, !IO) :-
    parse_and_expand_headers(Config, Headers0, Headers, Parsed, !IO),
    get_some_matching_account(Config, Parsed ^ ph_from, MaybeAccount),
    maintain_encrypt_keys(Parsed, !CryptoInfo, !IO),
    maintain_sign_keys(Parsed, !CryptoInfo, !IO),

    StagingInfo = staging_info(Config, MaybeAccount, Headers, Parsed, Text,
        MaybeOldDraft, init_history),
    AttachInfo = scrollable.init_with_cursor(Attachments),
    get_cols(Screen, Cols),
    setup_pager_for_staging(Config, Cols, Text, new_pager, PagerInfo),
    staging_screen(Screen, StagingInfo, AttachInfo, PagerInfo, Transition,
        !CryptoInfo, !IO).

:- pred parse_and_expand_headers(prog_config::in, headers::in, headers::out,
    parsed_headers::out, io::di, io::uo) is det.

parse_and_expand_headers(Config, Headers0, Headers, Parsed, !IO) :-
    Headers0 = headers(Date, From0, To0, Cc0, Bcc0, Subject, ReplyTo0,
        References, InReplyTo, Rest),

    % [RFC 6854] allows group syntax in From - saves us work.
    Expand = parse_and_expand_addresses(Config, backslash_quote_all),
    Expand(From0, From, ParsedFrom, !IO),
    Expand(To0, To, ParsedTo, !IO),
    Expand(Cc0, Cc, ParsedCc, !IO),
    Expand(Bcc0, Bcc, ParsedBcc, !IO),
    Expand(ReplyTo0, ReplyTo, ParsedReplyTo, !IO),

    Headers = headers(Date, From, To, Cc, Bcc, Subject, ReplyTo,
        References, InReplyTo, Rest),
    Parsed = parsed_headers(ParsedFrom, ParsedTo, ParsedCc, ParsedBcc,
        ParsedReplyTo).

:- pred parse_and_expand_addresses(prog_config::in, quote_opt::in,
    header_value::in, header_value::out, address_list::out, io::di, io::uo)
    is det.

parse_and_expand_addresses(Config, Opt, Input, header_value(Output), Addresses,
        !IO) :-
    parse_and_expand_addresses_string(Config, Opt, header_value_string(Input),
        Output, Addresses, _Valid, !IO).

parse_and_expand_addresses_string(Config, Opt, Input, Output, Addresses, Valid,
        !IO) :-
    parse_address_list(Opt, Input, Addresses0),
    list.map_foldl2(maybe_expand_address(Config, Opt), Addresses0, Addresses,
        no, _Cache, !IO),
    address_list_to_string(no_encoding, Addresses, Output, Valid).

:- pred maybe_expand_address(prog_config::in, quote_opt::in,
    address::in, address::out,
    maybe(notmuch_config)::in, maybe(notmuch_config)::out, io::di, io::uo)
    is det.

maybe_expand_address(Config, Opt, Address0, Address, !Cache, !IO) :-
    (
        Address0 = mailbox(Mailbox0),
        maybe_expand_mailbox(Config, Opt, Mailbox0, Mailbox, !Cache, !IO),
        Address = mailbox(Mailbox)
    ;
        Address0 = group(DisplayName, Mailboxes0),
        list.map_foldl2(maybe_expand_mailbox(Config, Opt),
            Mailboxes0, Mailboxes, !Cache, !IO),
        Address = group(DisplayName, Mailboxes)
    ).

:- pred maybe_expand_mailbox(prog_config::in, quote_opt::in,
    mailbox::in, mailbox::out,
    maybe(notmuch_config)::in, maybe(notmuch_config)::out, io::di, io::uo)
    is det.

maybe_expand_mailbox(Config, Opt, Mailbox0, Mailbox, !Cache, !IO) :-
    (
        Mailbox0 = mailbox(_, _),
        Mailbox = Mailbox0
    ;
        Mailbox0 = bad_mailbox(PotentialAlias),
        (
            !.Cache = yes(NotmuchConfig)
        ;
            !.Cache = no,
            get_notmuch_command(Config, Notmuch),
            get_notmuch_config(Notmuch, ResConfig, !IO),
            (
                ResConfig = ok(NotmuchConfig)
            ;
                ResConfig = error(_Error),
                NotmuchConfig = empty_notmuch_config
            ),
            !:Cache = yes(NotmuchConfig)
        ),
        ( search_addressbook(NotmuchConfig, PotentialAlias, Expansion0) ->
            MaybeFound = yes(Expansion0)
        ;
            search_notmuch_address_top(Config, PotentialAlias, MaybeFound, !IO)
        ),
        (
            MaybeFound = yes(Expansion),
            % Can't expand to a group or multiple mailboxes yet.
            parse_address(Opt, Expansion, mailbox(Mailbox1))
        ->
            Mailbox = Mailbox1
        ;
            Mailbox = Mailbox0
        )
    ).

%-----------------------------------------------------------------------------%

:- pred staging_screen(screen::in, staging_info::in, attach_info::in,
    pager_info::in, screen_transition(sent)::out,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

staging_screen(Screen, !.StagingInfo, !.AttachInfo, !.PagerInfo, Transition,
        !CryptoInfo, !IO) :-
    !.StagingInfo = staging_info(Config, MaybeAccount, Headers, ParsedHeaders,
        Text, MaybeOldDraft, _AttachHistory),
    Attrs = compose_attrs(Config),
    PagerAttrs = pager_attrs(Config),

    split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels),
    draw_header_lines(HeaderPanels, Attrs, Headers, ParsedHeaders,
        MaybeAccount, !.CryptoInfo, !IO),
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
        edit_header(Screen, from, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('t') ->
        edit_header(Screen, to, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('c') ->
        edit_header(Screen, cc, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('b') ->
        edit_header(Screen, bcc, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('s') ->
        edit_header(Screen, subject, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('r') ->
        edit_header(Screen, replyto, !StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('E') ->
        toggle_encrypt(!StagingInfo, !CryptoInfo, !IO),
        Action = continue
    ; KeyCode = char('S') ->
        toggle_sign(!StagingInfo, !CryptoInfo, !IO),
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
            !.CryptoInfo, Res, PostponeMsg, !IO),
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
        (
            MaybeAccount = yes(Account),
            send_mail(Config, Account, Screen, Headers, ParsedHeaders, Text,
                Attachments, !.CryptoInfo, Sent0, MessageUpdate0, !IO)
        ;
            MaybeAccount = no,
            Sent0 = not_sent,
            MessageUpdate0 = set_warning("No account to send message from.")
        ),
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
            Transition, !CryptoInfo, !IO)
    ;
        Action = resize,
        resize_staging_screen(Screen, NewScreen, !.StagingInfo, !PagerInfo,
            !IO),
        staging_screen(NewScreen, !.StagingInfo, !.AttachInfo, !.PagerInfo,
            Transition, !CryptoInfo, !IO)
    ;
        Action = edit,
        EditAttachments = get_lines_list(!.AttachInfo),
        % XXX make this tail-recursive in hlc
        create_edit_stage_2(Config, Screen, Headers, Text, EditAttachments,
            MaybeOldDraft, Transition, !CryptoInfo, !IO)
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
    staging_info::in, staging_info::out, crypto_info::in, crypto_info::out,
    io::di, io::uo) is det.

edit_header(Screen, HeaderType, !StagingInfo, !CryptoInfo, !IO) :-
    Config = !.StagingInfo ^ si_config,
    Headers0 = !.StagingInfo ^ si_headers,
    get_header(HeaderType, Headers0, Prompt, Initial),
    InitialString = header_value_string(Initial),
    (
        HeaderType = from,
        make_from_history(!.StagingInfo ^ si_config, InitialString, History0),
        Completion = complete_address(Config)
    ;
        ( HeaderType = to
        ; HeaderType = cc
        ; HeaderType = bcc
        ; HeaderType = replyto
        ),
        History0 = init_history,
        Completion = complete_address(Config)
    ;
        HeaderType = subject,
        History0 = init_history,
        Completion = complete_none
    ),
    text_entry_full(Screen, Prompt, History0, InitialString,
        Completion, no, Return, !IO),
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
        update_header(Config, backslash_quote_meta_chars, HeaderType, Value,
            Headers0, Headers, ParsedHeaders0, ParsedHeaders, !IO),
        get_some_matching_account(Config, ParsedHeaders ^ ph_from,
            MaybeAccount),
        !StagingInfo ^ si_headers := Headers,
        !StagingInfo ^ si_parsed_hdrs := ParsedHeaders,
        !StagingInfo ^ si_account := MaybeAccount,
        maintain_encrypt_keys(ParsedHeaders, !CryptoInfo, !IO),
        maintain_sign_keys(ParsedHeaders, !CryptoInfo, !IO)
    ;
        Return = no
    ).

:- pred make_from_history(prog_config::in, string::in, history::out) is det.

make_from_history(Config, Initial, History) :-
    get_all_accounts(Config, Accounts),
    map(get_from_address_as_string, Accounts, Strings0),
    ( delete_first(Strings0, Initial, Strings) ->
        History = init_history_list(Strings)
    ;
        History = init_history_list(Strings0)
    ).

:- pred get_header(header_type::in, headers::in, string::out,
    header_value::out) is det.

get_header(from,    H, "From: ",     H ^ h_from).
get_header(to,      H, "To: ",       H ^ h_to).
get_header(cc,      H, "Cc: ",       H ^ h_cc).
get_header(bcc,     H, "Bcc: ",      H ^ h_bcc).
get_header(subject, H, "Subject: ",  H ^ h_subject).
get_header(replyto, H, "Reply-To: ", H ^ h_replyto).

:- pred update_header(prog_config::in, quote_opt::in,
    header_type::in, header_value::in, headers::in, headers::out,
    parsed_headers::in, parsed_headers::out, io::di, io::uo) is det.

update_header(Config, Opt, HeaderType, Input, !Headers, !Parsed, !IO) :-
    (
        HeaderType = from,
        parse_and_expand_addresses(Config, Opt, Input, Output, Parsed, !IO),
        !Headers ^ h_from := Output,
        !Parsed ^ ph_from := Parsed
    ;
        HeaderType = to,
        parse_and_expand_addresses(Config, Opt, Input, Output, Parsed, !IO),
        !Headers ^ h_to := Output,
        !Parsed ^ ph_to := Parsed
    ;
        HeaderType = cc,
        parse_and_expand_addresses(Config, Opt, Input, Output, Parsed, !IO),
        !Headers ^ h_cc := Output,
        !Parsed ^ ph_cc := Parsed
    ;
        HeaderType = bcc,
        parse_and_expand_addresses(Config, Opt, Input, Output, Parsed, !IO),
        !Headers ^ h_bcc := Output,
        !Parsed ^ ph_bcc := Parsed
    ;
        HeaderType = replyto,
        parse_and_expand_addresses(Config, Opt, Input, Output, Parsed, !IO),
        !Headers ^ h_replyto := Output,
        !Parsed ^ ph_replyto := Parsed
    ;
        HeaderType = subject,
        !Headers ^ h_subject := Input
    ).

%-----------------------------------------------------------------------------%

:- pred toggle_encrypt(staging_info::in, staging_info::out,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

toggle_encrypt(!StagingInfo, !CryptoInfo, !IO) :-
    !CryptoInfo ^ ci_encrypt := not(!.CryptoInfo ^ ci_encrypt),
    ParsedHeaders = !.StagingInfo ^ si_parsed_hdrs,
    maintain_encrypt_keys(ParsedHeaders, !CryptoInfo, !IO).

:- pred toggle_sign(staging_info::in, staging_info::out,
    crypto_info::in, crypto_info::out, io::di, io::uo) is det.

toggle_sign(!StagingInfo, !CryptoInfo, !IO) :-
    !CryptoInfo ^ ci_sign := not(!.CryptoInfo ^ ci_sign),
    ParsedHeaders = !.StagingInfo ^ si_parsed_hdrs,
    maintain_sign_keys(ParsedHeaders, !CryptoInfo, !IO).

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
    detect_mime_type(FileName, ResDetect, !IO),
    (
        ResDetect = ok(mime_type_with_charset(Type, Charset)),
        ( dir.basename(FileName, BaseName0) ->
            BaseName = BaseName0
        ;
            BaseName = FileName
        ),
        ( acceptable_charset(Charset) ->
            do_attach_text_file(FileName, BaseName, Type, NumRows,
                MessageUpdate, !AttachInfo, !IO)
        ;
            do_attach_file_with_base64_encoding(FileName, BaseName, Type,
                NumRows, MessageUpdate, !AttachInfo, !IO)
        )
    ;
        ResDetect = error(Error),
        Msg = io.error_message(Error),
        MessageUpdate = set_warning(Msg)
    ).

:- pred acceptable_charset(string::in) is semidet.

acceptable_charset(Charset) :-
    ( strcase_equal(Charset, "us-ascii")
    ; strcase_equal(Charset, "utf-8")
    ).

:- pred do_attach_text_file(string::in, string::in, mime_type::in, int::in,
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

:- pred do_attach_file_with_base64_encoding(string::in, string::in,
    mime_type::in, int::in, message_update::out,
    attach_info::in, attach_info::out, io::di, io::uo) is det.

do_attach_file_with_base64_encoding(FileName, BaseName, Type, NumRows,
        MessageUpdate, !AttachInfo, !IO) :-
    make_quoted_command(base64_command, [FileName],
        redirect_input("/dev/null"), no_redirect, Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
    (
        CallRes = ok(Content),
        string.length(Content, Size),
        NewAttachment = new_attachment(Type, base64_encoded(Content), BaseName,
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
            TypeString0 = mime_type.to_string(Type0),
            text_entry_full(Screen, "Media type: ", History, TypeString0,
                complete_none, no, Return, !IO),
            (
                Return = yes(TypeString),
                TypeString \= ""
            ->
                ( accept_media_type(TypeString, Type) ->
                    Attachment = new_attachment(Type, Content, FileName, Size),
                    scrollable.set_cursor_line(Attachment, !AttachInfo),
                    MessageUpdate = clear_message
                ;
                    Msg = "Refusing to set media type: " ++ TypeString,
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

:- pred accept_media_type(string::in, mime_type::out) is semidet.

accept_media_type(String, MimeType) :-
    parse_mime_type(String, MimeType, Type, _SubType),
    ( Type = "application"
    ; Type = "audio"
    ; Type = "image"
   %; Type = "message"
    ; Type = "model"
   %; Type = "multipart"
    ; Type = "text"
    ; Type = "video"
    ).

%-----------------------------------------------------------------------------%

:- pred split_panels(screen::in, list(panel)::out, list(panel)::out,
    maybe(panel)::out, list(panel)::out) is det.

split_panels(Screen, HeaderPanels, AttachmentPanels, MaybeSepPanel,
        PagerPanels) :-
    get_main_panels(Screen, Panels0),
    list.split_upto(7, Panels0, HeaderPanels, Panels1),
    list.split_upto(3, Panels1, AttachmentPanels, Panels2),
    (
        Panels2 = [SepPanel | PagerPanels],
        MaybeSepPanel = yes(SepPanel)
    ;
        Panels2 = [],
        MaybeSepPanel = no,
        PagerPanels = []
    ).

:- pred draw_header_lines(list(panel)::in, compose_attrs::in,
    headers::in, parsed_headers::in, maybe(account)::in, crypto_info::in,
    io::di, io::uo) is det.

draw_header_lines(!.Panels, Attrs, Headers, Parsed, Account, CryptoInfo, !IO)
        :-
    DrawFrom = draw_addresses_and_account(Attrs, show_crypto(yes, yes),
        CryptoInfo, Account),
    DrawRecv = draw_addresses(Attrs, show_crypto(yes, no), CryptoInfo),
    DrawSubj = draw_unstruct(Attrs),
    DrawRepl = draw_addresses(Attrs, show_crypto(no, no), CryptoInfo),
    hdr(!Panels, Attrs, DrawFrom, "    From", Parsed ^ ph_from, !IO),
    hdr(!Panels, Attrs, DrawRecv, "      To", Parsed ^ ph_to, !IO),
    hdr(!Panels, Attrs, DrawRecv, "      Cc", Parsed ^ ph_cc, !IO),
    hdr(!Panels, Attrs, DrawRecv, "     Bcc", Parsed ^ ph_bcc, !IO),
    hdr(!Panels, Attrs, DrawSubj, " Subject", Headers ^ h_subject, !IO),
    hdr(!Panels, Attrs, DrawRepl, "Reply-To", Parsed ^ ph_replyto, !IO),
    (
        !.Panels = []
    ;
        !.Panels = [CryptoPanel | _],
        draw_crypto_line(Attrs, CryptoPanel, CryptoInfo, !IO)
    ).

:- pred hdr(list(panel), list(panel), compose_attrs,
    pred(panel, T, io, io), string, T, io, io).
:- mode hdr(in, out, in,
    pred(in, in, di, uo) is det, in, in, di, uo) is det.

hdr(Panels0, Panels, Attrs, DrawValue, FieldName, Value, !IO) :-
    (
        Panels0 = [],
        Panels = []
    ;
        Panels0 = [Panel | Panels],
        panel.erase(Panel, !IO),
        draw(Panel, Attrs ^ c_generic ^ field_name, FieldName, !IO),
        draw(Panel, ": ", !IO),
        DrawValue(Panel, Value, !IO)
    ).

:- pred draw_unstruct(compose_attrs::in, panel::in, header_value::in,
    io::di, io::uo) is det.

draw_unstruct(Attrs, Panel, Value, !IO) :-
    String = header_value_string(Value),
    draw(Panel, Attrs ^ c_generic ^ field_body, String, !IO).

:- pred draw_list(pred(panel, T, io, io), panel, attr, list(T), io, io).
:- mode draw_list(pred(in, in, di, uo) is det, in, in, in, di, uo) is det.

draw_list(_Pred, _Panel, _Attr, [], !IO).
draw_list(Pred, Panel, Attr, [H | T], !IO) :-
    Pred(Panel, H, !IO),
    (
        T = []
    ;
        T = [_ | _],
        draw(Panel, Attr, ", ", !IO),
        draw_list(Pred, Panel, Attr, T, !IO)
    ).

:- type show_crypto
    --->    show_crypto(
                show_encrypt_key    :: bool,
                show_sign_key       :: bool
            ).

:- pred draw_addresses(compose_attrs::in, show_crypto::in, crypto_info::in,
    panel::in, list(address)::in, io::di, io::uo) is det.

draw_addresses(Attrs, ShowCrypto, CryptoInfo, Panel, Addresses, !IO) :-
    Attr = Attrs ^ c_generic ^ field_body,
    draw_list(draw_address(Attrs, ShowCrypto, CryptoInfo),
        Panel, Attr, Addresses, !IO).

:- pred draw_addresses_and_account(compose_attrs::in, show_crypto::in,
    crypto_info::in, maybe(account)::in, panel::in, list(address)::in,
    io::di, io::uo) is det.

draw_addresses_and_account(Attrs, ShowCrypto, CryptoInfo, MaybeAccount,
        Panel, Addresses, !IO) :-
    draw_addresses(Attrs, ShowCrypto, CryptoInfo, Panel, Addresses, !IO),
    Attr = Attrs ^ c_generic ^ field_body,
    (
        MaybeAccount = yes(Account),
        get_account_name(Account, Name),
        AccountString = " (account: " ++ Name ++ ")"
    ;
        MaybeAccount = no,
        AccountString = " (no account)"
    ),
    draw(Panel, Attr, AccountString, !IO).

:- pred draw_address(compose_attrs::in, show_crypto::in, crypto_info::in,
    panel::in, address::in, io::di, io::uo) is det.

draw_address(Attrs, ShowCrypto, CryptoInfo, Panel, Address, !IO) :-
    (
        Address = mailbox(Mailbox),
        draw_mailbox(Attrs, ShowCrypto, CryptoInfo, Panel, Mailbox, !IO)
    ;
        Address = group(DisplayName, Mailboxes),
        draw_display_name(Attrs, Panel, DisplayName, !IO),
        Attr = Attrs ^ c_generic ^ field_body,
        draw(Panel, Attr, ": ", !IO),
        draw_list(draw_mailbox(Attrs, ShowCrypto, CryptoInfo), Panel, Attr,
            Mailboxes, !IO),
        draw(Panel, Attr, ";", !IO)
    ).

:- pred draw_display_name(compose_attrs::in, panel::in, display_name::in,
    io::di, io::uo) is det.

draw_display_name(Attrs, Panel, DisplayName, !IO) :-
    display_name_to_string(for_display, DisplayName, String, Valid),
    (
        Valid = yes,
        Attr = Attrs ^ c_generic ^ field_body
    ;
        Valid = no,
        Attr = Attrs ^ c_invalid
    ),
    draw(Panel, Attr, String, !IO).

:- pred draw_mailbox(compose_attrs::in, show_crypto::in, crypto_info::in,
    panel::in, mailbox::in, io::di, io::uo) is det.

draw_mailbox(Attrs, ShowCrypto, CryptoInfo, Panel, Mailbox, !IO) :-
    (
        Mailbox = mailbox(yes(DisplayName), AddrSpec),
        draw_display_name(Attrs, Panel, DisplayName, !IO),
        Attr = Attrs ^ c_generic ^ field_body,
        draw(Panel, Attr, " <", !IO),
        draw_addr_spec(Attrs, Panel, AddrSpec, !IO),
        draw(Panel, Attr, ">", !IO),
        draw_mailbox_crypto(Attrs, ShowCrypto, CryptoInfo, Panel, AddrSpec,
            !IO)
    ;
        Mailbox = mailbox(no, AddrSpec),
        draw_addr_spec(Attrs, Panel, AddrSpec, !IO),
        draw_mailbox_crypto(Attrs, ShowCrypto, CryptoInfo, Panel, AddrSpec,
            !IO)
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

:- pred draw_mailbox_crypto(compose_attrs::in, show_crypto::in,
    crypto_info::in, panel::in, addr_spec::in, io::di, io::uo) is det.

draw_mailbox_crypto(Attrs, ShowCrypto, CryptoInfo, Panel, AddrSpec, !IO) :-
    ShowCrypto = show_crypto(ShowEncryptKey, ShowSignKey),
    CryptoInfo = crypto_info(_, Encrypt, EncryptKeys, Sign, SignKeys),
    E = Encrypt `and` ShowEncryptKey,
    S = Sign `and` ShowSignKey,
    (
        E = yes,
        (
            S = yes,
            PrefixE = "E"
        ;
            S = no,
            PrefixE = ""
        ),
        ( map.search(EncryptKeys, AddrSpec, EncryptKey) ->
            DrawE = found(EncryptKey)
        ;
            DrawE = not_found
        )
    ;
        E = no,
        DrawE = none,
        PrefixE = ""
    ),
    (
        S = yes,
        PrefixS = "S",
        ( map.search(SignKeys, AddrSpec, SignKey) ->
            DrawS = found(SignKey)
        ;
            DrawS = not_found
        )
    ;
        S = no,
        DrawS = none,
        PrefixS = ""
    ),
    ( combine(DrawE, DrawS) ->
        % Combine E: S:
        draw_key(Attrs, Panel, PrefixE ++ PrefixS, DrawE, !IO)
    ;
        draw_key(Attrs, Panel, PrefixE, DrawE, !IO),
        draw_key(Attrs, Panel, PrefixS, DrawS, !IO)
    ).

:- type draw_key
    --->    none
    ;       found(key_userid)
    ;       not_found.

:- pred combine(draw_key::in, draw_key::in) is semidet.

combine(X, Y) :-
    X = found(key_userid(KeyX, UserId)),
    Y = found(key_userid(KeyY, UserId)),
    get_key_info(KeyX) = get_key_info(KeyY).

:- pred draw_key(compose_attrs::in, panel::in, string::in, draw_key::in,
    io::di, io::uo) is det.

draw_key(_Attrs, _Panel, _Prefix, none, !IO).
draw_key(Attrs, Panel, Prefix, found(key_userid(Key, UserId)), !IO) :-
    KeyInfo = get_key_info(Key),
    SubKeys = KeyInfo ^ key_subkeys,
    ( SubKeys = [SubKey | _] ->
        Fpr = short_fpr(SubKey ^ subkey_fingerprint),
        validity_attr(Attrs, UserId ^ uid_validity, Validity, Attr),
        ( Prefix = "" ->
            Text = format(" [%s, validity: %s]",
                [s(Fpr), s(Validity)])
        ;
            Text = format(" [%s: %s, validity: %s]",
                [s(Prefix), s(Fpr), s(Validity)])
        ),
        draw(Panel, Attr, Text, !IO)
    ;
        % Should not happen. 
        draw(Panel, Attrs ^ c_generic ^ good_key, " [no subkey]", !IO)
    ).
draw_key(Attrs, Panel, Prefix, not_found, !IO) :-
    ( Prefix = "" ->
        Text = " [no key found]"
    ;
        Text = format(" [%s: no key found]", [s(Prefix)])
    ),
    draw(Panel, Attrs ^ c_generic ^ bad_key, Text, !IO).

:- func short_fpr(string) = string.

short_fpr(Fpr) = string.right_by_codepoint(Fpr, 8).

:- pred validity_attr(compose_attrs::in, validity::in, string::out, attr::out)
    is det.

validity_attr(Attrs, Validity, String, Attr) :-
    validity(Validity, String, GoodKey),
    (
        GoodKey = yes,
        Attr = Attrs ^ c_generic ^ good_key
    ;
        GoodKey = no,
        Attr = Attrs ^ c_generic ^ bad_key
    ).

:- pred validity(validity::in, string::out, bool::out) is det.

validity(validity_unknown, "unknown", no).
validity(validity_undefined, "undefined", no).
validity(validity_never, "never", no).
validity(validity_marginal, "marginal", yes).
validity(validity_full, "full", yes).
validity(validity_ultimate, "ultimate", yes).

:- pred draw_crypto_line(compose_attrs::in, panel::in, crypto_info::in,
    io::di, io::uo) is det.

draw_crypto_line(Attrs, Panel, CryptoInfo, !IO) :-
    CryptoInfo ^ ci_encrypt = Encrypt,
    CryptoInfo ^ ci_sign = Sign,
    (
        Encrypt = yes,
        Sign = yes,
        Body = "encrypt & sign message body"
    ;
        Encrypt = yes,
        Sign = no,
        Body = "encrypt message body"
    ;
        Encrypt = no,
        Sign = yes,
        Body = "sign message body"
    ;
        Encrypt = no,
        Sign = no,
        Body = "none"
    ),
    panel.erase(Panel, !IO),
    draw(Panel, Attrs ^ c_generic ^ field_name, "  Crypto: ", !IO),
    draw(Panel, Attrs ^ c_generic ^ field_body, Body, !IO).

:- pred draw_attachment_line(compose_attrs::in, panel::in, attachment::in,
    int::in, bool::in, io::di, io::uo) is det.

draw_attachment_line(Attrs, Panel, Attachment, LineNr, IsCursor, !IO) :-
    (
        Attachment = old_attachment(Part),
        Part = part(_MessageId, _PartId, ContentType, MaybeContentDisposition,
            _Content, MaybeFilename, MaybeContentLength, _MaybeCTE,
            _IsDecrypted),
        (
            MaybeFilename = yes(filename(Filename))
        ;
            MaybeFilename = no,
            Filename = "(no filename)"
        )
    ;
        Attachment = new_attachment(ContentType, _, Filename, Size),
        MaybeContentDisposition = no,
        MaybeContentLength = yes(content_length(Size))
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
    draw(Panel, Attr, " (", !IO),
    draw(Panel, Attr, mime_type.to_string(ContentType), !IO),
    /*
    (
        MaybeContentCharset = yes(content_charset(Charset)),
        draw(Panel, Attr, "; charset=" ++ Charset, !IO)
    ;
        MaybeContentCharset = no
    ),
    */
    (
        MaybeContentDisposition = yes(content_disposition(Disposition)),
        Disposition \= "attachment"
    ->
        draw(Panel, Attr, "; " ++ Disposition, !IO)
    ;
        true
    ),
    draw(Panel, Attr, ")", !IO),
    (
        MaybeContentLength = yes(content_length(Length)),
        % This is the encoded size.
        draw2(Panel, " ", format_approx_length(Length), !IO)
    ;
        MaybeContentLength = no
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
    draw(Panel, Attr,
        "-- (ftcbsr) edit fields; (E)ncrypt, (S)ign; " ++
        "(a)ttach, (d)etach, media (T)ype ", !IO),
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
    string::in, list(attachment)::in, crypto_info::in, bool::out,
    message_update::out, io::di, io::uo) is det.

postpone(Config, Screen, Headers, ParsedHeaders, Text, Attachments, CryptoInfo,
        Res, MessageUpdate, !IO) :-
    create_temp_message_file(Config, prepare_postpone, Headers, ParsedHeaders,
        Text, Attachments, CryptoInfo, ResFilename, Warnings, !IO),
    (
        ResFilename = ok(Filename),
        (
            Warnings = [],
            Confirmation = yes
        ;
            Warnings = [_ | _],
            Prompt = string.join_list(" ", Warnings) ++ " Continue? (Y/n)",
            prompt_confirm(Screen, Prompt, Confirmation, !IO)
        ),
        (
            Confirmation = yes,
            update_message_immed(Screen, set_info("Postponing message..."),
                !IO),
            Sign = CryptoInfo ^ ci_sign,
            (
                Sign = yes,
                ExtraTagDeltas = [tag_delta("+draft-sign")]
            ;
                Sign = no,
                ExtraTagDeltas = []
            ),
            add_draft(Config, Filename, ExtraTagDeltas, DraftRes, !IO),
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
            Confirmation = no,
            MessageUpdate = set_info("Message not postponed."),
            Res = no
        ),
        io.remove_file(Filename, _, !IO)
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

:- pred send_mail(prog_config::in, account::in, screen::in, headers::in,
    parsed_headers::in, string::in, list(attachment)::in, crypto_info::in,
    sent::out, message_update::out, io::di, io::uo) is det.

send_mail(Config, Account, Screen, Headers, ParsedHeaders, Text, Attachments,
        CryptoInfo, Res, MessageUpdate, !IO) :-
    create_temp_message_file(Config, prepare_send, Headers, ParsedHeaders,
        Text, Attachments, CryptoInfo, ResFilename, Warnings, !IO),
    (
        ResFilename = ok(Filename),
        prompt_confirm_warnings(Screen, Warnings, ConfirmAll, !IO),
        (
            ConfirmAll = yes,
            update_message_immed(Screen, set_info("Sending message..."), !IO),
            call_send_mail(Config, Account, Filename, SendRes, !IO),
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
            ConfirmAll = no,
            MessageUpdate = set_info("Message not sent."),
            Res = not_sent
        ),
        io.remove_file(Filename, _, !IO)
    ;
        ResFilename = error(Error),
        MessageUpdate = set_warning(Error),
        Res = not_sent
    ).

:- pred prompt_confirm_warnings(screen::in, list(string)::in, bool::out,
    io::di, io::uo) is det.

prompt_confirm_warnings(Screen, Warnings, Res, !IO) :-
    (
        Warnings = [],
        Res = yes
    ;
        Warnings = [Warning | MoreWarnings],
        Prompt = Warning ++ " Continue? (Y/n)",
        prompt_confirm(Screen, Prompt, Res0, !IO),
        (
            Res0 = yes,
            prompt_confirm_warnings(Screen, MoreWarnings, Res, !IO)
        ;
            Res0 = no,
            Res = no
        )
    ).

:- pred prompt_confirm(screen::in, string::in, bool::out, io::di, io::uo)
    is det.

prompt_confirm(Screen, Prompt, Res, !IO) :-
    update_message_immed(Screen, set_prompt(Prompt), !IO),
    get_keycode_blocking(KeyCode, !IO),
    ( KeyCode = char('Y') ->
        Res = yes
    ; KeyCode = char('y') ->
        Res = yes
    ;
        Res = no
    ).

:- pred call_send_mail(prog_config::in, account::in, string::in, call_res::out,
    io::di, io::uo) is det.

call_send_mail(Config, Account, Filename, Res, !IO) :-
    get_sendmail_command(Account, sendmail_read_recipients, Sendmail),
    make_quoted_command(Sendmail, [], redirect_input(Filename), no_redirect,
        Command),
    % e.g. msmtp 'passwordeval' option may invoke pinentry-curses.
    curs.soft_suspend(io.call_system(Command), ResSend, !IO),
    (
        ResSend = ok(ExitStatus),
        ( ExitStatus = 0 ->
            do_post_sendmail(Config, Account, Filename, ResAfter, !IO),
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

:- pred do_post_sendmail(prog_config::in, account::in, string::in,
    maybe_error::out, io::di, io::uo) is det.

do_post_sendmail(Config, Account, Filename, Res, !IO) :-
    get_post_sendmail_action(Account, Action),
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
        curs.soft_suspend(io.call_system(Command), ResCall, !IO),
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
    crypto_info::in, maybe_error(string)::out, list(string)::out,
    io::di, io::uo) is det.

create_temp_message_file(Config, Prepare, Headers, ParsedHeaders, Text,
        Attachments, CryptoInfo, Res, Warnings, !IO) :-
    % We only use this to generate MIME boundaries.
    current_timestamp(timestamp(Seed), !IO),
    splitmix64.init(truncate_to_int(Seed), RS0),
    get_message_id_right_part(Config, MessageIdRight),
    generate_date_msg_id(MessageIdRight, Date, MessageId, !IO),
    make_headers(Prepare, Headers, ParsedHeaders, Date, MessageId,
        WriteHeaders),
    (
        Prepare = prepare_edit,
        Spec = message_spec(WriteHeaders, plain(plain_body(Text))),
        curs.soft_suspend(
            write_temp_message_file(Config, Prepare, Spec, i_paused_curses),
            Res, !IO),
        Warnings = []
    ;
        (
            Prepare = prepare_send,
            Sign = CryptoInfo ^ ci_sign,
            Encrypt = CryptoInfo ^ ci_encrypt,
            EncryptForWhom = from_and_recipients
        ;
            Prepare = prepare_postpone,
            Sign = no,
            Encrypt = CryptoInfo ^ ci_encrypt,
            EncryptForWhom = from_only
        ),
        (
            Encrypt = no,
            Sign = no,
            generate_boundary(MixedBoundary, RS0, _RS),
            make_text_and_attachments_mime_part(cte_8bit, Text, Attachments,
                boundary(MixedBoundary), MimePart),
            Spec = message_spec(WriteHeaders, mime_v1(MimePart)),
            curs.soft_suspend(
                write_temp_message_file(Config, Prepare, Spec,
                    i_paused_curses), Res, !IO),
            Warnings = []
        ;
            Encrypt = yes,
            (
                Sign = no,
                MaybeSigners = no,
                TextCTE = cte_8bit
            ;
                Sign = yes,
                get_sign_keys(CryptoInfo, ParsedHeaders, SignKeys),
                MaybeSigners = yes(SignKeys),
                % Not sure if this is required by RFC 3156:
                % "Messages which are encrypted and signed in this
                % combined fashion are REQUIRED to follow the same
                % canonicalization rules as multipart/signed objects."
                TextCTE = cte_base64
            ),
            get_encrypt_keys(CryptoInfo, ParsedHeaders, EncryptForWhom,
                EncryptKeys, Missing, LeakedBccs),
            ( MaybeSigners = yes([]) ->
                Res = error("No signing keys available."),
                Warnings = []
            ;
                EncryptKeys = [],
                Res = error("No encryption keys available."),
                Warnings = []
            ;
                EncryptKeys = [_ | _],
                missing_keys_warning(Missing, WarningsA),
                leaked_bccs_warning(LeakedBccs, WarningsB),
                generate_boundary(MixedBoundary, RS0, RS1),
                make_text_and_attachments_mime_part(TextCTE, Text, Attachments,
                    boundary(MixedBoundary), PartToEncrypt),
                generate_boundary(EncryptedBoundary, RS1, _RS),
                curs.soft_suspend(
                    encrypt_then_write_temp_message_file(Config, Prepare,
                        WriteHeaders, PartToEncrypt,
                        CryptoInfo, MaybeSigners, EncryptKeys,
                        boundary(EncryptedBoundary), i_paused_curses),
                    {Res, WarningsC}, !IO),
                Warnings = WarningsA ++ WarningsB ++ WarningsC
            )
        ;
            Encrypt = no,
            Sign = yes,
            get_sign_keys(CryptoInfo, ParsedHeaders, SignKeys),
            (
                SignKeys = [],
                Res = error("No signing keys available."),
                Warnings = []
            ;
                SignKeys = [_ | _],
                % Force text parts to be base64-encoded to avoid being mangled
                % during transfer.
                generate_boundary(MixedBoundary, RS0, RS1),
                make_text_and_attachments_mime_part(cte_base64, Text, Attachments,
                    boundary(MixedBoundary), PartToSign),
                generate_boundary(SignedBoundary, RS1, _RS),
                curs.soft_suspend(
                    sign_detached_then_write_temp_message_file(Config, Prepare,
                        WriteHeaders, PartToSign, CryptoInfo, SignKeys,
                        boundary(SignedBoundary), i_paused_curses),
                    {Res, Warnings}, !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred make_headers(prepare_temp::in, headers::in, parsed_headers::in,
    header_value::in, header_value::in, list(header)::out) is det.

make_headers(Prepare, Headers, ParsedHeaders, Date, MessageId, WriteHeaders) :-
    Headers = headers(_Date, _From, _To, _Cc, _Bcc, Subject, _ReplyTo,
        References, InReplyTo, RestHeaders),
    ParsedHeaders = parsed_headers(From, To, Cc, Bcc, ReplyTo),
    some [!Acc] (
        !:Acc = [],
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            cons(header(field_name("Date"), unstructured(Date, no_encoding)),
                !Acc)
        ;
            Prepare = prepare_edit
        ),
        (
            Prepare = prepare_send,
            cons(header(field_name("Message-ID"), unstructured(MessageId,
                no_encoding)), !Acc)
        ;
            Prepare = prepare_postpone
        ;
            Prepare = prepare_edit
        ),
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            SkipEmpty = yes,
            Options = rfc2047_encoding
        ;
            Prepare = prepare_edit,
            SkipEmpty = no,
            Options = no_encoding
        ),
        list.foldl(maybe_cons(SkipEmpty), [
            header(field_name("From"), address_list(From, Options)),
            header(field_name("To"), address_list(To, Options)),
            header(field_name("Cc"), address_list(Cc, Options)),
            header(field_name("Bcc"), address_list(Bcc, Options)),
            header(field_name("Subject"), unstructured(Subject, Options)),
            header(field_name("Reply-To"), address_list(ReplyTo, Options)),
            header(field_name("In-Reply-To"), references(InReplyTo))],
            !Acc),
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            cons(header(field_name("References"), references(References)),
                !Acc)
        ;
            Prepare = prepare_edit
        ),
        map.foldl(maybe_cons_unstructured(SkipEmpty, Options), RestHeaders,
            !Acc),
        list.reverse(!.Acc, WriteHeaders)
    ).

:- pred maybe_cons(bool::in, header::in, list(header)::in, list(header)::out)
    is det.

maybe_cons(SkipEmpty, Header, !Acc) :-
    Header = header(_, Body),
    (
        SkipEmpty = yes,
        is_empty_field_body(Body)
    ->
        true
    ;
        cons(Header, !Acc)
    ).

:- pred maybe_cons_unstructured(bool::in, write_header_options::in,
    string::in, header_value::in, list(header)::in, list(header)::out) is det.

maybe_cons_unstructured(SkipEmpty, Options, FieldName, Value, !Acc) :-
    (
        SkipEmpty = yes,
        is_empty_header_value(Value)
    ->
        true
    ;
        cons(header(field_name(FieldName), unstructured(Value, Options)), !Acc)
    ).

%-----------------------------------------------------------------------------%

:- pred make_text_and_attachments_mime_part(write_content_transfer_encoding::in,
    string::in, list(attachment)::in, boundary::in, mime_part::out) is det.

make_text_and_attachments_mime_part(TextCTE, Text, Attachments, MixedBoundary,
        MimePart) :-
    make_text_mime_part(write_content_disposition(inline, no), TextCTE, Text,
        TextPart),
    (
        Attachments = [],
        MimePart = TextPart
    ;
        Attachments = [_ | _],
        MimePart = composite(multipart_mixed, MixedBoundary,
            yes(write_content_disposition(inline, no)), yes(cte_8bit),
            [TextPart | AttachmentParts]),
        list.map(make_attachment_mime_part(TextCTE), Attachments,
            AttachmentParts)
    ).

:- pred make_text_mime_part(write_content_disposition::in,
    write_content_transfer_encoding::in, string::in, mime_part::out) is det.

make_text_mime_part(Disposition, TextCTE, Text, MimePart) :-
    % XXX detect charset
    MimePart = discrete(text_plain(yes(utf8)), yes(Disposition), yes(TextCTE),
        text(Text)).

:- pred make_attachment_mime_part(write_content_transfer_encoding::in,
    attachment::in, mime_part::out) is det.

make_attachment_mime_part(TextCTE, Attachment, MimePart) :-
    (
        Attachment = old_attachment(OldPart),
        OldPart = part(_MessageId, _OldPartId, OldContentType,
            OldContentDisposition, OldContent, OldFileName, _OldContentLength,
            _OldCTE, _IsDecrypted),
        ContentType = content_type(mime_type.to_string(OldContentType)),
        convert_old_content_disposition(OldContentDisposition, OldFileName,
            MaybeWriteContentDisposition),
        (
            OldContent = text(Text),
            MimePart = discrete(ContentType, MaybeWriteContentDisposition,
                yes(cte_8bit), text(Text))
        ;
            OldContent = unsupported,
            MimePart = discrete(ContentType, MaybeWriteContentDisposition,
                yes(cte_base64), external(OldPart))
        ;
            OldContent = subparts(_, _, _),
            unexpected($module, $pred, "nested part")
        ;
            OldContent = encapsulated_messages(_),
            unexpected($module, $pred, "encapsulated_messages")
        )
    ;
        Attachment = new_attachment(Type, Content, FileName, _Size),
        WriteContentDisposition = write_content_disposition(attachment,
            yes(filename(FileName))),
        (
            Content = text(Text),
            make_text_mime_part(WriteContentDisposition, TextCTE, Text,
                MimePart)
        ;
            Content = base64_encoded(Base64),
            TypeString = mime_type.to_string(Type),
            MimePart = discrete(content_type(TypeString),
                yes(WriteContentDisposition), yes(cte_base64), base64(Base64))
        )
    ).

:- pred convert_old_content_disposition(maybe(content_disposition)::in,
    maybe(filename)::in, maybe(write_content_disposition)::out) is det.

convert_old_content_disposition(OldContentDisposition, OldFileName,
        MaybeWriteContentDisposition) :-
    (
        OldFileName = no,
        OldContentDisposition = no
    ->
        MaybeWriteContentDisposition = no
    ;
        (
            OldContentDisposition = no,
            DispositionType = attachment
        ;
            OldContentDisposition = yes(OldDispositionType),
            DispositionType = convert_old_disposition_type(OldDispositionType)
        ),
        MaybeWriteContentDisposition = yes(write_content_disposition(
            DispositionType, OldFileName))
    ).

:- func convert_old_disposition_type(content_disposition) =
    write_content_disposition_type.

convert_old_disposition_type(DispositionType) =
    ( DispositionType = content_disposition("inline") ->
        inline
    ;
        attachment
    ).

%-----------------------------------------------------------------------------%

:- pred encrypt_then_write_temp_message_file(prog_config::in, prepare_temp::in,
    list(header)::in, mime_part::in,
    crypto_info::in, maybe(list(gpgme.key))::in, list(gpgme.key)::in,
    boundary::in, i_paused_curses::in,
    {maybe_error(string), list(string)}::out, io::di, io::uo) is det.

encrypt_then_write_temp_message_file(Config, Prepare, WriteHeaders,
        PartToEncrypt, CryptoInfo, MaybeSigners, EncryptKeys, EncryptedBoundary,
        PausedCurs, {Res, Warnings}, !IO) :-
    % XXX encrypt could return Cipher as a data buffer instead of returning it
    % as a string
    encrypt(CryptoInfo, MaybeSigners, EncryptKeys, Config, PartToEncrypt,
        PausedCurs, ResCipher, Warnings, !IO),
    (
        ResCipher = ok(Cipher),
        make_multipart_encrypted_mime_part(Cipher, EncryptedBoundary,
            MimePart),
        Spec = message_spec(WriteHeaders, mime_v1(MimePart)),
        write_temp_message_file(Config, Prepare, Spec, PausedCurs, Res, !IO)
    ;
        ResCipher = error(Error),
        Res = error("Encryption failed: " ++ Error)
    ).

:- pred sign_detached_then_write_temp_message_file(prog_config::in,
    prepare_temp::in, list(header)::in, mime_part::in,
    crypto_info::in, list(gpgme.key)::in, boundary::in, i_paused_curses::in,
    {maybe_error(string), list(string)}::out, io::di, io::uo) is det.

sign_detached_then_write_temp_message_file(Config, Prepare,
        WriteHeaders, PartToSign, CryptoInfo, SignKeys, MultiPartBoundary,
        PausedCurs, {Res, Warnings}, !IO) :-
    % XXX sign_detached converts PartToSign into a a data buffer, then we
    % convert it again when writing to the temp file
    sign_detached(CryptoInfo, SignKeys, Config, PartToSign, PausedCurs,
        ResSign, Warnings, !IO),
    (
        ResSign = ok(Sig - MicAlg),
        make_multipart_signed_mime_part(PartToSign, Sig, MicAlg,
            MultiPartBoundary, MimeMessage),
        Spec = message_spec(WriteHeaders, mime_v1(MimeMessage)),
        write_temp_message_file(Config, Prepare, Spec, PausedCurs, Res, !IO)
    ;
        ResSign = error(Error),
        Res = error("Signing failed: " ++ Error)
    ).

:- pred make_multipart_encrypted_mime_part(string::in, boundary::in,
    mime_part::out) is det.

make_multipart_encrypted_mime_part(Cipher, Boundary, MultiPart) :-
    % RFC 3156
    MultiPart = composite(multipart_encrypted(application_pgp_encrypted),
        Boundary, no, no, [SubPartA, SubPartB]),
    SubPartA = discrete(application_pgp_encrypted, no, no,
        text("Version: 1\n")),
    SubPartB = discrete(application_octet_stream, no, no, text(Cipher)).

:- pred make_multipart_signed_mime_part(mime_part::in, string::in, micalg::in,
    boundary::in, mime_part::out) is det.

make_multipart_signed_mime_part(SignedPart, Sig, MicAlg, Boundary, MultiPart) :-
    % RFC 3156
    MultiPart = composite(multipart_signed(MicAlg, application_pgp_signature),
        Boundary, no, no, [SignedPart, SignaturePart]),
    SignaturePart = discrete(application_pgp_signature, no, no, text(Sig)).

%-----------------------------------------------------------------------------%

:- pred write_temp_message_file(prog_config::in, prepare_temp::in,
    message_spec::in, i_paused_curses::in, maybe_error(string)::out,
    io::di, io::uo) is det.

write_temp_message_file(Config, Prepare, Spec, PausedCurs, Res, !IO) :-
    make_temp_suffix("", Res0, !IO),
    (
        Res0 = ok(Filename),
        io.open_output(Filename, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            write_message(Stream, Config, Spec, allow_header_error(Prepare),
                PausedCurs, ResWrite, !IO),
            io.close_output(Stream, !IO),
            (
                ResWrite = ok,
                Res = ok(Filename)
            ;
                ResWrite = error(Error),
                io.remove_file(Filename, _, !IO),
                Res = error(Error)
            )
        ;
            ResOpen = error(_Error),
            io.remove_file(Filename, _, !IO),
            Res = error("Error writing temporary file " ++ Filename)
        )
    ;
        Res0 = error(Error),
        Res = error("Error opening temporary file: " ++ Error)
    ).

:- func allow_header_error(prepare_temp) = bool.

allow_header_error(prepare_send) = no.
allow_header_error(prepare_edit) = yes.
allow_header_error(prepare_postpone) = yes.

:- pred missing_keys_warning(list(addr_spec)::in, list(string)::out) is det.

missing_keys_warning(Missing, Warnings) :-
    (
        Missing = [],
        Warnings = []
    ;
        Missing = [AddrSpec],
        addr_spec_to_string(AddrSpec, AddrString, _Valid),
        Warnings = ["Missing key for <" ++ AddrString ++ ">."]
    ;
        Missing = [_, _ | _],
        Num = length(Missing),
        Warnings = [format("Missing keys for %d addresses.", [i(Num)])]
    ).

:- pred leaked_bccs_warning(list(addr_spec)::in, list(string)::out) is det.

leaked_bccs_warning(LeakedBccs, Warnings) :-
    (
        LeakedBccs = [],
        Warnings = []
    ;
        LeakedBccs = [AddrSpec],
        addr_spec_to_string(AddrSpec, AddrString, _Valid),
        Warnings = ["Encryption may expose Bcc to <" ++ AddrString ++ ">."]
    ;
        LeakedBccs = [_, _ | _],
        Num = length(LeakedBccs),
        Warnings = [format("Encryption may expose %d Bcc identities.", [i(Num)])]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
