% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module copious_output.
:- interface.

:- import_module io.

:- import_module data.

:- pred expand_copious_output(message::in, message::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module popen.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module string_util.

%-----------------------------------------------------------------------------%

expand_copious_output(Message0, Message, !IO) :-
    Message0 = message(Id, Timestamp, Headers, Tags, Body0, Replies0),
    list.map_foldl(expand_part, Body0, Body, !IO),
    list.map_foldl(expand_copious_output, Replies0, Replies, !IO),
    Message = message(Id, Timestamp, Headers, Tags, Body, Replies).

:- pred expand_part(part::in, part::out, io::di, io::uo) is det.

expand_part(Part0, Part, !IO) :-
    Part0 = part(MessageId, PartId, Type, Content0, MaybeFilename),
    (
        Content0 = text(_),
        Part = Part0
    ;
        Content0 = subparts(SubParts0),
        list.map_foldl(expand_part, SubParts0, SubParts, !IO),
        Content = subparts(SubParts),
        Part = part(MessageId, PartId, Type, Content, MaybeFilename)
    ;
        Content0 = encapsulated_messages(EncapMessages0),
        list.map_foldl(expand_encapsulated_message, EncapMessages0,
            EncapMessages, !IO),
        Content = encapsulated_messages(EncapMessages),
        Part = part(MessageId, PartId, Type, Content, MaybeFilename)
    ;
        Content0 = unsupported,
        (
            % XXX we should use mailcap, though we don't want to show
            % everything
            strcase_equal(Type, "text/html")
        ->
            expand_html(MessageId, PartId, Content, !IO),
            Part = part(MessageId, PartId, Type, Content, MaybeFilename)
        ;
            Part = Part0
        )
    ).

:- pred expand_encapsulated_message(encapsulated_message::in,
    encapsulated_message::out, io::di, io::uo) is det.

expand_encapsulated_message(EncapMessage0, EncapMessage, !IO) :-
    EncapMessage0 = encapsulated_message(Headers, SubParts0),
    list.map_foldl(expand_part, SubParts0, SubParts, !IO),
    EncapMessage = encapsulated_message(Headers, SubParts).

:- pred expand_html(message_id::in, int::in, part_content::out,
    io::di, io::uo) is det.

expand_html(MessageId, PartId, Content, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    args_to_quoted_command([
        "show", "--format=raw", "--part=" ++ from_int(PartId),
        message_id_to_search_term(MessageId)
    ], ShowCommand),
    get_html_dump_command(DumpCommand, !IO),
    ( DumpCommand = "" ->
        Command = Notmuch ++ ShowCommand
    ;
        Command = Notmuch ++ ShowCommand ++ " | " ++ DumpCommand
    ),
    popen(Command, CallRes, !IO),
    (
        CallRes = ok(ContentString),
        Content = text(ContentString)
    ;
        CallRes = error(_),
        Content = unsupported
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
