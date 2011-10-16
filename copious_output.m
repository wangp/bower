%-----------------------------------------------------------------------------%

:- module copious_output.
:- interface.

:- import_module io.

:- import_module data.

:- pred expand_copious_output(message::in, message::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module callout.
:- import_module popen.
:- import_module quote_arg.
:- import_module string_util.

%-----------------------------------------------------------------------------%

expand_copious_output(Message0, Message, !IO) :-
    Message0 = message(Id, Timestamp, Headers, Tags, Body0, Replies0),
    BodyList0 = cord.list(Body0),
    list.map_foldl(expand_part, BodyList0, BodyList, !IO),
    Body = cord.from_list(BodyList),
    list.map_foldl(expand_copious_output, Replies0, Replies, !IO),
    Message = message(Id, Timestamp, Headers, Tags, Body, Replies).

:- pred expand_part(part::in, part::out, io::di, io::uo) is det.

expand_part(Part0, Part, !IO) :-
    Type = Part0 ^ pt_type,
    MaybeContent0 = Part0 ^ pt_content,
    (
        % XXX we should use mailcap, though we don't want to show everything
        strcase_equal(Type, "text/html"),
        MaybeContent0 = no
    ->
        MessageId = Part0 ^ pt_msgid,
        PartId = Part0 ^ pt_part,
        expand_html(MessageId, PartId, MaybeContent, !IO),
        Part = Part0 ^ pt_content := MaybeContent
    ;
        Part = Part0
    ).

:- pred expand_html(message_id::in, int::in, maybe(string)::out,
    io::di, io::uo) is det.

expand_html(MessageId, PartId, MaybeContent, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    args_to_quoted_command([
        "show", "--format=raw", "--part=" ++ from_int(PartId),
        message_id_to_search_term(MessageId)
    ], ShowCommand),
    popen(Notmuch ++ ShowCommand ++ dump_pipe, CallRes, !IO),
    (
        CallRes = ok(Content),
        MaybeContent = yes(Content)
    ;
        CallRes = error(_),
        MaybeContent = no
    ).

    % XXX we should use mailcap
:- func dump_pipe = string.

dump_pipe = "|lynx -dump -force-html -stdin".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
