% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module copious_output.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module data.

:- pred expand_html(message_id::in, int::in, maybe_error(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module call_system.
:- import_module prog_config.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

expand_html(MessageId, PartId, Res, !IO) :-
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
    call_system_capture_stdout(Command, CallRes, !IO),
    (
        CallRes = ok(ContentString),
        Res = ok(ContentString)
    ;
        CallRes = error(Error),
        Res = error(io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
