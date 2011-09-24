%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.

:- pred add_draft(string::in, io.res::out, io::di, io::uo) is det.

:- pred find_drafts(list(message_id)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module callout.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

add_draft(FileName, Res, !IO) :-
    Args = [
        "notmuch-deliver",
        "-t", "draft",
        "-r", "inbox",
        "-r", "unread",
        "-f", drafts_dir
    ],
    args_to_quoted_command(Args, redirect_input(FileName), no, Command),
    io.call_system(Command, CallRes, !IO),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = string.format("notmuch-deliver returned with exit status %d",
                [i(ExitStatus)]),
            Res = error(io.make_io_error(Msg))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

:- func drafts_dir = string.

drafts_dir = "Drafts".

%-----------------------------------------------------------------------------%

find_drafts(MessageIds, !IO) :-
    run_notmuch([
        "search", "--format=json", "--output=messages",
        "tag:draft", "-tag:deleted"
    ], parse_message_id_list, MessageIds, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
