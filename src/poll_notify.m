% Bower - a frontend for the Notmuch email system
% Copyright (C) 2018 Peter Wang

:- module poll_notify.
:- interface.

:- import_module io.

:- import_module prog_config.
:- import_module screen.

:- pred maybe_poll_notify(prog_config::in, string::in, message_update::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module quote_arg.

maybe_poll_notify(Config, Message, MessageUpdate, !IO) :-
    get_poll_notify_command(Config, MaybeCommandPrefix),
    (
        MaybeCommandPrefix = yes(CommandPrefix),
        make_quoted_command(CommandPrefix, [Message],
            redirect_input("/dev/null"), redirect_output("/dev/null"),
            Command),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                MessageUpdate = no_change
            ;
                string.format("poll_notify command returned exit status %d",
                    [i(ExitStatus)], Warning),
                MessageUpdate = set_warning(Warning)
            )
        ;
            CallRes = error(Error),
            Warning = "Error running poll_notify command: " ++
                io.error_message(Error),
            MessageUpdate = set_warning(Warning)
        )
    ;
        MaybeCommandPrefix = no,
        MessageUpdate = no_change
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
