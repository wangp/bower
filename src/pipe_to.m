% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module pipe_to.
:- interface.

:- import_module io.
:- import_module list.

:- import_module screen.
:- import_module text_entry.

:- pred prompt_and_pipe_to_command(screen::in, string::in, list(string)::in,
    message_update::out, history::in, history::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module parsing_utils.
:- import_module string.

:- import_module call_system.
:- import_module path_expand.
:- import_module process.
:- import_module prog_config.
:- import_module quote_command.
:- import_module shell_word.

:- use_module curs.

prompt_and_pipe_to_command(Screen, PromptCommand, Strings, MessageUpdate,
        !History, !IO) :-
    text_entry(Screen, PromptCommand, !.History, complete_none, Return, !IO),
    (
        Return = yes(Command),
        Command \= ""
    ->
        add_history_nodup(Command, !History),
        pipe_to_command(Command, Strings, MaybeError, !IO),
        (
            MaybeError = ok,
            MessageUpdate = clear_message
        ;
            MaybeError = error(Error),
            MessageUpdate = set_warning(Error)
        )
    ;
        MessageUpdate = clear_message
    ).

:- pred pipe_to_command(string::in, list(string)::in, maybe_error::out,
    io::di, io::uo) is det.

pipe_to_command(Command, Strings, MaybeError, !IO) :-
    promise_equivalent_solutions [MaybeError, !:IO] (
        shell_word.tokenise(Command, ParseResult),
        (
            ParseResult = ok(CommandTokens0),
            get_home_dir(Home, !IO),
            expand_tilde_home_in_shell_tokens(Home,
                CommandTokens0, CommandTokens),
            (
                CommandTokens = [],
                MaybeError = ok
            ;
                CommandTokens = [_ | _],
                ( shell_word.contains_graphic_metachars(CommandTokens) ->
                    Message = "Command contains unquoted metacharacters.",
                    MaybeError = error(Message)
                ;
                    pipe_to_command_2(CommandTokens, Strings, MaybeError, !IO)
                )
            )
        ;
            (
                ParseResult = error(yes(Error), _Line, Column),
                Message = string.format("parse error at column %d: %s",
                    [i(Column), s(Error)])
            ;
                ParseResult = error(no, _Line, Column),
                Message = string.format("parse error at column %d",
                    [i(Column)])
            ),
            MaybeError = error(Message)
        )
    ).

:- pred pipe_to_command_2(list(shell_token)::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

pipe_to_command_2(CommandTokens, Strings, MaybeError, !IO) :-
    make_pipe_to_command(CommandTokens, Command),
    Input = string.join_list(" ", Strings),
    curs.suspend(call_system_write_to_stdin(Command, environ([]), Input),
        CallRes, !IO),
    (
        CallRes = ok,
        MaybeError = ok
    ;
        CallRes = error(Error),
        MaybeError = error(io.error_message(Error))
    ).

:- pred make_pipe_to_command(list(shell_token)::in, string::out) is det.

make_pipe_to_command(CommandTokens, Command) :-
    % Could check for bg operator.
    serialise_quote_all(CommandTokens, QuotedCommandStr),
    QuoteTimes = ( detect_ssh(CommandTokens) -> quote_twice ; quote_once ),
    CommandPrefix = command_prefix(shell_quoted(QuotedCommandStr), QuoteTimes),
    make_quoted_command(CommandPrefix, [], no_redirect, no_redirect, Command).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
