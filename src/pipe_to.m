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
:- import_module prog_config.
:- import_module quote_arg.
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
        shell_word.split(Command, ParseResult),
        (
            ParseResult = ok([]),
            % Should not happen.
            MaybeError = ok
        ;
            ParseResult = ok(CommandWords),
            CommandWords = [_ | _],
            pipe_to_command_2(CommandWords, Strings, MaybeError, !IO)
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

:- pred pipe_to_command_2(list(word)::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

pipe_to_command_2(CommandWords, Strings, MaybeError, !IO) :-
    make_pipe_to_command(CommandWords, Command),
    Input = string.join_list(" ", Strings),
    curs.suspend(call_system_write_to_stdin(Command, Input), CallRes, !IO),
    (
        CallRes = ok,
        MaybeError = ok
    ;
        CallRes = error(Error),
        MaybeError = error(io.error_message(Error))
    ).

:- pred make_pipe_to_command(list(word)::in, string::out) is det.

make_pipe_to_command(CommandWords, Command) :-
    % Could check for bg operator.
    WordStrings = list.map(word_string, CommandWords),
    CommandPrefix = command_prefix(
        shell_quoted(string.join_list(" ", list.map(quote_arg, WordStrings))),
        ( detect_ssh(CommandWords) -> quote_twice ; quote_once )
    ),
    make_quoted_command(CommandPrefix, [], no_redirect, no_redirect, Command).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
