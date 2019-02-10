% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module pipe_to.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- pred pipe_to_command(string::in, list(string)::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parsing_utils.
:- import_module string.

:- import_module curs.
:- import_module make_temp.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module shell_word.

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
    % Use a temporary file for now.
    make_temp_suffix("", TempRes, !IO),
    (
        TempRes = ok(FileName),
        io.open_output(FileName, OpenRes, !IO),
        (
            OpenRes = ok(Stream),
            io.write_list(Stream, Strings, " ", io.write_string(Stream), !IO),
            io.close_output(Stream, !IO),

            make_pipe_to_command(CommandWords, FileName, Command),
            curs.suspend(io.call_system(Command), CallRes, !IO),
            (
                CallRes = ok(ExitStatus),
                ( ExitStatus = 0 ->
                    MaybeError = ok
                ;
                    string.format("%s returned with exit status %d",
                        [s(Command), i(ExitStatus)], Msg),
                    MaybeError = error(Msg)
                )
            ;
                CallRes = error(Error),
                MaybeError = error(io.error_message(Error))
            ),
            io.remove_file(FileName, _, !IO)
        ;
            OpenRes = error(Error),
            MaybeError = error(io.error_message(Error))
        )
    ;
        TempRes = error(Error),
        MaybeError = error(Error)
    ).

:- pred make_pipe_to_command(list(word)::in, string::in, string::out) is det.

make_pipe_to_command(CommandWords, FileName, Command) :-
    % Could check for bg operator.
    WordStrings = list.map(word_string, CommandWords),
    CommandPrefix = command_prefix(
        shell_quoted(string.join_list(" ", list.map(quote_arg, WordStrings))),
        ( detect_ssh(CommandWords) -> quote_twice ; quote_once )
    ),
    make_quoted_command(CommandPrefix, [], redirect_input(FileName),
        no_redirect, Command).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
