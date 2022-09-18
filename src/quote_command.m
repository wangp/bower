% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module quote_command.
:- interface.

:- import_module list.

:- type command_prefix
    --->    command_prefix(shell_quoted, quote_times).

:- type shell_quoted
    --->    shell_quoted(string). % shell metacharacters quoted/escaped

    % Number of times to quote arguments.
:- type quote_times
    --->    quote_once
    ;       quote_twice.

:- type redirect_input
    --->    no_redirect
    ;       redirect_input(string).

:- type redirect_output
    --->    no_redirect
    ;       redirect_output(string)
    ;       redirect_append(string).

:- type redirect_stderr
    --->    no_redirect
    ;       redirect_stderr(string).

:- type run_in_background
    --->    run_in_foreground
    ;       run_in_background.

:- pred make_quoted_command(command_prefix::in, list(string)::in,
    redirect_input::in, redirect_output::in, string::out) is det.

:- pred make_quoted_command(command_prefix::in, list(string)::in,
    redirect_input::in, redirect_output::in, redirect_stderr::in,
    run_in_background::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.
:- import_module string.

:- import_module quote_arg.

%-----------------------------------------------------------------------------%

make_quoted_command(CommandPrefix, UnquotedArgs, RedirectInput, RedirectOutput,
        Command) :-
    make_quoted_command(CommandPrefix, UnquotedArgs, RedirectInput,
        RedirectOutput, no_redirect, run_in_foreground, Command).

make_quoted_command(command_prefix(Prefix, QuoteTimes), UnquotedArgs,
        RedirectInput, RedirectOutput, RedirectStderr, RunInBackground,
        Command) :-
    some [!Acc] (
        Prefix = shell_quoted(PrefixString),
        !:Acc = ["exec", PrefixString | do_quote(QuoteTimes, UnquotedArgs)],
        (
            RedirectInput = redirect_input(InputFile),
            !:Acc = !.Acc ++ ["<", quote_arg(InputFile)]
        ;
            RedirectInput = no_redirect
        ),
        (
            RedirectOutput = redirect_output(OutputFile),
            !:Acc = !.Acc ++ [">", quote_arg(OutputFile)]
        ;
            RedirectOutput = redirect_append(OutputFile),
            !:Acc = !.Acc ++ [">>", quote_arg(OutputFile)]
        ;
            RedirectOutput = no_redirect
        ),
        (
            RedirectStderr = redirect_stderr(ErrorFile),
            !:Acc = !.Acc ++ ["2>", quote_arg(ErrorFile)]
        ;
            RedirectStderr = no_redirect
        ),
        (
            RunInBackground = run_in_background,
            !:Acc = !.Acc ++ ["&"]
        ;
            RunInBackground = run_in_foreground
        ),
        Command = string.join_list(" ", !.Acc)
    ).

:- func do_quote(quote_times, list(string)) = list(string).

do_quote(quote_once, Args) = list.map(quote_arg, Args).
do_quote(quote_twice, Args) = list.map(compose(quote_arg, quote_arg), Args).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
