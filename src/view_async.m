% Bower - a frontend for the Notmuch email system
% Copyright (C) 2018 Peter Wang

:- module view_async.
:- interface.

:- import_module io.

:- import_module screen.

:- pred poll_async_with_progress(screen,
    pred(screen, string, Info, Info, io, io), Info, Info, io, io).
:- mode poll_async_with_progress(in,
    in(pred(in, in, in, out, di, uo) is det), in, out, di, uo) is det.

:- pred flush_async_with_progress(screen::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module async.
:- import_module quote_arg.
:- import_module sleep.

%-----------------------------------------------------------------------------%

poll_async_with_progress(Screen, Handler, !Info, !IO) :-
    poll_async_nonblocking(Return, !IO),
    (
        Return = none
    ;
        (
            Return = child_succeeded
        ;
            Return = child_lowprio_output(Output),
            Handler(Screen, Output, !Info, !IO)
        ;
            Return = child_failed(Op, Failure),
            handle_async_failure(Screen, Op, Failure, !IO)
        ),
        poll_async_with_progress(Screen, Handler, !Info, !IO)
    ).

%---------------------------------------------------------------------------%

flush_async_with_progress(Screen, !IO) :-
    clear_lowprio_async(!IO),
    async_count(Count, !IO),
    ( Count = 0 ->
        true
    ;
        flush_async_with_progress_loop(Screen, yes, !IO)
    ).

:- pred flush_async_with_progress_loop(screen::in, bool::in, io::di, io::uo)
    is det.

flush_async_with_progress_loop(Screen, Display, !IO) :-
    async_count(Count, !IO),
    ( Count = 0 ->
        update_message(Screen, clear_message, !IO)
    ;
        (
            Display = yes,
            string.format("Flushing %d asynchronous operations.",
                [i(Count)], Message),
            update_message_immed(Screen, set_info(Message), !IO)
        ;
            Display = no
        ),
        poll_async_blocking(Return, !IO),
        (
            Return = none,
            % Don't busy wait.
            usleep(100000, !IO),
            flush_async_with_progress_loop(Screen, no, !IO)
        ;
            Return = child_succeeded,
            flush_async_with_progress_loop(Screen, yes, !IO)
        ;
            Return = child_lowprio_output(_),
            flush_async_with_progress_loop(Screen, yes, !IO)
        ;
            Return = child_failed(Op, Failure),
            handle_async_failure(Screen, Op, Failure, !IO),
            flush_async_with_progress_loop(Screen, no, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred handle_async_failure(screen::in, async_op::in, async_failure::in,
    io::di, io::uo) is det.

handle_async_failure(Screen, Op, Failure, !IO) :-
    Op = async_shell_command(Prefix, Args, RemainingAttempts0),
    Prefix = command_prefix(shell_quoted(PrefixString), _),
    FullCommand = string.join_list(" ", [PrefixString | Args]),
    ( string.count_codepoints(FullCommand) > 40 ->
        ShortCommand = "..." ++ string.right(FullCommand, 37)
    ;
        ShortCommand = FullCommand
    ),
    (
        Failure = failure_nonzero_exit(Status),
        ( RemainingAttempts0 = 0 ->
            string.format("'%s' returned exit status %d; not retrying.",
                [s(ShortCommand), i(Status)], Message)
        ;
            Delay = 5,
            string.format("'%s' returned exit status %d; retrying in %d secs.",
                [s(ShortCommand), i(Status), i(Delay)], Message),
            RemainingAttempts = RemainingAttempts0 - 1,
            RetryOp = async_shell_command(Prefix, Args, RemainingAttempts),
            retry_async(Delay, RetryOp, !IO)
        )
    ;
        Failure = failure_signal(Signal),
        string.format("'%s' received signal %d; not retrying.",
            [s(ShortCommand), i(Signal)], Message)
    ;
        Failure = failure_abnormal_exit,
        string.format("'%s' exited abnormally; not retrying.",
            [s(ShortCommand)], Message)
    ;
        Failure = failure_error(Error),
        string.format("'%s': %s; not retrying.",
            [s(ShortCommand), s(io.error_message(Error))], Message)
    ),
    update_message_immed(Screen, set_warning(Message), !IO),
    sleep(1, !IO).
handle_async_failure(_Screen, Op, _Failure, !IO) :-
    % Ignore poll command failures.
    Op = async_lowprio_command(_, _, _).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
