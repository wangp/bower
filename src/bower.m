% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module bower.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module async.
:- import_module compose.
:- import_module crypto.
:- import_module index_view.
:- import_module notmuch_config.
:- import_module prog_config.
:- import_module rfc6068.
:- import_module screen.
:- import_module search_term.
:- import_module signal.
:- import_module view_common.

:- use_module curs.
:- use_module curs_signal.

:- type entry_point
    --->    index_view_default_terms
    ;       index_view_terms(string)
    ;       compose(string).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    #include <locale.h>
").

%-----------------------------------------------------------------------------%

main(!IO) :-
    setlocale(!IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [FirstArg | RestArgs],
        is_mailto_uri(FirstArg)
    ->
        (
            RestArgs = [],
            ( parse_mailto_uri(FirstArg, _Headers) ->
                main_2(compose(FirstArg), !IO)
            ;
                io.stderr_stream(Stream, !IO),
                print_error(Stream, "Error parsing mailto: argument.", !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            RestArgs = [_ | _],
            io.stderr_stream(Stream, !IO),
            print_error(Stream,
                "Unexpected arguments following mailto: argument.", !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        (
            Args = [],
            EntryPoint = index_view_default_terms
        ;
            Args = [_ | _],
            Terms = string.join_list(" ", Args),
            EntryPoint = index_view_terms(Terms)
        ),
        main_2(EntryPoint, !IO)
    ).

:- pred main_2(entry_point::in, io::di, io::uo) is cc_multi.

main_2(EntryPoint, !IO) :-
    load_prog_config(ResConfig, !IO),
    (
        ResConfig = ok(Config, NotmuchConfig),
        init_crypto(ResCrypto, !IO),
        (
            ResCrypto = ok(Crypto),
            main_3(Config, NotmuchConfig, Crypto, EntryPoint, !IO),
            shutdown_crypto(Crypto, !IO)
        ;
            ResCrypto = error(Error),
            io.stderr_stream(Stream, !IO),
            io.write_string(Stream,
                "Error initialising crypto support:\n", !IO),
            print_error(Stream, Error, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        ResConfig = errors(Errors),
        io.stderr_stream(Stream, !IO),
        io.write_string(Stream, "Errors in configuration file:\n", !IO),
        list.foldl(print_error(Stream), Errors, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_3(prog_config::in, notmuch_config::in, crypto::in,
    entry_point::in, io::di, io::uo) is cc_multi.

main_3(Config, NotmuchConfig, Crypto, EntryPoint, !IO) :-
    curs_signal.install_suspend_handlers(!IO),
    curs_signal.install_exit_handlers(!IO),
    signal.ignore_sigint(no, !IO),
    async.install_sigchld_handler(!IO),
    curs.start(!IO),
    ( try [io(!IO)]
        main_4(Config, NotmuchConfig, Crypto, EntryPoint, MessageUpdate, !IO)
    then
        curs.stop(!IO)
      catch sigint_received ->
        curs.stop(!IO),
        kill_self_with_sigint(!IO)
    ),
    io.output_stream(Stream, !IO),
    print_message_update(Stream, MessageUpdate, !IO).

:- pred main_4(prog_config::in, notmuch_config::in, crypto::in,
    entry_point::in, message_update::out, io::di, io::uo) is det.

main_4(Config, NotmuchConfig, Crypto, EntryPoint, MessageUpdate, !IO) :-
    init_common_history(Config, CommonHistory0),
    create_screen(status_attrs(Config), Screen, !IO),
    draw_status_bar(Screen, !IO),
    curs.refresh(!IO),
    (
        (
            EntryPoint = index_view_default_terms,
            get_default_search_terms(NotmuchConfig, Terms)
        ;
            EntryPoint = index_view_terms(Terms)
        ),
        open_index(Config, NotmuchConfig, Crypto, Screen, Terms,
            CommonHistory0, !IO),
        MessageUpdate = no_change
    ;
        EntryPoint = compose(MailtoArg),
        start_compose(Config, Crypto, Screen, yes(MailtoArg), Transition,
            CommonHistory0, _CommonHistory, !IO),
        Transition = screen_transition(_Sent, MessageUpdate)
    ).

:- pred setlocale(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setlocale(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    setlocale(LC_ALL, """");
    IO = IO0;
").

:- pred print_error(io.output_stream::in, string::in, io::di, io::uo) is det.

print_error(Stream, Error, !IO) :-
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO).

:- pred print_message_update(io.output_stream::in, message_update::in,
    io::di, io::uo) is det.

print_message_update(Stream, MessageUpdate, !IO) :-
    (
        MessageUpdate = no_change
    ;
        MessageUpdate = clear_message
    ;
        ( MessageUpdate = set_info(Message)
        ; MessageUpdate = set_warning(Message)
        ; MessageUpdate = set_prompt(Message)
        ),
        io.write_string(Stream, Message, !IO),
        io.nl(Stream, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
