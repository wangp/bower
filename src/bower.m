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
:- import_module crypto.
:- import_module curs.
:- import_module index_view.
:- import_module notmuch_config.
:- import_module prog_config.
:- import_module screen.
:- import_module search_term.
:- import_module signal.
:- import_module view_common.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    #include <locale.h>
").

%-----------------------------------------------------------------------------%

main(!IO) :-
    setlocale(!IO),
    load_prog_config(ResConfig, !IO),
    (
        ResConfig = ok(Config, NotmuchConfig),
        init_crypto(ResCrypto, !IO),
        (
            ResCrypto = ok(Crypto),
            main_2(Config, NotmuchConfig, Crypto, !IO),
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

:- pred main_2(prog_config::in, notmuch_config::in, crypto::in, io::di, io::uo)
    is cc_multi.

main_2(Config, NotmuchConfig, Crypto, !IO) :-
    % Install our SIGINT, SIGCHLD handlers.
    signal.ignore_sigint(no, !IO),
    async.install_sigchld_handler(!IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        get_default_search_terms(NotmuchConfig, Terms)
    ;
        Args = [_ | _],
        Terms = string.join_list(" ", Args)
    ),
    init_common_history(Config, CommonHistory0),
    curs.start(!IO),
    ( try [io(!IO)] (
        create_screen(status_attrs(Config), Screen, !IO),
        draw_status_bar(Screen, !IO),
        curs.refresh(!IO),
        open_index(Config, NotmuchConfig, Crypto, Screen, Terms,
            CommonHistory0, !IO)
    ) then
        curs.stop(!IO)
      catch sigint_received ->
        curs.stop(!IO),
        kill_self_with_sigint(!IO)
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
