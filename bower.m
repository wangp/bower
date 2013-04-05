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
:- import_module curs.
:- import_module index_view.
:- import_module prog_config.
:- import_module screen.
:- import_module search_term.
:- import_module signal.

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
        ResConfig = ok,
        check_sendmail_command(ResSendmail, !IO),
        (
            ResSendmail = ok,
            main_2(!IO)
        ;
            ResSendmail = error(Error2),
            io.stderr_stream(Stream, !IO),
            io.write_string(Stream, Error2, !IO),
            io.nl(Stream, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        ResConfig = error(Error),
        print_error("Error loading config file: " ++ Error, !IO)
    ).

:- pred main_2(io::di, io::uo) is cc_multi.

main_2(!IO) :-
    % Install our SIGINT, SIGCHLD handlers.
    signal.ignore_sigint(no, !IO),
    async.install_sigchld_handler(!IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        get_default_search_terms(Terms, !IO)
    ;
        Args = [_ | _],
        Terms = string.join_list(" ", Args)
    ),
    curs.start(!IO),
    ( try [io(!IO)] (
        create_screen(Screen, !IO),
        draw_bar(Screen, !IO),
        curs.refresh(!IO),
        open_index(Screen, Terms, !IO)
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

:- pred print_error(string::in, io::di, io::uo) is det.

print_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
