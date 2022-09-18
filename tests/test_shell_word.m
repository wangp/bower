%-----------------------------------------------------------------------------%

:- module test_shell_word.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module parsing_utils.

:- import_module shell_word.

%-----------------------------------------------------------------------------%

main(!IO) :-
    main_loop(!IO).

:- pred main_loop(io::di, io::uo) is cc_multi.

main_loop(!IO) :-
    io.read_line_as_string(ReadRes, !IO),
    (
        ReadRes = ok(Input0),
        Input = chomp(Input0),
        ( if Input = "" ; string.prefix(Input, "#") then
            true
        else
            run_test_case(chomp(Input), !IO)
        ),
        main_loop(!IO)
    ;
        ReadRes = eof
    ;
        ReadRes = error(Error),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, io.error_message(Error), !IO),
        io.nl(Stderr, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_test_case(string::in, io::di, io::uo) is cc_multi.

run_test_case(Input, !IO) :-
    io.write_string("input: «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),
    tokenise(Input, ParseResult),
    (
        ParseResult = ok(Tokens),
        io.write_string("tokens: ", !IO),
        io.print_line(Tokens, !IO),
        serialise_as_is(Tokens, AsIs),
        io.write_string("serialise: «", !IO),
        io.write_string(AsIs, !IO),
        io.write_string("»\n", !IO),
        serialise_quote_all(Tokens, QuoteAll),
        io.write_string("quote all: «", !IO),
        io.write_string(QuoteAll, !IO),
        io.write_string("»\n", !IO)
    ;
        ParseResult = error(MaybeError, _, _),
        io.write_string("error: ", !IO),
        (
            MaybeError = yes(Error),
            io.write_string(Error, !IO)
        ;
            MaybeError = no,
            io.write_string("unknown parse error", !IO)
        ),
        io.nl(!IO)
    ),

    io.write_string("\n--------------------\n", !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
