%-----------------------------------------------------------------------------%

:- module test_search_term.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.

:- import_module search_term.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        Cases = cases
    ;
        Args = [_ | _],
        Cases = Args
    ),
    list.foldl(run_test_case, Cases, !IO).

:- pred run_test_case(string::in, io::di, io::uo) is det.

run_test_case(Input, !IO) :-
    io.write_string("input:  «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),
    parse_search_string(Input, Res),
    (
        Res = ok(Tokens),
        tokens_to_search_terms(Tokens, Terms),
        check_apply_limit(Tokens, ApplyLimit),
        io.write_string("tokens: ", !IO),
        io.print(Tokens, !IO),
        io.write_string("\n", !IO),
        io.write_string("terms:  «", !IO),
        io.write_string(Terms, !IO),
        io.write_string("»\n", !IO),
        io.write_string("apply limit: ", !IO),
        io.print(ApplyLimit, !IO),
        io.nl(!IO)
    ;
        Res = error(Error),
        io.write_string("error: ", !IO),
        io.write_string(Error, !IO),
        io.nl(!IO)
    ),

    io.write_string("\n--------------------\n", !IO).

:- func cases = list(string).

cases = [
    % whitespace
    "",
    "x  y",

    % parens/braces
    "{x(y)z}",
    "abc(x{yz})def",

    % double-quoted strings
    """",
    "x""""y",
    "x""a b""""cde""y",

    % tilde only special after whitespace or open paren/brace
    "~x",
    "~x~y",
    "x~y",
    "x ~y",
    """~x""",
    """x""~y",
    "(~x ~D ~F ~U ~A)",
    "x:~y",
    "x:""~y""",
    "x:/~y/",
    "x:(~y)",
    "x:{~y}",

    % macro names
    "~",
    "~xyz:",
    "~xyz(",
    "~xyz)",
    "~xyz{",
    "~xyz}",
    "~xyz""abc""",

    % date ranges
    "~d",
    "~dtoday",
    "~d today",
    "~d{today}",
    "~d..",
    "~d..today",
    "~d {last week}..",
    "~d {3 days ago}..{next year}",
    "~d 3.days.ago..next.year",

    % ~A
    "x ~A",
    "( ~A )",
    "{ ~A }",
    "x:(~A)"
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
