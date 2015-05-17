%-----------------------------------------------------------------------------%

:- module test_rfc6068.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pretty_printer.

:- import_module rfc6068.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test, cases, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(Input, !IO) :-
    io.write_string("input: «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),

    ( parse_mailto_uri(Input, Mailto) ->
        pretty_printer.write_doc(format(Mailto), !IO),
        io.nl(!IO)
    ;
        io.write_string("failed to parse\n", !IO)
    ),
    io.write_string("--------\n", !IO).

:- func cases = list(string).

cases = [
    "mailto:chris@example.com",
    "mailto:infobot@example.com?subject=current-issue",
    "mailto:infobot@example.com?body=send%20current-issue",
    "mailto:infobot@example.com?body=send%20current-issue%0D%0Asend%20index",
    "mailto:list@example.org?In-Reply-To=%3C3469A91.D10AF4C@example.com%3E",
    "mailto:majordomo@example.com?body=subscribe%20bamboo-l",
    "mailto:joe@example.com?cc=bob@example.com&body=hello",
    "mailto:joe@example.com?cc=bob@example.com?body=hello", % WRONG!
    "mailto:gorby%25kremvax@example.com",
    "mailto:unlikely%3Faddress@example.com?blat=foop",
    "mailto:Mike%26family@example.org",
    "mailto:%22not%40me%22@example.org",
    "mailto:%22oh%5C%5Cno%22@example.org",
    "mailto:%22%5C%5C%5C%22it's%5C%20ugly%5C%5C%5C%22%22@example.org",
    "mailto:user@example.org?subject=caf%C3%A9",
    "mailto:user@example.org?subject=%3D%3Futf-8%3FQ%3Fcaf%3DC3%3DA9%3F%3D",
    "mailto:user@example.org?subject=%3D%3Fiso-8859-1%3FQ%3Fcaf%3DE9%3F%3D",
    "mailto:user@example.org?subject=caf%C3%A9&body=caf%C3%A9",
    "mailto:user@%E7%B4%8D%E8%B1%86.example.org?subject=Test&body=NATTO"
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
