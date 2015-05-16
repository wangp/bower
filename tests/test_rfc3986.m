%-----------------------------------------------------------------------------%

:- module test_rfc3986.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pretty_printer.

:- import_module rfc3986.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test, cases, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(Input, !IO) :-
    io.write_string("input: «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),

    ( split_uri(Input, URI) ->
        pretty_printer.write_doc(format(URI), !IO),
        io.nl(!IO)
    ;
        io.write_string("failed to parse\n", !IO)
    ),
    io.write_string("--------\n", !IO).

:- func cases = list(string).

cases = [
    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    "http://www.ietf.org/rfc/rfc2396.txt",
    "ldap://[2001:db8::7]/c=GB?objectClass?one",
    "mailto:John.Doe@example.com",
    "news:comp.infosystems.www.servers.unix",
    "tel:+1-816-555-1212",
    "telnet://192.0.2.16:80/",
    "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
    "/example#frag",
    "/example?#frag",
    "/example??#frag",
    "/example?v=1#frag",
    "/example?v=1&w=2#frag",
    "/example?q#frag",
    "/example?q#frag#frag",
    ""
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
