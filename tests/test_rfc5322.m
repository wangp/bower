% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module test_rfc5322.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module pretty_printer.

:- import_module rfc5322.
:- import_module rfc5322.parser.
:- import_module rfc5322.writer.

:- type case == string.

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
    list.foldl(test, Cases, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(Input, !IO) :-
    io.write_string("input: «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),

    parse_address_list(Input, Addresses),
    list.foldl(show_address, Addresses, !IO),
    io.nl(!IO),

    pretty_printer.write_doc(format(Addresses), !IO),
    io.nl(!IO),
    io.write_string("--------\n", !IO).

:- pred show_address(address::in, io::di, io::uo) is det.

show_address(Address, !IO) :-
    address_to_string(Address, String, Valid),
    (
        Valid = yes,
        io.write_string("valid: «", !IO)
    ;
        Valid = no,
        io.write_string("invalid: «", !IO)
    ),
    io.write_string(String, !IO),
    io.write_string("»\n", !IO).

:- func cases = list(case).

cases = [
    (""),
    ("user"),
    ("user1 , user2"),
    ("user1 ,user2"),
    ("user@"),
    ("@"),
    ("@example.com"),

    ("user@example.com"),

    % Dot-atom
    ("user.name@example.com"),
    ("user.name@example.com.org"),
    ("user..name@example.com"),
    (".@example.com"),
    (".user@example.com"),
    ("user.@example.com"),

    % Quoted-string local part
    ("\"\"@example.com"),
    ("\"user name\"@example.com"),
    ("\"user..name\"@example.com"),
    ("\"user\\name\"@example.com"),
    ("\"user\\\\name\"@example.com"),
    ("\"user\\\"name\"@example.com"),

    % Whitespace around at-sign
    ("user @example.com"),
    ("user@ example.com"),

    % Domain literals
    ("user@[]"),
    ("user@[127.0.0.1]"),
    ("user@[ 127.0.0.1 ]"),
    ("user@["),
    ("user@[\\]"),
    ("user@[[]"),
    ("user@[]]"),

    % Non-ASCII
    ("uśer@example.com"),
    ("user@éxample.com"),

    % Angle-addr
    ("<user@example.com>"),
    ("< user@example.com>"),
    ("< user@example.com\t>"),

    % Display names
    ("Display Name <user@example.com>"),
    ("Display\\Name <user@example.com>"),
    ("Display\\\\Name <user@example.com>"),
    ("Display\\\"Name <user@example.com>"),
    ("\"Display Name\" <user@example.com>"),
    ("\"Display Name\" Name2 <user@example.com>"),
    ("\"Display\\Name\" Name2 <user@example.com>"),
    ("\"Display\\\\Name\" Name2 <user@example.com>"),
    ("\"Display\\\"Name\" Name2 <user@example.com>"),

    % Display names (may even exceed obs-phrase)
    ("D. Name <user@example.com>"),
    ("Display X. Name <user@example.com>"),
    ("Display..Name <user@example.com>"),
    ("Display. .Name <user@example.com>"),

    % Display name non-ASCII
    ("\"Dísplay Name\" Name2 <user@example.com>"),

    % Comments
    ("()"),
    ("user()@example.com"),
    ("user@()example.com"),
    ("user@example.com ( here is a comment )"),
    ("(comment one)user@example.com(comment two)"),
    ("(comment one)Display Name<user@example.com>(comment two)"),
    ("(comment one)Display Name()<user@example.com>(comment two)"),

    % Groups
    ("group:"),
    ("group:,"),
    ("group:;"),
    ("group :;"),
    ("group : ;"),
    ("group :,;"),
    ("group :,,,;"),
    ("group(comment):(comment);(comment)"),
    ("\"group name\":;"),
    ("gróup:;"),
    ("group : user@example.com ;"),
    ("group : user@example.com (comment) ;"),
    ("group : user@example.com junk ;"), % could do better
    ("group : user1@example.com, user@example.com;"),
    ("group : user@example.com, ;"),
    ("group : ,user@example.com ;"),
    ("group : ,user@example.com,;"),
    ("group : User One <user1@example.com>;"),
    ("group : User One <user1@example.com>, ;"),
    ("group : ,User One <user1@example.com> ;"),
    ("group : User One <user1@example.com>, user2@example.com ;"),
    ("group : user1, user2@example.com ;"),

    % Address list
    (","),
    ("user@example.com, user2@example.com"),
    ("user@example.com ,user2@example.com"),
    ("user@example.com , user2@example.com"),
    ("user@example.com, user2@example.com, user3@example.com"),
    ("user@example.com,"),
    ("user@example.com,,,"),
    (",,,user@example.com"),
    (",,,user@example.com,,,user2@example.com,,,"),
    ("Name1 <user1@example.com>, Name2 <user2@example.com>"),
    ("()Name1()<user1@example.com>(),()Name2 <user2@example.com>()"),
    ("Name1 <user1@example.com>,, Name2 <user2@example.com>")
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
