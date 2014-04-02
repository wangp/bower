%-----------------------------------------------------------------------------%

:- module test_rfc2047_decode.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pretty_printer.
:- import_module string.

:- import_module rfc2047.
:- import_module rfc2047.decoder.
:- import_module rfc5322.

:- type case == list(string).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("Phrases\n", !IO),
    io.write_string("=======\n", !IO),
    list.foldl(test_decode_phrase, decode_cases, !IO),
    io.write_string("\n", !IO),
    io.write_string("Unstructured\n", !IO),
    io.write_string("============\n", !IO),
    list.foldl(test_decode_unstructured, decode_cases, !IO),
    list.foldl(test_decode_unstructured, unstructured_extra_cases, !IO).

:- pred test_decode_phrase(case::in, io::di, io::uo) is det.

test_decode_phrase(Input, !IO) :-
    Phrase0 = list.map(ascii_atom, Input),
    decode_phrase(Phrase0, Phrase),

    io.write_string("input: ", !IO),
    io.write(Input, !IO),
    io.nl(!IO),
    pretty_printer.write_doc(format(Phrase), !IO),
    io.nl(!IO),
    io.write_string("--------\n", !IO).

:- pred test_decode_unstructured(case::in, io::di, io::uo) is det.

test_decode_unstructured(Case, !IO) :-
    Input = string.join_list(" ", Case),
    decode_unstructured(Input, Unstructured),
    io.write_string("«", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n«", !IO),
    io.write_string(Unstructured, !IO),
    io.write_string("»\n", !IO),
    io.write_string("--------\n", !IO).

:- func ascii_atom(string) = word.

ascii_atom(S) = word_atom(atom(ascii(S))).

:- func decode_cases = list(case).

decode_cases = [
    [""],
    ["=??="], % bad
    ["=????="], % bad
    ["=?UTF-x?Q??="], % bad
    ["=?UTF-8?x??="], % bad
    ["=?UTF-8?Q??="], % ok
    ["=?UTF-8?Q?a?="], % ok
    ["=?UTF-8?Q?abc?="], % ok
    ["=?UTF-8?Q? ?="], % bad - not token
    ["=?UTF-8?Q???="], % bad - question mark in payload
    ["=?UTF-8?Q?=?="], % bad - invalid escape sequence
    ["=?UTF-8?Q?=x?="], % bad - invalid escape sequence
    ["=?UTF-8?Q?=xx?="], % bad - invalid escape sequence
    ["=?UTF-8?Q?=A?="], % bad - invalid escape sequence
    ["=?UTF-8?Q?=00?="], % bad - reject NUL
    ["=?UTF-8?Q?=09?="], % ok
    ["=?UTF-8?Q?=20?="], % ok
    ["=?UTF-8?Q?_?="], % ok
    ["=?UTF-8?Q?=5F?="], % ok
    ["=?UTF-8?Q?é?="], % bad - not ASCII
    ["=?UTF-8?Q?=C3?="], % bad - not UTF-8
    ["=?UTF-8?Q?=C3=A9?="], % ok
    ["=?utf-8?q?=C3=A9?="], % ok
    ["=?UTF-8*EN?Q?=C3=A9?="], % ok
    ["=?UTF-8?Q?=22=C3=A9=22?="], % ok
    ["=?UTF-8?Q?=e5=99=b8?="], % ok
    ["=?UTF-8?Q?caf?=", "=?UTF-8?Q?=C3=A9?="], % ok
    ["=?UTF-8?Q?caf=C3?=", "=?UTF-8?Q?=A9?="], % bad - split multibyte
    ["=?UTF-8?Q?h=C3=BA?=", "=?UTF-8?Q?h=C3=BA?=", "=?UTF-8?Q?h=C3=BA?="], % ok
    ["=?UTF-8?Q?h=C3=BA?=", "hu", "=?UTF-8?Q?h=C3=BA?="], % ok

    ["=?UTF-8?B??="], % ok
    ["=?UTF-8?B?=?="], % ok?
    ["=?UTF-8?B?YQ==?="], % ok
    ["=?UTF-8?B?YWI=?="], % ok
    ["=?UTF-8?B?YWJj?="], % ok
    ["=?UTF-8?B?Y*==?="], % bad - non base64 character
    ["=?UTF-8?B?YQ*=?="], % bad - non base64 character
    ["=?UTF-8?B?AA==?="], % bad - reject NUL
    ["=?UTF-8?B?gA==?="], % bad - not UTF-8
    ["=?UTF-8?B?w6k=?="], % ok
    ["=?UTF-8?B?ww==?=", "=?UTF-8?B?qQ==?="], % bad - split multibyte
    ["=?UTF-8?B?Y2Fmw6k=?="], % ok
    ["=?UTF-8?B?5Zm4?="], % ok

    ["=?UTF-8?B?Y2Fmw6k=?=", "=?UTF-8?Q?h=C3=BA?="], % ok
    ["=?UTF-8?B?Y2Fmw6k=?=", "=?UTF-8?Q?_h=C3=BA?="], % ok

    ["=?ISO-8859-1?Q?caf=E9?="], % ok
    ["=?ISO-8859-1?Q?=A4=A6=A8=B4=B8=BC=BD=BE?="], % ok - not iso-8859-15
    ["=?ISO-8859-1?B?pKaotLi8vb4=?="] % ok - not iso-8859-15
].

:- func unstructured_extra_cases = list(case).

unstructured_extra_cases = [
    [" ", "=?UTF-8?Q?h=C3=BA?=", "=?UTF-8?Q?h=C3=BA?=", "hu",
        "=?UTF-8?Q?h=C3=BA?="] % ok
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
