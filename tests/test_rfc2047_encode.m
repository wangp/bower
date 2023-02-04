%-----------------------------------------------------------------------------%

:- module test_rfc2047_encode.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pprint.
:- import_module string.

:- import_module rfc2047.
:- import_module rfc2047.decoder.
:- import_module rfc2047.encoder.
:- import_module rfc5322.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("Phrases\n", !IO),
    io.write_string("=======\n", !IO),
    list.foldl(test_encode_phrase, encode_cases, !IO),
    io.write_string("\n", !IO),
    io.write_string("Unstructured\n", !IO),
    io.write_string("============\n", !IO),
    list.foldl(test_encode_unstructured, encode_cases, !IO).

:- pred test_encode_phrase(phrase::in, io::di, io::uo) is det.

test_encode_phrase(Phrase0, !IO) :-
    encode_phrase(Phrase0, Phrase1),
    decode_phrase(Phrase1, Phrase),

    % Note: we use pprint for now instead of pretty_printer as the
    % output of pretty_printer was changed slightly on 2022-12-27.
    pprint.write(80, to_doc(Phrase0), !IO),
    io.nl(!IO),
    pprint.write(80, to_doc(Phrase1), !IO),
    io.nl(!IO),
    pprint.write(80, to_doc(Phrase), !IO),
    io.write_string("\n--------\n", !IO).

:- pred test_encode_unstructured(phrase::in, io::di, io::uo) is det.

test_encode_unstructured(Phrase, !IO) :-
    Input = string.join_list(" ", list.map(word_to_string, Phrase)),
    encode_unstructured(Input, Encoded),
    decode_unstructured(Encoded, Decoded),

    io.write_string("", !IO),
    io.write_string(Input, !IO),
    io.write_string("\n", !IO),
    io.write_string(Encoded, !IO),
    io.write_string("\n", !IO),
    io.write_string(Decoded, !IO),
    io.write_string("\n", !IO),
    io.write_string("--------\n", !IO).

:- func encode_cases = list(phrase).

encode_cases = [
    [a("abc")],
    [a("噸")],
    [qs("now only €15")],
    % input looks like encoded-word
    [a("=??=")],
    [qs("=??=")],
    [a("=?abc?=")],
    [qs("=?abc?=")],
    [a("=?UTF-8?Q??=")],
    [qs("=?UTF-8?Q??=")],
    % choose Q-encoding
    [qs("Svifnökkvinn minn er fullur af álum")],
    [a("Svifnökkvinn"), a("álum")],
    % choose B-encoding
    [qs("Τὸ πλοῖόν μου τὸ μετεωριζόμενον ἐστι πλῆρες ἐγχελέων")],
    % intervening ASCII
    [qs("Τὸ πλοῖόν μου -- μετεωριζόμενον ---- πλῆρες ἐγχελέων")],
    % overlong word
    [a("123456789-123456789-123456789-123456789-123456789-123456789-123x")],
    % check characters allowed in Q encoded-words
    [a("ábcdefghijklmnopqrstuvwxyz!\"#$%")],
    [a("ábcdefghijklmnopqrstuvwxyz&'()")],
    [a("ábcdefghijklmnopqrstuvwxyz*+,-./")],
    [a("ábcdefghijklmnopqrstuvwxyz:;")],
    [a("ábcdefghijklmnopqrstuvwxyz<=>")],
    [a("ábcdefghijklmnopqrstuvwxyz?@")],
    [a("ábcdefghijklmnopqrstuvwxyz[\\]")],
    [a("ábcdefghijklmnopqrstuvwxyz^_`")],
    [a("ábcdefghijklmnopqrstuvwxyz{|}~")]
].

:- func a(string) = word.

a(String) = word_atom(atom(unicode(String))).

:- func qs(string) = word.

qs(String) = word_quoted_string(quoted_string(unicode(String))).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
