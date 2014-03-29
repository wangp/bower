%-----------------------------------------------------------------------------%

:- module test_rfc2047_encode.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pretty_printer.

:- import_module rfc2047.
:- import_module rfc2047.decoder.
:- import_module rfc2047.encoder.
:- import_module rfc5322.

%-----------------------------------------------------------------------------%

main(!IO) :-
    foldl(test_encode, encode_cases, !IO).

:- pred test_encode(phrase::in, io::di, io::uo) is det.

test_encode(Phrase0, !IO) :-
    encode_phrase(Phrase0, Phrase1),
    decode_phrase(Phrase1, Phrase),

    pretty_printer.write_doc(format(Phrase0), !IO),
    io.nl(!IO),
    pretty_printer.write_doc(format(Phrase1), !IO),
    io.nl(!IO),
    pretty_printer.write_doc(format(Phrase), !IO),
    io.write_string("\n--------\n", !IO).

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
    % overlong word
    [a("123456789-123456789-123456789-123456789-123456789-123456789-123x")]
].

:- func a(string) = word.

a(String) = word_atom(atom(unicode(String))).

:- func qs(string) = word.

qs(String) = word_quoted_string(quoted_string(unicode(String))).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
