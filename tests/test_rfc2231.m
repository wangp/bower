%-----------------------------------------------------------------------------%

:- module test_rfc2231.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

:- import_module rfc2231.
:- import_module rfc2045.
:- import_module rfc5322.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test, cases, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(Input, !IO) :-
    Attr = attribute("filename"),
    Value = quoted_string(quoted_string(unicode(Input))),
    encode_parameter(Attr - Value, Param),
    parameter_to_string(Param, String),
    io.write_string("«", !IO),
    io.write_string(String, !IO),
    io.write_string("»\n\n", !IO).

:- pred parameter_to_string(parameter::in, string::out) is det.

parameter_to_string(Param, String) :-
    parameter_to_string(Param, String, _Valid).

:- func cases = list(string).

cases = [
    "",
    " a B ",
    "a\\b",
    "a\"b",
    "a*b",
    "a'b",
    "a%b",
    "ab;",
    "{ab}",
    "café",
    "噸"
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
