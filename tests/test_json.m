%-----------------------------------------------------------------------------%

:- module test_json.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module pretty_printer.
:- import_module string.

:- import_module json.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test, cases, !IO).

:- pred test(string::in, io::di, io::uo) is cc_multi.

test(Input, !IO) :-
    parse_json(Input, ParseResult),
    io.write_string("«", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),
    (
        ParseResult = ok(Value),
        pretty_printer.write_doc(format(Value), !IO),
        io.nl(!IO),
        io.nl(!IO),
        test_unescaped_strings(Value, !IO)
    ;
        ParseResult = error(yes(Message), Line, Column),
        indent(Column, !IO),
        io.write_string("^\n", !IO),
        io.format("%d:%d: %s\n", [i(Line), i(Column), s(Message)], !IO)
    ;
        ParseResult = error(no, Line, Column),
        indent(Column, !IO),
        io.write_string("^\n", !IO),
        io.format("%d:%d: parse error\n", [i(Line), i(Column)], !IO)
    ),
    io.write_string("--------\n", !IO).

:- pred test_unescaped_strings(json::in, io::di, io::uo) is det.

test_unescaped_strings(Value, !IO) :-
    (
        Value = null
    ;
        Value = bool(_)
    ;
        Value = int(_)
    ;
        Value = float(_)
    ;
        Value = string(EscString),
        String = unescape(EscString),
        io.write_string("«", !IO),
        io.write_string(String, !IO),
        io.write_string("»\n", !IO)
    ;
        Value = list(List),
        list.foldl(test_unescaped_strings, List, !IO)
    ;
        Value = map(Map),
        map.foldl_values(test_unescaped_strings, Map, !IO)
    ).

:- pred indent(int::in, io::di, io::uo) is det.

indent(N, !IO) :-
    ( N < 1 ->
        true
    ;
        io.write_char(' ', !IO),
        indent(N - 1, !IO)
    ).

:- func cases = list(string).

cases = [
    "", % bad

    % literals
    "null",
    "nullx", % bad
    "Null", % bad
    "true",
    "truex", % bad
    "True", % bad
    "false",
    "falsex", % bad
    "False", % bad

    % whitespace
    " null ",
    " null , true ", % bad
    "\ufeffnull", % bad
    "null\ufeff", % bad

    % integer
    "0",
    "-0",
    "-1",
    "+1",
    "01", % bad
    "001", % bad
    "0x1", % bad
    "0b1", % bad
    "2147483647",  %  (2**31)-1
    "-2147483648", % -(2**31)

    % float
    "2.71828",
    "+2.71828", % bad
    "-2.71828",
    "2.", % bad
    "-2.", % bad
    "2e", % bad
    "2e+", % bad
    "2e-", % bad
    "271828e5",
    "271828e+5",
    "271828e-5",
    "271828.e-5", % bad
    "271828.0e-5",
    "271828.00000E-5",
    "0.00271828000000E3",
    "0.00271828e+3",
    "0.00271828e+33",
    "0.00271828e-33",
    "0.00271828e+333", % bad
    "0.00271828e-333",

    % strings
    """",
    """abc""",
    """ café""",
    """☿""",

    """\a""",       % bad
    """\b""",       % bad
    """\f""",       % bad
    """\n""",       % bad
    """\r""",       % bad
    """\t""",       % bad
    """\v""",       % bad
    """\u0001""",   % bad
    """\u001f""",   % bad
    """\\\"""",
    """\\\\""",
    """\\/""",
    """\\a""",      % bad
    """\\b""",
    """\\f""",
    """\\n""",
    """\\r""",
    """\\t""",
    """\\v""",      % bad
    """\\u""",      % bad
    """\\u0""",     % bad
    """\\u00""",    % bad
    """\\u000""",   % bad
    """\\u0000""",  % rejected
    """\\u0001""",
    """\\u005C""",
    """\\u00e9""",
    """\\u263f""",
    """a\u2028b""", % ok in JSON but not JavaScript
    """a\u2029b""", % ok in JSON but not JavaScript
    """𝄞""",                % U+1D11E
    """\\uD834\\uDD1E""",   % U+1D11E
    """\\ud834\\uDd1E""",   % U+1D11E
    """\\ud800\\udc00""",   % ok
    """\\udbff\\udfff""",   % ok
    """\\ud800""",          % bad - lead surrogate only
    """\\udc00""",          % bad - trail surrogate only
    """\\ud800\\udbff""",   % bad - two lead surrogates
    """\ufdd0\ufdef""",     % non-characters
    """\\ufdd0\\ufdef""",   % non-characters
    """\ufffe\uffff""",     % non-characters
    """\\ufffe\\uffff""",   % non-characters

    % array
    "[]",
    "[", % bad
    "]", % bad
    "[[]", % bad
    "[,]", % bad
    "[null]",
    "[null,]", % bad
    "[null ,true, false]",

    % object
    "{}",
    "{", % bad
    "}", % bad
    "{,}", % bad
    "{true:false}", % bad
    "{a:false}", % bad
    "{\"a\"}", % bad
    "{\"a\":}", % bad
    "{\"a\":null}",
    "{\"a\":null, }", % bad
    "{\"a\":true, \"a\": false}", % bad
    "{\"a\":false ,\"b\" : true, \"cd\": null}",
    "{\"ć\":-1234 ,\"b\" : 5.678, \"e\": [], \"A\" : ""nine"", ""d"": {}}"
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
