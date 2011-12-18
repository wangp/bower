% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module json.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module parsing_utils.

%-----------------------------------------------------------------------------%

:- type json
    --->    int(int)            % number (could also be float)
    ;       string(esc_string)
    ;       bool(bool)
    ;       array(list(json))   % array
    ;       map(json_map)       % object
    ;       null.

:- type esc_string.

:- type json_map == map(key, json).

:- type key == string.

:- pred parse_json(string::in, parse_result(json)::out) is det.

:- func unescape(esc_string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module string.

:- type esc_string
    --->    esc_string(string).

%-----------------------------------------------------------------------------%

parse_json(Input, ParseResult) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, parse_json_eof, ParseResult)
    ).

:- pred parse_json_eof(src::in, json::out, ps::in, ps::out) is semidet.

parse_json_eof(Src, Value, !PS) :-
    whitespace(Src, _, !PS),
    parse_json(Src, Value, !PS),
    eof(Src, _, !PS).

:- pred parse_json(src::in, json::out, ps::in, ps::out) is semidet.

parse_json(Src, Value, !PS) :-
    ( parse_array(Src, List, !PS) ->
        Value = array(List)
    ; parse_map(Src, Map, !PS) ->
        Value = map(Map)
    ; parse_string(Src, String, !PS) ->
        Value = string(String)
    ; parse_int(Src, Int, !PS) ->
        Value = int(Int)
    ; keyword(id_chars, "true", Src, _, !PS) ->
        Value = bool(yes)
    ; keyword(id_chars, "false", Src, _, !PS) ->
        Value = bool(no)
    ; keyword(id_chars, "null", Src, _, !PS) ->
        Value = null
    ;
        fail
    ).

:- pred parse_array(src::in, list(json)::out, ps::in, ps::out) is semidet.

parse_array(Src, List, !PS) :-
    brackets("[", "]", separated_list(",", parse_json), Src, List, !PS).

:- pred parse_map(src::in, map(string, json)::out, ps::in, ps::out) is semidet.

parse_map(Src, Map, !PS) :-
    brackets("{", "}", separated_list(",", parse_key_value), Src, AssocList,
        !PS),
    map.from_assoc_list(AssocList, Map).

:- pred parse_key_value(src::in, pair(string, json)::out, ps::in, ps::out)
    is semidet.

parse_key_value(Src, Key - Value, !PS) :-
    parse_string(Src, EscKey, !PS),
    Key = unescape(EscKey),
    punct(":", Src, _, !PS),
    parse_json(Src, Value, !PS).

:- pred parse_string(src::in, esc_string::out, ps::in, ps::out) is semidet.

parse_string(Src, esc_string(EscString), !PS) :-
    string_literal('"', Src, EscString, !PS).

:- pred parse_int(src::in, int::out, ps::in, ps::out) is semidet.

parse_int(Src, Int, !PS) :-
    int_literal(Src, Int, !PS).

:- func id_chars = string.

id_chars = "abcdefghijklmnopqrstuvwxyz".

%-----------------------------------------------------------------------------%

unescape(esc_string(S0)) = S :-
    ( contains_backslash(S0) ->
        unescape_loop(S0, 0, _, [], RevChars),
        string.from_rev_char_list(RevChars, S)
    ;
        S = S0
    ).

:- pred contains_backslash(string::in) is semidet.

:- pragma foreign_proc("C",
    contains_backslash(Str::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (strchr(Str, '\\\\') != NULL);
").

:- pred unescape_loop(string::in, int::in, int::out,
    list(char)::in, list(char)::out) is det.

unescape_loop(S, !I, !RevChars) :-
    ( string.unsafe_index_next(S, !I, C0) ->
        ( C0 = ('\\') ->
            ( string.unsafe_index_next(S, !I, C1) ->
                ( simple_escape(C1, RealChar) ->
                    !:RevChars = [RealChar | !.RevChars]
                ; C1 = 'u' ->
                    (
                        hex_digit(S, !I, U1),
                        hex_digit(S, !I, U2),
                        hex_digit(S, !I, U3),
                        hex_digit(S, !I, U4)
                    ->
                        Int = (U1 << 12) \/ (U2 << 8) \/ (U3 << 4) \/ U4,
                        (
                            char.from_int(Int, Unichar),
                            not char.is_surrogate(Unichar),
                            not char.is_noncharacter(Unichar)
                        ->
                            !:RevChars = [Unichar | !.RevChars]
                        ;
                            !:RevChars = [replacement_char | !.RevChars]
                        )
                    ;
                        !:RevChars = [replacement_char | !.RevChars]
                    )
                ;
                    !:RevChars = [replacement_char | !.RevChars]
                )
            ;
                !:RevChars = [replacement_char | !.RevChars]
            ),
            unescape_loop(S, !I, !RevChars)
        ;
            !:RevChars = [C0 | !.RevChars],
            unescape_loop(S, !I, !RevChars)
        )
    ;
        true
    ).

:- pred simple_escape(char::in, char::out) is semidet.

simple_escape('"', '"').
simple_escape('\\', '\\').
simple_escape('/', '/').
simple_escape('b', '\b').
simple_escape('f', '\f').
simple_escape('n', '\n').
simple_escape('r', '\r').
simple_escape('t', '\t').

:- pred hex_digit(string::in, int::in, int::out, int::out) is semidet.

hex_digit(S, !I, Int) :-
    string.unsafe_index_next(S, !I, C),
    char.is_hex_digit(C, Int).

:- func replacement_char = char.

replacement_char = '\ufffd'.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
