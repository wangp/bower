%-----------------------------------------------------------------------------%

:- module json.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module parsing_utils.

%-----------------------------------------------------------------------------%

:- type json
    --->    int(int)            % number (could also be float)
    ;       string(string)
    ;       bool(bool)
    ;       array(list(json))   % array
    ;       map(json_map)       % object
    ;       null.

:- type json_map == map(key, json).

:- type key == string.

:- pred parse_json(string::in, parse_result(json)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module unit.

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
    parse_string(Src, Key, !PS),
    punct(":", Src, _, !PS),
    parse_json(Src, Value, !PS).

:- pred parse_string(src::in, string::out, ps::in, ps::out) is semidet.

parse_string(Src, String, !PS) :-
    % XXX backslash escapes are left in
    string_literal('"', Src, String, !PS).

:- pred parse_int(src::in, int::out, ps::in, ps::out) is semidet.

parse_int(Src, Int, !PS) :-
    int_literal(Src, Int, !PS).

:- func id_chars = string.

id_chars = "abcdefghijklmnopqrstuvwxyz".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
