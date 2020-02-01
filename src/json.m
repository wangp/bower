% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module json.
:- interface.

:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module parsing_utils.

%-----------------------------------------------------------------------------%

:- type json
    --->    null
    ;       bool(bool)
    ;       int(int)
    ;       integer(integer)        % only if too big for int()
    ;       float(float)
    ;       string(esc_string)
    ;       list(list(json))        % array
    ;       map(map(string, json)). % object

:- type esc_string.

:- pred parse_json(string::in, parse_result(json)::out) is cc_multi.

:- func unescape(esc_string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module string.
:- import_module unit.

:- import_module string_util.

:- type esc_string
    --->    esc_string(string).

% RFC 7159: The JavaScript Object Notation (JSON) Data Interchange Format

%-----------------------------------------------------------------------------%

parse_json(Input, ParseResult) :-
    parsing_utils.parse(Input, skip_ws, parse_json_eof, ParseResult).

:- pred parse_json_eof(src::in, json::out, ps::in, ps::out) is semidet.

parse_json_eof(Src, Value, !PS) :-
    skip_ws(Src, _, !PS),
    value(Src, Value, !PS),
    eof(Src, _, !PS).

:- pred skip_ws(src::in, unit::out, ps::in, ps::out) is semidet.

skip_ws(Src, unit, !PS) :-
    (
        next_char_np(Src, C, !PS),
        ws(C)
    ->
        skip_ws(Src, _, !PS)
    ;
        true
    ).

:- pred ws(char::in) is semidet.

ws(' ').
ws('\t').
ws('\n').
ws('\r').

:- pred next_char_np(src::in, char::out, ps::in, ps::out) is semidet.

next_char_np(Src, C, !PS) :-
    % All calls to next_char_np can be changed to next_char_no_progress
    % when compatibility with Mercury 11.07 is dropped.
    parsing_utils.next_char(Src, C, !PS).

:- pred value(src::in, json::out, ps::in, ps::out) is semidet.

value(Src, Value, !PS) :-
    next_char_np(Src, C, !.PS, _PS),
    (
        C = ('n'),
        keyword(id_chars, "null", Src, _, !PS),
        Value = null
    ;
        C = ('f'),
        keyword(id_chars, "false", Src, _, !PS),
        Value = bool(no)
    ;
        C = ('t'),
        keyword(id_chars, "true", Src, _, !PS),
        Value = bool(yes)
    ;
        C = ('['),
        array(Src, List, !PS),
        Value = list(List)
    ;
        C = ('{'),
        object(Src, Map, !PS),
        Value = map(Map)
    ;
        ( C = ('-')
        ; C = ('0') ; C = ('1') ; C = ('2') ; C = ('3') ; C = ('4')
        ; C = ('5') ; C = ('6') ; C = ('7') ; C = ('8') ; C = ('9')
        ),
        number(Src, Number, !PS),
        Value = Number
    ;
        C = ('"'),
        string(Src, String, !PS),
        Value = string(String)
    ).

:- func id_chars = string.

id_chars = "abcdefghijklmnopqrstuvwxyz".

%-----------------------------------------------------------------------------%

:- pred array(src::in, list(json)::out, ps::in, ps::out) is semidet.

array(Src, List, !PS) :-
    brackets("[", "]", comma_separated_list(value), Src, List, !PS).

%-----------------------------------------------------------------------------%

:- pred object(src::in, map(string, json)::out, ps::in, ps::out)
    is semidet.

object(Src, Map, !PS) :-
    punct("{", Src, _, !PS),
    ( object_member(Src, Name, Value, !PS) ->
        Map0 = map.singleton(Name, Value),
        object_members(Src, Map0, Map, !PS)
    ;
        Map = map.init
    ),
    punct("}", Src, _, !PS).

:- pred object_members(src::in, map(string, json)::in, map(string, json)::out,
    ps::in, ps::out) is semidet.

object_members(Src, !Map, !PS) :-
    ( punct(",", Src, _, !PS) ->
        PS0 = !.PS,
        object_member(Src, Name, Value, !PS),
        ( map.insert(Name, Value, !Map) ->
            object_members(Src, !Map, !PS)
        ;
            fail_with_message("duplicate field name", Src, !:Map, PS0, !:PS)
        )
    ;
        true
    ).

:- pred object_member(src::in, string::out, json::out, ps::in, ps::out)
    is semidet.

object_member(Src, Name, Value, !PS) :-
    string(Src, EscName, !PS),
    Name = unescape(EscName),
    punct(":", Src, _, !PS),
    value(Src, Value, !PS).

%-----------------------------------------------------------------------------%

:- pred number(src::in, json::out, ps::in, ps::out) is semidet.

number(Src, Number, !PS) :-
    current_offset(Src, Start, !PS),
    optional(minus, Src, _, !PS),
    int(Src, _, !PS),
    ( frac(Src, _, !PS) ->
        HasFrac = yes
    ;
        HasFrac = no
    ),
    ( exp(Src, _, !PS) ->
        HasExp = yes
    ;
        HasExp = no
    ),
    current_offset(Src, End, !PS),
    input_substring(Src, Start, End, NumberString),
    ( if HasFrac = yes ; HasExp = yes then
        string.to_float(NumberString, Float),
        not is_nan_or_inf(Float),
        Number = float(Float)
    else if string.to_int(NumberString, Int) then
        Number = int(Int)
    else if integer.from_string(NumberString, Integer) then
        Number = integer(Integer)
    else
        fail
    ),
    skip_ws(Src, unit, !PS).

:- pred 'DIGIT'(src::in, char::out, ps::in, ps::out) is semidet.

'DIGIT'(Src, C, !PS) :-
    next_char_np(Src, C, !PS), % for better error messages
    char.is_digit(C).

:- pred digits0(src::in, unit::out, ps::in, ps::out) is det.

digits0(Src, unit, !PS) :-
    ( 'DIGIT'(Src, _, !PS) ->
        digits0(Src, _, !PS)
    ;
        true
    ).

:- pred digits1(src::in, unit::out, ps::in, ps::out) is semidet.

digits1(Src, unit, !PS) :-
    'DIGIT'(Src, _, !PS),
    digits0(Src, _, !PS).

:- pred int(src::in, unit::out, ps::in, ps::out) is semidet.

int(Src, unit, !PS) :-
    'DIGIT'(Src, C, !PS),
    ( C = ('0') ->
        true
    ;
        digits0(Src, unit, !PS)
    ).

:- pred frac(src::in, unit::out, ps::in, ps::out) is semidet.

frac(Src, unit, !PS) :-
    next_char(Src, '.', !PS), % progress
    digits1(Src, _, !PS).

:- pred exp(src::in, unit::out, ps::in, ps::out) is semidet.

exp(Src, unit, !PS) :-
    e(Src, _, !PS),
    optional(minus_or_plus, Src, _, !PS),
    digits1(Src, _, !PS).

:- pred e(src::in, unit::out, ps::in, ps::out) is semidet.

e(Src, unit, !PS) :-
    next_char(Src, E, !PS),
    ( E = ('e')
    ; E = ('E')
    ).

:- pred minus(src::in, unit::out, ps::in, ps::out) is semidet.

minus(Src, unit, !PS) :-
    next_char(Src, '-', !PS).

:- pred minus_or_plus(src::in, unit::out, ps::in, ps::out) is semidet.

minus_or_plus(Src, unit, !PS) :-
    next_char(Src, C, !PS),
    ( C = ('-')
    ; C = ('+')
    ).

%-----------------------------------------------------------------------------%

:- pred string(src::in, esc_string::out, ps::in, ps::out) is semidet.

string(Src, esc_string(EscString), !PS) :-
    next_char(Src, '"', !PS),
    current_offset(Src, Start, !PS),
    string_tail(Src, End, !PS),
    input_substring(Src, Start, End, EscString),
    skip_ws(Src, _, !PS).

:- pred string_tail(src::in, int::out, ps::in, ps::out) is semidet.

string_tail(Src, End, !PS) :-
    PS0 = !.PS,
    next_char(Src, C, !PS),
    ( C = ('"') ->
        current_offset(Src, End, PS0, _)
    ; C = ('\\') ->
        escape_sequence(Src, _, PS0, !PS),
        string_tail(Src, End, !PS)
    ; unescaped_char(C) ->
        string_tail(Src, End, !PS)
    ;
        fail_with_message("invalid character", Src, End, PS0, !:PS)
    ).

:- pred unescaped_char(char::in) is semidet.

unescaped_char(C) :-
    char.to_int(C, I),
    % 0x22 (") excluded earlier.
    % 0x5C (\) excluded earlier.
    0x20 =< I, I =< 0x10ffff.

:- pred escape_sequence(src::in, char::out, ps::in, ps::in, ps::out)
    is semidet.

escape_sequence(Src, Char, PS0, !PS) :-
    next_char(Src, C1, !PS),
    ( simple_escape(C1) ->
        Char = C1
    ; C1 = ('u') ->
        unicode_escape_sequence(Src, Char, PS0, !PS)
    ;
        fail_with_message("invalid escape sequence", Src, Char, PS0, !:PS)
    ).

:- pred unicode_escape_sequence(src::in, char::out, ps::in, ps::in, ps::out)
    is semidet.

unicode_escape_sequence(Src, UnicodeScalarValue, PS0, !PS) :-
    hex_codepoint(Src, CodePoint1, !PS),
    ( is_surrogate_lead(CodePoint1) ->
        PS1 = !.PS,
        (
            next_char(Src, '\\', !PS),
            next_char(Src, 'u', !PS),
            hex_codepoint(Src, CodePoint2, !PS),
            is_surrogate_trail(CodePoint2)
        ->
            surrogate_pair(CodePoint1, CodePoint2, UnicodeScalarValue)
        ;
            fail_with_message("expected trail surrogate code point",
                Src, UnicodeScalarValue, PS1, !:PS)
        )
    ; is_surrogate_trail(CodePoint1) ->
        fail_with_message("unexpected trail surrogate code point",
            Src, UnicodeScalarValue, PS0, !:PS)
    ; CodePoint1 = 0 ->
        fail_with_message("null character not allowed",
            Src, UnicodeScalarValue, PS0, !:PS)
    ;
        % This happens to allow Unicode Noncharacters.
        char.from_int(CodePoint1, UnicodeScalarValue)
    ).

:- pred hex_codepoint(src::in, int::out, ps::in, ps::out) is semidet.

hex_codepoint(Src, Int, !PS) :-
    parse_hex_digit(Src, U1, !PS),
    parse_hex_digit(Src, U2, !PS),
    parse_hex_digit(Src, U3, !PS),
    parse_hex_digit(Src, U4, !PS),
    Int = (U1 << 12) \/ (U2 << 8) \/ (U3 << 4) \/ U4.

:- pred parse_hex_digit(src::in, int::out, ps::in, ps::out) is semidet.

parse_hex_digit(Src, Int, !PS) :-
    next_char(Src, C, !PS),
    char.is_hex_digit(C, Int).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unescape(esc_string(ES)) = S :-
    ( contains_backslash(ES) ->
        Anchor0 = 0,
        Pos0 = 0,
        unescape_loop(ES, Anchor0, Pos0, _Pos, empty, RevPieces),
        string_from_rev_pieces(RevPieces, S)
    ;
        S = ES
    ).

:- pred contains_backslash(string::in) is semidet.

:- pragma foreign_proc("C",
    contains_backslash(Str::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (strchr(Str, '\\\\') != NULL);
").

:- pred unescape_loop(string::in, int::in, int::in, int::out,
    pieces::in, pieces::out) is det.

unescape_loop(Src, AnchorPos, !Pos, !RevPieces) :-
    Pos0 = !.Pos,
    next_char(Src, C0, !Pos),
    ( C0 = char.det_from_int(0):char ->
        % End of string.
        add_substring_piece(Src, AnchorPos, !.Pos, !RevPieces)
    ; C0 = ('\\') ->
        add_substring_piece(Src, AnchorPos, Pos0, !RevPieces),
        unescape_sequence(Src, MaybeValid, !Pos, !RevPieces),
        (
            MaybeValid = yes(SeqString),
            !:RevPieces = literal(SeqString, !.RevPieces)
        ;
            MaybeValid = no,
            add_replacement_char(!RevPieces)
        ),
        NextAnchor = !.Pos,
        unescape_loop(Src, NextAnchor, !Pos, !RevPieces)
    ;
        unescape_loop(Src, AnchorPos, !Pos, !RevPieces)
    ).

:- pred next_char(string::in, char::out, int::in, int::out) is det.

next_char(S, C, I0, I) :-
    ( string.unsafe_index_next(S, I0, I1, C1) ->
        C = C1,
        I = I1
    ;
        C = char.det_from_int(0),
        I = I0
    ).

:- pred add_substring_piece(string::in, int::in, int::in,
    pieces::in, pieces::out) is det.

add_substring_piece(Src, AnchorPos, Pos, RevPieces0, RevPieces) :-
    ( AnchorPos < Pos ->
        RevPieces = substring(Src, AnchorPos, Pos, RevPieces0)
    ;
        RevPieces = RevPieces0
    ).

:- pred add_replacement_char(pieces::in, pieces::out) is det.

add_replacement_char(RevPieces0, RevPieces) :-
    RevPieces = literal("\ufffd", RevPieces0).

:- pred unescape_sequence(string::in, maybe(string)::out, int::in, int::out,
    pieces::in, pieces::out) is det.

unescape_sequence(Src, MaybeValid, !Pos, !RevPieces) :-
    next_char(Src, Char, !Pos),
    ( simple_escape(Char, RealChar) ->
        MaybeValid = yes(RealChar)
    ; Char = 'u' ->
        unescape_unicode_sequence(Src, MaybeValid, !Pos, !RevPieces)
    ;
        MaybeValid = no
    ).

:- pred unescape_unicode_sequence(string::in, maybe(string)::out,
    int::in, int::out, pieces::in, pieces::out) is det.

unescape_unicode_sequence(Src, MaybeValid, !Pos, !RevPieces) :-
    maybe_hex_codepoint(Src, MaybeCodePoint1, !Pos),
    (
        MaybeCodePoint1 = yes(CodePoint1),
        ( is_surrogate_lead(CodePoint1) ->
            trail_surrogate_sequence(Src, MaybeTrail, !Pos),
            (
                MaybeTrail = yes(CodePoint2),
                ( surrogate_pair(CodePoint1, CodePoint2, UnicodeScalarValue) ->
                    UnicharString = string.from_char(UnicodeScalarValue),
                    MaybeValid = yes(UnicharString)
                ;
                    MaybeValid = no
                )
            ;
                MaybeTrail = no,
                MaybeValid = no
            )
        ;
            % This happens to allow Unicode Noncharacters.
            % Reject NUL.
            CodePoint1 \= 0,
            char.from_int(CodePoint1, Unichar)
        ->
            UnicharString = string.from_char(Unichar),
            MaybeValid = yes(UnicharString)
        ;
            MaybeValid = no
        )
    ;
        MaybeCodePoint1 = no,
        MaybeValid = no
    ).

:- pred maybe_hex_codepoint(string::in, maybe(int)::out, int::in, int::out)
    is det.

maybe_hex_codepoint(Src, MaybeCodePoint, !Pos) :-
    % We want to advance Pos as far as possible in presence of errors.
    ( hex_digit(Src, U1, !Pos) ->
        ( hex_digit(Src, U2, !Pos) ->
            ( hex_digit(Src, U3, !Pos) ->
                ( hex_digit(Src, U4, !Pos) ->
                    Int = (U1 << 12) \/ (U2 << 8) \/ (U3 << 4) \/ U4,
                    MaybeCodePoint = yes(Int)
                ;
                    MaybeCodePoint = no
                )
            ;
                MaybeCodePoint = no
            )
        ;
            MaybeCodePoint = no
        )
    ;
        MaybeCodePoint = no
    ).

:- pred hex_digit(string::in, int::out, int::in, int::out) is semidet.

hex_digit(Src, Int, !Pos) :-
    next_char(Src, C, !Pos),
    char.is_hex_digit(C, Int).

:- pred trail_surrogate_sequence(string::in, maybe(int)::out,
    int::in, int::out) is det.

trail_surrogate_sequence(Src, MaybeTrail, !Pos) :-
    (
        next_char(Src, '\\', !Pos),
        next_char(Src, 'u', !Pos)
    ->
        maybe_hex_codepoint(Src, MaybeCodePoint, !Pos),
        (
            MaybeCodePoint = yes(CodePoint),
            is_surrogate_trail(CodePoint)
        ->
            MaybeTrail = MaybeCodePoint
        ;
            MaybeTrail = no
        )
    ;
        MaybeTrail = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred simple_escape(char::in) is semidet.

simple_escape(C) :-
    simple_escape(C, _).

:- pred simple_escape(char::in, string::out) is semidet.

simple_escape('"', "\"").
simple_escape('\\', "\\").
simple_escape('/', "/").
simple_escape('b', "\b").
simple_escape('f', "\f").
simple_escape('n', "\n").
simple_escape('r', "\r").
simple_escape('t', "\t").

:- pred is_surrogate_lead(int::in) is semidet.

is_surrogate_lead(I) :-
    0xd800 =< I, I =< 0xdbff.

:- pred is_surrogate_trail(int::in) is semidet.

is_surrogate_trail(I) :-
    0xdc00 =< I, I =< 0xdfff.

:- pred surrogate_pair(int::in, int::in, char::out) is semidet.

surrogate_pair(Lead, Trail, UnicodeScalarValue) :-
    Hi = Lead - 0xd800,
    Lo = Trail - 0xdc00,
    Int = 0x10000 + ((Hi << 10) \/ Lo),
    char.from_int(Int, UnicodeScalarValue).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
