% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2047.decoder.
:- interface.

:- import_module rfc5322.

:- pred decode_phrase(phrase::in, phrase::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module stream.
:- import_module string.

:- import_module rfc5322.
:- import_module string_util.
:- use_module base64.

:- type encoding
    --->    b_encoding
    ;       q_encoding.

:- type maybe_decoded
    --->    orig(word)
    ;       decoded(string).

%-----------------------------------------------------------------------------%

decode_phrase(Words0, Words) :-
    list.map(decode_word, Words0, Words1),
    join_decoded_texts(Words1, Words).

:- pred decode_word(word::in, maybe_decoded::out) is det.

decode_word(Word, MaybeDecoded) :-
    (
        Word = word_atom(atom(ascii(PotentialEncodedWord))),
        decode_encoded_word(PotentialEncodedWord, DecodedText)
    ->
        MaybeDecoded = decoded(DecodedText)
    ;
        MaybeDecoded = orig(Word)
    ).

:- pred decode_encoded_word(string::in, string::out) is semidet.

decode_encoded_word(PotentialEncodedWord, DecodedText) :-
    is_encoded_word_utf8(PotentialEncodedWord, Encoding, EncodedTextStart,
        EncodedTextEnd),
    (
        Encoding = b_encoding,
        decode_b_encoded_text_utf8(PotentialEncodedWord, EncodedTextStart,
            EncodedTextEnd, DecodedText)
    ;
        Encoding = q_encoding,
        decode_q_encoded_text_utf8(PotentialEncodedWord, EncodedTextStart,
            EncodedTextEnd, DecodedText)
    ).

:- pred is_encoded_word_utf8(string::in, encoding::out, int::out, int::out)
    is semidet.

is_encoded_word_utf8(Input, Encoding, EncodedTextStart, EncodedTextEnd) :-
    % Encoded-words should to be limited to 75 characters but probably not in
    % practice.
    string.length(Input, InputLength),
    ( InputLength >= min_encoded_word_length ->
        L0 = 0,
        unsafe_index_next(Input, L0, L1, '='),
        unsafe_index_next(Input, L1, L, '?'),
        CharSetStart = L,

        R0 = InputLength,
        unsafe_prev_index(Input, R0, R1, '='),
        unsafe_prev_index(Input, R1, R, '?'),
        EncodedTextEnd = R,

        unsafe_strstr(Input, "?", CharSetStart, CharSetEnd),
        unsafe_between(Input, CharSetStart, CharSetEnd, CharSet),
        charset_is_utf8(CharSet),

        EncodingStart = CharSetEnd + 1,
        unsafe_index_next(Input, EncodingStart, EncodingEnd, EncodingChar),
        unsafe_index_next(Input, EncodingEnd, EncodedTextStart, '?'),
        char_to_encoding(EncodingChar, Encoding)
    ;
        fail
    ).

:- func min_encoded_word_length = int.

min_encoded_word_length = string.length("=?UTF-8?Q??=").

:- pred charset_is_utf8(string::in) is semidet.

charset_is_utf8(CharSet0) :-
    % Ignore RFC 2231 language tag if present.
    ( string.sub_string_search(CharSet0, "*", StarPos) ->
        string.unsafe_between(CharSet0, 0, StarPos, CharSet)
    ;
        CharSet = CharSet0
    ),
    % XXX handle some other charsets
    strcase_equal(CharSet, "UTF-8").

:- pred char_to_encoding(char::in, encoding::out) is semidet.

char_to_encoding('b', b_encoding).
char_to_encoding('B', b_encoding).
char_to_encoding('q', q_encoding).
char_to_encoding('Q', q_encoding).

%-----------------------------------------------------------------------------%

:- pred decode_b_encoded_text_utf8(string::in, int::in, int::in, string::out)
    is semidet.

decode_b_encoded_text_utf8(EncodedWord, EncodedTextStart, EncodedTextEnd,
        DecodedText) :-
    base64.decode(EncodedWord, EncodedTextStart, FinalPos, EncodedTextEnd,
        code_units_builder, code_units([]), code_units(RevCodeUnits)),
    check_b_encoded_word_final_pos(EncodedWord, FinalPos, EncodedTextEnd),
    list.reverse(RevCodeUnits, CodeUnits),
    string.from_code_unit_list(CodeUnits, DecodedText).

:- pred check_b_encoded_word_final_pos(string::in, int::in, int::in)
    is semidet.

check_b_encoded_word_final_pos(EncodedWord, FinalPos, EncodedTextEnd) :-
    ( FinalPos < EncodedTextEnd ->
        string.unsafe_index_code_unit(EncodedWord, FinalPos, equals)
    ;
        FinalPos = EncodedTextEnd
    ).

%-----------------------------------------------------------------------------%

    % This assumes UTF-8 string type.
:- pred decode_q_encoded_text_utf8(string::in, int::in, int::in, string::out)
    is semidet.

decode_q_encoded_text_utf8(EncodedWord, EncodedTextStart, EncodedTextEnd,
        DecodedText) :-
    decode_q(EncodedWord, EncodedTextStart, EncodedTextEnd, [], RevCodeUnits),
    list.reverse(RevCodeUnits, CodeUnits),
    string.from_code_unit_list(CodeUnits, DecodedText).

:- pred decode_q(string::in, int::in, int::in, list(int)::in, list(int)::out)
    is semidet.

decode_q(Input, Pos, EndPos, !RevCodeUnits) :-
    ( Pos < EndPos ->
        string.unsafe_index_code_unit(Input, Pos, Octet),
        ( Octet = equals ->
            decode_q_equals_sequence(Input, Pos, NextPos, EndPos, CodeUnit)
        ; Octet = underscore ->
            CodeUnit = 0x20,
            NextPos = Pos + 1
        ;
            Octet =< 0x7f,
            % Note that per Section 5, rule (3), encoded-words in phrases have
            % a more restricted set of characters than in other contexts.
            CodeUnit = Octet,
            NextPos = Pos + 1
        ),
        cons(CodeUnit, !RevCodeUnits),
        decode_q(Input, NextPos, EndPos, !RevCodeUnits)
    ;
        % End of encoded text.
        true
    ).

:- pred decode_q_equals_sequence(string::in, int::in, int::out, int::in,
    int::out) is semidet.

decode_q_equals_sequence(Input, EqualsPos, NextPos, EndPos, CodeUnit) :-
    PosA = EqualsPos + 1,
    PosB = EqualsPos + 2,
    NextPos = EqualsPos + 3,
    ( NextPos =< EndPos ->
        string.unsafe_index_code_unit(Input, PosA, HexA),
        string.unsafe_index_code_unit(Input, PosB, HexB),
        hex_digit(HexA, IntA),
        hex_digit(HexB, IntB),
        CodeUnit = (IntA << 4) \/ IntB
    ;
        fail
    ).

:- pred hex_digit(int::in, int::out) is semidet.

hex_digit(Octet, Int) :-
    char.from_int(Octet, Char),
    char.is_hex_digit(Char, Int).

%-----------------------------------------------------------------------------%

:- pred join_decoded_texts(list(maybe_decoded)::in, list(word)::out) is det.

join_decoded_texts([], []).
join_decoded_texts([H | T], Words) :-
    (
        H = orig(Word),
        join_decoded_texts(T, WordsTail),
        Words = [Word | WordsTail] % lcmc
    ;
        H = decoded(DecodedHead),
        takewhile_decoded(T, DecodedTail, Rest),
        string.append_list([DecodedHead | DecodedTail], DecodedText),
        decoded_text_to_word(DecodedText, Word),
        join_decoded_texts(Rest, WordsTail),
        Words = [Word | WordsTail] % lcmc
    ).

:- pred takewhile_decoded(list(maybe_decoded)::in, list(string)::out,
    list(maybe_decoded)::out) is det.

takewhile_decoded([], [], []).
takewhile_decoded([H | T], Decodeds, Rest) :-
    (
        H = orig(_),
        Decodeds = [],
        Rest = [H | T]
    ;
        H = decoded(DecodedHead),
        takewhile_decoded(T, DecodedTail, Rest),
        Decodeds = [DecodedHead | DecodedTail] % lcmc
    ).

:- pred decoded_text_to_word(string::in, word::out) is det.

decoded_text_to_word(String, Word) :-
    string.foldl2(classify, String, yes, AllAscii, yes, IsAtom),
    (
        AllAscii = yes,
        Inner = ascii(String)
    ;
        AllAscii = no,
        Inner = unicode(String)
    ),
    (
        IsAtom = yes,
        Word = word_atom(atom(Inner))
    ;
        IsAtom = no,
        Word = word_quoted_string(quoted_string(Inner))
    ).

:- pred classify(char::in, bool::in, bool::out, bool::in, bool::out) is det.

classify(C, !AllAscii, !IsAtom) :-
    (
        !.AllAscii = yes,
        nonascii(C)
    ->
        !:AllAscii = no
    ;
        true
    ),
    (
        !.IsAtom = yes,
        not atext_or_nonascii(C)
    ->
        !:IsAtom = no
    ;
        true
    ).

:- func equals = int.

equals = char.to_int('=').

:- func underscore = int.

underscore = char.to_int('_').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
