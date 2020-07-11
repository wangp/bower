% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2047.encoder.
:- interface.

:- import_module rfc5322.

:- pred encode_phrase(phrase::in, phrase::out) is det.

:- pred encode_unstructured(string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- use_module require.

:- import_module base64.
:- import_module list_util.
:- import_module rfc5234.
:- import_module string_util.

:- type encoding_context
    --->    phrase
    ;       unstructured.

:- type encode_accum
    --->    accum(
                % The number of UTF-8 code units.
                num_code_units  :: int,
                % The number of characters that can be written as a single byte
                % in Q-encoded phrase words.
                qsingle_chars   :: int,
                rev_pieces      :: list(string)
            ).

:- type unstructured_span
    --->    ascii(
                ascii       :: string,
                ascii_ws    :: string
            )
    ;       requires_encoding(
                strings     :: list(string),
                trailing_ws :: string
            ).

%-----------------------------------------------------------------------------%

encode_phrase([], []).
encode_phrase(Plain, Encoded) :-
    Plain = [PlainWord0 | PlainWords0],
    ( must_encode(PlainWord0) ->
        list_util.take_while(must_encode, Plain, PlainHead, PlainTail),
        make_encoded_words(PlainHead, EncodedHead),
        encode_phrase(PlainTail, EncodedTail),
        Encoded = EncodedHead ++ EncodedTail
    ;
        encode_phrase(PlainWords0, EncodedTail),
        Encoded = [PlainWord0 | EncodedTail]
    ).

:- pred must_encode(word::in) is semidet.

must_encode(Word) :-
    must_encode(Word, yes).

:- pred must_encode(word::in, bool::out) is det.

must_encode(Word, MustEncode) :-
    (
        ( Word = word_atom(atom(ascii(String)))
        ; Word = word_quoted_string(quoted_string(ascii(String)))
        ),
        % Encoded words are not valid within quoted-strings so user agents
        % should not mistake strings beginning with "=?" and ending with "?="
        % as encoded-words, if I am not mistaken. But implementations would
        % probably try to account for incorrectly encoded input, so we encode
        % quoted-strings whose contents happen to look like encoded-words.
        MustEncode = ( looks_like_encoded_word(String) -> yes ; no )
    ;
        ( Word = word_atom(atom(unicode(_)))
        ; Word = word_quoted_string(quoted_string(unicode(_)))
        ),
        MustEncode = yes
    ).

:- pred looks_like_encoded_word(string::in) is semidet.

looks_like_encoded_word(String) :-
    (
        string.length(String, End),
        End >= 4
    ->
        L0 = 0,
        R0 = End,
        string.unsafe_index_next(String, L0, L1, '='),
        string.unsafe_index_next(String, L1, _L, '?'),
        string.unsafe_prev_index(String, R0, R1, '='),
        string.unsafe_prev_index(String, R1, _R, '?')
    ;
        fail
    ).

:- pred make_encoded_words(list(word)::in, list(word)::out) is det.

make_encoded_words(Plains, Words) :-
    Context = phrase,
    String = string.join_list(" ", list.map(word_to_string, Plains)),
    foldl2_spans(encode_span(Context), String, 0, _Pos,
        init, Accum, [], RevEncodedWords0),
    add_encoded_word(Context, Accum, RevEncodedWords0, RevEncodedWords),
    list.reverse(RevEncodedWords, EncodedWords),
    list.map(word_atom_ascii, EncodedWords) = Words.

:- func word_atom_ascii(string) = word.

word_atom_ascii(S) = word_atom(atom(ascii(S))).

:- pred foldl2_spans(pred(string, A, A, B, B), string, int, int, A, A, B, B).
:- mode foldl2_spans(in(pred(in, in, out, in, out) is det), in, in, out,
    in, out, in, out) is det.

foldl2_spans(Pred, String, Pos0, Pos, !A, !B) :-
    advance_while(not_WSP, String, Pos0, Pos1),
    ( Pos0 = Pos1 ->
        true
    ;
        unsafe_between(String, Pos0, Pos1, Span1),
        Pred(Span1, !A, !B)
    ),
    advance_while('WSP', String, Pos1, Pos2),
    ( Pos1 = Pos2 ->
        true
    ;
        unsafe_between(String, Pos1, Pos2, Span2),
        Pred(Span2, !A, !B)
    ),
    ( Pos2 = Pos0 ->
        Pos = Pos0
    ;
        foldl2_spans(Pred, String, Pos2, Pos, !A, !B)
    ).

%-----------------------------------------------------------------------------%

encode_unstructured(Input, EncodedString) :-
    get_unstructured_spans(Input, 0, Spans0),
    list.foldr(combine_unstructured_spans, Spans0, [], Spans),
    list.map(encode_unstructured_span, Spans, EncodedStringss),
    list.condense(EncodedStringss, EncodedStrings),
    string.append_list(EncodedStrings, EncodedString).

:- pred get_unstructured_spans(string::in, int::in,
    list(unstructured_span)::out) is det.

get_unstructured_spans(String, Pos0, Spans) :-
    advance_while(not_WSP, String, Pos0, Pos1),
    advance_while('WSP', String, Pos1, Pos2),
    ( Pos2 = Pos0 ->
        Spans = []
    ;
        make_unstructured_span(String, Pos0, Pos1, Pos2, Span),
        get_unstructured_spans(String, Pos2, RestSpans),
        Spans = [Span | RestSpans] % lcmc
    ).

:- pred make_unstructured_span(string::in, int::in, int::in, int::in,
    unstructured_span::out) is det.

make_unstructured_span(String, Pos0, Pos1, Pos2, Span) :-
    string.unsafe_between(String, Pos0, Pos1, Nonws),
    string.unsafe_between(String, Pos1, Pos2, Trailer),
    (
        string.all_match(ascii, Nonws),
        not looks_like_encoded_word(Nonws)
    ->
        Span = ascii(Nonws, Trailer)
    ;
        Span = requires_encoding([Nonws], Trailer)
    ).

:- pred combine_unstructured_spans(unstructured_span::in,
    list(unstructured_span)::in, list(unstructured_span)::out) is det.

combine_unstructured_spans(Span0, Spans0, Spans) :-
    % We need to encode consecutive strings together because whitespace between
    % encoded-words is ignored.  We could bridge two requires_encoding spans by
    % some short ASCII spans if that would result in shorter output overall,
    % but we don't do that yet.
    (
        Span0 = requires_encoding(Strings0, Trailer0),
        Spans0 = [Span1 | Spans2],
        Span1 = requires_encoding(Strings1, Trailer1)
    ->
        Span2 = requires_encoding(Strings0 ++ [Trailer0 | Strings1], Trailer1),
        Spans = [Span2 | Spans2]
    ;
        Spans = [Span0 | Spans0]
    ).

:- pred encode_unstructured_span(unstructured_span::in, list(string)::out)
    is det.

encode_unstructured_span(Span, Output) :-
    (
        Span = ascii(String, Trailer),
        Output = [String, Trailer]
    ;
        Span = requires_encoding(Strings, Trailer),
        string.append_list(Strings, String),
        Context = unstructured,
        encode_span(Context, String, init, Accum, [], RevEncodedWords0),
        add_encoded_word(Context, Accum, RevEncodedWords0, RevEncodedWords),
        reverse_intersperse(" ", RevEncodedWords, [Trailer], Output)
    ).

:- pred reverse_intersperse(T::in, list(T)::in, list(T)::in, list(T)::out)
    is det.

reverse_intersperse(_Sep, [], Acc, Acc).
reverse_intersperse(Sep, [X | Xs], Acc0, Acc) :-
    (
        Xs = [],
        Acc = [X | Acc0]
    ;
        Xs = [_ | _],
        reverse_intersperse(Sep, Xs, [Sep, X | Acc0], Acc)
    ).

%-----------------------------------------------------------------------------%

:- func init = encode_accum.

init = accum(0, 0, []).

:- pred encode_span(encoding_context::in, string::in,
    encode_accum::in, encode_accum::out, list(string)::in, list(string)::out)
    is det.

encode_span(Context, Span, Accum0, Accum, !RevEncodedWords) :-
    accum_span(Context, Span, MaybeRemaining, Accum0, Accum1),
    (
        MaybeRemaining = no,
        Accum = Accum1
    ;
        MaybeRemaining = yes(Remaining),
        add_encoded_word(Context, Accum1, !RevEncodedWords),
        encode_span(Context, Remaining, init, Accum, !RevEncodedWords)
    ).

:- pred accum_span(encoding_context::in, string::in, maybe(string)::out,
    encode_accum::in, encode_accum::out) is det.

accum_span(Context, Span, MaybeRemaining, Accum0, Accum) :-
    Accum0 = accum(NumCodeUnits0, QSingleChars0, RevStrings0),

    NumCodeUnits1 = NumCodeUnits0 + count_utf8_code_units(Span),
    QSingleChars1 = QSingleChars0 + count_qsingle_chars(Context, Span),
    ( within_length_limit(NumCodeUnits1, QSingleChars1) ->
        RevStrings1 = [Span | RevStrings0],
        Accum = accum(NumCodeUnits1, QSingleChars1, RevStrings1),
        MaybeRemaining = no
    ;
        split_span(Context, Span, 0, SplitPos, NumCodeUnits0, NumCodeUnits,
            QSingleChars0, QSingleChars),
        ( SplitPos > 0 ->
            string.unsafe_between(Span, 0, SplitPos, Head),
            string.unsafe_between(Span, SplitPos, length(Span), Tail),
            Accum = accum(NumCodeUnits, QSingleChars, [Head | RevStrings0]),
            MaybeRemaining = yes(Tail)
        ;
            Accum = Accum0,
            MaybeRemaining = yes(Span)
        )
    ).

:- pred split_span(encoding_context::in, string::in, int::in, int::out,
    int::in, int::out, int::in, int::out) is det.

split_span(Context, String, I0, I, !NumCodeUnits, !QSingleChars) :-
    (
        string.unsafe_index_next(String, I0, I1, Char),
        !:NumCodeUnits = !.NumCodeUnits + (I1 - I0),
        add_qsingle_char(Context, Char, !QSingleChars),
        within_length_limit(!.NumCodeUnits, !.QSingleChars)
    ->
        split_span(Context, String, I1, I, !NumCodeUnits, !QSingleChars)
    ;
        I = I0
    ).

:- pred add_encoded_word(encoding_context::in, encode_accum::in,
    list(string)::in, list(string)::out) is det.

add_encoded_word(Context, Accum, !RevEncodedWords) :-
    Accum = accum(NumCodeUnits, QSingleChars, RevStrings),
    ( NumCodeUnits = 0 ->
        true
    ;
        string.append_list(reverse(RevStrings), String),
        BLength = b_encoded_text_length(NumCodeUnits),
        QLength = q_encoded_text_length(NumCodeUnits, QSingleChars),
        ( BLength < QLength ->
            make_b_encoded_word(String, EncodedWord)
        ;
            make_q_encoded_word(Context, String, EncodedWord)
        ),
        cons(EncodedWord, !RevEncodedWords)
    ).

:- pred within_length_limit(int::in, int::in) is semidet.

within_length_limit(NumCodeUnits, QSingleChars) :-
    (
        BLength = b_encoded_text_length(NumCodeUnits),
        BLength =< max_encoded_text_length
    ;
        QLength = q_encoded_text_length(NumCodeUnits, QSingleChars),
        QLength =< max_encoded_text_length
    ).

:- func max_encoded_word_length = int.

max_encoded_word_length = 75.

:- func max_encoded_text_length = int.

max_encoded_text_length =
    max_encoded_word_length - string.length("=?UTF-8?Q??=").

%-----------------------------------------------------------------------------%

:- func b_encoded_text_length(int) = int.

b_encoded_text_length(NumCodeUnits) = (NumCodeUnits + 2) / 3 * 4.

:- pred make_b_encoded_word(string::in, string::out) is det.

make_b_encoded_word(String, EncodedWord) :-
    base64.encode(String, 0, length(String),
        octets_builder, octets([]), octets(RevCodeUnits)),
    list.reverse(RevCodeUnits, CodeUnits),
    ( string.from_code_unit_list(CodeUnits, Base64) ->
        EncodedWord = "=?UTF-8?B?" ++ Base64 ++ "?="
    ;
        require.unexpected($module, $pred, "string.from_code_unit_list failed")
    ).

%-----------------------------------------------------------------------------%

:- func count_qsingle_chars(encoding_context, string) = int.

count_qsingle_chars(Context, String) = QSingleChars :-
    string.foldl(add_qsingle_char(Context), String, 0, QSingleChars).

:- pred add_qsingle_char(encoding_context::in, char::in, int::in, int::out)
    is det.

add_qsingle_char(Context, Char, QSingleChars0, QSingleChars) :-
    ( single_byte_char_in_q_encoding(Context, Char, _Encoded) ->
        QSingleChars = QSingleChars0 + 1
    ;
        QSingleChars = QSingleChars0
    ).

:- func q_encoded_text_length(int, int) = int.

q_encoded_text_length(NumCodeUnits, QSingleChars) =
    QSingleChars + 3 * (NumCodeUnits - QSingleChars).

:- pred make_q_encoded_word(encoding_context::in, string::in, string::out)
    is det.

make_q_encoded_word(Context, String, EncodedWord) :-
    foldr_code_units(q_encode_octet(Context), String, [], CodeChars),
    string.from_char_list(CodeChars, CodeString),
    EncodedWord = "=?UTF-8?Q?" ++ CodeString ++ "?=".

:- pred q_encode_octet(encoding_context::in, int::in,
    list(char)::in, list(char)::out) is det.

q_encode_octet(Context, Octet, CodeChars0, CodeChars) :-
    (
        Octet =< 0x7f,
        char.from_int(Octet, Char),
        single_byte_char_in_q_encoding(Context, Char, EncodedChar)
    ->
        CodeChars = [EncodedChar | CodeChars0]
    ;
        Hi = (Octet /\ 0xf0) >> 4,
        Lo = (Octet /\ 0x0f),
        (
            char.int_to_hex_char(Hi, HiChar),
            char.int_to_hex_char(Lo, LoChar)
        ->
            CodeChars = ['=', HiChar, LoChar | CodeChars0]
        ;
            require.unexpected($module, $pred, "char.int_to_hex_char failed")
        )
    ).

:- pred single_byte_char_in_q_encoding(encoding_context::in, char::in,
    char::out) is semidet.

single_byte_char_in_q_encoding(Context, C, EC) :-
    (
        ( char.is_alnum(C)
        ; C = ('!')
        ; C = ('*')
        ; C = ('+')
        ; C = ('-')
        ; C = ('/')
        )
    ->
        EC = C
    ;
        C = (' ')
    ->
        EC = ('_')
    ;
        Context = unstructured,
        printable_ascii(C), % not whitespace
        C \= ('?'),
        C \= ('='),
        C \= ('_'),
        EC = C
    ).

:- pred printable_ascii(char::in) is semidet.

printable_ascii(C) :-
    char.to_int(C, I),
    33 =< I, I =< 126.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
