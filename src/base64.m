% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module base64.
:- interface.

:- import_module char.
:- import_module stream.

:- type byte == int.

:- pred decode(string::in, int::in, int::out, int::in,
    Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, byte, State).

:- pred encode(string::in, int::in, int::in,
    Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, char, State).

:- pred encode_wrap(string::in, int::in, int::in,
    Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, char, State).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

% Based on C code from the libb64 project, placed in the public domain.
% http://sourceforge.net/projects/libb64

%-----------------------------------------------------------------------------%

decode(Code, CodePos0, CodePos, CodeEnd, Stream, !State) :-
    cursor_offset(Code, CodePos0, Cursor0),
    cursor_offset(Code, CodeEnd, CursorEnd),
    decode_loop(Cursor0, Cursor, CursorEnd, Stream, !State),
    cursor_offset(Code, CodePos, Cursor).

:- pred decode_loop(cursor::in, cursor::out, cursor::in,
    Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, byte, State).

decode_loop(!CodePos, CodeEnd, Stream, !State) :-
    (
        next_value(!CodePos, CodeEnd, A),
        next_value(!CodePos, CodeEnd, B)
    ->
        OutA = mask_lsh(A, 0x3f, 2) \/ mask_rsh(B, 0x30, 4),
        ( next_value(!CodePos, CodeEnd, C) ->
            OutB = mask_lsh(B, 0x0f, 4) \/ mask_rsh(C, 0x3c, 2),
            ( next_value(!CodePos, CodeEnd, D) ->
                OutC = mask_lsh(C, 0x03, 6) \/ mask_rsh(D, 0x3f, 0),
                stream.put(Stream, OutA, !State),
                stream.put(Stream, OutB, !State),
                stream.put(Stream, OutC, !State),
                decode_loop(!CodePos, CodeEnd, Stream, !State)
            ;
                stream.put(Stream, OutA, !State),
                stream.put(Stream, OutB, !State)
            )
        ;
            stream.put(Stream, OutA, !State)
        )
    ;
        true
    ).

:- pred next_value(cursor::in, cursor::out, cursor::in, int::out) is semidet.
:- pragma inline(next_value/4).

next_value(!Cursor, CursorEnd, Value) :-
    next_byte(!Cursor, CursorEnd, Byte),
    ( val(Byte, ValuePrime) ->
        Value = ValuePrime
    ;
        next_value(!Cursor, CursorEnd, Value)
    ).

%-----------------------------------------------------------------------------%

encode(Plain, PlainPos0, PlainEnd, Stream, !State) :-
    cursor_offset(Plain, PlainPos0, Cursor0),
    cursor_offset(Plain, PlainEnd, CursorEnd),
    encode_loop(Cursor0, _Cursor, CursorEnd, Stream, !State, -1, _Wrap).

encode_wrap(Plain, PlainPos0, PlainEnd, Stream, !State) :-
    cursor_offset(Plain, PlainPos0, Cursor0),
    cursor_offset(Plain, PlainEnd, CursorEnd),
    encode_loop(Cursor0, _Cursor, CursorEnd, Stream, !State,
        wrap_width, _Wrap).

:- pred encode_loop(cursor::in, cursor::out, cursor::in,
    Stream::in, State::di, State::uo, int::in, int::out) is det
    <= stream.writer(Stream, char, State).

encode_loop(!Cursor, CursorEnd, Stream, !State, !Wrap) :-
    ( next_byte(!Cursor, CursorEnd, ByteA) ->
        ValueA = mask_rsh(ByteA, 0xfc, 2),
        encode_value(Stream, ValueA, !State),
        ValueB0 = mask_lsh(ByteA, 0x03, 4),
        ( next_byte(!Cursor, CursorEnd, ByteB) ->
            ValueB = ValueB0 \/ mask_rsh(ByteB, 0xf0, 4),
            encode_value(Stream, ValueB, !State),
            ValueC0 = mask_lsh(ByteB, 0x0f, 2),
            ( next_byte(!Cursor, CursorEnd, ByteC) ->
                ValueC = ValueC0 \/ mask_rsh(ByteC, 0xc0, 6),
                ValueD = mask_rsh(ByteC, 0x3f, 0),
                encode_value(Stream, ValueC, !State),
                encode_value(Stream, ValueD, !State),
                maybe_wrap(Stream, !State, !Wrap),
                encode_loop(!Cursor, CursorEnd, Stream, !State, !Wrap)
            ;
                encode_value(Stream, ValueC0, !State),
                pad(Stream, !State)
            )
        ;
            encode_value(Stream, ValueB0, !State),
            pad(Stream, !State),
            pad(Stream, !State)
        )
    ;
        true
    ).

:- pred encode_value(Stream::in, int::in, State::di, State::uo) is det
    <= stream.writer(Stream, char, State).

encode_value(Stream, Value, !State) :-
    ( val(Code, Value) ->
        char.det_from_int(Code, Char),
        stream.put(Stream, Char, !State)
    ;
        % Should never happen.
        pad(Stream, !State)
    ).

:- pred pad(Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, char, State).

pad(Stream, !State) :-
    stream.put(Stream, '=', !State).

:- pred maybe_wrap(Stream::in, State::di, State::uo, int::in, int::out) is det
    <= stream.writer(Stream, char, State).

maybe_wrap(Stream, !State, Wrap0, Wrap) :-
    ( Wrap0 < 0 ->
        Wrap = Wrap0
    ; Wrap0 - 4 = 0 ->
        stream.put(Stream, '\n', !State),
        Wrap = wrap_width
    ;
        Wrap = Wrap0 - 4
    ).

:- func wrap_width = int.

wrap_width = 76.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The code is significantly faster using cursors than base & offset,
% although the difference is overshadowed by the output.

:- type cursor.

:- pragma foreign_type("C", cursor, "const unsigned char *").

:- pred cursor_offset(string, int, cursor).
:- mode cursor_offset(in, in, out) is det.
:- mode cursor_offset(in, out, in) is det.

:- pragma foreign_proc("C",
    cursor_offset(Base::in, Offset::in, Cursor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cursor = (const unsigned char *) Base + Offset;
").

:- pragma foreign_proc("C",
    cursor_offset(Base::in, Offset::out, Cursor::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Offset = Cursor - (const unsigned char *) Base;
").

:- pred next_byte(cursor::in, cursor::out, cursor::in, byte::out) is semidet.

:- pragma foreign_proc("C",
    next_byte(Cursor0::in, Cursor::out, Limit::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Cursor0 < Limit);
    if (SUCCESS_INDICATOR) {
        Byte = *Cursor0;
        Cursor = Cursor0 + 1;
    }
").

%-----------------------------------------------------------------------------%

:- func mask_lsh(int, int, int) = int.
:- pragma inline(mask_lsh/3).

mask_lsh(X, Mask, Shift) = (X /\ Mask) `unchecked_left_shift` Shift.

:- func mask_rsh(int, int, int) = int.
:- pragma inline(mask_rsh/3).

mask_rsh(X, Mask, Shift) = (X /\ Mask) >> Shift.

%-----------------------------------------------------------------------------%

:- pred val(byte, int).
:- mode val(in, out) is semidet.
:- mode val(out, in) is semidet.

val(43, 62). % +
val(47, 63). % /
val(48, 52). % 0
val(49, 53).
val(50, 54).
val(51, 55).
val(52, 56).
val(53, 57).
val(54, 58).
val(55, 59).
val(56, 60).
val(57, 61). % 9
val(65, 0). % A
val(66, 1).
val(67, 2).
val(68, 3).
val(69, 4).
val(70, 5).
val(71, 6).
val(72, 7).
val(73, 8).
val(74, 9).
val(75, 10).
val(76, 11).
val(77, 12).
val(78, 13).
val(79, 14).
val(80, 15).
val(81, 16).
val(82, 17).
val(83, 18).
val(84, 19).
val(85, 20).
val(86, 21).
val(87, 22).
val(88, 23).
val(89, 24).
val(90, 25). % Z
val(97, 26). % a
val(98, 27).
val(99, 28).
val(100, 29).
val(101, 30).
val(102, 31).
val(103, 32).
val(104, 33).
val(105, 34).
val(106, 35).
val(107, 36).
val(108, 37).
val(109, 38).
val(110, 39).
val(111, 40).
val(112, 41).
val(113, 42).
val(114, 43).
val(115, 44).
val(116, 45).
val(117, 46).
val(118, 47).
val(119, 48).
val(120, 49).
val(121, 50).
val(122, 51). % z

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
