% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module make_utf8.
:- interface.

:- import_module list.
:- import_module maybe.

:- import_module byte_array.

:- type buffer == byte_array.

    % Make a UTF-8 string from the data in the buffers.
    % NUL will be encoded to U+FFFD.  Code units which form illegal code
    % sequences will be treated as ISO-8859-1 and encoded into UTF-8.
    %
    % Each buffer must have at least buffer_margin bytes available at
    % the tail, i.e. buf->len + buffer_margin <= buf->cap.
    %
:- pred make_utf8_string(maybe(int)::in, list(buffer)::di, string::out)
    is semidet.

:- func buffer_margin = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.

:- typeclass encode(T) where [
    pred set_byte(int::in, int::in, T::di, T::uo) is det,
    pred copy_bytes(buffer::ui, int::in, int::in, T::di, T::uo, int::in)
        is det
].

:- type preflight ---> preflight.

:- instance encode(preflight) where [
    set_byte(_, _, X, X),
    copy_bytes(_, _, _, X, X, _)
].

:- instance encode(byte_array) where [
    pred(set_byte/4) is byte_array.unsafe_set_byte,
    pred(copy_bytes/6) is byte_array.unsafe_copy_bytes
].

%-----------------------------------------------------------------------------%

make_utf8_string(ErrorLimit, Buffers0, String) :-
    preflight(Buffers0, Buffers, 0, FirstBads, ErrorLimit, 0, _Errors,
        0, TotalLength),

    allocate_for_string(TotalLength, Enc0),
    second_pass(Buffers, 0, FirstBads, 0, EncPos, Enc0, Enc1),
    unsafe_set_length(EncPos, Enc1, Enc),
    expect(unify(EncPos, TotalLength), $module, $pred, "wrong length: " ++
        from_int(EncPos) ++ " != " ++ from_int(TotalLength)),

    ( finalise_as_string(Enc, StringPrime) ->
        String = StringPrime
    ;
        unexpected($module, $pred, "finalise_as_string failed")
    ).

%-----------------------------------------------------------------------------%

:- pred preflight(list(buffer)::di, list(buffer)::uo, int::in, list(int)::out,
    maybe(int)::in, int::in, int::out, int::in, int::out) is semidet.

preflight([], [], _BufPos0, [], _ErrorLimit, !Errors, !TotalLength).
preflight([Buf0 | Bufs0], [Buf | Bufs], BufPos0, [FirstBad | FirstBads],
        ErrorLimit, !Errors, !TotalLength) :-
    clear_margin(Buf0, Buf1),
    (
        Bufs0 = [],
        Buf = Buf1
    ;
        Bufs0 = [NextBuf | _],
        unsafe_promise_unique(NextBuf, NextBufUniq),
        copy_head_into_margin(NextBufUniq, Buf1, Buf)
    ),

    MarginPos = length(Buf),
    scan(Buf, BufPos0, BufPos1, MarginPos, -1, FirstBad, 0, Errors,
        !TotalLength, preflight, _),
    expect(FirstBad >= 0, $module, $pred, "FirstBad < 0"),

    !:Errors = !.Errors + Errors,
    (
        ErrorLimit = yes(MaxError),
        !.Errors =< MaxError
    ;
        ErrorLimit = no
    ),

    unsafe_promise_unique(Bufs0, Bufs0_uniq),
    preflight(Bufs0_uniq, Bufs, BufPos1 - MarginPos, FirstBads, ErrorLimit,
        !Errors, !TotalLength).

:- pred clear_margin(buffer::di, buffer::uo) is det.

clear_margin(!ByteArray) :-
    Len = length(!.ByteArray),
    % buffer_margin = 3
    unsafe_set_byte(Len,     0x0, !ByteArray),
    unsafe_set_byte(Len + 1, 0x0, !ByteArray),
    unsafe_set_byte(Len + 2, 0x0, !ByteArray).

buffer_margin = 3.

:- pred copy_head_into_margin(buffer::ui, buffer::di, buffer::uo) is det.

copy_head_into_margin(Src, !Dest) :-
    SrcLen = length(Src),
    DestLen = length(!.Dest),
    unsafe_copy_bytes(Src, 0, min(SrcLen, buffer_margin), !Dest, DestLen).

%-----------------------------------------------------------------------------%

:- pred second_pass(list(buffer)::di, int::in, list(int)::in,
    int::in, int::out, T::di, T::uo) is det <= encode(T).

second_pass([], BufPos0, [], !EncPos, !Enc) :-
    expect(unify(BufPos0, 0), $module, $pred, "BufPos0 != 0").

second_pass([Buf | Bufs], BufPos0, [FirstBad | FirstBads], !EncPos, !Enc) :-
    % In the first pass we recorded the first invalid code unit in the buffer.
    % If this is beyond MarginPos then the entire buffer was valid.  In the
    % second pass, we can immediately copy the data from BufPos0 up to that
    % point, directly into the target string.  If the buffer really contains
    % text, most or all of the buffer will be valid so we avoid scanning the
    % buffer again.
    expect(FirstBad >= BufPos0, $module, $pred, "FirstBad < BufPos0"),
    BufPos1 = FirstBad,
    add_run(Buf, BufPos0, BufPos1, !EncPos, !Enc),

    MarginPos = length(Buf),
    scan(Buf, BufPos1, BufPos, MarginPos, FirstBad, _FirstBad, 0, _Errors,
        !EncPos, !Enc),

    NextBufPos = BufPos - MarginPos,
    second_pass(Bufs, NextBufPos, FirstBads, !EncPos, !Enc).

second_pass([_ | _], _, [], !EncPos, !Enc) :-
    unexpected($module, $pred, "list length mismatch").

second_pass([], _, [_ | _], !EncPos, !Enc) :-
    unexpected($module, $pred, "list length mismatch").

%-----------------------------------------------------------------------------%

:- pred scan(buffer::ui, int::in, int::out, int::in, int::in, int::out,
    int::in, int::out, int::in, int::out, T::di, T::uo) is det <= encode(T).

scan(Buf, BufPos0, BufPos, MarginPos, !FirstBad, !Errors, !EncPos, !Enc) :-
    ( BufPos0 >= MarginPos ->
        % End of current buffer.
        BufPos = BufPos0,
        ( !.FirstBad < 0 ->
            % Entire buffer was valid.
            !:FirstBad = BufPos
        ;
            true
        )
    ;
        unsafe_byte(Buf, BufPos0, C0),
        ( C0 = 0x00 ->
            % Replace NUL.
            % XXX and unprintables?
            on_error(BufPos0, !FirstBad, !Errors),
            add_replacement_char(!EncPos, !Enc),
            scan(Buf, BufPos0 + 1, BufPos, MarginPos, !FirstBad, !Errors,
                !EncPos, !Enc)
        ; C0 =< 0x7F ->
            % Plain ASCII.
            add_octet(C0, !EncPos, !Enc),
            scan(Buf, BufPos0 + 1, BufPos, MarginPos, !FirstBad, !Errors,
                !EncPos, !Enc)
        ;
            MaxPos = MarginPos + buffer_margin,
            extract_multibyte(Buf, BufPos0, SeqEnd, MaxPos, C0, _Char)
        ->
            add_run(Buf, BufPos0, SeqEnd, !EncPos, !Enc),
            scan(Buf, SeqEnd, BufPos, MarginPos, !FirstBad, !Errors,
                !EncPos, !Enc)
        ;
            % Otherwise invalid.
            on_error(BufPos0, !FirstBad, !Errors),
            add_latin1(C0, !EncPos, !Enc),
            scan(Buf, BufPos0 + 1, BufPos, MarginPos, !FirstBad, !Errors,
                !EncPos, !Enc)
        )
    ).

:- pred extract_multibyte(buffer::ui, int::in, int::out, int::in,
    int::in, char::out) is semidet.

extract_multibyte(Buf, BufPos0, BufPos, MaxPos, LeadByte, Char) :-
    (
        LeadByte =< 0xDF
    ->
        LeadByte > 0xC1,
        % 2-byte sequence.
        SeqLen = 2,
        C1 = LeadByte /\ 0x1F,
        MinCodePoint = 0x80
    ;
        LeadByte =< 0xEF
    ->
        % 3-byte sequence.
        SeqLen = 3,
        C1 = LeadByte /\ 0x0F,
        MinCodePoint = 0x800
    ;
        LeadByte =< 0xF4
    ->
        % 4-byte sequence.
        SeqLen = 4,
        C1 = LeadByte /\ 0x07,
        MinCodePoint = 0x10000
    ;
        fail
    ),
    BufPos = BufPos0 + SeqLen,
    ( BufPos =< MaxPos ->
        (
            SeqLen = 2,
            extract_trail_byte(Buf, BufPos0 + 1, C1, CodePoint)
        ;
            SeqLen = 3,
            extract_trail_byte(Buf, BufPos0 + 1, C1, C2),
            extract_trail_byte(Buf, BufPos0 + 2, C2, CodePoint)
        ;
            SeqLen = 4,
            extract_trail_byte(Buf, BufPos0 + 1, C1, C2),
            extract_trail_byte(Buf, BufPos0 + 2, C2, C3),
            extract_trail_byte(Buf, BufPos0 + 3, C3, CodePoint)
        )
    ;
        fail
    ),
    MinCodePoint =< CodePoint,
    char.from_int(CodePoint, Char),
    not char.is_surrogate(Char),
    not char.is_noncharacter(Char).

:- pred extract_trail_byte(buffer::ui, int::in, int::in, int::out) is semidet.

extract_trail_byte(Buf, I, Acc0, Acc) :-
    unsafe_byte(Buf, I, TrailByte),
    TrailByte /\ 0xC0 = 0x80,
    Acc = (Acc0 `unchecked_left_shift` 6) \/ (TrailByte /\ 0x3F).

:- pred on_error(int::in, int::in, int::out, int::in, int::out) is det.

on_error(Pos, Bad0, Bad, Errors, Errors + 1) :-
    ( Bad0 < 0 ->
        Bad = Pos
    ;
        Bad = Bad0
    ).

:- pred add_octet(int::in, int::in, int::out, T::di, T::uo)
    is det <= encode(T).

add_octet(Octet, EncPos, EncPos + 1, !Enc) :-
    set_byte(EncPos, Octet, !Enc).

    % Assumes: 0x80 =< Int =< 0xff
    % (Technically the range includes C1 controls.)
    %
:- pred add_latin1(int::in, int::in, int::out, T::di, T::uo) is det
    <= encode(T).

add_latin1(Int, !EncPos, !Enc) :-
    A = 0xc0 \/ ((Int >> 6) /\ 0x1f),
    B = 0x80 \/  (Int       /\ 0x3f),
    add_octet(A, !EncPos, !Enc),
    add_octet(B, !EncPos, !Enc).

:- pred add_replacement_char(int::in, int::out, T::di, T::uo) is det
    <= encode(T).

add_replacement_char(!EncPos, !String) :-
    % UTF-8 encoding of U+FFFD
    add_octet(0xef, !EncPos, !String),
    add_octet(0xbf, !EncPos, !String),
    add_octet(0xbd, !EncPos, !String).

:- pred add_run(buffer::ui, int::in, int::in, int::in, int::out, T::di, T::uo)
    is det <= encode(T).

add_run(Buf, BufPos0, BufPos, EncPos, EncPos + RunLen, !Enc) :-
    copy_bytes(Buf, BufPos0, BufPos, !Enc, EncPos),
    RunLen = BufPos - BufPos0.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
