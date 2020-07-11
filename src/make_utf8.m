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
:- import_module string.
:- use_module require.

:- typeclass encode(T) where [
    pred set_byte(int::in, int::in, T::di, T::uo) is det,
    pred set_length(int::in, T::di, T::uo) is det,
    pred copy_bytes(buffer::ui, int::in, int::in, T::di, T::uo, int::in)
        is det
].

:- type preflight ---> preflight.

:- instance encode(preflight) where [
    set_byte(_, _, X, X),
    set_length(_, X, X),
    copy_bytes(_, _, _, X, X, _)
].

:- instance encode(byte_array) where [
    pred(set_byte/4) is byte_array.set_byte,
    pred(set_length/3) is byte_array.set_length,
    pred(copy_bytes/6) is byte_array.copy_bytes
].

%-----------------------------------------------------------------------------%

make_utf8_string(ErrorLimit, Buffers0, String) :-
    preflight(Buffers0, BuffersWithMargins, 0, FirstBads, ErrorLimit,
        0, _Errors, 0, TotalLength),
    % Add 1 for NUL terminator.
    allocate(TotalLength + 1, Enc0),
    second_pass(BuffersWithMargins, 0, FirstBads, 0, EncPos, Enc0, Enc),
    require.expect(unify(EncPos, TotalLength), $module, $pred,
        "wrong length: " ++ from_int(EncPos) ++ " != " ++ from_int(TotalLength)),
    make_string(Enc, String).

%-----------------------------------------------------------------------------%

:- pred preflight(list(buffer)::di, list(buffer)::uo, int::in, list(int)::out,
    maybe(int)::in, int::in, int::out, int::in, int::out) is semidet.

preflight([], [], _BufPos0, [], _ErrorLimit, !Errors, !TotalLength).
preflight([Buf0 | Bufs0], [Buf | Bufs], BufPos0, [FirstBad | FirstBads],
        ErrorLimit, !Errors, !TotalLength) :-
    % MarginPos is the length of Buf before adding the margin.
    MarginPos = length(Buf0),
    (
        Bufs0 = [],
        make_empty_margin(Buf0, Buf)
    ;
        Bufs0 = [NextBuf | _],
        unsafe_promise_unique(NextBuf, NextBufUniq),
        make_filled_margin(NextBufUniq, Buf0, Buf)
    ),

    scan(Buf, BufPos0, BufPos1, MarginPos, -1, FirstBad, 0, Errors,
        !TotalLength, preflight, _),
    require.expect(FirstBad >= 0, $module, $pred, "FirstBad < 0"),

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

buffer_margin = 3.

:- pred make_empty_margin(buffer::di, buffer::uo) is det.

make_empty_margin(!ByteArray) :-
    Len = length(!.ByteArray),
    byte_array.set_length(Len + buffer_margin, !ByteArray).

:- pred make_filled_margin(buffer::ui, buffer::di, buffer::uo) is det.

make_filled_margin(Src, !Dest) :-
    % Extend Dest with margin space and then fill it with head of Src.
    DestPos = length(!.Dest),
    make_empty_margin(!Dest),
    byte_array.copy_bytes(Src, 0, min(length(Src), buffer_margin),
        !Dest, DestPos).

%-----------------------------------------------------------------------------%

:- pred second_pass(list(buffer)::di, int::in, list(int)::in,
    int::in, int::out, T::di, T::uo) is det <= encode(T).

second_pass([], BufPos0, [], !EncPos, !Enc) :-
    require.expect(unify(BufPos0, 0), $module, $pred, "BufPos0 != 0").

second_pass([BufWithMargin | BufsWithMargins], BufPos0, [FirstBad | FirstBads],
        !EncPos, !Enc) :-
    % In the first pass we recorded the first invalid code unit in the buffer.
    % If this is beyond MarginPos then the entire buffer was valid.  In the
    % second pass, we can immediately copy the data from BufPos0 up to that
    % point, directly into the target string.  If the buffer really contains
    % text, most or all of the buffer will be valid so we avoid scanning the
    % buffer again.
    require.expect(FirstBad >= BufPos0, $module, $pred, "FirstBad < BufPos0"),
    BufPos1 = FirstBad,
    add_run(BufWithMargin, BufPos0, BufPos1, !EncPos, !Enc),

    % Every buffer was extended with a margin in preflight.
    MarginPos = length(BufWithMargin) - buffer_margin,
    scan(BufWithMargin, BufPos1, BufPos, MarginPos, FirstBad, _FirstBad,
        0, _Errors, !EncPos, !Enc),

    NextBufPos = BufPos - MarginPos,
    second_pass(BufsWithMargins, NextBufPos, FirstBads, !EncPos, !Enc).

second_pass([_ | _], _, [], !EncPos, !Enc) :-
    require.unexpected($module, $pred, "list length mismatch").

second_pass([], _, [_ | _], !EncPos, !Enc) :-
    require.unexpected($module, $pred, "list length mismatch").

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
        /* unsafe */ get_byte(Buf, BufPos0, C0),
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
    /* unsafe */ get_byte(Buf, I, TrailByte),
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

add_octet(Octet, EncPos0, EncPos, !Enc) :-
    EncPos = EncPos0 + 1,
    set_length(EncPos, !Enc),
    set_byte(EncPos0, Octet, !Enc).

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

add_run(Buf, BufPos0, BufPos, EncPos0, EncPos, !Enc) :-
    RunLen = BufPos - BufPos0,
    EncPos = EncPos0 + RunLen,
    set_length(EncPos, !Enc),
    copy_bytes(Buf, BufPos0, BufPos, !Enc, EncPos0).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
