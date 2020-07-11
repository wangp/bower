% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module byte_array.
:- interface.

:- type byte_array.

:- type index == int.

:- type byte == int.

:- type bounds_error
    --->    bounds_error(string, string).

    % allocate(Capacity, ByteArray)
    %
:- pred allocate(int::in, byte_array::uo) is det.

    % Return length of ByteArray.
    %
:- func length(byte_array::ui) = (int::out) is det.

    % Set length of ByteArray.
    % New space is filled with zeros.
    %
:- pred set_length(int::in, byte_array::di, byte_array::uo) is det.

    % Return byte at the given index.
    % Throws an exception if Index is not in [0, Length].
    %
:- pred get_byte(byte_array::ui, index::in, byte::out) is det.
:- pred unsafe_get_byte(byte_array::ui, index::in, byte::out) is det.

    % Set byte at the given index.
    % Throws an exception if Index is not in [0, Length].
    %
:- pred set_byte(index::in, byte::in, byte_array::di, byte_array::uo) is det.
:- pred unsafe_set_byte(index::in, byte::in, byte_array::di, byte_array::uo)
    is det.

    % copy_bytes(Src, SrcBegin, SrcEnd, Dest, DestBegin)
    %
:- pred copy_bytes(byte_array::ui, int::in, int::in,
    byte_array::di, byte_array::uo, int::in) is det.
:- pred unsafe_copy_bytes(byte_array::ui, int::in, int::in,
    byte_array::di, byte_array::uo, int::in) is det.

    % Convert a byte array to a string.
    %
:- pred make_string(byte_array::di, string::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.
:- use_module exception.

:- pragma foreign_type("C", byte_array, "struct byte_array *").

:- pragma foreign_decl("C", "
struct byte_array {
    unsigned int cap;   /* MR_Integer is overkill */
    unsigned int len;   /* MR_Integer is overkill */
    unsigned char *data;
};

unsigned int
byte_array_pot(unsigned int x);

void *
byte_array_gc_realloc_atomic(void *oldptr, unsigned int size);
").

:- pragma foreign_code("C", "
unsigned int
byte_array_pot(unsigned int x)
{
    ssize_t y = 1;
    while (y < x && y > 0) {
        y *= 2;
    }
    assert(y >= x);
    return y;
}

void *
byte_array_gc_realloc_atomic(void *oldptr, unsigned int size)
{
    if (oldptr == NULL) {
        MR_Word ptr;
        MR_incr_hp_atomic(ptr, MR_bytes_to_words(size));
        return (void *) ptr;
    } else {
        return MR_GC_realloc(oldptr, size);
    }
}
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    allocate(Cap::in, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ByteArray = MR_GC_NEW_ATTRIB(struct byte_array, MR_ALLOC_ID);
    ByteArray->len = 0;
    ByteArray->cap = Cap;
    ByteArray->data = byte_array_gc_realloc_atomic(NULL, Cap);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    length(ByteArray::ui) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = ByteArray->len;
").

%-----------------------------------------------------------------------------%

set_length(Length, !ByteArray) :-
    ( Length >= 0 ->
        do_set_length(Length, !ByteArray)
    ;
        exception.throw(bounds_error($pred, "Length: " ++ from_int(Length)))
    ).

:- pred do_set_length(int::in, byte_array::di, byte_array::uo) is det.

:- pragma foreign_proc("C",
    do_set_length(Length::in, ByteArray0::di, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const unsigned int oldlen = ByteArray0->len;

    ByteArray = ByteArray0;

    if (Length > ByteArray->cap) {
        unsigned int newcap = byte_array_pot(Length);
        ByteArray->data = byte_array_gc_realloc_atomic(ByteArray->data, newcap);
        ByteArray->cap = newcap;
    }

    if (Length > oldlen) {
        memset(ByteArray->data + oldlen, 0, Length - oldlen);
    }

    ByteArray->len = Length;
").

%-----------------------------------------------------------------------------%

get_byte(ByteArray, Index, Byte) :-
    Length = length(ByteArray),
    (
        Index >= 0,
        Index < Length
    ->
        unsafe_get_byte(ByteArray, Index, Byte)
    ;
        exception.throw(bounds_error($pred,
            "Index: " ++ from_int(Index) ++ ", Length: " ++ from_int(Length)))
    ).

:- pragma foreign_proc("C",
    unsafe_get_byte(ByteArray::ui, Index::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Byte = ByteArray->data[Index];
").

%-----------------------------------------------------------------------------%

set_byte(Index, Byte, !ByteArray) :-
    Length = length(!.ByteArray),
    (
        Index >= 0,
        Index < Length
    ->
        unsafe_set_byte(Index, Byte, !ByteArray)
    ;
        exception.throw(bounds_error($pred,
            "Index: " ++ from_int(Index) ++ ", Length: " ++ from_int(Length)))
    ).

:- pragma foreign_proc("C",
    unsafe_set_byte(Index::in, Byte::in, ByteArray0::di, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ByteArray = ByteArray0;
    ByteArray->data[Index] = Byte;
").

%-----------------------------------------------------------------------------%

copy_bytes(Src, SrcBegin, SrcEnd, Dest0, Dest, DestBegin) :-
    SrcLength = length(Src),
    DestLength = length(Dest0),
    (
        0 =< SrcBegin, SrcBegin =< SrcEnd, SrcEnd =< SrcLength,
        DestEnd = DestBegin + (SrcEnd - SrcBegin),
        0 =< DestBegin, DestEnd =< DestLength
    ->
        unsafe_copy_bytes(Src, SrcBegin, SrcEnd, Dest0, Dest, DestBegin)
    ;
        exception.throw(bounds_error($pred,
            "SrcBegin: " ++ from_int(SrcBegin) ++
            ", SrcEnd: " ++ from_int(SrcEnd) ++
            ", SrcLength: " ++ from_int(SrcLength) ++
            ", DestBegin: " ++ from_int(DestBegin) ++
            ", DestLength: " ++ from_int(DestLength)))
    ).

:- pragma foreign_proc("C",
    unsafe_copy_bytes(Src::ui, SrcBegin::in, SrcEnd::in,
        Dest0::di, Dest::uo, DestBegin::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Dest = Dest0;
    memcpy(Dest->data + DestBegin, Src->data + SrcBegin, SrcEnd - SrcBegin);
").

%-----------------------------------------------------------------------------%

make_string(!.ByteArray, String) :-
    % Add NUL terminator.
    Length = length(!.ByteArray),
    set_length(Length + 1, !ByteArray),
    make_string_2(!.ByteArray, String).

:- pred make_string_2(byte_array::di, string::uo) is det.

:- pragma foreign_proc("C",
    make_string_2(ByteArray::di, String::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    String = (MR_String) ByteArray->data;
    assert(ByteArray->len > 0);
    assert(String[ByteArray->len - 1] == '\\0');
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
