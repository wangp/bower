% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module byte_array.
:- interface.

:- type byte_array.

:- type index == int.

:- type byte == int.

    % allocate(Capacity, ByteArray)
    %
:- pred allocate(int::in, byte_array::uo) is det.

    % allocate_for_string(MaxCodeUnits, ByteArray)
    % Allocate a byte array with enough space to hold MaxCodeUnits
    % followed by a NUL terminator.
    %
:- pred allocate_for_string(int::in, byte_array::uo) is det.

:- func length(byte_array::ui) = (int::out) is det.

:- pred unsafe_set_length(int::in, byte_array::di, byte_array::uo) is det.

:- pred unsafe_byte(byte_array::ui, index::in, byte::out) is det.

:- pred unsafe_set_byte(index::in, byte::in, byte_array::di, byte_array::uo)
    is det.

:- pred unsafe_copy_bytes(byte_array::ui, int::in, int::in,
    byte_array::di, byte_array::uo, int::in) is det.

    % Convert a byte array to a string.  Fail if there is not enough space
    % remaining in the byte array for the NUL terminator.
    %
:- pred finalise_as_string(byte_array::di, string::uo) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_type("C", byte_array, "struct byte_array *").

:- pragma foreign_decl("C", "
struct byte_array {
    unsigned int cap;   /* MR_Integer is overkill */
    unsigned int len;   /* MR_Integer is overkill */
    unsigned char *data;
};
").

:- pragma foreign_proc("C",
    allocate(Cap::in, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_String data;

    /*
    ** MR_allocate_aligned_string_msg allocates at least one more byte
    ** for the NUL terminator so subtract it.
    */
    MR_allocate_aligned_string_msg(data, Cap - 1, MR_ALLOC_ID);
    ByteArray = MR_GC_NEW_ATTRIB(struct byte_array, MR_ALLOC_ID);
    ByteArray->len = 0;
    ByteArray->cap = Cap;
    ByteArray->data = (unsigned char *) data;
").

allocate_for_string(MaxCodeUnits, ByteArray) :-
    allocate(MaxCodeUnits + 1, ByteArray).

:- pragma foreign_proc("C",
    length(ByteArray::ui) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = ByteArray->len;
").

:- pragma foreign_proc("C",
    unsafe_set_length(Length::in, ByteArray0::di, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ByteArray = ByteArray0;
    ByteArray0->len = Length;
").

:- pragma foreign_proc("C",
    unsafe_byte(ByteArray::ui, Index::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Byte = ByteArray->data[Index];
").

:- pragma foreign_proc("C",
    unsafe_set_byte(Index::in, Byte::in, ByteArray0::di, ByteArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ByteArray = ByteArray0;
    ByteArray->data[Index] = Byte;
").

:- pragma foreign_proc("C",
    unsafe_copy_bytes(Src::ui, SrcBegin::in, SrcEnd::in,
        Dest0::di, Dest::uo, DestOffset::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Dest = Dest0;
    memcpy(Dest->data + DestOffset, Src->data + SrcBegin, SrcEnd - SrcBegin);
").

:- pragma foreign_proc("C",
    finalise_as_string(ByteArray::di, String::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (ByteArray->len < ByteArray->cap) {
        String = (MR_String) ByteArray->data;
        String[ByteArray->len] = '\\0';
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        String = MR_make_string_const("""");
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
