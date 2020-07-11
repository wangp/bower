% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.key_array.
:- interface.

:- type gpgme_key_array.

:- pred with_key_array(pred(gpgme_key_array, T, io, io), list(key), T, io, io).
:- mode with_key_array(pred(in, out, di, uo) is det, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- use_module exception.
:- use_module require.

:- pragma foreign_type("C", gpgme_key_array, "gpgme_key_t *").

%-----------------------------------------------------------------------------%

with_key_array(Pred, Keys, Out, !IO) :-
    promise_pure
    (
        make_key_array(Keys, KeyArray, !IO),
        promise_equivalent_solutions [Out, !:IO]
        (
            exception.try_io(
                (pred(R::out, IO0::di, IO::uo) is det :-
                    Pred(KeyArray, R, IO0, IO)),
                TryResult, !IO),
            (
                TryResult = exception.succeeded(Out),
                free_key_array(KeyArray, !IO)
            ;
                TryResult = exception.exception(Excp),
                free_key_array(KeyArray, !IO),
                exception.throw(Excp)
            )
        )
    ).

:- pred make_key_array(list(key)::in, gpgme_key_array::uo, io::di, io::uo)
    is det.

make_key_array(Keys, KeyArray, !IO) :-
    allocate_key_array(length(Keys), KeyArray0),
    fill_key_array(0, Keys, KeyArray0, KeyArray, !IO).

:- pred allocate_key_array(int::in, gpgme_key_array::uo) is det.

:- pragma foreign_proc("C",
    allocate_key_array(Size::in, KeyArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* NULL terminated */
    KeyArray = calloc(Size + 1, sizeof(gpgme_key_t));
").

:- pred fill_key_array(int::in, list(key)::in,
    gpgme_key_array::di, gpgme_key_array::uo, io::di, io::uo) is det.

fill_key_array(_Index, [], !KeyArray, !IO).
fill_key_array(Index, [Key | Keys], !KeyArray, !IO) :-
    Key = key(_KeyInfo, Mutvar),
    get_mutvar(Mutvar, MaybeKey, !IO),
    (
        MaybeKey = yes(GpgmeKey),
        set_key_array(Index, GpgmeKey, !KeyArray),
        fill_key_array(Index + 1, Keys, !KeyArray, !IO)
    ;
        MaybeKey = no,
        require.unexpected($module, $pred, "key already unref'd")
    ).

:- pred set_key_array(int::in, gpgme_key::in,
    gpgme_key_array::di, gpgme_key_array::uo) is det.

:- pragma foreign_proc("C",
    set_key_array(Index::in, Key::in, KeyArray0::di, KeyArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    KeyArray = KeyArray0;
    KeyArray[Index] = Key;
").

:- pred free_key_array(gpgme_key_array::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    free_key_array(KeyArray::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    free(KeyArray);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
