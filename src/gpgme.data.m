% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.data.
:- interface.

:- import_module char.
:- import_module stream.

:- pred gpgme_data_new(maybe_error(data)::out, io::di, io::uo) is det.

:- pred gpgme_data_new_from_string(string::in, maybe_error(data)::out,
    io::di, io::uo) is det.

:- pred gpgme_data_release(data::in, io::di, io::uo) is det.

:- pred gpgme_data_write_char(data::in, char::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred gpgme_data_write_string(data::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred gpgme_data_rewind(data::in, maybe_error::out, io::di, io::uo) is det.

:- pred gpgme_data_to_string(data::in, maybe_error(string)::out,
    io::di, io::uo) is det.

:- instance stream.stream(data, io).
:- instance stream.output(data, io).
:- instance stream.writer(data, char, io).
:- instance stream.writer(data, string, io).

    % Write to a data object where \n is written as CR/LF.
    %
:- type data_crlf
    --->    data_crlf(data).

:- instance stream.stream(data_crlf, io).
:- instance stream.output(data_crlf, io).
:- instance stream.writer(data_crlf, char, io).
:- instance stream.writer(data_crlf, string, io).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module string.

:- import_module string_util.

%-----------------------------------------------------------------------------%

gpgme_data_new(Res, !IO) :-
    gpgme_data_new_2(Ok, Data, Error, !IO),
    (
        Ok = yes,
        Res = ok(data(Data, ""))
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_new_2(bool::out, gpgme_data::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_new_2(Ok::out, Data::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_data_new(&Data);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

gpgme_data_new_from_string(String, Res, !IO) :-
    % XXX could clobber String?
    gpgme_data_new_from_string_2(String, Ok, Data, Error, !IO),
    (
        Ok = yes,
        Res = ok(data(Data, String))
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_new_from_string_2(string::in, bool::out, gpgme_data::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_new_from_string_2(String::in, Ok::out, Data::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_data_new_from_mem(&Data, String, strlen(String),
        0 /* no copy */);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%

gpgme_data_release(data(Data, _), !IO) :-
    gpgme_data_release_2(Data, !IO).

:- pred gpgme_data_release_2(gpgme_data::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_release_2(Data::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_data_release(Data);
").

%-----------------------------------------------------------------------------%

gpgme_data_write_char(data(Data, _), Char, Res, !IO) :-
    gpgme_data_write_char_2(Data, Char, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_write_char_2(gpgme_data::in, char::in, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_write_char_2(Data::in, Char::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    unsigned char cs[1] = {Char};

    for (;;) {
        ssize_t written = gpgme_data_write(Data, cs, sizeof(cs));
        if (written == -1) {
            Ok = MR_NO;
            Error = MR_make_string(MR_ALLOC_ID,
                ""gpgme_data_write failed: errno=%d"", errno);
            break;
        }
        if (written > 0) {
            Ok = MR_YES;
            Error = MR_make_string_const("""");
            break;
        }
    }
").



%-----------------------------------------------------------------------------%

gpgme_data_write_string(Data, String, Res, !IO) :-
    unsafe_write_substring(Data, String, 0, length(String), Res, !IO).

:- pred unsafe_write_substring(data::in, string::in, int::in, int::in,
    maybe_error::out, io::di, io::uo) is det.

unsafe_write_substring(data(Data, _), String, Start, End, Res, !IO) :-
    unsafe_write_substring_2(Data, String, Start, End, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred unsafe_write_substring_2(gpgme_data::in, string::in, int::in, int::in,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unsafe_write_substring_2(Data::in, String::in, Start::in, End::in,
        Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    off_t offset;
    ssize_t remaining;

    Ok = MR_YES;
    Error = MR_make_string_const("""");

    offset = Start;
    remaining = End - Start;

    while (remaining > 0) {
        ssize_t written = gpgme_data_write(Data, String + offset, remaining);
        if (written == -1) {
            Ok = MR_NO;
            Error = MR_make_string(MR_ALLOC_ID,
                ""gpgme_data_write failed: errno=%d"", errno);
            break;
        } else {
            remaining -= written;
            offset += written;
        }
    }
").

%-----------------------------------------------------------------------------%

gpgme_data_rewind(data(Data, _), Res, !IO) :-
    gpgme_data_rewind_2(Data, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_rewind_2(gpgme_data::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_rewind_2(Data::in, Ok::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    off_t start;

    start = gpgme_data_seek(Data, 0, SEEK_SET);
    if (start == -1) {
        Ok = MR_NO;
        Error = MR_make_string_const(""gpgme_data_seek failed"");
    } else {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

gpgme_data_to_string(data(Data, _), Res, !IO) :-
    gpgme_data_to_string_2(Data, Ok, String, Error, !IO),
    (
        Ok = yes,
        Res = ok(String)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_to_string_2(gpgme_data::in, bool::out, string::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_to_string_2(Data::in, Ok::out, String::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    off_t end;
    off_t start;
    ssize_t len;

    end = gpgme_data_seek(Data, 0, SEEK_END);
    if (end == -1) {
        Ok = MR_NO;
        Error = MR_make_string_const(""gpgme_data_seek failed"");
        String = MR_make_string_const("""");
    } else {
        start = gpgme_data_seek(Data, 0, SEEK_SET);
        if (start == -1) {
            Ok = MR_NO;
            Error = MR_make_string_const(""gpgme_data_seek failed"");
            String = MR_make_string_const("""");
        } else {
            MR_allocate_aligned_string_msg(String, end - start, MR_ALLOC_ID);
            len = gpgme_data_read(Data, String, end - start);
            if (len == end - start) {
                String[len] = '\\0';
                Ok = MR_YES;
                Error = MR_make_string_const("""");
            } else {
                Ok = MR_NO;
                Error = MR_make_string_const(""gpgme_data_read failed"");
            }
        }
    }
").

%-----------------------------------------------------------------------------%

:- instance stream(data, io) where [
    name(_, "<<gpgme.data>>", !IO)
].

:- instance stream.output(data, io) where [
    flush(_, !IO)
].

:- instance stream.writer(data, char, io) where [
    ( put(Data, Char, !IO) :-
        write_char_or_throw(Data, Char, !IO)
    )
].

:- instance stream.writer(data, string, io) where [
    ( put(Data, String, !IO) :-
        write_substring_or_throw(Data, String, 0, length(String), !IO)
    )
].

:- pred write_char_or_throw(data::in, char::in, io::di, io::uo) is det.

write_char_or_throw(Data, Char, !IO) :-
    gpgme_data_write_char(Data, Char, Res, !IO),
    (
        Res = ok
    ;
        Res = error(Error),
        throw(io.make_io_error(Error))
    ).

:- pred write_substring_or_throw(data::in, string::in, int::in, int::in,
    io::di, io::uo) is det.

write_substring_or_throw(Data, String, Start, End, !IO) :-
    unsafe_write_substring(Data, String, Start, End, Res, !IO),
    (
        Res = ok
    ;
        Res = error(Error),
        throw(io.make_io_error(Error))
    ).

%-----------------------------------------------------------------------------%

:- instance stream(data_crlf, io) where [
    name(_, "<<gpgme.data_crlf>>", !IO)
].

:- instance stream.output(data_crlf, io) where [
    flush(_, !IO)
].

:- instance stream.writer(data_crlf, char, io) where [
    ( put(data_crlf(Data), Char, !IO) :-
        ( Char = ('\n') ->
            write_char_or_throw(Data, ('\r'), !IO)
        ;
            true
        ),
        write_char_or_throw(Data, Char, !IO)
    )
].

:- instance stream.writer(data_crlf, string, io) where [
    ( put(Data, String, !IO) :-
        put_string_crlf_loop(Data, String, 0, length(String), !IO)
    )
].

:- pred put_string_crlf_loop(data_crlf::in, string::in, int::in, int::in,
    io::di, io::uo) is det.

put_string_crlf_loop(data_crlf(Data), String, Start, End, !IO) :-
    ( Start >= End ->
        true
    ;
        ( unsafe_strstr(String, "\n", Start, Nl) ->
            write_substring_or_throw(Data, String, Start, Nl, !IO),
            write_char_or_throw(Data, ('\r'), !IO),
            write_char_or_throw(Data, ('\n'), !IO),
            put_string_crlf_loop(data_crlf(Data), String, Nl + 1, End, !IO)
        ;
            write_substring_or_throw(Data, String, Start, End, !IO)
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
