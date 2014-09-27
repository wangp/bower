% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.data.
:- interface.

:- import_module stream.

:- pred gpgme_data_new(maybe_error(data)::out, io::di, io::uo) is det.

:- pred gpgme_data_new_from_string(string::in, maybe_error(data)::out,
    io::di, io::uo) is det.

:- pred gpgme_data_release(data::in, io::di, io::uo) is det.

:- pred gpgme_data_write_string(data::in, string::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred gpgme_data_rewind(data::in, maybe_error::out, io::di, io::uo) is det.

:- pred gpgme_data_to_string(data::in, maybe_error(string)::out,
    io::di, io::uo) is det.

:- instance stream.stream(data, io).
:- instance stream.output(data, io).
:- instance stream.writer(data, string, io).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

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
        Error = _gpgme_error_to_string(err);
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
        Error = _gpgme_error_to_string(err);
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

gpgme_data_write_string(data(Data, _), String, Res, !IO) :-
    gpgme_data_write_string_2(Data, String, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_write_string_2(gpgme_data::in, string::in, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_write_string_2(Data::in, String::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    off_t offset;
    ssize_t remaining;

    Ok = MR_YES;
    Error = MR_make_string_const("""");

    offset = 0;
    remaining = strlen(String);

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

:- instance stream.writer(data, string, io) where [
    put(Data, String, !IO) :-
    (
        gpgme_data_write_string(Data, String, Res, !IO),
        (
            Res = ok
        ;
            Res = error(Error),
            throw(io.make_io_error(Error))
        )
    )
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
