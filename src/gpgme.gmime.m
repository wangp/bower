% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.gmime.
:- interface.

:- import_module gmime.

:- pred gpgme_data_new_from_gmime_stream(g_mime_stream(T)::in,
    maybe_error(data)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_import_module("C", gmime).

:- pragma foreign_decl("C", local, "
static ssize_t
read_callback(void *handle, void *buffer, size_t size)
{
    GMimeStream *stream = handle;
    ssize_t r;

    r = g_mime_stream_read(stream, buffer, size);
    if (r == -1) {
        errno = EIO;
    }
    return r;
}

static ssize_t
write_callback(void *handle, const void *buffer, size_t size)
{
    GMimeStream *stream = handle;
    ssize_t r;

    r = g_mime_stream_write(stream, buffer, size);
    if (r == -1) {
        errno = EIO;
    }
    return r;
}

static off_t
seek_callback(void *handle, off_t offset, int whence)
{
    GMimeStream *stream = handle;
    gint64 r;

    r = g_mime_stream_seek(stream, offset, whence);
    if (r == -1) {
        errno = EIO;
    }
    return r;
}

static void
release_callback(void *handle)
{
    (void) handle;
}

static struct gpgme_data_cbs callbacks =
{
    read_callback,
    write_callback,
    seek_callback,
    release_callback
};
").

%-----------------------------------------------------------------------------%

gpgme_data_new_from_gmime_stream(Stream, Res, !IO) :-
    gpgme_data_new_from_gmime_stream_2(Stream, Ok, Data, Error, !IO),
    (
        Ok = yes,
        Res = ok(data(Data, ""))
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_data_new_from_gmime_stream_2(g_mime_stream(T)::in,
    bool::out, gpgme_data::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_data_new_from_gmime_stream_2(Stream::in, Ok::out, Data::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_data_new_from_cbs(&Data, &callbacks, Stream);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
