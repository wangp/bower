% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module gpgme.
:- interface.

:- import_module io.
:- import_module maybe.

:- include_module gpgme.gmime.

%-----------------------------------------------------------------------------%

:- pred gpgme_init(io::di, io::uo) is det.

% Protocols and Engines

:- type protocol
    --->    openpgp.

:- pred gpgme_engine_check_version(protocol::in, maybe_error::out,
    io::di, io::uo) is det.

% Contexts

:- type ctx.

:- pred gpgme_new(maybe_error(ctx)::out, io::di, io::uo) is det.

:- pred gpgme_release(ctx::in, io::di, io::uo) is det.

:- pred gpgme_set_protocol(ctx::in, protocol::in, maybe_error::out,
    io::di, io::uo) is det.

% Data buffers

:- type data.

:- pred gpgme_data_new(maybe_error(data)::out, io::di, io::uo) is det.

:- pred gpgme_data_new_from_string(string::in, maybe_error(data)::out,
    io::di, io::uo) is det.

:- pred gpgme_data_release(data::in, io::di, io::uo) is det.

% Crypto Operations

:- include_module gpgme.decrypt.
:- include_module gpgme.verify.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- pragma foreign_decl("C", "
    #include <gpgme.h>
").

:- pragma foreign_decl("C", local, "
    #include <locale.h>
").

:- pragma foreign_enum("C", protocol/0, [
    openpgp - "GPGME_PROTOCOL_OpenPGP"
]).

:- pragma foreign_type("C", ctx, "gpgme_ctx_t").

:- type data
    --->    data(
                real_data   :: gpgme_data,
                retain      :: string % prevent GC
            ).

:- type gpgme_data.

:- pragma foreign_type("C", gpgme_data, "gpgme_data_t").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
MR_String
_gpgme_error_to_string(gpgme_error_t err);
").

:- pragma foreign_code("C", "
MR_String
_gpgme_error_to_string(gpgme_error_t err)
{
    char buf[128];

    gpgme_strerror_r(err, buf, sizeof(buf));
    return MR_make_string(MR_ALLOC_ID, ""%s"", buf);
}
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gpgme_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_check_version(NULL);
    gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_CTYPE, NULL));
#ifdef LC_MESSAGES
    gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_MESSAGES, NULL));
#endif
").

%-----------------------------------------------------------------------------%

gpgme_engine_check_version(Proto, Res, !IO) :-
    gpgme_engine_check_version_2(Proto, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_engine_check_version_2(protocol::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_engine_check_version_2(Proto::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_engine_check_version(Proto);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%

gpgme_new(Res, !IO) :-
    gpgme_new_2(Ok, Ctx, Error, !IO),
    (
        Ok = yes,
        Res = ok(Ctx)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_new_2(bool::out, ctx::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    gpgme_new_2(Ok::out, Ctx::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_new(&Ctx);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gpgme_release(Ctx::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_release(Ctx);
").

%-----------------------------------------------------------------------------%

gpgme_set_protocol(Ctx, Proto, Res, !IO) :-
    gpgme_set_protocol_2(Ctx, Proto, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred gpgme_set_protocol_2(ctx::in, protocol::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gpgme_set_protocol_2(Ctx::in, Proto::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    gpgme_error_t err;

    err = gpgme_set_protocol(Ctx, Proto);
    if (err == GPG_ERR_NO_ERROR) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = _gpgme_error_to_string(err);
    }
").

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
% vim: ft=mercury ts=4 sts=4 sw=4 et
