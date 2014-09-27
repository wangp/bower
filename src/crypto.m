% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module crypto.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module gpgme.

%-----------------------------------------------------------------------------%

:- type crypto == gpgme.ctx.

:- pred init_crypto(maybe_error(crypto)::out, io::di, io::uo) is det.

:- pred shutdown_crypto(crypto::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module gpgme.

%-----------------------------------------------------------------------------%

init_crypto(Res, !IO) :-
    gpgme_init(!IO),
    gpgme_engine_check_version(openpgp, ResGpgme, !IO),
    (
        ResGpgme = ok,
        gpgme_new(ResContext, !IO),
        (
            ResContext = ok(Context),
            gpgme_set_protocol(Context, openpgp, ResProto, !IO),
            (
                ResProto = ok,
                gpgme_set_armor(Context, ascii_armor, !IO),
                Res = ok(Context)
            ;
                ResProto = error(Error),
                shutdown_crypto(Context, !IO),
                Res = error(Error)
            )
        ;
            ResContext = error(Error),
            Res = error(Error)
        )
    ;
        ResGpgme = error(Error),
        Res = error(Error)
    ).

shutdown_crypto(Context, !IO) :-
    gpgme_release(Context, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
