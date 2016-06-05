%-----------------------------------------------------------------------------%

:- module test_fqdn.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.

:- import_module sys_util.

%-----------------------------------------------------------------------------%

main(!IO) :-
    get_hostname_fqdn(MaybeHostName, MaybeFQDN, !IO),
    write_string("host name = ", !IO),
    write(MaybeHostName, !IO),
    nl(!IO),
    write_string("fqdn = ", !IO),
    write(MaybeFQDN, !IO),
    nl(!IO).

%-----------------------------------------------------------------------------%
