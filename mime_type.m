%-----------------------------------------------------------------------------%

:- module mime_type.
:- interface.

:- import_module io.

:- type mime_type
    --->    mime_type(
                mt_type     :: string,  % e.g. text/plain
                mt_charset  :: string   % e.g. us-ascii, utf-8, binary
            ).

:- pred lookup_mime_type(string::in, io.res(mime_type)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module popen.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

lookup_mime_type(FileName, Res, !IO) :-
    args_to_quoted_command(["file", "--brief", "--mime", FileName], Command),
    popen(Command, CallRes, !IO),
    (
        CallRes = ok(String0),
        String = string.chomp(String0),
        ( string.split_at_string("; charset=", String) = [Type, Charset] ->
            MimeType = mime_type(Type, Charset),
            Res = ok(MimeType)
        ;
            Res = error(io.make_io_error("could not parse mime type"))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
