% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module detect_mime_type.
:- interface.

:- import_module io.

:- import_module mime_type.

:- type mime_type_with_charset
    --->    mime_type_with_charset(
                mtc_type    :: mime_type,   % e.g. text/plain
                mtc_charset :: string       % e.g. us-ascii, utf-8, binary
            ).

:- pred detect_mime_type(string::in, io.res(mime_type_with_charset)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module call_system.
:- import_module quote_arg.

%-----------------------------------------------------------------------------%

detect_mime_type(FileName, Res, !IO) :-
    make_quoted_command(file_command, ["--brief", "--mime", FileName],
        redirect_input("/dev/null"), no_redirect, Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
    (
        CallRes = ok(String0),
        String = string.chomp(String0),
        ( string.split_at_string("; charset=", String) = [TypeStr, Charset] ->
            Type = make_mime_type(TypeStr),
            Res = ok(mime_type_with_charset(Type, Charset))
        ;
            Res = error(io.make_io_error("could not parse mime type"))
        )
    ;
        CallRes = error(Error),
        Res = error(Error)
    ).

:- func file_command = command_prefix.

file_command = command_prefix(shell_quoted("file"), quote_once).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
