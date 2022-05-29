% Bower - a frontend for the Notmuch email system
% Copyright (C) 2022 Peter Wang

:- module help.
:- interface.

:- import_module io.

:- pred print_help(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

print_help(ProgName, !IO) :-
    Lines = [
        "Usage: " ++ ProgName ++ " [OPTION...] [SEARCH-TERMS...]",
        "       " ++ ProgName ++ " [OPTION...] mailto:MAILTO",
        "",
        "Options:",
        "  -h, --help                  Display usage and options.",
        ""
    ],
    io.output_stream(Stream, !IO),
    list.foldl(write_string_nl(Stream), Lines, !IO).

:- pred write_string_nl(io.text_output_stream::in, string::in, io::di, io::uo)
    is det.

write_string_nl(Stream, S, !IO) :-
    io.write_string(Stream, S, !IO),
    io.nl(Stream, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
