%-----------------------------------------------------------------------------%

:- module notmuch_frontend.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.

:- import_module json.
:- import_module popen.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = ["--show-thread", TId] ->
        Command = "notmuch show --format=json thread:" ++ TId,
        popen(Command, Res, !IO),
        (
            Res = ok(String),
            parse_json(String, ParseResult),
            (
                ParseResult = ok(JSON),
                ( JSON = array([List]) ->
                    show_message_list(List, !IO)
                ; JSON = array([]) ->
                    io.write_string("no matches\n", !IO)
                ;
                    notmuch_json_error
                )
            ;
                ParseResult = error(_, _, _),
                io.write(ParseResult, !IO),
                io.nl(!IO)
            )
        ;
            Res = error(_)
        )
    ;
        io.write_string("command line error\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred show_message_list(json::in, io::di, io::uo) is det.

show_message_list(JSON, !IO) :-
    ( JSON = array(List) ->
        list.foldl(show_message_cons_list, List, !IO)
    ;
        notmuch_json_error
    ).

:- pred show_message_cons_list(json::in, io::di, io::uo) is det.

show_message_cons_list(JSON, !IO) :-
    ( JSON = array([]) ->
        true
    ; JSON = array([Head | Tail]) ->
        ( Head = array(_) ->
            show_message_cons_list(Head, !IO)
        ;
            show_message(Head, !IO)
        ),
        show_message_cons_list(array(Tail), !IO)
    ;
        notmuch_json_error
    ).

:- pred show_message(json::in, io::di, io::uo) is det.

show_message(JSON, !IO) :-
    (
        JSON/"headers" = Headers,
        Headers/"Subject" = string(Subject),
        Headers/"From" = string(From),
        Headers/"Date" = string(Date),
        JSON/"body" = array(Body)
    ->
        write_hdr("Subject: ", Subject, !IO),
        write_hdr("From: ", From, !IO),
        write_hdr("Date: ", Date, !IO),
        io.nl(!IO),
        list.foldl(show_content, Body, !IO)
    ;
        notmuch_json_error
    ).

:- pred show_content(json::in, io::di, io::uo) is det.

show_content(JSON, !IO) :-
    (
        JSON/"id" = int(Id),
        JSON/"content-type" = string(ContentType)
    ->
        ( JSON/"content" = string(ContentString) ->
            Lines = string.split_at_string("\\n", ContentString),
            list.foldl(write_nl, Lines, !IO)
        ; JSON/"content" = array(SubParts) ->
            list.foldl(show_content, SubParts, !IO)
        ;
            % "content" is unavailable for non-text parts.
            % We can those by running notmuch show --part=N id:NNN
            io.write_string("[", !IO),
            io.write_string(ContentType, !IO),
            io.write_string(" part id=", !IO),
            io.write_int(Id, !IO),
            io.write_string("]\n", !IO)
        )
    ;
        notmuch_json_error
    ).

:- pred write_hdr(string::in, string::in, io::di, io::uo) is det.

write_hdr(Header, Value, !IO) :-
    io.write_string(Header, !IO),
    io.write_string(Value, !IO),
    io.nl(!IO).

:- pred write_nl(string::in, io::di, io::uo) is det.

write_nl(String, !IO) :-
    io.write_string(String, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- func json / string = json is semidet.

map(Map) / Key = Value :-
    map.search(Map, Key, Value).

:- pred notmuch_json_error is erroneous.

notmuch_json_error :-
    error("notmuch_json_error").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
