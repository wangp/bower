% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module message_file.
:- interface.

:- import_module io.
:- import_module pair.

:- import_module data.

%-----------------------------------------------------------------------------%

:- pred parse_message_file(string::in, io.res(pair(headers, string))::out,
    io::di, io::uo) is det.

:- pred read_headers_from_string(string::in, int::in,
    headers::in, headers::out, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module string.

:- import_module string_util.

%-----------------------------------------------------------------------------%

parse_message_file(Filename, Res, !IO) :-
    io.open_input(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        read_headers_from_stream(Stream, ResHeaders, MaybeBodyLine,
            init_headers, Headers, !IO),
        (
            ResHeaders = ok,
            read_file_as_string(Stream, ResBody, !IO),
            (
                ResBody = ok(Body0),
                (
                    MaybeBodyLine = yes(BodyLine),
                    Body = BodyLine ++ Body0
                ;
                    MaybeBodyLine = no,
                    Body = Body0
                ),
                Res = ok(Headers - Body)
            ;
                ResBody = error(_, Error),
                Res = error(Error)
            )
        ;
            ResHeaders = error(Error),
            Res = error(Error)
        ),
        io.close_input(Stream, !IO)
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

    % XXX implement proper parser
:- pred read_headers_from_stream(io.input_stream::in, io.res::out,
    maybe(string)::out, headers::in, headers::out, io::di, io::uo) is det.

read_headers_from_stream(Stream, Res, MaybeBodyLine, !Headers, !IO) :-
    io.read_line_as_string(Stream, ResRead, !IO),
    (
        ResRead = ok(Line),
        ( Line = "\n" ->
            Res = ok,
            MaybeBodyLine = no
        ; is_header_line(Line, Name, Value) ->
            add_header(Name, Value, !Headers),
            read_headers_from_stream(Stream, Res, MaybeBodyLine, !Headers, !IO)
        ;
            Res = ok,
            MaybeBodyLine = yes(Line)
        )
    ;
        ResRead = eof,
        Res = ok,
        MaybeBodyLine = no
    ;
        ResRead = error(Error),
        Res = error(Error),
        MaybeBodyLine = no
    ).

read_headers_from_string(String, Pos0, !Headers, Body) :-
    ( string.sub_string_search_start(String, "\n", Pos0, Nl) ->
        ( Pos0 = Nl ->
            End = string.count_code_units(String),
            string.between(String, Nl + 1, End, Body)
        ;
            string.unsafe_between(String, Pos0, Nl, Line),
            ( is_header_line(Line, Name, Value) ->
                add_header(Name, Value, !Headers),
                read_headers_from_string(String, Nl + 1, !Headers, Body)
            ;
                End = string.count_code_units(String),
                string.between(String, Pos0, End, Body)
            )
        )
    ;
        End = string.count_code_units(String),
        string.between(String, Pos0, End, Body)
    ).

:- pred is_header_line(string::in, string::out, string::out) is semidet.

is_header_line(Line, Name, Value) :-
    string.sub_string_search(Line, ":", Colon),
    require_det (
        End = string.count_code_units(Line),
        string.between(Line, 0, Colon, Name0),
        string.between(Line, Colon + 1, End, Value0),
        Name = string.strip(Name0),
        Value = string.strip(Value0)
    ).

:- pred add_header(string::in, string::in,
    headers::in, headers::out) is det.

add_header(Name, Value, !Headers) :-
    ( add_standard_header(Name, Value, !Headers) ->
        true
    ;
        Rest0 = !.Headers ^ h_rest,
        map.set(Name, Value, Rest0, Rest),
        !Headers ^ h_rest := Rest
    ).

:- pred add_standard_header(string::in, string::in,
    headers::in, headers::out) is semidet.

add_standard_header(Name, Value, !Headers) :-
    ( strcase_equal(Name, "From") ->
        !Headers ^ h_from := Value
    ; strcase_equal(Name, "To") ->
        !Headers ^ h_to := Value
    ; strcase_equal(Name, "Cc") ->
        !Headers ^ h_cc := Value
    ; strcase_equal(Name, "Bcc") ->
        !Headers ^ h_bcc := Value
    ; strcase_equal(Name, "Subject") ->
        !Headers ^ h_subject := Value
    ; strcase_equal(Name, "Reply-To") ->
        !Headers ^ h_replyto := Value
    ; strcase_equal(Name, "References") ->
        !Headers ^ h_references := Value
    ; strcase_equal(Name, "In-Reply-To") ->
        !Headers ^ h_inreplyto := Value
    ; strcase_equal(Name, "Date") ->
        % Ignore it.
        true
    ; strcase_equal(Name, "Message-ID") ->
        % Ignore it.
        true
    ;
        fail
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
