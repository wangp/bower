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

:- pred parse_message(string::in, headers::out, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module string.

:- import_module rfc2047.
:- import_module rfc2047.decoder.
:- import_module string_util.

%-----------------------------------------------------------------------------%

parse_message_file(Filename, Res, !IO) :-
    io.open_input(Filename, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        read_file_as_string(Stream, ResRead, !IO),
        (
            ResRead = ok(String),
            parse_message(String, Headers, Body),
            Res = ok(Headers - Body)
        ;
            ResRead = error(_, Error),
            Res = error(Error)
        ),
        io.close_input(Stream, !IO)
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

parse_message(String, Headers, Body) :-
    read_headers(String, 0, PosBody, init_headers, Headers),
    string.unsafe_between(String, PosBody, count_code_units(String), Body).

:- pred read_headers(string::in, int::in, int::out, headers::in, headers::out)
    is det.

read_headers(String, Pos0, Pos, !Headers) :-
    unfolded_line(String, Pos0, Pos1, Line),
    ( Line = "" ->
        Pos = Pos1
    ; is_header_line(Line, Name, RawValue) ->
        % Other headers should be decoded as well.
        ( strcase_equal(Name, "Subject") ->
            decode_unstructured(RawValue, Decoded),
            Value = decoded_unstructured(string.replace_all(Decoded, "\t", " "))
        ;
            Value = header_value(RawValue)
        ),
        add_header(Name, Value, !Headers),
        read_headers(String, Pos1, Pos, !Headers)
    ;
        Pos = Pos0
    ).

:- pred unfolded_line(string::in, int::in, int::out, string::out) is det.

unfolded_line(String, Start, End, Line) :-
    unfolded_line_2(String, Start, Start, End, [], RevPieces),
    list.reverse(RevPieces, Pieces),
    string.append_list(Pieces, Line).

:- pred unfolded_line_2(string::in, int::in, int::in, int::out,
    list(string)::in, list(string)::out) is det.

unfolded_line_2(String, Start, Pos0, LineEnd, !Acc) :-
    ( string.unsafe_index_next(String, Pos0, Pos1, Char0) ->
        (
            (
                % CR LF
                Char0 = '\r',
                skip_lf(String, Pos1, Pos2)
            ;
                % LF
                Char0 = '\n',
                Pos2 = Pos1
            )
        ->
            % Drop the CRLF / LF.
            string.unsafe_between(String, Start, Pos0, Piece),
            cons(Piece, !Acc),
            % "Unfolding is accomplished by regarding CRLF immediately followed
            % by a LWSP-char as equivalent to the LWSP-char."
            (
                string.unsafe_index_next(String, Pos2, _Pos3, Char2),
                lwsp(Char2)
            ->
                unfolded_line_2(String, Pos2, Pos2, LineEnd, !Acc)
            ;
                LineEnd = Pos2
            )
        ;
            unfolded_line_2(String, Start, Pos1, LineEnd, !Acc)
        )
    ;
        % End of string.
        LineEnd = Pos0,
        string.unsafe_between(String, Start, LineEnd, Piece),
        cons(Piece, !Acc)
    ).

:- pred skip_lf(string::in, int::in, int::out) is det.

skip_lf(String, Pos0, Pos) :-
    ( string.unsafe_index_next(String, Pos0, Pos1, '\n') ->
        Pos = Pos1
    ;
        Pos = Pos0
    ).

:- pred lwsp(char::in) is semidet. % linear whitespace

lwsp(' ').
lwsp('\t').

:- pred is_header_line(string::in, string::out, string::out) is semidet.

is_header_line(Line, Name, Value) :-
    string.sub_string_search(Line, ":", Colon),
    require_det (
        End = string.count_code_units(Line),
        string.unsafe_between(Line, 0, Colon, Name0),
        string.unsafe_between(Line, Colon + 1, End, Value0),
        Name = string.strip(Name0),
        Value = string.strip(Value0)
    ).

:- pred add_header(string::in, header_value::in, headers::in, headers::out)
    is det.

add_header(Name, Value, !Headers) :-
    ( add_standard_header(Name, Value, !Headers) ->
        true
    ;
        Rest0 = !.Headers ^ h_rest,
        map.set(Name, Value, Rest0, Rest),
        !Headers ^ h_rest := Rest
    ).

:- pred add_standard_header(string::in, header_value::in,
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
