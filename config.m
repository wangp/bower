% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module config.
:- interface.

:- import_module io.
:- import_module map.

:- type config == map(section, map(string, string)).

:- type section == string.

:- func init_config = config.

:- pred load_config_file(string::in, io.res(config)::out, io::di, io::uo)
    is det.

:- pred search_config(config::in, section::in, string::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

init_config = map.init.

load_config_file(FileName, Res, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        load_config_stream(Stream, "", map.init, Config, !IO),
        io.close_input(Stream, !IO),
        Res = ok(Config)
    ;
        OpenRes = error(Error),
        Res = error(Error)
    ).

:- pred load_config_stream(io.input_stream::in, section::in,
    config::in, config::out, io::di, io::uo) is det.

load_config_stream(Stream, Section0, !Config, !IO) :-
    io.read_line_as_string(Stream, ReadRes, !IO),
    (
        ReadRes = ok(Line0),
        Line = string.strip(Line0),
        ( Line = "" ->
            Section = Section0
        ;
            parse_line(Line, Section0, Section, !Config)
        ),
        load_config_stream(Stream, Section, !Config, !IO)
    ;
        ReadRes = eof
    ;
        ReadRes = error(Error),
        error(io.error_message(Error))
    ).

:- pred parse_line(string::in, section::in, section::out,
    config::in, config::out) is det.

parse_line(Line, !Section, !Config) :-
    string.unsafe_index(Line, 0, FirstChar),
    ( FirstChar = ('#') ->
        % Comment line.
        true
    ; FirstChar = ('[') ->
        ( string.sub_string_search_start(Line, "]", 1, CloseIndex) ->
            % Everything after the closing bracket is ignored.
            string.between(Line, 1, CloseIndex, !:Section)
        ;
            % Invalid line.
            true
        )
    ; string.sub_string_search(Line, "=", EqIndex) ->
        End = string.count_code_units(Line),
        string.between(Line, 0, EqIndex, Left),
        string.between(Line, EqIndex + 1, End, Right),
        Key = string.rstrip(Left),
        Value = string.lstrip(Right),
        ( map.search(!.Config, !.Section, Map0) ->
            map.set(Key, Value, Map0, Map),
            map.det_update(!.Section, Map, !Config)
        ;
            Map = map.singleton(Key, Value),
            map.det_insert(!.Section, Map, !Config)
        )
    ;
        % Invalid line.
        true
    ).

search_config(Config, Section, Key, Value) :-
    map.search(Config, Section, SectionMap),
    map.search(SectionMap, Key, Value).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
