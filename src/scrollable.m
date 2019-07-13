% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module scrollable.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module version_array.

:- import_module screen.

%-----------------------------------------------------------------------------%

:- type scrollable(T).

:- type search_direction
    --->    dir_forward
    ;       dir_reverse.

:- type draw_line(T) == pred(screen, vpanel, T, int, bool, io, io).
:- inst draw_line    == (pred(in, in, in, in, in, di, uo) is det).

:- func init(list(T)) = scrollable(T).

:- func init_with_cursor(list(T)) = scrollable(T).

:- pred reinit(list(T)::in, int::in, scrollable(T)::in, scrollable(T)::out)
    is det.

:- func get_lines(scrollable(T)) = version_array(T).

:- func get_lines_list(scrollable(T)) = list(T).

:- func get_num_lines(scrollable(T)) = int.

:- pred get_line(scrollable(T)::in, int::in, T::out) is semidet.

:- pred set_line(int::in, T::in, scrollable(T)::in, scrollable(T)::out) is det.

:- pred set_lines_list(list(T)::in, scrollable(T)::in, scrollable(T)::out)
    is det.

:- func get_top(scrollable(T)) = int.

:- pred set_top(int::in, scrollable(T)::in, scrollable(T)::out) is det.

:- pred get_cursor(scrollable(T)::in, int::out) is semidet.

:- pred set_cursor(int::in, scrollable(T)::in, scrollable(T)::out) is det.

:- pred set_cursor_centred(int::in, int::in,
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred set_cursor_visible(int::in, int::in,
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred set_cursor_none(scrollable(T)::in, scrollable(T)::out) is det.

:- pred get_cursor_line(scrollable(T)::in, int::out, T::out) is semidet.

:- pred set_cursor_line(T::in, scrollable(T)::in, scrollable(T)::out) is det.

:- pred map_lines(pred(T, T)::in(pred(in, out) is det),
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred scroll(int::in, int::in, bool::out,
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred move_cursor(int::in, int::in, bool::out,
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred search(pred(T)::in(pred(in) is semidet), search_direction::in,
    scrollable(T)::in, int::in, int::out) is semidet.

:- pred search_forward(pred(T)::in(pred(in) is semidet),
    scrollable(T)::in, int::in, int::out, T::out) is semidet.

:- pred search_forward_limit(pred(T)::in(pred(in) is semidet),
    scrollable(T)::in, int::in, int::in, int::out, T::out) is semidet.

:- pred search_reverse(pred(T)::in(pred(in) is semidet),
    scrollable(T)::in, int::in, int::out) is semidet.

:- pred search_reverse_limit(pred(T)::in(pred(in) is semidet),
    scrollable(T)::in, int::in, int::in, int::out, T::out) is semidet.

:- pred append_line(T::in, scrollable(T)::in, scrollable(T)::out) is det.

:- pred delete_cursor_line(scrollable(T)::in, scrollable(T)::out) is semidet.

:- pred draw(draw_line(T)::in(draw_line), screen::in, list(vpanel)::in,
    scrollable(T)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module require.

:- type scrollable(T)
    --->    scrollable(
                s_lines     :: version_array(T),
                s_top       :: int,
                s_cursor    :: maybe(int)
            ).

%-----------------------------------------------------------------------------%

init(Lines) = Scrollable :-
    LinesArray = version_array.from_list(Lines),
    Top = 0,
    Scrollable = scrollable(LinesArray, Top, no).

init_with_cursor(Lines) = Scrollable :-
    LinesArray = version_array.from_list(Lines),
    Top = 0,
    (
        Lines = [],
        MaybeCursor = no
    ;
        Lines = [_ | _],
        MaybeCursor = yes(0)
    ),
    Scrollable = scrollable(LinesArray, Top, MaybeCursor).

reinit(Lines, NumRows, Scrollable0, Scrollable) :-
    % For now keeping the same Top and MaybeCursor values is good enough,
    % but ensure they are in bounds.
    Scrollable0 = scrollable(_LinesArray0, Top0, MaybeCursor0),
    LinesArray = version_array.from_list(Lines),
    Size = version_array.size(LinesArray),
    ( Size = 0 ->
        Top = 0
    ;
        Top = min(Top0, Size - 1)
    ),
    Scrollable1 = scrollable(LinesArray, Top, no),
    (
        MaybeCursor0 = yes(Cursor0),
        set_cursor_visible(Cursor0, NumRows, Scrollable1, Scrollable)
    ;
        MaybeCursor0 = no,
        Scrollable = Scrollable1
    ).

get_lines(Scrollable) = Scrollable ^ s_lines.

get_lines_list(Scrollable) = version_array.to_list(get_lines(Scrollable)).

get_num_lines(Scrollable) = size(Scrollable ^ s_lines).

get_line(Scrollable, LineNum, Line) :-
    Lines = Scrollable ^ s_lines,
    (
        LineNum >= 0,
        LineNum < size(Lines)
    ->
        Line = version_array.lookup(Lines, LineNum)
    ;
        fail
    ).

set_line(LineNum, Line, !Scrollable) :-
    Lines0 = !.Scrollable ^ s_lines,
    version_array.set(LineNum, Line, Lines0, Lines),
    !Scrollable ^ s_lines := Lines.

set_lines_list(List, !Scrollable) :-
    OldArray = !.Scrollable ^ s_lines,
    NewArray = version_array.from_list(List),
    ( version_array.size(OldArray) = version_array.size(NewArray) ->
        !Scrollable ^ s_lines := NewArray
    ;
        unexpected($module, $pred, "changed size")
    ).

get_top(Scrollable) = Scrollable ^ s_top.

set_top(Top, !Scrollable) :-
    !Scrollable ^ s_top := Top.

get_cursor(Scrollable, Cursor) :-
    Scrollable ^ s_cursor = yes(Cursor).

set_cursor(Cursor, !Scrollable) :-
    !Scrollable ^ s_cursor := yes(Cursor).

set_cursor_centred(Cursor, NumRows, !Scrollable) :-
    Top = max(0, Cursor - NumRows//2),
    !Scrollable ^ s_top := Top,
    !Scrollable ^ s_cursor := yes(Cursor).

set_cursor_visible(Cursor, NumRows, !Scrollable) :-
    Top0 = !.Scrollable ^ s_top,
    ( Cursor < Top0 ->
        !Scrollable ^ s_top := Cursor
    ; Cursor >= Top0 + NumRows ->
        !Scrollable ^ s_top := Cursor - NumRows + 1
    ;
        true
    ),
    !Scrollable ^ s_cursor := yes(Cursor).

set_cursor_none(!Scrollable) :-
    !Scrollable ^ s_cursor := no.

get_cursor_line(Scrollable, Cursor, Line) :-
    Lines = Scrollable ^ s_lines,
    MaybeCursor = Scrollable ^ s_cursor,
    MaybeCursor = yes(Cursor),
    Line = version_array.lookup(Lines, Cursor).

set_cursor_line(Line, !Scrollable) :-
    (
        !.Scrollable ^ s_lines = Lines0,
        !.Scrollable ^ s_cursor = yes(Cursor)
    ->
        version_array.set(Cursor, Line, Lines0, Lines),
        !Scrollable ^ s_lines := Lines
    ;
        unexpected($module, $pred, "failed")
    ).

map_lines(P, !Scrollable) :-
    !.Scrollable ^ s_lines = Array0,
    List0 = version_array.to_list(Array0),
    list.map(P, List0, List),
    !Scrollable ^ s_lines := version_array.from_list(List).

scroll(NumRows, Delta, HitLimit, !Scrollable) :-
    !.Scrollable = scrollable(Lines, Top0, MaybeCursor0),
    NumLines = version_array.size(Lines),
    TopLimit = max(max(0, NumLines - NumRows), Top0),
    Top = clamp(0, Top0 + Delta, TopLimit),
    ( Top = Top0, Delta < 0 ->
        HitLimit = yes
    ; Top = Top0, Delta > 0 ->
        HitLimit = yes
    ;
        HitLimit = no
    ),
    % XXX cursor
    MaybeCursor = MaybeCursor0,
    !:Scrollable = scrollable(Lines, Top, MaybeCursor).

move_cursor(NumRows, Delta, HitLimit, !Scrollable) :-
    !.Scrollable = scrollable(Lines, Top0, MaybeCursor0),
    NumLines = version_array.size(Lines),
    (
        MaybeCursor0 = yes(Cursor0),
        Cursor = clamp(0, Cursor0 + Delta, NumLines - 1),
        ( Cursor = Cursor0 ->
            HitLimit = yes
        ;
            HitLimit = no,
            ( Cursor < Top0 ->
                Top = Cursor
            ; Top0 + NumRows - 1 < Cursor ->
                ( Delta > 0 ->
                    Top = int.max(0, Cursor - NumRows + 1)
                ;
                    Top = Cursor
                )
            ;
                Top = Top0
            ),
            !:Scrollable = scrollable(Lines, Top, yes(Cursor))
        )
    ;
        MaybeCursor0 = no,
        HitLimit = no
    ).

search(P, dir_forward, Scrollable, I0, I) :-
    search_forward(P, Scrollable, I0, I, _MatchLine).
search(P, dir_reverse, Scrollable, I0, I) :-
    search_reverse(P, Scrollable, I0, I).

search_forward(P, Scrollable, I0, I, MatchLine) :-
    search_forward_limit(P, Scrollable, I0, int.max_int, I, MatchLine).

search_forward_limit(P, Scrollable, I0, Limit, I, MatchLine) :-
    Scrollable = scrollable(Lines, _Top, _MaybeCursor),
    Size = version_array.size(Lines),
    search_forward_2(P, Lines, int.min(Limit, Size), I0, I, MatchLine).

:- pred search_forward_2(pred(T)::in(pred(in) is semidet),
    version_array(T)::in, int::in, int::in, int::out, T::out) is semidet.

search_forward_2(P, Array, Limit, N0, N, MatchX) :-
    ( N0 < Limit ->
        X = version_array.lookup(Array, N0),
        ( P(X) ->
            N = N0,
            MatchX = X
        ;
            search_forward_2(P, Array, Limit, N0 + 1, N, MatchX)
        )
    ;
        fail
    ).

search_reverse(P, Scrollable, I0, I) :-
    Limit = 0,
    search_reverse_limit(P, Scrollable, I0, Limit, I, _MatchLine).

search_reverse_limit(P, Scrollable, I0, Limit, I, MatchLine) :-
    Scrollable = scrollable(Lines, _Top, _MaybeCursor),
    search_reverse_2(P, Lines, Limit, I0 - 1, I, MatchLine).

:- pred search_reverse_2(pred(T)::in(pred(in) is semidet),
    version_array(T)::in, int::in, int::in, int::out, T::out) is semidet.

search_reverse_2(P, Array, Limit, N0, N, MatchX) :-
    ( N0 >= Limit ->
        X = version_array.lookup(Array, N0),
        ( P(X) ->
            N = N0,
            MatchX = X
        ;
            search_reverse_2(P, Array, Limit, N0 - 1, N, MatchX)
        )
    ;
        fail
    ).

append_line(NewLine, Scrollable0, Scrollable) :-
    Scrollable0 = scrollable(Array0, Top, MaybeCursor),
    List0 = version_array.to_list(Array0),
    List = List0 ++ [NewLine],
    Array = version_array.from_list(List),
    Scrollable = scrollable(Array, Top, MaybeCursor).

delete_cursor_line(Scrollable0, Scrollable) :-
    Scrollable0 = scrollable(Array0, Top0, yes(Cursor0)),
    List0 = version_array.to_list(Array0),
    list.split_list(Cursor0, List0, Start, [_ | End]),
    (
        End = [],
        Start = [],
        Top = 0,
        MaybeCursor = no
    ;
        End = [],
        Start = [_ | _],
        Cursor = int.max(0, Cursor0 - 1),
        Top = int.min(Top0, Cursor),
        MaybeCursor = yes(Cursor)
    ;
        End = [_ | _],
        MaybeCursor = yes(Cursor0),
        Top = Top0
    ),
    Array = version_array.from_list(Start ++ End),
    Scrollable = scrollable(Array, Top, MaybeCursor).

%-----------------------------------------------------------------------------%

draw(Pred, Screen, RowPanels, Scrollable, !IO) :-
    Scrollable = scrollable(Lines, Top, MaybeCursor),
    (
        MaybeCursor = yes(Cursor)
    ;
        MaybeCursor = no,
        Cursor = -1
    ),
    draw_lines(Pred, Screen, RowPanels, Lines, Top, Cursor, !IO).

:- pred draw_lines(draw_line(T)::in(draw_line), screen::in, list(vpanel)::in,
    version_array(T)::in, int::in, int::in, io::di, io::uo) is det.

draw_lines(_Pred, _, [], _, _, _, !IO).
draw_lines(Pred, Screen, [Panel | Panels], Lines, I, Cursor, !IO) :-
    erase(Screen, Panel, !IO),
    Size = version_array.size(Lines),
    ( I < Size ->
        Line = version_array.lookup(Lines, I),
        IsCursor = (I = Cursor -> yes ; no),
        Pred(Screen, Panel, Line, I, IsCursor, !IO)
    ;
        true
    ),
    draw_lines(Pred, Screen, Panels, Lines, I + 1, Cursor, !IO).

:- func clamp(int, int, int) = int.

clamp(Min, X, Max) =
    ( X < Min -> Min
    ; X > Max -> Max
    ; X
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
