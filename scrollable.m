%-----------------------------------------------------------------------------%

:- module scrollable.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module curs.
:- import_module curs.panel.

%-----------------------------------------------------------------------------%

:- type scrollable(T).

:- typeclass line(T) where [
    pred draw_line(panel::in, T::in, io::di, io::uo) is det
].

:- func init(list(T)) = scrollable(T).

:- func get_lines(scrollable(T)) = list(T).

:- func get_top(scrollable(T)) = int.

:- pred scroll(int::in, int::in, bool::out,
    scrollable(T)::in, scrollable(T)::out) is det.

:- pred search_forward(pred(T)::in(pred(in) is semidet), bool::in,
    scrollable(T)::in, scrollable(T)::out) is semidet.

:- pred search_reverse(pred(T)::in(pred(in) is semidet),
    scrollable(T)::in, scrollable(T)::out) is semidet.

:- pred draw(list(panel)::in, scrollable(T)::in, io::di, io::uo) is det
    <= scrollable.line(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- type scrollable(T)
    --->    scrollable(
                s_lines     :: list(T),
                s_numlines  :: int,
                s_top       :: int
            ).

%-----------------------------------------------------------------------------%

init(Lines) = scrollable(Lines, NumLines, Top) :-
    list.length(Lines, NumLines),
    Top = 0.

get_lines(Scrollable) = Scrollable ^ s_lines.

get_top(Scrollable) = Scrollable ^ s_top.

scroll(NumRows, Delta, HitLimit, !Scrollable) :-
    !.Scrollable = scrollable(Lines, NumLines, Top0),
    TopLimit = max(max(0, NumLines - NumRows), Top0),
    Top = clamp(0, Top0 + Delta, TopLimit),
    ( Top = Top0, Delta < 0 ->
        HitLimit = yes
    ; Top = Top0, Delta > 0 ->
        HitLimit = yes
    ;
        HitLimit = no
    ),
    !:Scrollable = scrollable(Lines, NumLines, Top).

search_forward(P, SkipInitial, !Scrollable) :-
    !.Scrollable = scrollable(Lines0, _NumLines, Top0),
    (
        SkipInitial = yes,
        Top1 = Top0 + 1
    ;
        SkipInitial = no,
        Top1 = Top0
    ),
    list.drop(Top1, Lines0, Lines),
    search_loop(P, Lines, Top1, Top),
    !Scrollable ^ s_top := Top.

search_reverse(P, !Scrollable) :-
    !.Scrollable = scrollable(Lines0, _NumLines, Top0),
    list.take_upto(Top0, Lines0, Lines1),
    list.reverse(Lines1, RevLines),
    search_loop(P, RevLines, 1, N),
    Top = Top0 - N,
    !Scrollable ^ s_top := Top.
    
:- pred search_loop(pred(T)::in(pred(in) is semidet),
    list(T)::in, int::in, int::out) is semidet.

search_loop(P, [X | Xs], N0, N) :-
    ( P(X) ->
        N = N0
    ;
        search_loop(P, Xs, N0 + 1, N)
    ).

draw(RowPanels, Scrollable, !IO) :-
    !.Scrollable = scrollable(Lines0, _NumLines, Top),
    ( list.drop(Top, Lines0, Lines1) ->
        Lines = Lines1
    ;
        Lines = []
    ),
    draw_lines(RowPanels, Lines, !IO).

:- pred draw_lines(list(panel)::in, list(T)::in,
    io::di, io::uo) is det
    <= scrollable.line(T).

draw_lines([], _, !IO).
draw_lines([Panel | Panels], Lines, !IO) :-
    panel.erase(Panel, !IO),
    (
        Lines = [Line | RestLines],
        draw_line(Panel, Line, !IO)
    ;
        Lines = [],
        RestLines = []
    ),
    draw_lines(Panels, RestLines, !IO).

:- func clamp(int, int, int) = int.

clamp(Min, X, Max) =
    ( X < Min -> Min
    ; X > Max -> Max
    ; X
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
