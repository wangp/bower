% Bower - a frontend for the Notmuch email system
% Copyright (C) 2016 Peter Wang

:- module list_util.
:- interface.

:- import_module list.

    % Avoid list.takewhile deprecated from 2016-04-22
    %
:- pred take_while(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out) is det.
:- pred take_while(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out, list(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

take_while(_, [], []).
take_while(P, [X | Xs], Start) :-
    ( if P(X) then
        list_util.take_while(P, Xs, Start0),
        Start = [X | Start0]
    else
        Start = []
    ).

take_while(_, [], [], []).
take_while(P, [X | Xs], Ins, Outs) :-
    ( if P(X) then
        Ins = [X | Ins0],
        list_util.take_while(P, Xs, Ins0, Outs)
    else
        Ins = [],
        Outs = [X | Xs]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
