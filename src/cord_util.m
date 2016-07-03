% Bower - a frontend for the Notmuch email system
% Copyright (C) 2016 Peter Wang

:- module cord_util.
:- interface.

:- import_module cord.

    % Also in cord.m from 2016-06-08
    %
:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

snoc(X, C, snoc(C, X)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
