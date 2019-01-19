% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module size_util.
:- interface.

:- import_module maybe.

:- import_module data.

:- pred estimate_decoded_length(maybe(content_transfer_encoding)::in, int::in,
    int::out) is semidet.

:- func format_approx_length(int) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.

estimate_decoded_length(MaybeCTE, Length, DecodedLength) :-
    MaybeCTE = yes(content_transfer_encoding("base64")),
    % DecodedLength = Length * 3 / 4.
    % This produces a better estimate assuming the usual line breaks.
    DecodedLength = round_to_int(float(Length) * 0.7402595).

format_approx_length(Size) = String :-
    ( Size = 0 ->
        String = "0 bytes"
    ; Size =< 1000000 ->
        Ks = float(Size) / 1000.0,
        String = format("%.1f kB", [f(Ks)])
    ;
        Ms = float(Size) / 1000000.0,
        String = format("%.1f MB", [f(Ms)])
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
