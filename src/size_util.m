% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module size_util.
:- interface.

:- import_module maybe.

:- import_module data.

:- pred estimate_decoded_length(maybe(content_transfer_encoding)::in, int::in,
    int::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

estimate_decoded_length(MaybeCTE, Length, DecodedLength) :-
    MaybeCTE = yes(content_transfer_encoding("base64")),
    DecodedLength = Length * 3 / 4.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
