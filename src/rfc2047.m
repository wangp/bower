% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2047.
:- interface.

:- include_module rfc2047.encoder.
:- include_module rfc2047.decoder.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module stream.

:- use_module base64.

% Build up a list of octets for base64 encoding/decoding.
% We could use destructively-updated buffers for efficiency.

:- type octets_builder
    --->    octets_builder.

:- type octets
    --->    octets(list(int)). % reverse

:- instance stream.stream(octets_builder, octets) where [
    name(_, "octets_builder", !State)
].
:- instance stream.output(octets_builder, octets) where [
    flush(_, !State)
].
:- instance stream.writer(octets_builder, base64.byte, octets) where [
    put(octets_builder, Byte, octets(Acc0), octets(Acc)) :- (
        copy(Byte, UniqueByte),
        Acc = [UniqueByte | Acc0]
    )
].
:- instance stream.writer(octets_builder, char, octets) where [
    put(octets_builder, Char, octets(Acc0), octets(Acc)) :- (
        char.to_int(Char, Byte),
        copy(Byte, UniqueByte),
        Acc = [UniqueByte | Acc0]
    )
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
