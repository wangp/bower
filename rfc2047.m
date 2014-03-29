% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc2047.
:- interface.

:- include_module rfc2047.encoder.
:- include_module rfc2047.decoder.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module stream.

:- use_module base64.

% Build up a list of code units for base64 encoding/decoding.
% We could use destructively-updated buffers for efficiency.

:- type code_units_builder
    --->    code_units_builder.

:- type code_units
    --->    code_units(list(int)). % reverse

:- instance stream.stream(code_units_builder, code_units) where [
    name(_, "code_units_builder", !State)
].
:- instance stream.output(code_units_builder, code_units) where [
    flush(_, !State)
].
:- instance stream.writer(code_units_builder, base64.byte, code_units) where [
    put(code_units_builder, Byte, code_units(Acc0), code_units(Acc)) :- (
        copy(Byte, UniqueByte),
        Acc = [UniqueByte | Acc0]
    )
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
