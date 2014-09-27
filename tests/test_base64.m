%-----------------------------------------------------------------------------%

:- module test_base64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module base64.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = ["-d"] ->
        test_decode(!IO)
    ; Args = ["-e"] ->
        test_encode(no, !IO)
    ; Args = ["-ew"] ->
        test_encode(yes, !IO)
    ;
        io.set_exit_status(1, !IO)
    ).

:- pred test_decode(io::di, io::uo) is det.

test_decode(!IO) :-
    io.read_file_as_string(ReadRes, !IO),
    (
        ReadRes = ok(Input),
        string.length(Input, InputLength),
        io.stdout_binary_stream(Stream, !IO),
        base64.decode(Input, 0, FinalPos, InputLength, Stream, !IO),
        ( FinalPos = InputLength ->
            true
        ;
            FinalPos < InputLength,
            string.unsafe_index_code_unit(Input, FinalPos, char.to_int('='))
        ->
            true
        ;
            io.set_exit_status(1, !IO)
        )
    ;
        ReadRes = error(_, Error),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, io.error_message(Error), !IO),
        io.nl(Stderr, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred test_encode(bool::in, io::di, io::uo) is det.

test_encode(Wrap, !IO) :-
    io.read_file_as_string(ReadRes, !IO),
    (
        ReadRes = ok(Input),
        (
            Wrap = no,
            Encode = base64.encode
        ;
            Wrap = yes,
            Encode = base64.encode_wrap
        ),
        io.stdout_stream(Stream, !IO),
        Encode(Input, 0, string.length(Input), Stream, !IO)
    ;
        ReadRes = error(_, Error),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, io.error_message(Error), !IO),
        io.nl(Stderr, !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
