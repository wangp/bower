%-----------------------------------------------------------------------------%

:- module test_crypto.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module pretty_printer.

:- import_module data.
:- import_module gmime.
:- import_module gmime_adaptor.
:- import_module gpgme.
:- import_module gpgme.decrypt.
:- import_module gpgme.decrypt_verify.
:- import_module gpgme.encrypt.
:- import_module gpgme.gmime.
:- import_module gpgme.key.
:- import_module gpgme.sign.
:- import_module gpgme.verify.

:- type op
    --->    decrypt(string)
    ;       decrypt_verify(string)
    ;       verify_detached(string, string)
    ;       verify_clearsigned(string)
    ;       sign(string)
    ;       encrypt(string, list(string)).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = ["--decrypt", FileName] ->
        read_file_as_string(FileName, ResRead, !IO),
        (
            ResRead = ok(CipherText),
            main_1(decrypt(CipherText), !IO)
        ;
            ResRead = error(Error),
            report_error(Error, !IO)
        )
    ; Args = ["--decrypt-verify", FileName] ->
        read_file_as_string(FileName, ResRead, !IO),
        (
            ResRead = ok(CipherText),
            main_1(decrypt_verify(CipherText), !IO)
        ;
            ResRead = error(Error),
            report_error(Error, !IO)
        )
    ; Args = ["--verify", SigFileName, TextFileName] ->
        read_file_as_string(SigFileName, ResSig, !IO),
        read_file_as_string(TextFileName, ResText, !IO),
        (
            ResSig = ok(Sig),
            ResText = ok(Text)
        ->
            main_1(verify_detached(Sig, Text), !IO)
        ;
            report_error("error reading files", !IO)
        )
    ; Args = ["--verify", FileName] ->
        read_file_as_string(FileName, ResRead, !IO),
        (
            ResRead = ok(Sig),
            main_1(verify_clearsigned(Sig), !IO)
        ;
            ResRead = error(Error),
            report_error(Error, !IO)
        )
    ; Args = ["--sign", FileName] ->
        read_file_as_string(FileName, ResRead, !IO),
        (
            ResRead = ok(Text),
            main_1(sign(Text), !IO)
        ;
            ResRead = error(Error),
            report_error(Error, !IO)
        )
    ; Args = ["--encrypt", FileName | Recipients] ->
        read_file_as_string(FileName, ResRead, !IO),
        (
            ResRead = ok(Text),
            main_1(encrypt(Text, Recipients), !IO)
        ;
            ResRead = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("bad arguments", !IO)
    ).

:- pred main_1(op::in, io::di, io::uo) is det.

main_1(Op, !IO) :-
    g_mime_init(!IO),
    gpgme_init(!IO),
    gpgme_engine_check_version(openpgp, ResGpgme, !IO),
    (
        ResGpgme = ok,
        gpgme_new(ResContext, !IO),
        (
            ResContext = ok(Context),
            gpgme_set_protocol(Context, openpgp, ResProto, !IO),
            (
                ResProto = ok,
                gpgme_set_armor(Context, ascii_armor, !IO),
                main_2(Context, Op, !IO)
            ;
                ResProto = error(Error),
                report_error(Error, !IO)
            ),
            gpgme_release(Context, !IO)
        ;
            ResContext = error(Error),
            report_error(Error, !IO)
        )
    ;
        ResGpgme = error(Error),
        report_error(Error, !IO)
    ).

:- pred main_2(ctx::in, op::in, io::di, io::uo) is det.

main_2(Ctx, Op, !IO) :-
    (
        Op = decrypt(CipherText),
        decrypt(Ctx, CipherText, ResDecrypt, ResPart, !IO),
        MaybeVerifyResult = no
    ;
        Op = decrypt_verify(CipherText),
        decrypt_verify(Ctx, CipherText, ResDecrypt, ResPart,
            MaybeVerifyResult, !IO)
    ),
    (
        ResDecrypt = ok(DecryptResult),
        write_string("DecryptResult:\n", !IO),
        write_doc(format(DecryptResult), !IO),
        (
            ResPart = ok(Part),
            write_string("\n\nPart:\n", !IO),
            write_doc(format(Part), !IO),
            write_string("\n", !IO)
        ;
            ResPart = error(Error),
            report_error(Error, !IO)
        )
    ;
        ResDecrypt = error(Error),
        report_error(Error, !IO)
    ),
    (
        MaybeVerifyResult = yes(VerifyResult),
        write_string("\nVerifyResult:\n", !IO),
        write_doc(format(VerifyResult), !IO),
        write_string("\n", !IO)
    ;
        MaybeVerifyResult = no
    ).

main_2(Ctx, Op, !IO) :-
    Op = verify_detached(Sig, SignedText),
    verify_detached(Ctx, Sig, SignedText, Res, !IO),
    (
        Res = ok(VerifyResult),
        write_string("VerifyResult:\n", !IO),
        write_doc(format(VerifyResult), !IO),
        write_string("\n", !IO)
    ;
        Res = error(Error),
        report_error(Error, !IO)
    ).

main_2(Ctx, Op, !IO) :-
    Op = verify_clearsigned(Sig),
    verify_clearsigned(Ctx, Sig, Res, !IO),
    (
        Res = ok(VerifyResult),
        write_string("VerifyResult:\n", !IO),
        write_doc(format(VerifyResult), !IO),
        write_string("\n", !IO)
    ;
        Res = error(Error),
        report_error(Error, !IO)
    ).

main_2(Ctx, Op, !IO) :-
    Op = sign(Text),
    sign_detached(Ctx, Text, ResSign, ResSig, !IO),
    (
        ResSign = ok(SignResult),
        write_string("SignResult:\n", !IO),
        write_doc(format(SignResult), !IO),
        write_string("\n\n", !IO),
        (
            ResSig = ok(Sig),
            io.write_string(Sig, !IO),
            io.write_string("\n", !IO)
        ;
            ResSig = error(Error),
            report_error(Error, !IO)
        )
    ;
        ResSign = error(Error),
        report_error(Error, !IO)
    ).

main_2(Ctx, Op, !IO) :-
    Op = encrypt(Text, Recipients),
    encrypt(Ctx, Text, Recipients, ResEncrypt, ResCipher, !IO),
    (
        ResEncrypt = ok(EncryptResult),
        write_string("EncryptResult:\n", !IO),
        write_doc(format(EncryptResult), !IO),
        write_string("\n\n", !IO),
        (
            ResCipher = ok(Cipher),
            io.write_string(Cipher, !IO)
        ;
            ResCipher = error(Error),
            report_error(Error, !IO)
        )
    ;
        ResEncrypt = error(Error),
        report_error(Error, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred decrypt(ctx::in, string::in, maybe_error(decrypt_result)::out,
    maybe_error(part)::out, io::di, io::uo) is det.

decrypt(Ctx, InputString, ResDecrypt, ResPart, !IO) :-
    gpgme_data_new_from_string(InputString, ResCipher, !IO),
    (
        ResCipher = ok(Cipher),
        stream_mem_new(PlainStream, !IO),
        gpgme_data_new_from_gmime_stream(PlainStream, ResPlain, !IO),
        (
            ResPlain = ok(Plain),
            gpgme_op_decrypt(Ctx, Cipher, Plain, ResDecrypt, !IO),
            gpgme_data_release(Plain, !IO),
            (
                ResDecrypt = ok(_),
                stream_to_part(PlainStream, ResPart, !IO)
            ;
                ResDecrypt = error(_),
                ResPart = error("no data")
            )
        ;
            ResPlain = error(Error),
            ResDecrypt = error(Error),
            ResPart = error("no data")
        ),
        stream_unref(PlainStream, !IO),
        gpgme_data_release(Cipher, !IO)
    ;
        ResCipher = error(Error),
        ResDecrypt = error(Error),
        ResPart = error("no data")
    ).

:- pred stream_to_part(g_mime_stream(T)::in, maybe_error(part)::out,
    io::di, io::uo) is det.

stream_to_part(PlainStream, Res, !IO) :-
    seek_start(PlainStream, ResSeek, !IO),
    (
        ResSeek = ok,
        parser_new_with_stream(PlainStream, Parser, !IO),
        construct_message(Parser, MaybeMessage, !IO),
        parser_unref(Parser, !IO),
        (
            MaybeMessage = yes(Message),
            message_to_part(Message, Part, !IO),
            message_unref(Message, !IO),
            Res = ok(Part)
        ;
            MaybeMessage = no,
            Res = error("could not parse message")
        )
    ;
        ResSeek = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred decrypt_verify(ctx::in, string::in, maybe_error(decrypt_result)::out,
    maybe_error(part)::out, maybe(verify_result)::out, io::di, io::uo) is det.

decrypt_verify(Ctx, InputString, ResDecrypt, ResPart, MaybeVerifyResult, !IO)
        :-
    gpgme_data_new_from_string(InputString, ResCipher, !IO),
    (
        ResCipher = ok(Cipher),
        stream_mem_new(PlainStream, !IO),
        gpgme_data_new_from_gmime_stream(PlainStream, ResPlain, !IO),
        (
            ResPlain = ok(Plain),
            gpgme_op_decrypt_verify(Ctx, Cipher, Plain, ResDecrypt,
                MaybeVerifyResult, !IO),
            gpgme_data_release(Plain, !IO),
            (
                ResDecrypt = ok(_),
                stream_to_part(PlainStream, ResPart, !IO)
            ;
                ResDecrypt = error(_),
                ResPart = error("no data")
            )
        ;
            ResPlain = error(Error),
            ResDecrypt = error(Error),
            ResPart = error("no data"),
            MaybeVerifyResult = no
        ),
        stream_unref(PlainStream, !IO),
        gpgme_data_release(Cipher, !IO)
    ;
        ResCipher = error(Error),
        ResDecrypt = error(Error),
        ResPart = error("no data"),
        MaybeVerifyResult = no
    ).

%-----------------------------------------------------------------------------%

:- pred verify_detached(ctx::in, string::in, string::in,
    maybe_error(verify_result)::out, io::di, io::uo) is det.

verify_detached(Ctx, Sig, SignedText, Res, !IO) :-
    gpgme_data_new_from_string(Sig, ResSigData, !IO),
    (
        ResSigData = ok(SigData),
        gpgme_data_new_from_string(SignedText, ResSignedTextData, !IO),
        (
            ResSignedTextData = ok(SignedTextData),
            gpgme_op_verify_detached(Ctx, SigData, SignedTextData, Res, !IO),
            gpgme_data_release(SignedTextData, !IO)
        ;
            ResSignedTextData = error(Error),
            Res = error(Error)
        ),
        gpgme_data_release(SigData, !IO)
    ;
        ResSigData = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred verify_clearsigned(ctx::in, string::in,
    maybe_error(verify_result)::out, io::di, io::uo) is det.

verify_clearsigned(Ctx, Sig, Res, !IO) :-
    gpgme_data_new_from_string(Sig, ResSigData, !IO),
    (
        ResSigData = ok(SigData),
        gpgme_data_new(ResPlainData, !IO),
        (
            ResPlainData = ok(PlainData),
            gpgme_op_verify_clearsigned(Ctx, SigData, PlainData, Res, !IO),
            gpgme_data_release(PlainData, !IO)
        ;
            ResPlainData = error(Error),
            Res = error(Error)
        ),
        gpgme_data_release(SigData, !IO)
    ;
        ResSigData = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred sign_detached(ctx::in, string::in, maybe_error(sign_result)::out,
    maybe_error(string)::out, io::di, io::uo) is det.

sign_detached(Ctx, Plain, ResSign, ResSig, !IO) :-
    gpgme_data_new_from_string(Plain, ResPlainData, !IO),
    (
        ResPlainData = ok(PlainData),
        gpgme_data_new(ResSigData, !IO),
        (
            ResSigData = ok(SigData),
            % XXX set signers
            gpgme_op_sign_detached(Ctx, PlainData, SigData, ResSign, !IO),
            (
                ResSign = ok(_),
                gpgme_data_to_string(SigData, ResSig, !IO)
            ;
                ResSign = error(_),
                ResSig = error("sign failed")
            ),
            gpgme_data_release(SigData, !IO)
        ;
            ResSigData = error(Error),
            ResSign = error(Error),
            ResSig = error("sign failed")
        ),
        gpgme_data_release(PlainData, !IO)
    ;
        ResPlainData = error(Error),
        ResSign = error(Error),
        ResSig = error("sign failed")
    ).

%-----------------------------------------------------------------------------%

:- pred encrypt(ctx::in, string::in, list(string)::in,
    maybe_error(encrypt_result)::out, maybe_error(string)::out,
    io::di, io::uo) is det.

encrypt(Ctx, Plain, Recipients, ResEncrypt, ResCipher, !IO) :-
    get_keys(Ctx, Recipients, ResKeys, [], Keys, !IO),
    (
        ResKeys = ok,
        Keys = [],
        ResEncrypt = error("no keys"),
        ResCipher = error("encrypt failed")
    ;
        ResKeys = ok,
        Keys = [_ | _],
        io.write_string("Encryption keys:\n", !IO),
        list.foldl(write_key_info, Keys, !IO),
        io.flush_output(!IO),
        encrypt_2(Ctx, Plain, Keys, ResEncrypt, ResCipher, !IO)
    ;
        ResKeys = error(Error),
        ResEncrypt = error(Error),
        ResCipher = error("encrypt failed")
    ),
    unref_keys(Keys, !IO).

:- pred encrypt_2(ctx::in, string::in, list(key)::in,
    maybe_error(encrypt_result)::out, maybe_error(string)::out,
    io::di, io::uo) is det.

encrypt_2(Ctx, Plain, Keys, ResEncrypt, ResCipher, !IO) :-
    gpgme_data_new_from_string(Plain, ResPlainData, !IO),
    (
        ResPlainData = ok(PlainData),
        gpgme_data_new(ResCipherData, !IO),
        (
            ResCipherData = ok(CipherData),
            gpgme_op_encrypt(Ctx, Keys, [always_trust], PlainData,
                CipherData, ResEncrypt, !IO),
            (
                ResEncrypt = ok(_),
                gpgme_data_to_string(CipherData, ResCipher, !IO)
            ;
                ResEncrypt = error(_),
                ResCipher = error("encrypt failed")
            ),
            gpgme_data_release(CipherData, !IO)
        ;
            ResCipherData = error(Error),
            ResEncrypt = error(Error),
            ResCipher = error("encrypt failed")
        ),
        gpgme_data_release(PlainData, !IO)
    ;
        ResPlainData = error(Error),
        ResEncrypt = error(Error),
        ResCipher = error("encrypt failed")
    ).

:- pred get_keys(ctx::in, list(string)::in, maybe_error::out,
    list(key)::in, list(key)::out, io::di, io::uo) is det.

get_keys(Ctx, Recipients, Res, !Keys, !IO) :-
    (
        Recipients = [],
        Res = ok
    ;
        Recipients = [Recp | Recps],
        gpgme_op_keylist(Ctx, yes(Recp), not_secret_only, ResKeys, !IO),
        (
            ResKeys = ok(Keys),
            append(Keys, !Keys),
            get_keys(Ctx, Recps, Res, !Keys, !IO)
        ;
            ResKeys = error(Error),
            Res = error(Error)
        )
    ).

:- pred write_key_info(key::in, io::di, io::uo) is det.

write_key_info(Key, !IO) :-
    Info = get_key_info(Key),
    write_doc(format(Info), !IO),
    write_string("\n\n", !IO).

%-----------------------------------------------------------------------------%

:- pred read_file_as_string(string::in, maybe_error(string)::out,
    io::di, io::uo) is det.

read_file_as_string(FileName, Res, !IO) :-
    io.open_input(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        io.read_file_as_string(Stream, ResRead, !IO),
        (
            ResRead = ok(String),
            Res = ok(String)
        ;
            ResRead = error(_, Error),
            Res = error(error_message(Error))
        ),
        io.close_input(Stream, !IO)
    ;
        ResOpen = error(Error),
        Res = error(error_message(Error))
    ).

:- pred report_error(string::in, io::di, io::uo) is det.

report_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
