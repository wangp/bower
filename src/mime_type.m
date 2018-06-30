% Bower - a frontend for the Notmuch email system
% Copyright (C) 2018 Peter Wang

:- module mime_type.
:- interface.

:- type mime_type.

    % Assumes input is valid.
    %
:- func make_mime_type(string) = mime_type.

:- func to_string(mime_type) = string.

:- pred parse_mime_type(string, mime_type, string, string).
:- mode parse_mime_type(in, out, out, out) is semidet.

:- pred is_multipart(mime_type::in) is semidet.

:- func text_plain = mime_type.
:- func text_html = mime_type.
:- func message_rfc822 = mime_type.
:- func multipart_alternative = mime_type.
:- func multipart_mixed = mime_type.
:- func multipart_related = mime_type.
:- func multipart_signed = mime_type.
:- func multipart_encrypted = mime_type.
:- func application_pgp_encrypted = mime_type.
:- func application_pgp_signature = mime_type.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module rfc2045.

:- type mime_type
    --->    mime_type(string).  % type/subtype (all lowercase)

%-----------------------------------------------------------------------------%

make_mime_type(String) = mime_type(string.to_lower(String)).

to_string(mime_type(String)) = String.

parse_mime_type(String, MimeType, Type, SubType) :-
    string.to_lower(String, LowerString),
    string.split_at_char('/', LowerString) = [Type, SubType],
    Type \= "",
    SubType \= "",
    all_match(rfc2045.token_char, Type),
    all_match(rfc2045.token_char, SubType),
    MimeType = mime_type(LowerString).

%-----------------------------------------------------------------------------%

is_multipart(mime_type(Type)) :-
    string.prefix(Type, "multipart/").

text_plain = mime_type("text/plain").
text_html = mime_type("text/html").
message_rfc822 = mime_type("message/rfc822").
multipart_alternative = mime_type("multipart/alternative").
multipart_mixed = mime_type("multipart/mixed").
multipart_related = mime_type("multipart/related").
multipart_signed = mime_type("multipart/signed").
multipart_encrypted = mime_type("multipart/encrypted").
application_pgp_encrypted = mime_type("application/pgp-encrypted").
application_pgp_signature = mime_type("application/pgp-signature").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
