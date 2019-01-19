% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module forward.
:- interface.

:- import_module list.

:- import_module data.

:- pred prepare_forward_message(message::in(message), string::in,
    headers::out, string::out, list(part)::out) is det.

:- pred select_body_text_and_attachments(list(part)::in, string::out,
    list(part)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module string.

:- import_module fold_lines.
:- import_module mime_type.

%-----------------------------------------------------------------------------%

prepare_forward_message(OrigMessage, From, Headers, Body, AttachmentParts) :-
    OrigHeaders = OrigMessage ^ m_headers,
    OrigSubject = header_value_string(OrigHeaders ^ h_subject),
    OrigDate = header_value_string(OrigHeaders ^ h_date),
    OrigFrom = header_value_string(OrigHeaders ^ h_from),
    OrigTo = header_value_string(OrigHeaders ^ h_to),
    OrigCc = header_value_string(OrigHeaders ^ h_cc),

    some [!Headers] (
        !:Headers = init_headers,
        !Headers ^ h_from := header_value(From),
        !Headers ^ h_subject := header_value("Fwd: " ++ OrigSubject),
        Headers = !.Headers
    ),

    OrigBody = OrigMessage ^ m_body,
    select_body_text_and_attachments(OrigBody, Text, AttachmentParts),

    Body = append_list([
        "-------- Forwarded message --------\n",
        maybe_header("Subject: ", OrigSubject),
        maybe_header("Date: ", OrigDate),
        maybe_header("From: ", OrigFrom),
        maybe_header("To: ", OrigTo),
        maybe_header("Cc: ", OrigCc),
        "\n",
        Text,
        "\n",
        "-------- End forwarded message --------\n"
    ]).

%---------------------------------------------------------------------------%

:- func maybe_header(string, string) = string.

maybe_header(FieldColonSp, Value) = S :-
    get_spans_by_whitespace(Value, Spans0),
    (
        Spans0 = [],
        S = ""
    ;
        Spans0 = [Head0 | Tail],
        Head0 = span(Mandatory, Trailer),
        Head = span(FieldColonSp ++ lstrip(Mandatory), Trailer),
        Spans = [Head | Tail],

        fill_lines(soft_line_length, Spans, Lines),
        S = join_list("\n ", Lines) ++ "\n"
    ).

:- func soft_line_length = int.

soft_line_length = 78.

%-----------------------------------------------------------------------------%

select_body_text_and_attachments(Body, Text, AttachmentParts) :-
    ( find_body_text(Body, Text0, PathToText0) ->
        Text = Text0,
        PathToText = PathToText0
    ;
        Text = "",
        PathToText = []
    ),
    foldl(select_attachments(PathToText), Body, [], RevAttachmentParts),
    reverse(RevAttachmentParts, AttachmentParts).

%-----------------------------------------------------------------------------%

:- pred find_body_text(list(part)::in, string::out, list(int)::out) is semidet.

find_body_text([Part | _Parts], Text, PathToText) :-
    Part = part(_MessageId, PartId, ContentType, MaybeContentDisposition,
        Content, _MaybeFileName, _MaybeContentLength, _MaybeCTE, _IsDecrypted),
    require_complete_switch [Content]
    (
        Content = text(PartText),
        ContentType = text_plain,
        MaybeContentDisposition \= yes(content_disposition_attachment),
        Text = PartText,
        PathToText = maybe_cons(PartId, [])
    ;
        Content = unsupported,
        fail
    ;
        Content = subparts(Encryption, _Signatures, SubParts0),
        filter_subparts(ContentType, SubParts0, SubParts),
        (
            ( Encryption = not_encrypted
            ; Encryption = decryption_good
            ),
            ( ContentType = multipart_alternative ->
                % We're supposed to select the LAST part that we can.
                find_body_text_in_alternatives(reverse(SubParts),
                    Text, PathToText0)
            ;
                % Types we may have to deal with:
                %   multipart/mixed
                %   multipart/related
                %   multipart/digest
                %   multipart/parallel
                %   multipart/signed
                %   multipart/encrypted
                find_body_text(SubParts, Text, PathToText0)
            ),
            PathToText = maybe_cons(PartId, PathToText0)
        ;
            Encryption = encrypted,
            % XXX Parts manually decrypted in the thread/pager view will be
            % ignored.
            fail
        ;
            Encryption = decryption_bad,
            fail
        )
    ;
        Content = encapsulated_messages(_),
        fail
    ),
    % We search for the main body text down the left spine of the tree
    % (except in multipart/alternative parts).
    true.

:- pred find_body_text_in_alternatives(list(part)::in, string::out,
    list(int)::out) is semidet.

find_body_text_in_alternatives([Part | AltParts], Text, PathToText) :-
    ( find_body_text([Part], Text0, PathToText0) ->
        Text = Text0,
        PathToText = PathToText0
    ;
        find_body_text_in_alternatives(AltParts, Text, PathToText)
    ).

:- func content_disposition_attachment = content_disposition.

content_disposition_attachment = content_disposition("attachment").

:- func maybe_cons(maybe(T), list(T)) = list(T).

maybe_cons(no, Xs) = Xs.
maybe_cons(yes(X), Xs) = [X | Xs].

%-----------------------------------------------------------------------------%

:- pred select_attachments(list(int)::in, part::in,
    list(part)::in, list(part)::out) is det.

select_attachments(PathToText, Part, !RevAttachmentParts) :-
    Part = part(_MessageId, PartId, ContentType, _MaybeContentDisposition,
        Content, MaybeFileName, _MaybeContentLength, _MaybeCTE, _IsDecrypted),
    (
        ( Content = text(_)
        ; Content = unsupported     % includes text/html
        ),
        (
            MaybeFileName = yes(_),
            not path_to_text_contains(PathToText, PartId)
        ->
            cons(Part, !RevAttachmentParts)
        ;
            true
        )
    ;
        Content = subparts(Encryption, _Signatures, SubParts0),
        filter_subparts(ContentType, SubParts0, SubParts),
        (
            ( Encryption = not_encrypted
            ; Encryption = decryption_good
            ),
            ( ContentType = multipart_alternative ->
                % Follow only the alternative with the chosen text.
                list.filter(path_to_text_contains_part(PathToText),
                    SubParts, SubPartsToConsider)
            ;
                SubPartsToConsider = SubParts
            ),
            foldl(select_attachments(PathToText), SubPartsToConsider,
                !RevAttachmentParts)
        ;
            Encryption = encrypted
            % XXX Parts manually decrypted in the thread/pager view will be
            % ignored.
        ;
            Encryption = decryption_bad
        )
    ;
        Content = encapsulated_messages(_)
        % make_attachment_mime_part can't write these yet anyway.
    ).

:- pred path_to_text_contains(list(int)::in, maybe(int)::in) is semidet.

path_to_text_contains(PathToText, yes(PartId)) :-
    list.contains(PathToText, PartId).

:- pred path_to_text_contains_part(list(int)::in, part::in) is semidet.

path_to_text_contains_part(PathToText, Part) :-
    path_to_text_contains(PathToText, Part ^ pt_part).

%-----------------------------------------------------------------------------%

:- pred filter_subparts(mime_type::in, list(part)::in, list(part)::out)
    is det.

filter_subparts(ContentType, Parts0, Parts) :-
    ( ContentType = multipart_signed ->
        negated_filter(part_has_content_type(application_pgp_signature),
            Parts0, Parts)
    ; ContentType = multipart_encrypted ->
        negated_filter(part_has_content_type(application_pgp_encrypted),
            Parts0, Parts)
    ;
        Parts = Parts0
    ).

:- pred part_has_content_type(mime_type::in, part::in) is semidet.

part_has_content_type(ContentType, Part) :-
    Part ^ pt_content_type = ContentType.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
