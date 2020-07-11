% Bower - a frontend for the Notmuch email system
% Copyright (C) 2019 Peter Wang

:- module forward.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module data.
:- import_module prog_config.

:- pred prepare_forward_message(prog_config::in, message::in(message),
    part_visibility_map::in, string::in, headers::out, string::out,
    list(part)::out, io::di, io::uo) is det.

:- pred select_main_part_and_attachments(part_visibility_map::in,
    list(part)::in, maybe(part)::out, list(part)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module fold_lines.
:- import_module message_template.
:- import_module mime_type.
:- import_module string_util.

%-----------------------------------------------------------------------------%

prepare_forward_message(Config, OrigMessage, PartVisibilityMap, From,
        Headers, BodyText, AttachmentParts, !IO) :-
    OrigMessage = message(_MessageId, _Timestamp, OrigHeaders, _Tags,
        OrigBody, _Replies),

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

    select_main_part_and_attachments(PartVisibilityMap, [OrigBody],
        MaybeMainPart, AttachmentParts),
    (
        MaybeMainPart = yes(MainPart),
        render_part_to_text(Config, PartVisibilityMap, no_quote_marker,
            MainPart, MainText, !IO)
    ;
        MaybeMainPart = no,
        MainText = []
    ),

    BodyText = unlines(
        ["-------- Forwarded message --------"] ++
        maybe_header("Subject: ", OrigSubject) ++
        maybe_header("Date: ", OrigDate) ++
        maybe_header("From: ", OrigFrom) ++
        maybe_header("To: ", OrigTo) ++
        maybe_header("Cc: ", OrigCc) ++
        ["" | MainText] ++
        ["-------- End forwarded message --------"]
    ).

%---------------------------------------------------------------------------%

:- func maybe_header(string, string) = list(string).

maybe_header(FieldColonSp, Value) = Lines :-
    get_spans_by_whitespace(Value, Spans0),
    (
        Spans0 = [],
        Lines = []
    ;
        Spans0 = [Head0 | Tail],
        Head0 = span(Mandatory, Trailer),
        Head = span(FieldColonSp ++ lstrip(Mandatory), Trailer),
        Spans = [Head | Tail],
        fill_lines(soft_line_length, Spans, Lines)
    ).

:- func soft_line_length = int.

soft_line_length = 78.

%-----------------------------------------------------------------------------%

select_main_part_and_attachments(PartVisibilityMap, Body, MaybeMainPart,
        AttachmentParts) :-
    ( find_main_part(PartVisibilityMap, Body, MainPart, PathToMain0) ->
        MaybeMainPart = yes(MainPart),
        PathToMain = PathToMain0
    ;
        MaybeMainPart = no,
        PathToMain = []
    ),
    foldl(select_attachments(PathToMain), Body, [], RevAttachmentParts),
    reverse(RevAttachmentParts, AttachmentParts).

%-----------------------------------------------------------------------------%

    % This is similar but different to message_template.add_reply_part.
    % Perhaps the similarity should run deeper...
    %
:- pred find_main_part(part_visibility_map::in, list(part)::in,
    part::out, list(part_id)::out) is semidet.

find_main_part(PartVisibilityMap, [Part | _Parts], MainPart, PathToMain) :-
    Part = part(_MessageId, PartId, ContentType, _MaybeContentCharset,
        MaybeContentDisposition, Content, _MaybeFileName, _MaybeContentLength,
        _MaybeCTE, _IsDecrypted),
    require_complete_switch [Content]
    (
        Content = text(_PartText),
        ContentType = text_plain,
        MaybeContentDisposition \= yes(content_disposition_attachment),
        MainPart = Part,
        PathToMain = maybe_cons(PartId, [])
    ;
        Content = unsupported,
        ContentType = text_html,
        part_has_visibility(PartVisibilityMap, part_visible, Part),
        MainPart = Part,
        PathToMain = maybe_cons(PartId, [])
    ;
        Content = subparts(Encryption, _Signatures, SubParts0),
        filter_subparts(ContentType, SubParts0, SubParts),
        (
            ( Encryption = not_encrypted
            ; Encryption = decryption_good
            ),
            ( ContentType = multipart_alternative ->
                find_main_part_in_alternatives(PartVisibilityMap, SubParts,
                    MainPart, PathToMain0)
            ;
                % Types we may have to deal with:
                %   multipart/mixed
                %   multipart/related
                %   multipart/digest
                %   multipart/parallel
                %   multipart/signed
                %   multipart/encrypted
                find_main_part(PartVisibilityMap, SubParts,
                    MainPart, PathToMain0)
            ),
            PathToMain = maybe_cons(PartId, PathToMain0)
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
        Content = encapsulated_message(_),
        fail
    ),
    % We search for the main body text down the left spine of the tree
    % (except in multipart/alternative parts) hence ignore later parts.
    true.

:- pred find_main_part_in_alternatives(part_visibility_map::in, list(part)::in,
    part::out, list(part_id)::out) is semidet.

find_main_part_in_alternatives(PartVisibilityMap, Parts, MainPart, PathToMain)
        :-
    select_alternative_by_visibility(PartVisibilityMap, Parts, MainPart),
    PartId = MainPart ^ pt_part,
    PathToMain = maybe_cons(PartId, []).

:- func content_disposition_attachment = content_disposition.

content_disposition_attachment = content_disposition("attachment").

:- func maybe_cons(maybe(T), list(T)) = list(T).

maybe_cons(no, Xs) = Xs.
maybe_cons(yes(X), Xs) = [X | Xs].

%-----------------------------------------------------------------------------%

:- pred select_attachments(list(part_id)::in, part::in,
    list(part)::in, list(part)::out) is det.

select_attachments(PathToMain, Part, !RevAttachmentParts) :-
    Part = part(_MessageId, PartId, ContentType, _MaybeContentCharset,
        _MaybeContentDisposition, Content, MaybeFileName, _MaybeContentLength,
        _MaybeCTE, _IsDecrypted),
    (
        ( Content = text(_)
        ; Content = unsupported     % includes text/html
        ),
        (
            MaybeFileName = yes(_),
            not path_to_text_contains(PathToMain, PartId)
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
                list.filter(path_to_text_contains_part(PathToMain),
                    SubParts, SubPartsToConsider)
            ;
                SubPartsToConsider = SubParts
            ),
            foldl(select_attachments(PathToMain), SubPartsToConsider,
                !RevAttachmentParts)
        ;
            Encryption = encrypted
            % XXX Parts manually decrypted in the thread/pager view will be
            % ignored.
        ;
            Encryption = decryption_bad
        )
    ;
        Content = encapsulated_message(_)
        % make_attachment_mime_part can't write these yet anyway.
    ).

:- pred path_to_text_contains(list(part_id)::in, maybe(part_id)::in)
    is semidet.

path_to_text_contains(PathToMain, yes(PartId)) :-
    list.contains(PathToMain, PartId).

:- pred path_to_text_contains_part(list(part_id)::in, part::in) is semidet.

path_to_text_contains_part(PathToMain, Part) :-
    path_to_text_contains(PathToMain, Part ^ pt_part).

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
