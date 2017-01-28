% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module tags.
:- interface.

:- import_module data.

:- import_module list.
:- import_module set.

:- type new
    --->    new
    ;       old.

:- type unread
    --->    unread
    ;       read.

:- type replied
    --->    replied
    ;       not_replied.

:- type deleted
    --->    deleted
    ;       not_deleted.

:- type flagged
    --->    flagged
    ;       unflagged.

:- type standard_tags
    --->    standard_tags(
                unread :: unread,
                replied :: replied,
                deleted :: deleted,
                flagged :: flagged
            ).

:- type tag_delta
    --->    tag_delta(string). % +tag or -tag

:- func tag_delta_to_string(tag_delta) = string.

    % There should be more of these instead of bare strings.
:- func draft_tag = tag.
:- func draft_sign_tag = tag.
:- func encrypted_tag = tag.

:- pred display_tag(tag::in) is semidet.

:- pred get_standard_tags(set(tag)::in, standard_tags::out, int::out) is det.

:- pred validate_tag_deltas(list(string)::in, list(tag_delta)::out,
    set(tag)::out, set(tag)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- import_module string_util.

%-----------------------------------------------------------------------------%

tag_delta_to_string(tag_delta(String)) = String.

%-----------------------------------------------------------------------------%

draft_tag = tag("draft").
draft_sign_tag = tag("draft-sign").
encrypted_tag = tag("encrypted").

%-----------------------------------------------------------------------------%

display_tag(Tag) :-
    not nondisplay_tag(Tag).

:- pred nondisplay_tag(tag::in) is semidet.

nondisplay_tag(tag("deleted")).
nondisplay_tag(tag("flagged")).
nondisplay_tag(tag("new")).
nondisplay_tag(tag("replied")).
nondisplay_tag(tag("sent")).
nondisplay_tag(tag("signed")).
nondisplay_tag(tag("unread")).

%-----------------------------------------------------------------------------%

get_standard_tags(Tags, StdTags, DisplayTagsWidth) :-
    StdTags0 = standard_tags(read, not_replied, not_deleted, unflagged),
    set.fold2(get_standard_tags_2, Tags, StdTags0, StdTags,
        0, DisplayTagsWidth).

:- pred get_standard_tags_2(tag::in, standard_tags::in, standard_tags::out,
    int::in, int::out) is det.

get_standard_tags_2(Tag, !StdTags, !DisplayTagsWidth) :-
    ( Tag = tag("unread") ->
        !StdTags ^ unread := unread
    ; Tag = tag("replied") ->
        !StdTags ^ replied := replied
    ; Tag = tag("deleted") ->
        !StdTags ^ deleted := deleted
    ; Tag = tag("flagged") ->
        !StdTags ^ flagged := flagged
    ; display_tag(Tag) ->
        Tag = tag(TagName),
        % Add one for separator.
        !:DisplayTagsWidth = !.DisplayTagsWidth + string_wcwidth(TagName) + 1
    ;
        true
    ).

%-----------------------------------------------------------------------------%

validate_tag_deltas(Words, TagDeltas, AddTags, RemoveTags) :-
    list.map_foldl2(validate_tag_delta, Words, TagDeltas,
        set.init, AddTags, set.init, RemoveTags).

:- pred validate_tag_delta(string::in, tag_delta::out,
    set(tag)::in, set(tag)::out, set(tag)::in, set(tag)::out) is semidet.

validate_tag_delta(Word, tag_delta(Word), !AddTags, !RemoveTags) :-
    (
        string.remove_prefix("+", Word, Tag),
        not blacklist_tag(Tag)
    ->
        set.insert(tag(Tag), !AddTags)
    ;
        string.remove_prefix("-", Word, Tag),
        not blacklist_tag(Tag)
    ->
        set.insert(tag(Tag), !RemoveTags)
    ;
        fail
    ).

:- pred blacklist_tag(string::in) is semidet.

blacklist_tag("").
blacklist_tag("-").
blacklist_tag("+").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
