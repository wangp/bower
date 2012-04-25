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

:- type tag_delta
    --->    tag_delta(string). % +tag or -tag

:- func tag_delta_to_string(tag_delta) = string.

:- pred standard_tag(tag::in) is semidet.

:- pred get_standard_tag_state(set(tag)::in,
    unread::out, replied::out, deleted::out, flagged::out) is det.

:- pred apply_standard_tag_state(unread::in, replied::in, deleted::in,
    flagged::in, set(tag)::in, set(tag)::out) is det.

:- pred get_nonstandard_tags_width(set(tag)::in, int::out) is det.

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

standard_tag(tag("deleted")).
standard_tag(tag("flagged")).
standard_tag(tag("inbox")).
standard_tag(tag("new")).
standard_tag(tag("replied")).
standard_tag(tag("sent")).
standard_tag(tag("signed")).
standard_tag(tag("unread")).

%-----------------------------------------------------------------------------%

get_standard_tag_state(Tags, Unread, Replied, Deleted, Flagged) :-
    Unread = ( set.contains(Tags, tag("unread")) -> unread ; read ),
    Replied = ( set.contains(Tags, tag("replied")) -> replied ; not_replied ),
    Deleted = ( set.contains(Tags, tag("deleted")) -> deleted ; not_deleted ),
    Flagged = ( set.contains(Tags, tag("flagged")) -> flagged ; unflagged ).

%-----------------------------------------------------------------------------%

apply_standard_tag_state(Unread, Replied, Deleted, Flagged, !TagSet) :-
    (
        Unread = unread,
        set.insert(tag("unread"), !TagSet)
    ;
        Unread = read,
        set.delete(tag("unread"), !TagSet)
    ),
    (
        Replied = replied,
        set.insert(tag("replied"), !TagSet)
    ;
        Replied = not_replied,
        set.delete(tag("replied"), !TagSet)
    ),
    (
        Deleted = deleted,
        set.insert(tag("deleted"), !TagSet)
    ;
        Deleted = not_deleted,
        set.delete(tag("deleted"), !TagSet)
    ),
    (
        Flagged = flagged,
        set.insert(tag("flagged"), !TagSet)
    ;
        Flagged = unflagged,
        set.delete(tag("flagged"), !TagSet)
    ).

%-----------------------------------------------------------------------------%

get_nonstandard_tags_width(Tags, Length) :-
    set.to_sorted_list(Tags, TagList),
    list.negated_filter(standard_tag, TagList, NonstdTags),
    list.length(NonstdTags, NumNonstdTags),
    ( NumNonstdTags > 0 ->
        SepLength = NumNonstdTags,
        list.foldl(sum_tag_length, NonstdTags, SepLength, Length)
    ;
        Length = 0
    ).

:- pred sum_tag_length(tag::in, int::in, int::out) is det.

sum_tag_length(tag(TagName), Len, Len + string_wcwidth(TagName)).

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
