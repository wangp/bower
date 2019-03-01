% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module color.
:- interface.

:- import_module config.

:- import_module curs.

:- type generic_attrs
    --->    generic_attrs(
                current         :: attr, % cursor
                relative_date   :: attr,
                selected        :: attr, % *
                standard_tag    :: attr,
                flagged         :: attr,
                author          :: attr,
                subject         :: attr,
                other_tag       :: attr,
                field_name      :: attr,
                field_body      :: attr,
                good_key        :: attr,
                bad_key         :: attr
            ).

:- type status_attrs
    --->    status_attrs(
                bar             :: attr,
                info            :: attr,
                warning         :: attr,
                prompt          :: attr
            ).

:- type pager_attrs
    --->    pager_attrs(
                p_generic       :: generic_attrs,
                p_body          :: attr,
                p_quote_odd     :: attr,
                p_quote_even    :: attr,
                p_diff_common   :: attr,
                p_diff_add      :: attr,
                p_diff_rem      :: attr,
                p_diff_hunk     :: attr,
                p_diff_index    :: attr,
                p_url           :: attr,
                p_part_head     :: attr,
                p_part_head_low :: attr,
                p_part_message  :: attr,
                p_fold          :: attr,
                p_separator     :: attr
            ).

:- type index_attrs
    --->    index_attrs(
                i_generic       :: generic_attrs,
                i_count         :: attr,
                i_total         :: attr
            ).

:- type thread_attrs
    --->    thread_attrs(
                t_generic       :: generic_attrs,
                t_status        :: status_attrs,
                t_tree          :: attr
            ).

:- type compose_attrs
    --->    compose_attrs(
                c_generic       :: generic_attrs,
                c_status        :: status_attrs,
                c_address       :: attr,
                c_invalid       :: attr
            ).

:- type colors
    --->    colors(
                generic_attrs   :: generic_attrs,
                status_attrs    :: status_attrs,
                pager_attrs     :: pager_attrs,
                index_attrs     :: index_attrs,
                thread_attrs    :: thread_attrs,
                compose_attrs   :: compose_attrs
            ).

:- pred make_colors(config::in, colors::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

make_colors(Config, Colors) :-
    make_generic_attrs(Config, GenericAttrs),
    make_status_attrs(Config, StatusAttrs),
    make_pager_attrs(Config, GenericAttrs, PagerAttrs),
    make_index_attrs(Config, GenericAttrs, IndexAttrs),
    make_thread_attrs(Config, GenericAttrs, StatusAttrs, ThreadAttrs),
    make_compose_attrs(Config, GenericAttrs, StatusAttrs, ComposeAttrs),
    Colors = colors(GenericAttrs, StatusAttrs, PagerAttrs, IndexAttrs,
        ThreadAttrs, ComposeAttrs).

:- pred make_generic_attrs(config::in, generic_attrs::out) is det.

make_generic_attrs(Config, Attrs) :-
    override_generic_attrs(Config, generic_section, default_generic_attrs,
        Attrs).

:- func generic_section = section.

generic_section = "color".

:- pred override_generic_attrs(config::in, section::in,
    generic_attrs::in, generic_attrs::out) is det.

override_generic_attrs(Config, Section, Def, Attrs) :-
    Sections = [Section],
    cfg(Config, Def ^ current, Sections, "current", Current),
    cfg(Config, Def ^ relative_date, Sections, "relative_date", RelDate),
    cfg(Config, Def ^ selected, Sections, "selected", Selected),
    cfg(Config, Def ^ standard_tag, Sections, "standard_tag", StandardTag),
    cfg(Config, Def ^ flagged, Sections, "flagged", Flagged),
    cfg(Config, Def ^ author, Sections, "author", Author),
    cfg(Config, Def ^ subject, Sections, "subject", Subject),
    cfg(Config, Def ^ other_tag, Sections, "other_tag", OtherTag),
    cfg(Config, Def ^ field_name, Sections, "field_name", FieldName),
    cfg(Config, Def ^ field_body, Sections, "field_body", FieldBody),
    cfg(Config, Def ^ good_key, Sections, "good_key", GoodKey),
    cfg(Config, Def ^ bad_key, Sections, "bad_key", BadKey),
    Attrs1 = generic_attrs(Current, RelDate, Selected, StandardTag, Flagged,
        Author, Subject, OtherTag, FieldName, FieldBody, GoodKey, BadKey),
    % Keep common structure if possible.
    Attrs = ( Def = Attrs1 -> Def ; Attrs1 ).

:- pred make_status_attrs(config::in, status_attrs::out) is det.

make_status_attrs(Config, Attrs) :-
    Def = default_status_attrs,
    Sections = ["color.status", generic_section],
    cfg(Config, Def ^ bar, Sections, "bar", Bar),
    cfg(Config, Def ^ info, Sections, "info", Info),
    cfg(Config, Def ^ warning, Sections, "warning", Warning),
    cfg(Config, Def ^ prompt, Sections, "prompt", Prompt),
    Attrs = status_attrs(Bar, Info, Warning, Prompt).

:- pred make_pager_attrs(config::in, generic_attrs::in, pager_attrs::out)
    is det.

make_pager_attrs(Config, GenericAttrs0, Attrs) :-
    Section = "color.pager",
    override_generic_attrs(Config, Section, GenericAttrs0, GenericAttrs),

    Def = default_pager_attrs(GenericAttrs),
    Sections = [Section, generic_section],
    cfg(Config, Def ^ p_body, Sections, "body", Body),
    cfg(Config, Def ^ p_quote_odd, Sections, "quote_odd", QuoteOdd),
    cfg(Config, Def ^ p_quote_even, Sections, "quote_even", QuoteEven),
    cfg(Config, Def ^ p_diff_common, Sections, "diff_common", DiffCommon),
    cfg(Config, Def ^ p_diff_add, Sections, "diff_add", DiffAdd),
    cfg(Config, Def ^ p_diff_rem, Sections, "diff_rem", DiffRem),
    cfg(Config, Def ^ p_diff_hunk, Sections, "diff_hunk", DiffHunk),
    cfg(Config, Def ^ p_diff_index, Sections, "diff_index", DiffIndex),
    cfg(Config, Def ^ p_url, Sections, "url", Url),
    cfg(Config, Def ^ p_part_head, Sections, "part_head", PartHead),
    cfg(Config, Def ^ p_part_head_low, Sections, "part_head_low", PartHeadLow),
    cfg(Config, Def ^ p_part_message, Sections, "part_message", PartMsg),
    cfg(Config, Def ^ p_fold, Sections, "fold", Fold),
    cfg(Config, Def ^ p_separator, Sections, "separator", Separator),

    Attrs = pager_attrs(GenericAttrs, Body, QuoteOdd, QuoteEven,
        DiffCommon, DiffAdd, DiffRem, DiffHunk, DiffIndex, Url,
        PartHead, PartHeadLow, PartMsg, Fold, Separator).

:- pred make_index_attrs(config::in, generic_attrs::in, index_attrs::out)
    is det.

make_index_attrs(Config, GenericAttrs0, Attrs) :-
    Section = "color.index",
    override_generic_attrs(Config, Section, GenericAttrs0, GenericAttrs),

    Def = default_index_attrs(GenericAttrs),
    Sections = [Section, generic_section],
    cfg(Config, Def ^ i_count, Sections, "count", Count),
    cfg(Config, Def ^ i_total, Sections, "total", Total),
    Attrs = index_attrs(GenericAttrs, Count, Total).

:- pred make_thread_attrs(config::in, generic_attrs::in, status_attrs::in,
    thread_attrs::out) is det.

make_thread_attrs(Config, !.GenericAttrs, StatusAttrs, Attrs) :-
    Section = "color.thread",
    override_generic_attrs(Config, Section, !GenericAttrs),

    % Special case: if 'author' and 'subject' are not configured in either the
    % thread or generic sections, then override them for the default theme.
    ( is_set(Config, Sections, "author") ->
        true
    ;
        !GenericAttrs ^ author := normal
    ),
    ( is_set(Config, Sections, "subject") ->
        true
    ;
        !GenericAttrs ^ subject := normal(green)
    ),

    Def = default_thread_attrs(!.GenericAttrs, StatusAttrs),
    Sections = [Section, generic_section],
    cfg(Config, Def ^ t_tree, Sections, "tree", Tree),
    Attrs = thread_attrs(!.GenericAttrs, StatusAttrs, Tree).

:- pred make_compose_attrs(config::in, generic_attrs::in, status_attrs::in,
    compose_attrs::out) is det.

make_compose_attrs(Config, GenericAttrs0, StatusAttrs, Attrs) :-
    Section = "color.compose",
    override_generic_attrs(Config, Section, GenericAttrs0, GenericAttrs),

    Def = default_compose_attrs(GenericAttrs, StatusAttrs),
    Sections = [Section, generic_section],
    cfg(Config, Def ^ c_address, Sections, "address", Address),
    cfg(Config, Def ^ c_invalid, Sections, "invalid", Invalid),
    Attrs = compose_attrs(GenericAttrs, StatusAttrs, Address, Invalid).

:- pred cfg(config::in, attr::in, list(section)::in, string::in, attr::out)
    is det.

cfg(_Config, Default, [], _Key, Attr) :-
    Attr = Default.
cfg(Config, Default, [Section | Sections], Key, Attr) :-
    ( search_config(Config, Section, Key, Value) ->
        ( parse_attr(Value, AttrPrime) ->
            Attr = AttrPrime
        ;
            Attr = Default
        )
    ;
        cfg(Config, Default, Sections, Key, Attr)
    ).

:- pred is_set(config::in, list(section)::in, string::in) is semidet.

is_set(Config, Sections, Key) :-
    list.member(Section, Sections),
    search_config(Config, Section, Key, _Value).

:- pred parse_attr(string::in, attr::out) is semidet.

parse_attr(String, Attr) :-
    % Be case insensitive.
    string.to_lower(String, Lower),
    LR = string.split_at_char('/', Lower),
    (
        LR = [L],
        TokensL0 = string.words(L),
        TokensR0 = []
    ;
        LR = [L, R],
        TokensL0 = string.words(L),
        TokensR0 = string.words(R)
    ),

    attribute(Attrib, TokensL0, TokensL1),
    colour(Foreground, TokensL1, TokensL),
    TokensL = [],

    colour(Background, TokensR0, TokensR),
    TokensR = [],

    Attr = Attrib + fg_bg(Foreground, Background).

:- pred attribute(attr::out, list(string)::in, list(string)::out) is det.

attribute(Attrib, !Tokens) :-
    (
        !.Tokens = [Token | !:Tokens],
        attribute(Token, AttribPrime)
    ->
        Attrib = AttribPrime
    ;
        Attrib = normal
    ).

:- pred colour(colour::out, list(string)::in, list(string)::out) is det.

colour(Colour, !Tokens) :-
    (
        !.Tokens = [Token | !:Tokens],
        colour(Token, ColourPrime)
    ->
        Colour = ColourPrime
    ;
        Colour = default
    ).

:- pred attribute(string::in, attr::out) is semidet.

attribute("normal", normal).
attribute("bold", bold).

:- pred colour(string::in, colour::out) is semidet.

colour("default", default).
colour("black", black).
colour("red", red).
colour("green", green).
colour("yellow", yellow).
colour("blue", blue).
colour("magenta", magenta).
colour("cyan", cyan).
colour("white", white).

%-----------------------------------------------------------------------------%

:- func default_generic_attrs = generic_attrs.

default_generic_attrs =
    generic_attrs(
        bold(yellow, red),
        bold(blue),
        bold(magenta),
        normal,
        bold(red),
        normal,
        normal,
        bold(red),
        bold(red),
        normal,
        bold(cyan),
        normal(white, red)
    ).

:- func default_status_attrs = status_attrs.

default_status_attrs =
    status_attrs(
        normal(white, blue),
        bold(cyan),
        bold(red),
        normal
    ).

:- func default_pager_attrs(generic_attrs) = pager_attrs.

default_pager_attrs(GenericAttrs) =
    pager_attrs(
        GenericAttrs,
        normal,
        bold(blue),
        normal(green),
        normal,
        bold(cyan),
        bold(red),
        bold(yellow),
        bold(green),
        normal(magenta),
        bold(magenta),
        normal(magenta), % part_head_low
        normal(magenta),
        normal(magenta),
        bold(blue)
    ).

:- func default_index_attrs(generic_attrs) = index_attrs.

default_index_attrs(GenericAttrs0) =
    index_attrs(
        GenericAttrs0,
        normal(green),
        bold(black)
    ).

:- func default_thread_attrs(generic_attrs, status_attrs) = thread_attrs.

default_thread_attrs(GenericAttrs, StatusAttrs) = Attrs :-
    Attrs = thread_attrs(
        GenericAttrs,
        StatusAttrs,
        normal(magenta)
    ).

:- func default_compose_attrs(generic_attrs, status_attrs) = compose_attrs.

default_compose_attrs(GenericAttrs, StatusAttrs) =
    compose_attrs(
        GenericAttrs,
        StatusAttrs,
        bold(blue),
        normal(red)
    ).

:- func normal(colour) = attr.

normal(F) = fg_bg(F, default).

:- func normal(colour, colour) = attr.

normal(F, B) = fg_bg(F, B).

:- func bold(colour) = attr.

bold(F) = fg_bg(F, default) + bold.

:- func bold(colour, colour) = attr.

bold(F, B) = fg_bg(F, B) + bold.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
