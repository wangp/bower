% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module color.
:- interface.

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
                field_body      :: attr
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
                p_part_message  :: attr,
                p_fold          :: attr,
                p_separator     :: attr
            ).

:- type index_attrs
    --->    index_attrs(
                i_generic       :: generic_attrs,
                i_count         :: attr
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

:- func default_generic_attrs = generic_attrs.
:- func default_status_attrs = status_attrs.
:- func default_pager_attrs = pager_attrs.
:- func default_index_attrs = index_attrs.
:- func default_thread_attrs = thread_attrs.
:- func default_compose_attrs = compose_attrs.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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
        normal
    ).

default_status_attrs =
    status_attrs(
        normal(white, blue),
        bold(cyan),
        bold(red),
        normal
    ).

default_pager_attrs =
    pager_attrs(
        default_generic_attrs,
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
        normal(magenta),
        normal(magenta),
        bold(blue)
    ).

default_index_attrs =
    index_attrs(
        default_generic_attrs,
        normal(green)
    ).

default_thread_attrs = Attrs :-
    GenericAttrs = ((
        default_generic_attrs
            ^ author := normal)
            ^ subject := normal(green)),
    Attrs = thread_attrs(
        GenericAttrs,
        default_status_attrs,
        normal(magenta)
    ).

default_compose_attrs =
    compose_attrs(
        default_generic_attrs,
        default_status_attrs,
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
