% Bower - a frontend for the Notmuch email system
% Copyright (C) 2014 Peter Wang

:- module rfc5322.
:- interface.

:- import_module list.
:- import_module maybe.

:- include_module rfc5322.parser.
:- include_module rfc5322.writer.

%-----------------------------------------------------------------------------%

:- type ascii_unicode
    --->    ascii(string)
    ;       unicode(string).

:- type atom
    --->    atom(ascii_unicode).

:- type dot_atom
    --->    dot_atom(ascii_unicode).

:- type quoted_string
    --->    quoted_string(ascii_unicode).

:- type word
    --->    word_atom(atom)
    ;       word_quoted_string(quoted_string).

:- type phrase == list(word).

:- type address_list == list(address).

:- type address
    --->    mailbox(mailbox)
    ;       group(
                display_name,
                list(mailbox)
            ).

:- type mailbox
    --->    mailbox(
                maybe(display_name),
                addr_spec
            )
    ;       bad_mailbox(string).

:- type display_name == phrase.

:- type addr_spec
    --->    addr_spec(local_part, domain).

:- type local_part
    --->    lpart_atom(dot_atom)
    ;       lpart_quoted_string(quoted_string).

:- type domain
    --->    domain_name(dot_atom)
    ;       domain_literal(ascii_unicode). % [blah]

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
