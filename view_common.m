% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module view_common.
:- interface.

:- import_module text_entry.

:- type common_history
    --->    common_history(
                ch_limit_history    :: history,
                ch_internal_search_history :: history,
                ch_tag_history      :: history,
                ch_to_history       :: history,
                ch_subject_history  :: history,
                ch_prog_history     :: history,
                ch_save_history     :: history
            ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
