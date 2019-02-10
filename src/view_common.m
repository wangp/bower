% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module view_common.
:- interface.

:- import_module prog_config.
:- import_module text_entry.

:- type common_history
    --->    common_history(
                ch_limit_history    :: history,
                ch_internal_search_history :: history,
                ch_tag_history      :: history,
                ch_to_history       :: history,
                ch_subject_history  :: history,
                ch_open_part_history:: history,
                ch_open_url_history :: history,
                ch_pipe_id_history  :: history,
                ch_save_history     :: history
            ).

:- pred init_common_history(prog_config::in, common_history::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

init_common_history(Config, CommonHistory) :-
    get_open_part_command(Config, OpenPart),
    get_open_url_command(Config, OpenUrl),
    get_pipe_id_command(Config, PipeId),

    CommonHistory ^ ch_limit_history = init_history,
    CommonHistory ^ ch_internal_search_history = init_history,
    CommonHistory ^ ch_tag_history = init_history,
    CommonHistory ^ ch_to_history = init_history,
    CommonHistory ^ ch_subject_history = init_history,
    CommonHistory ^ ch_open_part_history = init_history(OpenPart),
    CommonHistory ^ ch_open_url_history = init_history(OpenUrl),
    CommonHistory ^ ch_pipe_id_history = init_history(PipeId),
    CommonHistory ^ ch_save_history = init_history.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
