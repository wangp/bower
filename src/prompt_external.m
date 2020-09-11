% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module prompt_external.
:- interface.

:- import_module io.
:- import_module screen.
:- import_module pager.

:- type action_after_prompt
    --->    redraw
    ;       press_key_to_delete(string)
    ;       continue.

:- pred save_part(screen::in, action_after_prompt::out, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

:- pred open_part(screen::in, action_after_prompt::out, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module data.
:- import_module prog_config.
:- import_module view_common.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module parsing_utils.
:- import_module string.

:- import_module make_temp.
:- import_module path_expand.
:- import_module quote_arg.
:- import_module shell_word.
:- import_module string_util.
:- import_module text_entry.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- func decrypt_arg(maybe_decrypted) = string.

decrypt_arg(is_decrypted) = decrypt_arg_bool(yes).
decrypt_arg(not_decrypted) = decrypt_arg_bool(no).

:- func decrypt_arg_bool(bool) = string.

decrypt_arg_bool(yes) = "--decrypt".
decrypt_arg_bool(no) = "--decrypt=false".

%-----------------------------------------------------------------------------%

save_part(Screen, Action, MessageUpdate, !Info, !IO) :-
    ( get_highlighted_thing(!.Info, highlighted_part(Part, MaybeSubject)) ->
        %Action = prompt_save_part(Part, MaybeSubject),
        prompt_save_part(Screen, Part, MaybeSubject, !Info, !IO),
        MessageUpdate = no_change,
        Action = redraw
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_save_part(screen::in, part::in, maybe(header_value)::in,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

prompt_save_part(Screen, Part, MaybeSubject, !Info, !IO) :-
    MessageId = Part ^ pt_msgid,
    MaybePartId = Part ^ pt_part,
    MaybePartFilename = Part ^ pt_filename,
    (
        MaybePartFilename = yes(filename(PartFilename))
    ;
        MaybePartFilename = no,
        MaybeSubject = yes(Subject),
        make_filename_from_subject(Subject, PartFilename)
    ;
        MaybePartFilename = no,
        MaybeSubject = no,
        MessageId = message_id(IdStr),
        (
            MaybePartId = yes(part_id(PartIdInt)),
            PartFilename = string.format("%s.part_%d", [s(IdStr), i(PartIdInt)])
        ;
            MaybePartId = yes(part_id_string(PartIdStr)),
            PartFilename = string.format("%s.part_%s", [s(IdStr), s(PartIdStr)])
        ;
            MaybePartId = no,
            ( IdStr \= "" ->
                PartFilename = IdStr ++ ".part"
            ;
                PartFilename = "message.part"
            )
        )
    ),
    History0 = !.Info ^ p_history ^ ch_save_history,
    make_save_part_initial_prompt(History0, PartFilename, Initial),
    get_home_dir(Home, !IO),
    text_entry_initial(Screen, "Save to file: ", History0, Initial,
        complete_path(Home), Return, !IO),
    (
        Return = yes(FileName0),
        FileName0 \= ""
    ->
        add_history_nodup(FileName0, History0, History),
        !Info ^ p_history ^ ch_save_history := History,
        expand_tilde_home(Home, FileName0, FileName),
        FollowSymLinks = no,
        io.file_type(FollowSymLinks, FileName, ResType, !IO),
        (
            ResType = ok(_),
            % XXX prompt to overwrite
            Error = FileName ++ " already exists.",
            MessageUpdate = set_warning(Error)
        ;
            ResType = error(_),
            % This assumes the file doesn't exist.
            Config = !.Info ^ p_config,
            do_save_part(Config, Part, FileName, Res, !IO),
            (
                Res = ok,
                ( MaybePartId = yes(part_id(0)) ->
                    MessageUpdate = set_info("Message saved.")
                ;
                    MessageUpdate = set_info("Attachment saved.")
                )
            ;
                Res = error(Error),
                MessageUpdate = set_warning(Error)
            )
        )
    ;
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred make_filename_from_subject(header_value::in, string::out) is det.

make_filename_from_subject(Subject, Filename) :-
    SubjectString = header_value_string(Subject),
    string.to_char_list(SubjectString, CharList0),
    list.filter_map(replace_subject_char, CharList0, CharList),
    string.from_char_list(CharList, Filename).

:- pred replace_subject_char(char::in, char::out) is semidet.

replace_subject_char(C0, C) :-
    (
        ( char.is_alnum_or_underscore(C0)
        ; C0 = ('+')
        ; C0 = ('-')
        ; C0 = ('.')
        ; char.to_int(C0) >= 0x80
        )
    ->
        C = C0
    ;
        ( C0 = (' ')
        ; C0 = ('/')
        ; C0 = ('\\')
        ; C0 = (':')
        ),
        C = ('-')
    ).

:- pred make_save_part_initial_prompt(history::in, string::in, string::out)
    is det.

make_save_part_initial_prompt(History, PartFilename, Initial) :-
    choose_text_initial(History, "", PrevFilename),
    dir.dirname(PrevFilename, PrevDirName),
    ( PrevDirName = "." ->
        Initial = PartFilename
    ;
        Initial = PrevDirName / PartFilename
    ).

:- pred do_save_part(prog_config::in, part::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

do_save_part(Config, Part, FileName, Res, !IO) :-
    (
        Part ^ pt_content = text(PartContent)
    ->
        tell(FileName, FileWrite, !IO),
        write_string(PartContent, !IO),
        told(!IO),
        (
            FileWrite = ok, Res = ok
        ;
            FileWrite = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MessageId = Part ^ pt_msgid,
        MaybePartId = Part ^ pt_part,
        IsDecrypted = Part ^ pt_decrypted,
        (
            MaybePartId = yes(PartId),
            get_notmuch_command(Config, Notmuch),
            make_quoted_command(Notmuch, [
                "show", "--format=raw", decrypt_arg(IsDecrypted),
                part_id_to_part_option(PartId),
                "--", message_id_to_search_term(MessageId)
            ], no_redirect, redirect_output(FileName), Command),
            % Decryption may invoke pinentry-curses.
            curs.soft_suspend(io.call_system(Command), CallRes, !IO)
        ;
            MaybePartId = no,
            CallRes = error(io.make_io_error("no part id"))
        ),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                Res = ok
            ;
                string.format("notmuch show returned exit status %d",
                    [i(ExitStatus)], Msg),
                Res = error(Msg)
            )
        ;
            CallRes = error(Error),
            Res = error(io.error_message(Error))
        )
    ).

%-----------------------------------------------------------------------------%

open_part(Screen, Action, MessageUpdate, !Info, !IO) :-
    ( get_highlighted_thing(!.Info, Thing) ->
        (
            Thing = highlighted_part(Part, _MaybeFilename),
            prompt_open_part(Screen, Part, MessageUpdate, Tempfile,
                !Info, !IO),
            (
                Tempfile = yes(FileName),
                Action = press_key_to_delete(FileName)
            ;
                Tempfile = no, Action = redraw
            )
        ;
            Thing = highlighted_url(Url),
            prompt_open_url(Screen, Url, MessageUpdate, !Info, !IO),
            Action = redraw
        ;
            Thing = highlighted_fold_marker,
            % This is a bit ugly as we will end up looking up the line again.
            % Action = toggle_content(toggle_expanded)
            Action = continue,
            MessageUpdate = clear_message
        )
    ;
        Action = continue,
        MessageUpdate = set_warning("No message or attachment selected.")
    ).

:- pred prompt_open_part(screen::in, part::in, message_update::out,
    maybe(string)::out, pager_info::in, pager_info::out, io::di, io::uo)
    is det.

prompt_open_part(Screen, Part, MessageUpdate, Tempfile, !Info, !IO) :-
    History0 = !.Info ^ p_history ^ ch_open_part_history,
    text_entry(Screen, "Open with command: ", History0, complete_none,
        Return, !IO),
    (
        Return = yes(Command1),
        Command1 \= ""
    ->
        add_history_nodup(Command1, History0, History),
        !Info ^ p_history ^ ch_open_part_history := History,
        Config = !.Info ^ p_config,
        do_open_part(Config, Screen, Part, Command1, MessageUpdate,
            Tempfile, !Info, !IO)
    ;
        MessageUpdate = clear_message,
        Tempfile = no
    ).

:- pred do_open_part(prog_config::in, screen::in, part::in, string::in,
    message_update::out, maybe(string)::out, pager_info::in, pager_info::out,
    io::di, io::uo) is det.

do_open_part(Config, Screen, Part, Command, MessageUpdate, Tempfile,
        !Info, !IO) :-
    promise_equivalent_solutions [MessageUpdate, Tempfile, !:Info, !:IO] (
        shell_word.split(Command, ParseResult),
        (
            ParseResult = ok([]),
            % Should not happen.
            MessageUpdate = clear_message,
            Tempfile = no
        ;
            ParseResult = ok(CommandWords),
            CommandWords = [_ | _],
            do_open_part_2(Config, Screen, Part, CommandWords, MessageUpdate,
                Tempfile, !Info, !IO)
        ;
            (
                ParseResult = error(yes(Error), _Line, Column),
                Message = string.format("parse error at column %d: %s",
                    [i(Column), s(Error)])
            ;
                ParseResult = error(no, _Line, Column),
                Message = string.format("parse error at column %d",
                    [i(Column)])
            ),
            MessageUpdate = set_warning(Message),
            Tempfile = no
        )
    ).

:- pred do_open_part_2(prog_config::in, screen::in, part::in,
    list(word)::in(non_empty_list), message_update::out, maybe(string)::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

do_open_part_2(Config, Screen, Part, CommandWords, MessageUpdate, Tempfile,
        !Info, !IO) :-
    MaybePartFileName = Part ^ pt_filename,
    (
        MaybePartFileName = yes(filename(PartFilename)),
        get_extension(PartFilename, Ext)
    ->
        make_temp_suffix(Ext, Res0, !IO)
    ;
        make_temp_suffix("", Res0, !IO)
    ),
    (
        Res0 = ok(FileName),
        do_save_part(Config, Part, FileName, Res, !IO),
        (
            Res = ok,
            call_open_command(Screen, CommandWords, FileName, MaybeError, !IO),
            (
                MaybeError = ok,
                Msg = "Press any key to continue (deletes temporary file)",
                MessageUpdate = set_info(Msg),
                Tempfile = yes(FileName)
            ;
                MaybeError = error(Msg),
                MessageUpdate = set_warning(Msg),
                Tempfile = no
            )
        ;
            Res = error(Error),
            string.format("Error saving to %s: %s", [s(FileName), s(Error)],
                Msg),
            MessageUpdate = set_warning(Msg),
            Tempfile = no
        )
    ;
        Res0 = error(Error),
        string.format("Error opening temporary file: %s", [s(Error)], Msg),
        MessageUpdate = set_warning(Msg),
        Tempfile = no
    ).

:- pred call_open_command(screen::in, list(word)::in(non_empty_list),
    string::in, maybe_error::out, io::di, io::uo) is det.

call_open_command(Screen, CommandWords, Arg, MaybeError, !IO) :-
    make_open_command(CommandWords, Arg, CommandToShow, CommandToRun, Bg),
    CallMessage = set_info("Calling " ++ CommandToShow ++ "..."),
    update_message_immed(Screen, CallMessage, !IO),
    (
        Bg = run_in_background,
        io.call_system(CommandToRun, CallRes, !IO)
    ;
        Bg = run_in_foreground,
        curs.suspend(io.call_system(CommandToRun), CallRes, !IO)
    ),
    (
        CallRes = ok(ExitStatus),
        ( ExitStatus = 0 ->
            MaybeError = ok
        ;
            string.format("%s returned with exit status %d",
                [s(CommandToShow), i(ExitStatus)], Msg),
            MaybeError = error(Msg)
        )
    ;
        CallRes = error(Error),
        MaybeError = error("Error: " ++ io.error_message(Error))
    ).

:- pred make_open_command(list(word)::in(non_empty_list), string::in,
    string::out, string::out, run_in_background::out) is det.

make_open_command(CommandWords0, Arg, CommandToShow, CommandToRun, Bg) :-
    remove_bg_operator(CommandWords0, CommandWords, Bg),
    WordStrings = list.map(word_string, CommandWords),
    CommandToShow = string.join_list(" ", WordStrings),
    CommandPrefix = command_prefix(
        shell_quoted(string.join_list(" ", list.map(quote_arg, WordStrings))),
        ( detect_ssh(CommandWords) -> quote_twice ; quote_once )
    ),
    (
        Bg = run_in_background,
        make_quoted_command(CommandPrefix, [Arg],
            redirect_input("/dev/null"), redirect_output("/dev/null"),
            redirect_stderr("/dev/null"), run_in_background,
            CommandToRun)
    ;
        Bg = run_in_foreground,
        make_quoted_command(CommandPrefix, [Arg], no_redirect, no_redirect,
            CommandToRun)
    ).

:- pred remove_bg_operator(list(word)::in(non_empty_list), list(word)::out,
    run_in_background::out) is det.

remove_bg_operator(Words0, Words, Bg) :-
    (
        list.split_last(Words0, ButLast, Last0),
        remove_bg_operator_2(Last0, Last)
    ->
        (
            Last = word([]),
            Words = ButLast
        ;
            Last = word([_ | _]),
            Words = ButLast ++ [Last]
        ),
        Bg = run_in_background
    ;
        Words = Words0,
        Bg = run_in_foreground
    ).

:- pred remove_bg_operator_2(word::in, word::out) is semidet.

remove_bg_operator_2(word(Segments0), word(Segments)) :-
    list.split_last(Segments0, ButLast, Last0),
    Last0 = unquoted(LastString0),
    string.remove_suffix(LastString0, "&", LastString),
    ( LastString = "" ->
        Segments = ButLast
    ;
        Segments = ButLast ++ [unquoted(LastString)]
    ).

%-----------------------------------------------------------------------------%

:- pred prompt_open_url(screen::in, string::in, message_update::out,
    pager_info::in, pager_info::out, io::di, io::uo) is det.

prompt_open_url(Screen, Url, MessageUpdate, !Info, !IO) :-
    History0 = !.Info ^ p_history ^ ch_open_url_history,
    % No completion for command inputs yet.
    text_entry(Screen, "Open URL with command: ", History0, complete_none,
        Return, !IO),
    (
        Return = yes(Command1),
        Command1 \= ""
    ->
        add_history_nodup(Command1, History0, History),
        !Info ^ p_history ^ ch_open_url_history := History,
        do_open_url(Screen, Command1, Url, MessageUpdate, !IO)
    ;
        MessageUpdate = clear_message
    ).

:- pred do_open_url(screen::in, string::in, string::in, message_update::out,
    io::di, io::uo) is det.

do_open_url(Screen, Command, Url, MessageUpdate, !IO) :-
    promise_equivalent_solutions [MessageUpdate, !:IO] (
        shell_word.split(Command, ParseResult),
        (
            ParseResult = ok([]),
            % Should not happen.
            MessageUpdate = clear_message
        ;
            ParseResult = ok(CommandWords),
            CommandWords = [_ | _],
            call_open_command(Screen, CommandWords, Url, MaybeError, !IO),
            (
                MaybeError = ok,
                MessageUpdate = no_change
            ;
                MaybeError = error(Msg),
                MessageUpdate = set_warning(Msg)
            )
        ;
            (
                ParseResult = error(yes(Error), _Line, Column),
                Message = string.format("parse error at column %d: %s",
                    [i(Column), s(Error)])
            ;
                ParseResult = error(no, _Line, Column),
                Message = string.format("parse error at column %d",
                    [i(Column)])
            ),
            MessageUpdate = set_warning(Message)
        )
    ).

