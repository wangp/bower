% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module recall.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module data.
:- import_module screen.
:- import_module prog_config.

:- pred select_recall(prog_config::in, screen::in, maybe(thread_id)::in,
    screen_transition(maybe(message))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module set.
:- import_module time.

:- import_module callout.
:- import_module color.
:- import_module maildir.
:- import_module sanitise.
:- import_module scrollable.
:- import_module tags.
:- import_module time_util.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- type recall_info
    --->    recall_info(
                r_config        :: prog_config,
                r_scrollable    :: scrollable(recall_line)
            ).

:- type recall_line
    --->    recall_line(
                r_message       :: message_for_recall,
                r_reldate       :: string,
                r_to            :: presentable_string,
                r_subject       :: presentable_string,
                r_tags          :: set(tag)
            ).

%-----------------------------------------------------------------------------%

select_recall(Config, Screen, MaybeThreadId, Transition, !IO) :-
    find_drafts(Config, MaybeThreadId, Res0, !IO),
    (
        Res0 = ok([]),
        (
            MaybeThreadId = yes(_),
            Message = "No postponed messages for this thread."
        ;
            MaybeThreadId = no,
            Message = "No postponed messages."
        ),
        MaybeSelected = no,
        Transition = screen_transition(MaybeSelected, set_warning(Message))
    ;
        Res0 = ok(MessageIds),
        MessageIds = [_ | _],
        current_timestamp(Time, !IO),
        localtime(Time, Nowish, !IO),
        make_recall_lines(Config, Nowish, MessageIds, Res, [], RevLines, !IO),
        (
            Res = ok,
            reverse(RevLines, Lines),
            Scrollable = scrollable.init_with_cursor(Lines),
            update_message(Screen, clear_message, !IO),
            Info = recall_info(Config, Scrollable),
            recall_screen_loop(Screen, Transition, Info, _Info, !IO)
        ;
            Res = error(Error),
            Transition = screen_transition(no, set_warning(Error))
        )
    ;
        Res0 = error(Error),
        Transition = screen_transition(no, set_warning(Error))
    ).

:- pred make_recall_lines(prog_config::in, tm::in, list(message_id)::in,
    maybe_error::out, list(recall_line)::in, list(recall_line)::out,
    io::di, io::uo) is det.

make_recall_lines(Config, Nowish, MessageIds, Res, !AccLines, !IO) :-
    (
        MessageIds = [],
        Res = ok
    ;
        MessageIds = [MessageId | TailIds],
        make_recall_line(Config, Nowish, MessageId, Res0, !AccLines, !IO),
        (
            Res0 = ok,
            make_recall_lines(Config, Nowish, TailIds, Res, !AccLines, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred make_recall_line(prog_config::in, tm::in, message_id::in,
    maybe_error::out, list(recall_line)::in, list(recall_line)::out,
    io::di, io::uo) is det.

make_recall_line(Config, Nowish, MessageId, Res, !AccLines, !IO) :-
    run_notmuch(Config,
        [
            "show", "--format=json", "--part=0", "--body=false", "--",
            message_id_to_search_term(MessageId)
        ],
        no_suspend_curses,
        parse_message_for_recall, Res0, !IO),
    (
        Res0 = ok(Message),
        Message = message_for_recall(_Id, Timestamp, Headers, Tags),
        To = header_value_string(Headers ^ h_to),
        Subject = header_value_string(Headers ^ h_subject),
        localtime(Timestamp, TM, !IO),
        Shorter = no,
        make_reldate(Nowish, TM, Shorter, RelDate),
        Line = recall_line(Message, RelDate, make_presentable(To),
            make_presentable(Subject), Tags),
        cons(Line, !AccLines),
        Res = ok
    ;
        Res0 = error(Error),
        Res = error("notmuch show: " ++ Error)
    ).

%-----------------------------------------------------------------------------%

:- pred recall_screen_loop(screen::in, screen_transition(maybe(message))::out,
    recall_info::in, recall_info::out, io::di, io::uo) is det.

recall_screen_loop(Screen, Transition, !Info, !IO) :-
    draw_recall(Screen, !.Info, !IO),
    draw_status_bar(Screen, !IO),
    update_panels(Screen, !IO),
    get_keycode_blocking(KeyCode, !IO),
    (
        ( KeyCode = char('j')
        ; KeyCode = code(curs.key_down)
        )
    ->
        move_cursor(Screen, 1, !Info, !IO),
        recall_screen_loop(Screen, Transition, !Info, !IO)
    ;
        ( KeyCode = char('k')
        ; KeyCode = code(curs.key_up)
        )
    ->
        move_cursor(Screen, -1, !Info, !IO),
        recall_screen_loop(Screen, Transition, !Info, !IO)
    ;
        KeyCode = char('q')
    ->
        Transition = screen_transition(no, clear_message)
    ;
        KeyCode = char('\r')
    ->
        enter(!.Info, Transition, !IO)
    ;
        KeyCode = char('d')
    ->
        delete_draft(Screen, !Info, !IO),
        NumLines = get_num_lines(!.Info ^ r_scrollable),
        ( NumLines = 0 ->
            Transition = screen_transition(no, no_change)
        ;
            recall_screen_loop(Screen, Transition, !Info, !IO)
        )
    ;
        KeyCode = code(curs.key_resize)
    ->
        recreate_screen_for_resize(Screen, !IO),
        recall_screen_loop(Screen, Transition, !Info, !IO)
    ;
        recall_screen_loop(Screen, Transition, !Info, !IO)
    ).

:- pred move_cursor(screen::in, int::in, recall_info::in, recall_info::out,
    io::di, io::uo) is det.

move_cursor(Screen, Delta, !Info, !IO) :-
    !.Info ^ r_scrollable = Scrollable0,
    get_main_rows(Screen, NumRows, !IO),
    scrollable.move_cursor(NumRows, Delta, HitLimit, Scrollable0, Scrollable),
    !Info ^ r_scrollable := Scrollable,
    (
        HitLimit = yes,
        ( Delta < 0 ->
            MessageUpdate = set_warning("You are on the first entry.")
        ;
            MessageUpdate = set_warning("You are on the last entry.")
        )
    ;
        HitLimit = no,
        MessageUpdate = clear_message
    ),
    update_message(Screen, MessageUpdate, !IO).

:- pred enter(recall_info::in, screen_transition(maybe(message))::out,
    io::di, io::uo) is det.

enter(Info, Transition, !IO) :-
    Scrollable = Info ^ r_scrollable,
    (
        get_cursor_line(Scrollable, _, CursorLine),
        Message0 = CursorLine ^ r_message,
        Message0 = message_for_recall(MessageId, _, _, _)
    ->
        Info = recall_info(Config, _Scrollable0),
        run_notmuch(Config,
            [
                "show", "--format=json", "--part=0", "--decrypt",
                "--", message_id_to_search_term(MessageId)
            ],
            soft_suspend_curses, % Decryption may invoke pinentry-curses.
            parse_message, Result, !IO),
        (
            Result = ok(Message),
            (
                Message = message(_, _, _, _, _, _),
                Transition = screen_transition(yes(Message), clear_message)
            ;
                Message = excluded_message(_, _, _, _, _),
                Error = "Cannot recall excluded message",
                Transition = screen_transition(no, set_warning(Error))
            )
        ;
            Result = error(Error0),
            Error = "notmuch show: " ++ Error0,
            Transition = screen_transition(no, set_warning(Error))
        )
    ;
        Transition = screen_transition(no, clear_message)
    ).

:- pred delete_draft(screen::in, recall_info::in, recall_info::out,
    io::di, io::uo) is det.

delete_draft(Screen, !Info, !IO) :-
    !.Info = recall_info(Config, Scrollable0),
    (
        get_cursor_line(Scrollable0, _, CursorLine0),
        delete_cursor_line(Scrollable0, Scrollable)
    ->
        Message = CursorLine0 ^ r_message,
        Message = message_for_recall(MessageId, _, _, _),
        tag_messages(Config, [tag_delta("+deleted")], [MessageId], Res, !IO),
        (
            Res = ok,
            !Info ^ r_scrollable := Scrollable,
            MessageUpdate = set_info("Draft message deleted.")
        ;
            Res = error(Error),
            MessageUpdate = set_warning(Error)
        ),
        update_message(Screen, MessageUpdate, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred draw_recall(screen::in, recall_info::in, io::di, io::uo) is det.

draw_recall(Screen, Info, !IO) :-
    get_main_panels(Screen, Panels, !IO),
    Info = recall_info(Config, Scrollable),
    Attrs = generic_attrs(Config),
    scrollable.draw(draw_recall_line(Attrs), Screen, Panels, Scrollable, !IO).

:- pred draw_recall_line(generic_attrs::in, screen::in, vpanel::in,
    recall_line::in, int::in, bool::in, io::di, io::uo) is det.

draw_recall_line(Attrs, Screen, Panel, Line, _LineNr, IsCursor, !IO) :-
    Line = recall_line(_FileName, RelDate, presentable_string(To),
        presentable_string(Subject), Tags),
    (
        IsCursor = yes,
        RelDateAttr = Attrs ^ current
    ;
        IsCursor = no,
        RelDateAttr = Attrs ^ relative_date
    ),
    draw_fixed(Screen, Panel, RelDateAttr, 13, RelDate, ' ', !IO),

    NameAttr = Attrs ^ field_name,
    BodyAttr = Attrs ^ field_body,
    mattr_draw(Screen, Panel, unless(IsCursor, NameAttr), "To: ", !IO),
    mattr_draw_fixed(Screen, Panel, unless(IsCursor, BodyAttr), 25, To, ' ',
        !IO),
    mattr_draw(Screen, Panel, unless(IsCursor, NameAttr), " Subject: ", !IO),
    mattr_draw(Screen, Panel, unless(IsCursor, BodyAttr), Subject, !IO),
    attr(Screen, Panel, Attrs ^ other_tag, !IO),
    set.fold(draw_display_tag(Screen, Panel), Tags, !IO).

:- pred draw_display_tag(screen::in, vpanel::in, tag::in, io::di, io::uo)
    is det.

draw_display_tag(Screen, Panel, Tag, !IO) :-
    (
        Tag \= draft_tag,
        display_tag(Tag)
    ->
        Tag = tag(TagName),
        draw2(Screen, Panel, " ", TagName, !IO)
    ;
        true
    ).

:- func unless(bool, curs.attr) = maybe(curs.attr).

unless(no, X) = yes(X).
unless(yes, _) = no.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
