%-----------------------------------------------------------------------------%

:- module recall.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module screen.

:- pred select_recall(screen::in, maybe(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.
:- import_module maildir.
:- import_module message_file.
:- import_module scrollable.

%-----------------------------------------------------------------------------%

:- type recall_info
    --->    recall_info(
                r_scrollable    :: scrollable(recall_line)
            ).

:- type recall_line
    --->    recall_line(
                r_filename      :: string,
                r_to            :: string,
                r_subject       :: string
            ).

:- instance scrollable.line(recall_line) where [
    pred(draw_line/5) is draw_recall_line
].

%-----------------------------------------------------------------------------%

select_recall(Screen, MaybeFileName, !IO) :-
    find_drafts(ResFind, !IO),
    (
        ResFind = ok([]),
        update_message(Screen, set_warning("No postponed messages."), !IO),
        MaybeFileName = no
    ;
        ResFind = ok(FileNames),
        FileNames = [_ | _],
        list.map_foldl(make_recall_line, FileNames, Lines, !IO),
        Scrollable = scrollable.init_with_cursor(Lines, 0),
        Info = recall_info(Scrollable),
        update_message(Screen, clear_message, !IO),
        recall_screen_loop(Screen, MaybeFileName, Info, _Info, !IO)
    ;
        ResFind = error(Error),
        Msg = io.error_message(Error),
        update_message(Screen, set_warning(Msg), !IO),
        MaybeFileName = no
    ).

:- pred make_recall_line(string::in, recall_line::out, io::di, io::uo) is det.

make_recall_line(FileName, Line, !IO) :-
    parse_message_file(FileName, Res, !IO),
    (
        Res = ok(Headers - _Body),
        To = Headers ^ h_to,
        Subject = Headers ^ h_subject
    ;
        Res = error(_),
        % XXX what to do?
        To = "(error)",
        Subject = "(" ++ FileName ++ ")"
    ),
    Line = recall_line(FileName, To, Subject).

%-----------------------------------------------------------------------------%

:- pred recall_screen_loop(screen::in, maybe(string)::out,
    recall_info::in, recall_info::out, io::di, io::uo) is det.

recall_screen_loop(Screen, MaybeFileName, !Info, !IO) :-
    draw_recall(Screen, !.Info, !IO),
    panel.update_panels(!IO),
    get_char(Char, !IO),
    ( Char = 'j' ->
        move_cursor(Screen, 1, !Info, !IO),
        recall_screen_loop(Screen, MaybeFileName, !Info, !IO)
    ; Char = 'k' ->
        move_cursor(Screen, -1, !Info, !IO),
        recall_screen_loop(Screen, MaybeFileName, !Info, !IO)
    ; Char = 'q' ->
        update_message(Screen, clear_message, !IO),
        MaybeFileName = no
    ; Char = '\r' ->
        enter(!.Info, MaybeFileName)
    ;
        recall_screen_loop(Screen, MaybeFileName, !Info, !IO)
    ).

:- pred move_cursor(screen::in, int::in, recall_info::in, recall_info::out,
    io::di, io::uo) is det.

move_cursor(Screen, Delta, !Info, !IO) :-
    NumRows = list.length(Screen ^ main_panels),
    !.Info ^ r_scrollable = Scrollable0,
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

:- pred enter(recall_info::in, maybe(string)::out) is det.

enter(Info, MaybeFileName) :-
    Scrollable = Info ^ r_scrollable,
    ( get_cursor_line(Scrollable, CursorLine) ->
        FileName = CursorLine ^ r_filename,
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no
    ).

%-----------------------------------------------------------------------------%

:- pred draw_recall(screen::in, recall_info::in, io::di, io::uo) is det.

draw_recall(Screen, Info, !IO) :-
    Panels = Screen ^ main_panels,
    Info = recall_info(Scrollable),
    scrollable.draw(Panels, Scrollable, !IO).

:- pred draw_recall_line(panel::in, recall_line::in, bool::in,
    io::di, io::uo) is det.

draw_recall_line(Panel, Line, IsCursor, !IO) :-
    Line = recall_line(_FileName, To, Subject),
    (
        IsCursor = yes,
        panel.attr_set(Panel, fg_bg(yellow, red) + bold, !IO)
    ;
        IsCursor = no
    ),
    FieldAttr = fg_bg(red, black) + bold,
    cond_attr_set(Panel, FieldAttr, IsCursor, !IO),
    my_addstr(Panel, "To: ", !IO),
    cond_attr_set(Panel, normal, IsCursor, !IO),
    my_addstr_fixed(Panel, 35, To, ' ', !IO),
    cond_attr_set(Panel, FieldAttr, IsCursor, !IO),
    my_addstr(Panel, " Subject: ", !IO),
    cond_attr_set(Panel, normal, IsCursor, !IO),
    my_addstr(Panel, Subject, !IO).

:- pred cond_attr_set(panel::in, attr::in, bool::in, io::di, io::uo) is det.

cond_attr_set(Panel, Attr, IsCursor, !IO) :-
    (
        IsCursor = no,
        panel.attr_set(Panel, Attr, !IO)
    ;
        IsCursor = yes
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
