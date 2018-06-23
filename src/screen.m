% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module screen.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module color.
:- import_module curs.
:- import_module curs.panel.
:- import_module time_util.

%-----------------------------------------------------------------------------%

:- type screen.

:- type message_update
    --->    no_change
    ;       clear_message
    ;       set_info(string)
    ;       set_warning(string)
    ;       set_prompt(string).

:- type screen_transition(T)
    --->    screen_transition(T, message_update).
            % The caller needs to account for the screen being resized
            % during the call.

:- type screen_resized
    --->    yes
    ;       no.

:- type sigint_received
    --->    sigint_received.

:- pred create_screen(status_attrs::in, screen::uo, io::di, io::uo) is det.

:- pred replace_screen_for_resize(screen::in, screen::out, io::di, io::uo)
    is det.

:- pred fast_forward_screen(screen::in, screen::out, screen_resized::out,
    io::di, io::uo) is det.

:- pred get_cols(screen::in, int::out) is det.

:- pred get_rows_cols(screen::in, int::out, int::out) is det.

:- pred get_main_panels(screen::in, list(panel)::out) is det.

:- pred get_main_rows(screen::in, int::out) is det.

:- pred get_bar_panel(screen::in, panel::out) is det.

:- pred get_msgentry_panel(screen::in, panel::out) is det.

%-----------------------------------------------------------------------------%

    % The draw routines are like curs.addstr but doesn't add the string if the
    % cursor is already at the end of the panel.
    %
    % The draw_fixed routines additionally truncate the string if it would
    % exceed the given number of columns.  Afterwards the cursor is placed
    % NrCols after the original position (or otherwise the right margin of the
    % panel).

:- pred attr(panel::in, attr::in, io::di, io::uo) is det.

:- pred mattr(panel::in, maybe(attr)::in, io::di, io::uo) is det.

:- pred draw(panel::in, string::in, io::di, io::uo) is det.

:- pred draw2(panel::in, string::in, string::in, io::di, io::uo) is det.

:- pred draw(panel::in, attr::in, string::in, io::di, io::uo) is det.

:- pred draw(panel::in, attr::in, string::in, int::in, int::in,
    io::di, io::uo) is det.

:- pred draw_fixed(panel::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

:- pred draw_fixed(panel::in, attr::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

:- pred mattr_draw(panel::in, maybe(attr)::in, string::in, io::di, io::uo)
    is det.

:- pred mattr_draw_fixed(panel::in, maybe(attr)::in, int::in, string::in,
    char::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred update_message(screen::in, message_update::in, io::di, io::uo) is det.

:- pred update_message_immed(screen::in, message_update::in, io::di, io::uo)
    is det.

:- pred draw_status_bar(screen::in, io::di, io::uo) is det.
:- pred draw_status_bar(screen::in, maybe(string)::in, maybe(string)::in,
    io::di, io::uo) is det.

:- type keycode
    --->    char(char)
    ;       meta(char)
    ;       code(int)
    ;       metacode(int)
    ;       timeout_or_error.

:- pred get_keycode_blocking(keycode::out, io::di, io::uo) is det.

:- pred get_keycode_async_aware(maybe(timestamp)::in, keycode::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module mutvar.
:- import_module require.
:- import_module string.

:- import_module async.
:- import_module signal.
:- import_module string_util.

%-----------------------------------------------------------------------------%

:- type screen == mutvar(screen_version).

:- type screen_version
    --->    screen(
                rows            :: int,
                cols            :: int,
                main_panels     :: list(panel),
                bar_panel       :: panel,
                msgentry_panel  :: panel,
                status_attrs    :: status_attrs
            )
    ;       forward_screen(
                % This version of the screen is obsolete,
                % replaced by a forwarding pointer.
                old_rows        :: int,
                old_cols        :: int,
                fwd_screen      :: screen
            ).

:- inst real_screen
    --->    screen(ground, ground, ground, ground, ground, ground).

%-----------------------------------------------------------------------------%

create_screen(Attrs, Screen, !IO) :-
    promise_pure (
        curs.rows_cols(Rows, Cols, !IO),
        MainRows = Rows - 2,
        BarRow = MainRows,
        MsgEntryRow = BarRow + 1,

        list.map_foldl(create_row_panel(Cols), 0..(MainRows - 1), MainPanels,
            !IO),
        create_row_panel(Cols, BarRow, BarPanel, !IO),
        create_row_panel(Cols, MsgEntryRow, MsgEntryPanel, !IO),
        RealScreen0 = screen(Rows, Cols, MainPanels, BarPanel, MsgEntryPanel,
            Attrs),
        % This is safe because RealScreen0 is definitely dynamically allocated.
        unsafe_promise_unique(RealScreen0, RealScreen),
        impure new_mutvar(RealScreen, Screen)
    ).

:- pred create_row_panel(int::in, int::in, panel::out, io::di, io::uo) is det.

create_row_panel(Cols, Row, Panel, !IO) :-
    panel.new(1, Cols, Row, 0, normal, Panel, !IO),
    panel.scrollok(Panel, no, !IO).

%-----------------------------------------------------------------------------%

replace_screen_for_resize(Screen0, Screen, !IO) :-
    promise_pure (
        impure destroy_screen(Screen0, OldRows, OldCols, Attrs, !IO),
        create_screen(Attrs, Screen, !IO),
        % Replace Screen0 by a forwarding pointer.
        impure set_mutvar(Screen0, forward_screen(OldRows, OldCols, Screen))
    ).

:- impure pred destroy_screen(screen::in, int::out, int::out,
    status_attrs::out, io::di, io::uo) is det.

destroy_screen(Screen, OldRows, OldCols, Attrs, !IO) :-
    impure get_mutvar(Screen, ScreenVersion),
    (
        ScreenVersion = screen(OldRows, OldCols, MainPanels, BarPanel,
            MsgEntryPanel, Attrs),
        % impure set_mutvar(Screen, destroyed_screen),
        list.foldl(panel.delete, [BarPanel, MsgEntryPanel | MainPanels], !IO)
    ;
        ScreenVersion = forward_screen(_, _, _),
        unexpected($module, $pred, "screen already destroyed")
    ).

%-----------------------------------------------------------------------------%

fast_forward_screen(Screen0, Screen, Resized, !IO) :-
    promise_pure (
        impure get_mutvar(Screen0, ScreenVersion),
        (
            ScreenVersion = screen(_, _, _, _, _, _),
            Screen = Screen0,
            Resized = no
        ;
            ScreenVersion = forward_screen(OldRows, OldCols, Screen1),
            impure fast_forward_screen_2(Screen1, Screen, Rows, Cols),
            Resized = ( Rows = OldRows, Cols = OldCols -> yes ; no )
        )
    ).

:- impure pred fast_forward_screen_2(screen::in, screen::out,
    int::out, int::out) is det.

fast_forward_screen_2(Screen0, Screen, Rows, Cols) :-
    impure get_mutvar(Screen0, ScreenVersion),
    (
        ScreenVersion = screen(Rows, Cols, _, _, _, _),
        Screen = Screen0
    ;
        ScreenVersion = forward_screen(_, _, Screen1),
        impure fast_forward_screen_2(Screen1, Screen, Rows, Cols)
    ).

%-----------------------------------------------------------------------------%

get_cols(Screen, Cols) :-
    get_real_screen(Screen, RealScreen),
    Cols = RealScreen ^ cols.

get_rows_cols(Screen, Rows, Cols) :-
    get_real_screen(Screen, RealScreen),
    Rows = RealScreen ^ rows,
    Cols = RealScreen ^ cols.

get_main_panels(Screen, MainPanels) :-
    get_real_screen(Screen, RealScreen),
    MainPanels = RealScreen ^ main_panels.

get_main_rows(Screen, NumRows) :-
    get_main_panels(Screen, MainPanels),
    NumRows = list.length(MainPanels).

get_bar_panel(Screen, BarPanel) :-
    get_real_screen(Screen, RealScreen),
    BarPanel = RealScreen ^ bar_panel.

get_msgentry_panel(Screen, MsgEntryPanel) :-
    get_real_screen(Screen, RealScreen),
    MsgEntryPanel = RealScreen ^ msgentry_panel.

:- pred get_status_attrs(screen::in, status_attrs::out) is det.

get_status_attrs(Screen, Attrs) :-
    get_real_screen(Screen, RealScreen),
    Attrs = RealScreen ^ status_attrs.

:- pred get_real_screen(screen::in, screen_version::out(real_screen)) is det.

get_real_screen(Screen, RealScreen) :-
    promise_pure (
        impure get_mutvar(Screen, RealScreen),
        (
            RealScreen = screen(_, _, _, _, _, _)
        ;
            RealScreen = forward_screen(_, _, _),
            unexpected($module, $pred, "screen already destroyed")
        )
    ).

%-----------------------------------------------------------------------------%

attr(Panel, Attr, !IO) :-
    panel.attr_set(Panel, Attr, !IO).

mattr(_Panel, no, !IO).
mattr(Panel, yes(Attr), !IO) :-
    attr(Panel, Attr, !IO).

draw(Panel, String, !IO) :-
    my_addstr(Panel, String, !IO).

draw2(Panel, StringA, StringB, !IO) :-
    my_addstr(Panel, StringA, StringB, !IO).

draw(Panel, Attr, String, !IO) :-
    panel.attr_set(Panel, Attr, !IO),
    draw(Panel, String, !IO).

draw(Panel, Attr, String, Start, End, !IO) :-
    panel.attr_set(Panel, Attr, !IO),
    draw(Panel, string.between(String, Start, End), !IO).

draw_fixed(Panel, NrCols, String, PadChar, !IO) :-
    my_addstr_fixed(Panel, NrCols, String, PadChar, !IO).

draw_fixed(Panel, Attr, NrCols, String, PadChar, !IO) :-
    panel.attr_set(Panel, Attr, !IO),
    my_addstr_fixed(Panel, NrCols, String, PadChar, !IO).

mattr_draw(Panel, MaybeAttr, String, !IO) :-
    mattr(Panel, MaybeAttr, !IO),
    draw(Panel, String, !IO).

mattr_draw_fixed(Panel, MaybeAttr, NrCols, String, PadChar, !IO) :-
    mattr(Panel, MaybeAttr, !IO),
    draw_fixed(Panel, NrCols, String, PadChar, !IO).

%-----------------------------------------------------------------------------%

:- pred my_addstr(panel::in, string::in, io::di, io::uo) is det.

my_addstr(Panel, String, !IO) :-
    getyx(Panel, _, X, !IO),
    getmaxyx(Panel, _, MaxX, !IO),
    % XXX We cannot tell if we have already hit the last column, before or
    % after having drawn to it.  This hack checks the second last column
    % instead, which means it actually prevents drawing one column early.
    % A solution may be to use a curses pad larger than the screen.
    ( X >= MaxX - 1 ->
        true
    ;
        addstr(Panel, String, !IO)
    ).

:- pred my_addstr(panel::in, string::in, string::in, io::di, io::uo) is det.

my_addstr(Panel, StringA, StringB, !IO) :-
    getyx(Panel, _, X, !IO),
    getmaxyx(Panel, _, MaxX, !IO),
    % XXX See comment in my_addstr.
    ( X >= MaxX - 1 ->
        true
    ;
        addstr(Panel, StringA, !IO),
        addstr(Panel, StringB, !IO)
    ).

:- pred my_addstr_fixed(panel::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

my_addstr_fixed(Panel, NrCols, String, PadChar, !IO) :-
    getyx(Panel, _, X0, !IO),
    getmaxyx(Panel, _, MaxX, !IO),
    % XXX See comment in my_addstr.
    ( X0 >= MaxX - 1 ->
        true
    ;
        MaxRemCols = MaxX - X0,
        ( MaxRemCols < NrCols ->
            RemCols = MaxRemCols
        ;
            RemCols = NrCols
        ),
        Start = 0,
        count_loop(String, RemCols, Start, End),
        addstr(Panel, string.between(String, Start, End), !IO),

        getyx(Panel, _, X1, !IO),
        N = (X0 + NrCols) - X1,
        add_padding(Panel, N, string.from_char(PadChar), !IO)
    ).

:- pred add_padding(panel::in, int::in, string::in, io::di, io::uo) is det.

add_padding(Panel, N, PadChar, !IO) :-
    ( N =< 0 ->
        true
    ;
        panel.addstr(Panel, PadChar, !IO),
        add_padding(Panel, N - 1, PadChar, !IO)
    ).

:- pred count_loop(string::in, int::in, int::in, int::out) is det.

count_loop(String, RemCols, !Index) :-
    (
        RemCols > 0,
        string.unsafe_index_next(String, !Index, Char),
        RemCols1 = RemCols - wcwidth(Char),
        RemCols1 >= 0
    ->
        count_loop(String, RemCols1, !Index)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

update_message(Screen, MessageUpdate, !IO) :-
    get_msgentry_panel(Screen, Panel),
    get_status_attrs(Screen, Attrs),
    (
        MessageUpdate = no_change
    ;
        MessageUpdate = clear_message,
        panel.erase(Panel, !IO)
    ;
        (
            MessageUpdate = set_info(String),
            Attr = Attrs ^ info
        ;
            MessageUpdate = set_warning(String),
            Attr = Attrs ^ warning
        ;
            MessageUpdate = set_prompt(String),
            Attr = Attrs ^ prompt
        ),
        panel.erase(Panel, !IO),
        draw(Panel, Attr, String, !IO)
    ).

update_message_immed(Screen, MessageUpdate, !IO) :-
    update_message(Screen, MessageUpdate, !IO),
    panel.update_panels(!IO).

%-----------------------------------------------------------------------------%

draw_status_bar(Screen, !IO) :-
    draw_status_bar(Screen, no, no, !IO).

draw_status_bar(Screen, MaybeText, MaybeProgress, !IO) :-
    get_status_attrs(Screen, Attrs),
    get_cols(Screen, Cols),
    get_bar_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    attr(Panel, Attrs ^ bar, !IO),
    hline(Panel, char.to_int('-'), Cols, !IO),
    (
        MaybeText = yes(Text),
        move(Panel, 0, 4, !IO),
        draw(Panel, " ", !IO),
        draw(Panel, Text, !IO),
        draw(Panel, " ", !IO),
        getyx(Panel, _, MinX, !IO)
    ;
        MaybeText = no,
        MinX = 0
    ),
    (
        MaybeProgress = yes(ProgressText),
        % Just drop progress text if it won't fit.
        ProgressTextCol = Cols - 4 - string_wcwidth(ProgressText) - 1,
        ( if ProgressTextCol >= MinX then
            move(Panel, 0, ProgressTextCol, !IO),
            draw(Panel, " ", !IO),
            draw(Panel, ProgressText, !IO),
            draw(Panel, " ", !IO)
        else
            true
        )
    ;
        MaybeProgress = no
    ).

%-----------------------------------------------------------------------------%

get_keycode_blocking(Code, !IO) :-
    curs.cbreak(!IO),
    get_keycode_2(Code, !IO).

:- pred get_keycode_timeout(int::in, keycode::out, io::di, io::uo) is det.

get_keycode_timeout(Tenths, Code, !IO) :-
    % halfdelay argument must be between 1 and 255.
    HalfDelay = max(1, min(Tenths, 255)),
    curs.halfdelay(HalfDelay, !IO),
    get_keycode_2(Code, !IO).

:- pred get_keycode_2(keycode::out, io::di, io::uo) is det.

get_keycode_2(Code, !IO) :-
    throw_sigint_if_received(!IO),
    curs.get_wch(C, IsCode, !IO),
    ( C = 12 ->
        % Redraw the whole screen with ^L.
        % I have a feeling this is not really correct.
        curs.redrawwin_stdscr(!IO),
        panel.update_panels(!IO),
        get_keycode_2(Code, !IO)
    ;
        IsCode = yes,
        Code = code(C)
    ;
        IsCode = no,
        ( C = 0 ->
            Code = timeout_or_error
        ; C = 27 -> % Escape
            nodelay(yes, !IO),
            curs.get_wch(C2, IsCode2, !IO),
            nodelay(no, !IO),
            (
                IsCode2 = yes,
                Code = metacode(C2)
            ;
                IsCode2 = no,
                (
                    C2 \= 0,
                    char.from_int(C2, Char2)
                ->
                    Code = meta(Char2)
                ;
                    Code = char(det_from_int(27)) % ESC
                )
            )
        ;
            ( char.from_int(C, Char0) ->
                Code = char(Char0)
            ;
                get_keycode_2(Code, !IO)
            )
        )
    ).

get_keycode_async_aware(MaybeDeadline, Code, !IO) :-
    async.have_child_process(HaveChild, !IO),
    (
        HaveChild = yes,
        Tenths = 10,
        get_keycode_child_process_loop(Tenths, Code, !IO)
    ;
        HaveChild = no,
        (
            MaybeDeadline = yes(Deadline),
            current_timestamp(Time, !IO),
            DeltaSecs = Deadline - Time,
            ( DeltaSecs =< 0.0 ->
                Tenths = 10
            ;
                Tenths = 10 * floor_to_int(DeltaSecs) + 1
            ),
            get_keycode_timeout(Tenths, Code, !IO)
        ;
            MaybeDeadline = no,
            get_keycode_blocking(Code, !IO)
        )
    ).

:- pred get_keycode_child_process_loop(int::in, keycode::out,
    io::di, io::uo) is det.

get_keycode_child_process_loop(Tenths, Code, !IO) :-
    get_keycode_timeout(Tenths, Code0, !IO),
    ( Code0 = timeout_or_error ->
        async.received_sigchld_since_spawn(Sigchld, !IO),
        (
            Sigchld = yes,
            Code = Code0
        ;
            Sigchld = no,
            get_keycode_child_process_loop(Tenths, Code, !IO)
        )
    ;
        Code = Code0
    ).

%-----------------------------------------------------------------------------%

:- pred throw_sigint_if_received(io::di, io::uo) is det.

throw_sigint_if_received(!IO) :-
    get_sigint_count(Sigints, !IO),
    ( Sigints > 0 ->
        throw(sigint_received)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
