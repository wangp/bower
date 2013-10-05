% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module screen.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- import_module curs.
:- import_module curs.panel.

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

:- pred create_screen(screen::uo, io::di, io::uo) is det.

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

    % Like addstr but doesn't add the string if the cursor is already at the
    % end of the panel.
    %
:- pred my_addstr(panel::in, string::in, io::di, io::uo) is det.

    % my_addstr_fixed(Panel, NrCols, String, PadChar, !IO)
    %
    % Like my_addstr, but truncate the string if it would exceed the given
    % number of columns.  Afterwards the cursor is placed NrCols after the
    % original position (or otherwise the right margin of the panel).
    %
:- pred my_addstr_fixed(panel::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

:- pred update_message(screen::in, message_update::in, io::di, io::uo) is det.

:- pred update_message_immed(screen::in, message_update::in, io::di, io::uo)
    is det.

:- pred draw_bar(screen::in, io::di, io::uo) is det.

:- pred draw_bar_with_text(screen::in, string::in, io::di, io::uo) is det.

:- type keycode
    --->    char(char)
    ;       meta(char)
    ;       code(int)
    ;       timeout_or_error.

:- pred get_keycode_blocking(keycode::out, io::di, io::uo) is det.

:- pred get_keycode_timeout(int::in, keycode::out, io::di, io::uo) is det.

:- pred get_char_blocking(char::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module mutvar.
:- import_module require.
:- import_module string.

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
                msgentry_panel  :: panel
            )
    ;       forward_screen(
                % This version of the screen is obsolete,
                % replaced by a forwarding pointer.
                old_rows        :: int,
                old_cols        :: int,
                fwd_screen      :: screen
            ).

:- inst real_screen
    --->    screen(ground, ground, ground, ground, ground).

%-----------------------------------------------------------------------------%

create_screen(Screen, !IO) :-
    promise_pure (
        curs.rows_cols(Rows, Cols, !IO),
        MainRows = Rows - 2,
        BarRow = MainRows,
        MsgEntryRow = BarRow + 1,

        list.map_foldl(create_row_panel(Cols), 0..(MainRows - 1), MainPanels,
            !IO),
        create_row_panel(Cols, BarRow, BarPanel, !IO),
        create_row_panel(Cols, MsgEntryRow, MsgEntryPanel, !IO),
        RealScreen0 = screen(Rows, Cols, MainPanels, BarPanel, MsgEntryPanel),
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
        impure destroy_screen(Screen0, OldRows, OldCols, !IO),
        create_screen(Screen, !IO),
        % Replace Screen0 by a forwarding pointer.
        impure set_mutvar(Screen0, forward_screen(OldRows, OldCols, Screen))
    ).

:- impure pred destroy_screen(screen::in, int::out, int::out, io::di, io::uo)
    is det.

destroy_screen(Screen, OldRows, OldCols, !IO) :-
    impure get_mutvar(Screen, ScreenVersion),
    (
        ScreenVersion = screen(OldRows, OldCols, MainPanels, BarPanel,
            MsgEntryPanel),
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
            ScreenVersion = screen(_, _, _, _, _),
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
        ScreenVersion = screen(Rows, Cols, _, _, _),
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

:- pred get_real_screen(screen::in, screen_version::out(real_screen)) is det.

get_real_screen(Screen, RealScreen) :-
    promise_pure (
        impure get_mutvar(Screen, RealScreen),
        (
            RealScreen = screen(_, _, _, _, _)
        ;
            RealScreen = forward_screen(_, _, _),
            unexpected($module, $pred, "screen already destroyed")
        )
    ).

%-----------------------------------------------------------------------------%

my_addstr(Panel, String, !IO) :-
    getyx(Panel, _, X, !IO),
    getmaxyx(Panel, _, MaxX, !IO),
    % XXX stops drawing one column early
    ( X >= MaxX - 1 ->
        true
    ;
        addstr(Panel, String, !IO)
    ).

%-----------------------------------------------------------------------------%

my_addstr_fixed(Panel, NrCols, String, PadChar, !IO) :-
    getyx(Panel, _, X0, !IO),
    getmaxyx(Panel, _, MaxX, !IO),
    % XXX stops drawing one column early
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
    (
        MessageUpdate = no_change
    ;
        MessageUpdate = clear_message,
        panel.erase(Panel, !IO)
    ;
        MessageUpdate = set_info(String),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, fg_bg(cyan, default) + bold, !IO),
        my_addstr(Panel, String, !IO)
    ;
        MessageUpdate = set_warning(String),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, fg_bg(red, default) + bold, !IO),
        my_addstr(Panel, String, !IO)
    ;
        MessageUpdate = set_prompt(String),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, normal, !IO),
        my_addstr(Panel, String, !IO)
    ).

update_message_immed(String, MessageUpdate, !IO) :-
    update_message(String, MessageUpdate, !IO),
    panel.update_panels(!IO).

%-----------------------------------------------------------------------------%

draw_bar(Screen, !IO) :-
    get_cols(Screen, Cols),
    get_bar_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

draw_bar_with_text(Screen, Text, !IO) :-
    get_cols(Screen, Cols),
    get_bar_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, fg_bg(white, blue), !IO),
    my_addstr(Panel, "--- ", !IO),
    my_addstr(Panel, Text, !IO),
    my_addstr(Panel, " -", !IO),
    hline(Panel, char.to_int('-'), Cols, !IO).

%-----------------------------------------------------------------------------%

get_keycode_blocking(Code, !IO) :-
    curs.cbreak(!IO),
    get_keycode_2(Code, !IO).

get_keycode_timeout(Tenths, Code, !IO) :-
    curs.halfdelay(Tenths, !IO),
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
                IsCode2 = no,
                C2 \= 0,
                char.from_int(C2, Char2)
            ->
                Code = meta(Char2)
            ;
                Code = char('\033') % ESC
            )
        ;
            ( char.from_int(C, Char0) ->
                Code = char(Char0)
            ;
                get_keycode_2(Code, !IO)
            )
        )
    ).

get_char_blocking(Char, !IO) :-
    get_keycode_blocking(Code, !IO),
    (
        Code = char(Char)
    ;
        ( Code = code(_)
        ; Code = meta(_)
        ; Code = timeout_or_error
        ),
        get_char_blocking(Char, !IO)
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
