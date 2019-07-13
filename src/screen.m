% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module screen.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module color.
:- import_module time_util.

:- use_module curs.

%-----------------------------------------------------------------------------%

:- type screen.

:- type vpanel
    --->    main_panel(int)
    ;       bar_panel
    ;       msgentry_panel.

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

:- pred create_screen(status_attrs::in, screen::out, io::di, io::uo) is det.

:- pred recreate_screen_for_resize(screen::in, io::di, io::uo) is det.

:- pred get_cols(screen::in, int::out, io::di, io::uo) is det.

:- pred get_rows_cols(screen::in, int::out, int::out, io::di, io::uo) is det.

:- pred get_main_panels(screen::in, list(vpanel)::out, io::di, io::uo) is det.

:- pred get_main_rows(screen::in, int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred getyx(screen::in, vpanel::in, int::out, int::out, io::di, io::uo)
    is det.

:- pred getmaxyx(screen::in, vpanel::in, int::out, int::out, io::di, io::uo)
    is det.

:- pred move(screen::in, vpanel::in, int::in, int::in, io::di, io::uo) is det.

:- pred erase(screen::in, vpanel::in, io::di, io::uo) is det.

:- pred attr(screen::in, vpanel::in, curs.attr::in, io::di, io::uo) is det.

:- pred mattr(screen::in, vpanel::in, maybe(curs.attr)::in, io::di, io::uo)
    is det.

    % The draw routines do not add the string if the cursor is already at the
    % end of the panel.
    %
:- pred draw(screen::in, vpanel::in, string::in, io::di, io::uo) is det.

:- pred draw2(screen::in, vpanel::in, string::in, string::in, io::di, io::uo)
    is det.

:- pred draw(screen::in, vpanel::in, curs.attr::in, string::in, io::di, io::uo)
    is det.

:- pred draw(screen::in, vpanel::in, curs.attr::in, string::in, int::in,
    int::in, io::di, io::uo) is det.

    % The draw_fixed routines truncate the string if it would exceed the given
    % number of columns. Afterwards the cursor is placed NrCols after the
    % original position (or otherwise the right margin of the panel).
    %
:- pred draw_fixed(screen::in, vpanel::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

:- pred draw_fixed(screen::in, vpanel::in, curs.attr::in, int::in, string::in,
    char::in, io::di, io::uo) is det.

:- pred mattr_draw(screen::in, vpanel::in, maybe(curs.attr)::in, string::in,
    io::di, io::uo) is det.

:- pred mattr_draw_fixed(screen::in, vpanel::in, maybe(curs.attr)::in, int::in,
    string::in, char::in, io::di, io::uo) is det.

:- pred hline(screen::in, vpanel::in, char::in, int::in, io::di, io::uo)
    is det.

:- pred update_panels(screen::in, io::di, io::uo) is det.

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

:- import_module array.
:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module store.
:- import_module string.

:- import_module async.
:- import_module signal.
:- import_module string_util.

:- use_module curs.panel.

%-----------------------------------------------------------------------------%

:- type screen == io_mutvar(real_screen).

:- type real_screen
    --->    screen(
                rows            :: int,
                cols            :: int,
                main_vpanels    :: list(vpanel),
                main_panels     :: array(curs.panel.panel),
                bar_panel       :: curs.panel.panel,
                msgentry_panel  :: curs.panel.panel,
                status_attrs    :: status_attrs
            ).

%-----------------------------------------------------------------------------%

create_screen(Attrs, ScreenVar, !IO) :-
    do_create_screen(Attrs, Screen, !IO),
    new_mutvar(Screen, ScreenVar, !IO).

:- pred do_create_screen(status_attrs::in, real_screen::out, io::di, io::uo)
    is det.

do_create_screen(Attrs, Screen, !IO) :-
    curs.rows_cols(Rows, Cols, !IO),
    MainRows = Rows - 2,
    BarRow = MainRows,
    MsgEntryRow = BarRow + 1,
    MainVPanels = list.map(make_main_vpanel, 0..(MainRows - 1)),
    list.map_foldl(create_row_panel(Cols), 0..(MainRows - 1), MainPanels, !IO),
    create_row_panel(Cols, BarRow, BarPanel, !IO),
    create_row_panel(Cols, MsgEntryRow, MsgEntryPanel, !IO),
    Screen = screen(Rows, Cols, MainVPanels, array(MainPanels), BarPanel,
        MsgEntryPanel, Attrs).

:- func make_main_vpanel(int) = vpanel.

make_main_vpanel(N) = main_panel(N).

:- pred create_row_panel(int::in, int::in, curs.panel.panel::out,
    io::di, io::uo) is det.

create_row_panel(Cols, Row, Panel, !IO) :-
    curs.panel.new(1, Cols, Row, 0, curs.normal, Panel, !IO),
    curs.panel.scrollok(Panel, no, !IO).

%-----------------------------------------------------------------------------%

recreate_screen_for_resize(ScreenVar, !IO) :-
    get_mutvar(ScreenVar, Screen0, !IO),
    Screen0 = screen(_Rows, _Cols, _MainVPanels, MainPanels, BarPanel,
        MsgEntryPanel, Attrs),

    array.foldl(curs.panel.delete, MainPanels, !IO),
    curs.panel.delete(BarPanel, !IO),
    curs.panel.delete(MsgEntryPanel, !IO),

    do_create_screen(Attrs, Screen, !IO),
    set_mutvar(ScreenVar, Screen, !IO).

%-----------------------------------------------------------------------------%

get_cols(ScreenVar, Cols, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    Cols = Screen ^ cols.

get_rows_cols(ScreenVar, Rows, Cols, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    Rows = Screen ^ rows,
    Cols = Screen ^ cols.

get_main_panels(ScreenVar, MainVPanels, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    MainVPanels = Screen ^ main_vpanels.

get_main_rows(ScreenVar, NumRows, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    MainPanels = Screen ^ main_panels,
    array.size(MainPanels, NumRows).

:- pred get_status_attrs(screen::in, status_attrs::out, io::di, io::uo) is det.

get_status_attrs(ScreenVar, Attrs, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    Attrs = Screen ^ status_attrs.

%-----------------------------------------------------------------------------%

:- pragma inline(get_panel/5).
:- pred get_panel(screen::in, vpanel::in, maybe(curs.panel.panel)::out,
    io::di, io::uo) is det.

get_panel(ScreenVar, VPanel, MaybePanel, !IO) :-
    get_mutvar(ScreenVar, Screen, !IO),
    Screen = screen(_Rows, _Cols, _MainVPanels, MainPanels, BarPanel,
        MsgEntryPanel, _Attrs),
    (
        VPanel = main_panel(N),
        ( semidet_lookup(MainPanels, N, Panel) ->
            MaybePanel = yes(Panel)
        ;
            MaybePanel = no
        )
    ;
        VPanel = bar_panel,
        MaybePanel = yes(BarPanel)
    ;
        VPanel = msgentry_panel,
        MaybePanel = yes(MsgEntryPanel)
    ).

%-----------------------------------------------------------------------------%

getyx(Screen, VPanel, Y, X, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.getyx(Panel, Y, X, !IO)
    ;
        MaybePanel = no,
        Y = 0,
        X = 0
    ).

getmaxyx(Screen, VPanel, MaxY, MaxX, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.getmaxyx(Panel, MaxY, MaxX, !IO)
    ;
        MaybePanel = no,
        MaxY = 0,
        MaxX = 0
    ).

move(Screen, VPanel, Row, Col, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.move(Panel, Row, Col, !IO)
    ;
        MaybePanel = no
    ).

erase(Screen, VPanel, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.erase(Panel, !IO)
    ;
        MaybePanel = no
    ).

%-----------------------------------------------------------------------------%

attr(Screen, VPanel, Attr, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.attr_set(Panel, Attr, !IO)
    ;
        MaybePanel = no
    ).

mattr(_Screen, _Panel, no, !IO).
mattr(Screen, Panel, yes(Attr), !IO) :-
    attr(Screen, Panel, Attr, !IO).

draw(Screen, VPanel, String, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        my_addstr(Panel, String, !IO)
    ;
        MaybePanel = no
    ).

draw2(Screen, VPanel, StringA, StringB, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        my_addstr(Panel, StringA, StringB, !IO)
    ;
        MaybePanel = no
    ).

draw(Screen, VPanel, Attr, String, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, String, !IO)
    ;
        MaybePanel = no
    ).

draw(Screen, VPanel, Attr, String, Start, End, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, string.between(String, Start, End), !IO)
    ;
        MaybePanel = no
    ).

draw_fixed(Screen, VPanel, NrCols, String, PadChar, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        my_addstr_fixed(Panel, NrCols, String, PadChar, !IO)
    ;
        MaybePanel = no
    ).

draw_fixed(Screen, VPanel, Attr, NrCols, String, PadChar, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.attr_set(Panel, Attr, !IO),
        my_addstr_fixed(Panel, NrCols, String, PadChar, !IO)
    ;
        MaybePanel = no
    ).

mattr_draw(Screen, VPanel, MaybeAttr, String, !IO) :-
    mattr(Screen, VPanel, MaybeAttr, !IO),
    draw(Screen, VPanel, String, !IO).

mattr_draw_fixed(Screen, VPanel, MaybeAttr, NrCols, String, PadChar, !IO) :-
    mattr(Screen, VPanel, MaybeAttr, !IO),
    draw_fixed(Screen, VPanel, NrCols, String, PadChar, !IO).

hline(Screen, VPanel, C, N, !IO) :-
    get_panel(Screen, VPanel, MaybePanel, !IO),
    (
        MaybePanel = yes(Panel),
        curs.panel.hline(Panel, char.to_int(C), N, !IO)
    ;
        MaybePanel = no
    ).

update_panels(_Screen, !IO) :-
    curs.panel.update_panels(!IO).

%-----------------------------------------------------------------------------%

:- pred my_addstr(curs.panel.panel::in, string::in, io::di, io::uo) is det.

my_addstr(Panel, String, !IO) :-
    curs.panel.getyx(Panel, _, X, !IO),
    curs.panel.getmaxyx(Panel, _, MaxX, !IO),
    % XXX We cannot tell if we have already hit the last column, before or
    % after having drawn to it.  This hack checks the second last column
    % instead, which means it actually prevents drawing one column early.
    % A solution may be to use a curses pad larger than the screen.
    ( X >= MaxX - 1 ->
        true
    ;
        curs.panel.addstr(Panel, String, !IO)
    ).

:- pred my_addstr(curs.panel.panel::in, string::in, string::in, io::di, io::uo)
    is det.

my_addstr(Panel, StringA, StringB, !IO) :-
    curs.panel.getyx(Panel, _, X, !IO),
    curs.panel.getmaxyx(Panel, _, MaxX, !IO),
    % XXX See comment in my_addstr.
    ( X >= MaxX - 1 ->
        true
    ;
        curs.panel.addstr(Panel, StringA, !IO),
        curs.panel.addstr(Panel, StringB, !IO)
    ).

:- pred my_addstr_fixed(curs.panel.panel::in, int::in, string::in, char::in,
    io::di, io::uo) is det.

my_addstr_fixed(Panel, NrCols, String, PadChar, !IO) :-
    curs.panel.getyx(Panel, _, X0, !IO),
    curs.panel.getmaxyx(Panel, _, MaxX, !IO),
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
        curs.panel.addstr(Panel, string.between(String, Start, End), !IO),

        curs.panel.getyx(Panel, _, X1, !IO),
        N = (X0 + NrCols) - X1,
        add_padding(Panel, N, string.from_char(PadChar), !IO)
    ).

:- pred add_padding(curs.panel.panel::in, int::in, string::in, io::di, io::uo)
    is det.

add_padding(Panel, N, PadChar, !IO) :-
    ( N =< 0 ->
        true
    ;
        curs.panel.addstr(Panel, PadChar, !IO),
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
    Panel = msgentry_panel,
    get_status_attrs(Screen, Attrs, !IO),
    (
        MessageUpdate = no_change
    ;
        MessageUpdate = clear_message,
        erase(Screen, Panel, !IO)
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
        erase(Screen, Panel, !IO),
        draw(Screen, Panel, Attr, String, !IO)
    ).

update_message_immed(Screen, MessageUpdate, !IO) :-
    update_message(Screen, MessageUpdate, !IO),
    update_panels(Screen, !IO).

%-----------------------------------------------------------------------------%

draw_status_bar(Screen, !IO) :-
    draw_status_bar(Screen, no, no, !IO).

draw_status_bar(Screen, MaybeText, MaybeProgress, !IO) :-
    Panel = bar_panel,
    get_status_attrs(Screen, Attrs, !IO),
    get_cols(Screen, Cols, !IO),
    erase(Screen, Panel, !IO),
    attr(Screen, Panel, Attrs ^ bar, !IO),
    hline(Screen, Panel, '-', Cols, !IO),
    (
        MaybeText = yes(Text),
        move(Screen, Panel, 0, 4, !IO),
        draw(Screen, Panel, " ", !IO),
        draw(Screen, Panel, Text, !IO),
        draw(Screen, Panel, " ", !IO),
        getyx(Screen, Panel, _, MinX, !IO)
    ;
        MaybeText = no,
        MinX = 0
    ),
    (
        MaybeProgress = yes(ProgressText),
        % Just drop progress text if it won't fit.
        ProgressTextCol = Cols - 4 - string_wcwidth(ProgressText) - 1,
        ( if ProgressTextCol >= MinX then
            move(Screen, Panel, 0, ProgressTextCol, !IO),
            draw(Screen, Panel, " ", !IO),
            draw(Screen, Panel, ProgressText, !IO),
            draw(Screen, Panel, " ", !IO)
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
        curs.panel.update_panels(!IO),
        get_keycode_2(Code, !IO)
    ;
        IsCode = yes,
        Code = code(C)
    ;
        IsCode = no,
        ( C = 0 ->
            Code = timeout_or_error
        ; C = 27 -> % Escape
            curs.nodelay(yes, !IO),
            curs.get_wch(C2, IsCode2, !IO),
            curs.nodelay(no, !IO),
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
