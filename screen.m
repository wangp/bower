%-----------------------------------------------------------------------------%

:- module screen.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- import_module curs.
:- import_module curs.panel.

%-----------------------------------------------------------------------------%

:- type screen
    --->    screen(
                rows            :: int,
                cols            :: int,
                main_panels     :: list(panel),
                bar_panel       :: panel,
                msgentry_panel  :: panel
            ).

:- type message_update
    --->    no_change
    ;       clear_message
    ;       set_info(string)
    ;       set_warning(string).

:- pred create_screen(screen::out, io::di, io::uo) is det.

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

:- pred get_char(char::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.

:- pragma foreign_decl("C", local,
"
    #include <wchar.h>
").

%-----------------------------------------------------------------------------%

create_screen(Screen, !IO) :-
    curs.rows_cols(Rows, Cols, !IO),
    MainRows = Rows - 2,
    BarRow = MainRows,
    MsgEntryRow = BarRow + 1,

    list.map_foldl(create_row_panel(Cols), 0..(MainRows - 1), MainPanels, !IO),
    create_row_panel(Cols, BarRow, BarPanel, !IO),
    create_row_panel(Cols, MsgEntryRow, MsgEntryPanel, !IO),
    Screen = screen(Rows, Cols, MainPanels, BarPanel, MsgEntryPanel).

:- pred create_row_panel(int::in, int::in, panel::out, io::di, io::uo) is det.

create_row_panel(Cols, Row, Panel, !IO) :-
    panel.new(1, Cols, Row, 0, normal, Panel, !IO),
    panel.scrollok(Panel, no, !IO).

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
    getyx(Panel, Y, X0, !IO),
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
        wcwidth(Char, CharWidth),
        RemCols1 = RemCols - CharWidth,
        RemCols1 >= 0
    ->
        count_loop(String, RemCols1, !Index)
    ;
        true
    ).

:- pred wcwidth(char::in, int::out) is det.

:- pragma foreign_proc("C",
    wcwidth(C::in, Width::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Width = wcwidth(C);
").

%-----------------------------------------------------------------------------%

update_message(Screen, MessageUpdate, !IO) :-
    Panel = Screen ^ msgentry_panel,
    (
        MessageUpdate = no_change
    ;
        MessageUpdate = clear_message,
        panel.erase(Panel, !IO)
    ;
        MessageUpdate = set_info(String),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, fg_bg(cyan, black) + bold, !IO),
        my_addstr(Panel, String, !IO)
    ;
        MessageUpdate = set_warning(String),
        panel.erase(Panel, !IO),
        panel.attr_set(Panel, fg_bg(red, black) + bold, !IO),
        my_addstr(Panel, String, !IO)
    ).

%-----------------------------------------------------------------------------%

get_char(Char, !IO) :-
    curs.get_wch(C, !IO),
    ( char.from_int(C, Char0) ->
        Char = Char0
    ;
        get_char(Char, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
