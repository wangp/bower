%-----------------------------------------------------------------------------%
% curs.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Thu Jan 11 13:47:25 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
% Simplified Mercury interface to the ncurses and panel libraries.
%
% This is largely inspired by Tomas Conway and Robert Jeschofnik's
% mcurses module; it is intended to more closely match the facilities
% offered by the ncurses package and leave the issue of window management
% to the ncurses and panel libraries rather than doing so in Mercury.
%
% XXX This module no error checking.
%
% NOTE: you will need to include `-lpanel -lncurses' in MLLIBS when
% linking against this module.
%
%-----------------------------------------------------------------------------%

:- module curs.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % Start a curses session (colour, unbuffered input, no echoing, 
    % invisible cursor if possible, scrolling on when output past
    % the bottom of the main display and any windows).
    %
:- pred start(io::di, io::uo) is det.

    % Enable cbreak mode (no line buffering).
    %
:- pred cbreak(io::di, io::uo) is det.

    % Enable or disable the no-delay option.  If enabled (first argument is
    % yes) then getch will be a non-blocking call, i.e. return immediately
    % if no input is ready rather than waiting for input.
    %
:- pred nodelay(bool::in, io::di, io::uo) is det.

    % halfdelay(Tenths, !IO)
    % Enable halfdelay mode. The delay is in tenths of a second
    % between 1 and 255.
    %
:- pred halfdelay(int::in, io::di, io::uo) is det.

    % Close a curses session; necessary to return the tty to a sensible
    % state.
    %
:- pred stop(io::di, io::uo) is det.

    % A wrapper predicate that handles calling start and stop.
    %
:- pred session(pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is det.

    % Save the current terminal mode as the "program" (in curses) state.
    %
:- pred def_prog_mode(io::di, io::uo) is det.

    % Save the current terminal mode as the "shell" (in curses) state.
    %
:- pred def_shell_mode(io::di, io::uo) is det.

    % Restore the terminal to "program" (in curses) state.
    %
:- pred reset_prog_mode(io::di, io::uo) is det.

    % Restore the terminal to "shell" (not in curses) state.
    %
:- pred reset_shell_mode(io::di, io::uo) is det.

    % A wrapper predicate that saves the "program" state,
    % temporarily leaves curses, runs the given predicate,
    % restores the saved "program" state, then refreshes the display.
    %
:- pred suspend(pred(T, io, io), T, io, io).
:- mode suspend(in(pred(out, di, uo) is det), out, di, uo) is det.

    % As above, without stopping curses (endwin).
    %
:- pred soft_suspend(pred(T, io, io), T, io, io).
:- mode soft_suspend(in(pred(out, di, uo) is det), out, di, uo) is det.

    % Number of rows and columns on the physical screen.
    %
:- pred rows_cols(int::out, int::out, io::di, io::uo) is det.

    % Move the virtual cursor to given row and column; (0, 0) are the
    % coordinates for the upper left hand corner of the display.
    %
:- pred move(int::in, int::in, io::di, io::uo) is det.

    % Erase the whole display.
    %
:- pred erase(io::di, io::uo) is det.

    % Clear the whole display.
    % Unlike erase, the display will be repainted from scratch.
    %
:- pred clear(io::di, io::uo) is det.

    % Touch the entire screen so it will be repainted.
    %
:- pred redrawwin_stdscr(io::di, io::uo) is det.

    % Output a character (with the given attributes) and advance the cursor.
    % Note that char codes are passed rather than plain chars.
    %
:- pred addch(attr::in, int::in, io::di, io::uo) is det.

    % Output a string and advance the cursor.
    %
:- pred addstr(string::in, io::di, io::uo) is det.

    % Turn on/off or set attributes that will be applied by default.
    %
:- pred attr_on(attr::in, io::di, io::uo) is det.
:- pred attr_off(attr::in, io::di, io::uo) is det.
:- pred attr_set(attr::in, io::di, io::uo) is det.

    % Update the display.  Changes made to the display are not made
    % visible until refresh is called.
    %
:- pred refresh(io::di, io::uo) is det.

    % This was supposed to do what refresh does but without preceding calls 
    % to wnoutrefresh it does nothing.
    %
:- pragma obsolete(doupdate/2).
:- pred doupdate(io::di, io::uo) is det.

    % Read a character from the keyboard (unbuffered) and translate it
    % if necessary.  In no-delay mode, if no input is waiting, the value
    % curs__err is returned.
    %
:- pred getch(int::out, io::di, io::uo) is det.

:- pred get_wch(int::out, bool::out, io::di, io::uo) is det.

    % Throw away any typeahead that has not yet been read by the program.
    %
:- pred flushinp(io::di, io::uo) is det.

    % Draws a border around the inside edge of the display.
    %
:- pred border(io::di, io::uo) is det.

    % Draws an horizontal line of char codes C length N moving to the right.
    %
:- pred hline(int::in, int::in, io::di, io::uo) is det.

    % Draws a vertical line of char codes C length N moving down.
    %
:- pred vline(int::in, int::in, io::di, io::uo) is det.

    % Error code; currently only used as return value of getch to
    % indicate that no input is ready.
    %
:- func err = int.
    
    % Various key code translations outside the normal ASCII range.
    %
:- func key_down = int.
:- func key_up = int.
:- func key_left = int.
:- func key_right = int.
:- func key_home = int.
:- func key_backspace = int.
:- func key_f(int) = int.               % Function key no. (0 to 63).
:- func key_del = int.
:- func key_ins = int.
:- func key_pageup = int.
:- func key_pagedown = int.
:- func key_enter = int.                % Key pad enter.
:- func key_a1 = int.                   % Key pad upper left.
:- func key_a3 = int.                   % Key pad upper right.
:- func key_b2 = int.                   % Key pad middle centre.
:- func key_c1 = int.                   % Key pad lower left.
:- func key_c3 = int.                   % Key pad lower right.
:- func key_btab = int.                 % Back-tab.
:- func key_end = int.
:- func key_resize = int.               % Resize event.



    % Special char codes (not always available).
    %
                                        % Default   Description
                                        % -------   -----------
:- func acs_block = int.                % #         solid square block
:- func acs_board = int.                % #         board of squares
:- func acs_btee = int.                 % +         bottom tee
:- func acs_bullet = int.               % o         bullet
:- func acs_ckboard = int.              % :         checker board (stipple)
:- func acs_darrow = int.               % v         arrow pointing down
:- func acs_degree = int.               % '         degree symbol
:- func acs_diamond = int.              % +         diamond
:- func acs_gequal = int.               % >         greater-than-or-equal-to
:- func acs_hline = int.                % -         horizontal line
:- func acs_lantern = int.              % #         lantern symbol
:- func acs_larrow = int.               % <         arrow pointing left
:- func acs_lequal = int.               % <         less-than-or-equal-to
:- func acs_llcorner = int.             % +         lower left-hand corner
:- func acs_lrcorner = int.             % +         lower right-hand corner
:- func acs_ltee = int.                 % +         left tee
:- func acs_nequal = int.               % !         not-equal
:- func acs_pi = int.                   % *         greek pi
:- func acs_plminus = int.              % #         plus/minus
:- func acs_plus = int.                 % +         plus
:- func acs_rarrow = int.               % >         arrow pointing right
:- func acs_rtee = int.                 % +         right tee
:- func acs_s1 = int.                   % -         scan line 1
:- func acs_s3 = int.                   % -         scan line 3
:- func acs_s7 = int.                   % -         scan line 7
:- func acs_s9 = int.                   % _         scan line 9
:- func acs_sterling = int.             % f         pound-sterling symbol
:- func acs_ttee = int.                 % +         top tee
:- func acs_uarrow = int.               % ^         arrow pointing up
:- func acs_ulcorner = int.             % +         upper left-hand corner
:- func acs_urcorner = int.             % +         upper right-hand corner
:- func acs_vline = int.                % |         vertical line

    % Character attributes.
    %
:- type attr.

:- func attr + attr = attr.             % Combines attributes.

:- func normal = attr.
:- func standout = attr.
:- func underline = attr.
:- func reverse = attr.
:- func blink = attr.
:- func dim = attr.
:- func bold = attr.
:- func invis = attr.
:- func fg_bg(colour, colour) = attr.   % Provide the appropriate colour pair no

    % Colour attributes.
    %
:- type colour.

:- func default = colour.               % Ncurses extension
:- func black = colour.
:- func red = colour.
:- func green = colour.
:- func yellow = colour.
:- func blue = colour.
:- func magenta = colour.
:- func cyan = colour.
:- func white = colour.

    %-------------------------------------------------------------------------%
    %-------------------------------------------------------------------------%

        % Panels are windows over the main display; they may be
        % stacked, moved, ordered and hidden.  Contents of panels
        % closer to the top of the stack obscure the parts of panels
        % they overlap that are lower in the stack.
        %
    :- module panel.
    :- interface.
    
    :- type panel.

        % new(Rows, Cols, Row, Col, Attr, Panel) creates a new panel
        % Panel whose size is given by (Rows, Cols) and whose position
        % on the display is given by (Row, Col).  The new panel starts
        % visible and at the top of the stack.  The default attributes
        % for the panel are set to Attr.
        %
    :- pred new(int::in, int::in, int::in, int::in, attr::in, panel::out,
        io::di, io::uo) is det.

        % Destroy a panel.
        %
    :- pred delete(panel::in, io::di, io::uo) is det.

        % Enable or disable scrolling when cursor leaves bottom of panel.
        %
    :- pred scrollok(panel::in, bool::in, io::di, io::uo) is det.

        % Get the current cursor position.
        %
    :- pred getyx(panel::in, int::out, int::out, io::di, io::uo) is det.

        % Get the size of the panel.
        %
    :- pred getmaxyx(panel::in, int::out, int::out, io::di, io::uo) is det.

        % Raise/lower a panel to the top/bottom of the stack.
        %
    :- pred raise(panel::in, io::di, io::uo) is det.
    :- pred lower(panel::in, io::di, io::uo) is det.

        % Hide/reveal a panel (revealing places it at the top of the stack).
        %
    :- pred hide(panel::in, io::di, io::uo) is det.
    :- pred reveal(panel::in, io::di, io::uo) is det.

        % Move a panel to (Row, Col) on the display.
        %
    :- pred relocate(panel::in, int::in, int::in, io::di, io::uo) is det.

        % Erase a panel.
        %
    :- pred erase(panel::in, io::di, io::uo) is det.

        % Clear a panel.  Unlike erase, the panel will be repainted from
        % scratch.
        %
    :- pred clear(panel::in, io::di, io::uo) is det.

        % Move the virtual cursor to given row and column; (0, 0) are the
        % coordinates for the upper left hand corner of the panel.
        %
    :- pred move(panel::in, int::in, int::in, io::di, io::uo) is det.

        % Add a char/string to a panel with the given attributes.
        % Note that char codes are passed rather than plain chars.
        %
    :- pred addch(panel::in, attr::in, int::in, io::di, io::uo) is det.
    :- pred addstr(panel::in, string::in, io::di, io::uo) is det.

        % Turn on/off or set attributes that will be applied by default.
        %
    :- pred attr_on(panel::in, attr::in, io::di, io::uo) is det.
    :- pred attr_off(panel::in, attr::in, io::di, io::uo) is det.
    :- pred attr_set(panel::in, attr::in, io::di, io::uo) is det.

        % Update the display (also calls doupdate).
        % NOTE: doupdate does not call update_panels.
        %
    :- pred update_panels(io::di, io::uo) is det.

        % Draws a border around the inside edge of the display.
        %
    :- pred border(panel::in, io::di, io::uo) is det.

        % Draws an horizontal line of length N moving to the right.
        %
    :- pred hline(panel::in, int::in, int::in, io::di, io::uo) is det.

        % Draws a vertical line of length N moving down.
        %
    :- pred vline(panel::in, int::in, int::in, io::di, io::uo) is det.

    :- end_module panel.

    %-------------------------------------------------------------------------%
    %-------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module exception.

    % Using the foreign type prevents problems with type ambiguity when
    % compiling with intermodule optimisation.
    %
% :- type attr == int.
:- pragma foreign_type("C", attr, "int").

:- type colour == int.

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#ifdef WITH_NCURSESW_DIR
    #include <ncursesw/ncurses.h>
    #include <ncursesw/panel.h>
#else
    #include <ncurses.h>
    #include <panel.h>
#endif
#include <wchar.h>

#ifdef NCURSES_VERSION
    #define COLOR_DEFAULT      (COLOR_WHITE + 1)
    #define COLOR_MAX          (COLOR_DEFAULT)
#else
    #define COLOR_MAX          (COLOR_WHITE)
#endif

#define FG_BG(fg, bg)          ((fg) * COLOR_MAX + (bg))

").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    start(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"

    initscr();                          /* Start the show */

#ifdef NCURSES_VERSION
    /* Allow the use of colour -1 */
    use_default_colors();
#endif

    start_color();                      /* Enable colour */

    nonl();                             /* Don't translate \n */
    scrollok(stdscr, TRUE);             /* Scroll when output past bottom */
    leaveok(stdscr, TRUE);              /* Turn off the cursor */
    keypad(stdscr, TRUE);               /* Translate compound input chars */
    noecho();                           /* Don't echo typed characters */
    cbreak();                           /* Disable line buffering */

                                        /* Set up default colour pairs */
init_pair(FG_BG(COLOR_BLACK, COLOR_BLACK),      COLOR_BLACK, COLOR_BLACK);
init_pair(FG_BG(COLOR_BLACK, COLOR_RED),        COLOR_BLACK, COLOR_RED);
init_pair(FG_BG(COLOR_BLACK, COLOR_GREEN),      COLOR_BLACK, COLOR_GREEN);
init_pair(FG_BG(COLOR_BLACK, COLOR_YELLOW),     COLOR_BLACK, COLOR_YELLOW);
init_pair(FG_BG(COLOR_BLACK, COLOR_BLUE),       COLOR_BLACK, COLOR_BLUE);
init_pair(FG_BG(COLOR_BLACK, COLOR_MAGENTA),    COLOR_BLACK, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_BLACK, COLOR_CYAN),       COLOR_BLACK, COLOR_CYAN);
init_pair(FG_BG(COLOR_BLACK, COLOR_WHITE),      COLOR_BLACK, COLOR_WHITE);
init_pair(FG_BG(COLOR_RED, COLOR_BLACK),        COLOR_RED, COLOR_BLACK);
init_pair(FG_BG(COLOR_RED, COLOR_RED),          COLOR_RED, COLOR_RED);
init_pair(FG_BG(COLOR_RED, COLOR_GREEN),        COLOR_RED, COLOR_GREEN);
init_pair(FG_BG(COLOR_RED, COLOR_YELLOW),       COLOR_RED, COLOR_YELLOW);
init_pair(FG_BG(COLOR_RED, COLOR_BLUE),         COLOR_RED, COLOR_BLUE);
init_pair(FG_BG(COLOR_RED, COLOR_MAGENTA),      COLOR_RED, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_RED, COLOR_CYAN),         COLOR_RED, COLOR_CYAN);
init_pair(FG_BG(COLOR_RED, COLOR_WHITE),        COLOR_RED, COLOR_WHITE);
init_pair(FG_BG(COLOR_GREEN, COLOR_BLACK),      COLOR_GREEN, COLOR_BLACK);
init_pair(FG_BG(COLOR_GREEN, COLOR_RED),        COLOR_GREEN, COLOR_RED);
init_pair(FG_BG(COLOR_GREEN, COLOR_GREEN),      COLOR_GREEN, COLOR_GREEN);
init_pair(FG_BG(COLOR_GREEN, COLOR_YELLOW),     COLOR_GREEN, COLOR_YELLOW);
init_pair(FG_BG(COLOR_GREEN, COLOR_BLUE),       COLOR_GREEN, COLOR_BLUE);
init_pair(FG_BG(COLOR_GREEN, COLOR_MAGENTA),    COLOR_GREEN, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_GREEN, COLOR_CYAN),       COLOR_GREEN, COLOR_CYAN);
init_pair(FG_BG(COLOR_GREEN, COLOR_WHITE),      COLOR_GREEN, COLOR_WHITE);
init_pair(FG_BG(COLOR_YELLOW, COLOR_BLACK),     COLOR_YELLOW, COLOR_BLACK);
init_pair(FG_BG(COLOR_YELLOW, COLOR_RED),       COLOR_YELLOW, COLOR_RED);
init_pair(FG_BG(COLOR_YELLOW, COLOR_GREEN),     COLOR_YELLOW, COLOR_GREEN);
init_pair(FG_BG(COLOR_YELLOW, COLOR_YELLOW),    COLOR_YELLOW, COLOR_YELLOW);
init_pair(FG_BG(COLOR_YELLOW, COLOR_BLUE),      COLOR_YELLOW, COLOR_BLUE);
init_pair(FG_BG(COLOR_YELLOW, COLOR_MAGENTA),   COLOR_YELLOW, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_YELLOW, COLOR_CYAN),      COLOR_YELLOW, COLOR_CYAN);
init_pair(FG_BG(COLOR_YELLOW, COLOR_WHITE),     COLOR_YELLOW, COLOR_WHITE);
init_pair(FG_BG(COLOR_BLUE, COLOR_BLACK),       COLOR_BLUE, COLOR_BLACK);
init_pair(FG_BG(COLOR_BLUE, COLOR_RED),         COLOR_BLUE, COLOR_RED);
init_pair(FG_BG(COLOR_BLUE, COLOR_GREEN),       COLOR_BLUE, COLOR_GREEN);
init_pair(FG_BG(COLOR_BLUE, COLOR_YELLOW),      COLOR_BLUE, COLOR_YELLOW);
init_pair(FG_BG(COLOR_BLUE, COLOR_BLUE),        COLOR_BLUE, COLOR_BLUE);
init_pair(FG_BG(COLOR_BLUE, COLOR_MAGENTA),     COLOR_BLUE, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_BLUE, COLOR_CYAN),        COLOR_BLUE, COLOR_CYAN);
init_pair(FG_BG(COLOR_BLUE, COLOR_WHITE),       COLOR_BLUE, COLOR_WHITE);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_BLACK),    COLOR_MAGENTA, COLOR_BLACK);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_RED),      COLOR_MAGENTA, COLOR_RED);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_GREEN),    COLOR_MAGENTA, COLOR_GREEN);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_YELLOW),   COLOR_MAGENTA, COLOR_YELLOW);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_BLUE),     COLOR_MAGENTA, COLOR_BLUE);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_MAGENTA),  COLOR_MAGENTA, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_CYAN),     COLOR_MAGENTA, COLOR_CYAN);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_WHITE),    COLOR_MAGENTA, COLOR_WHITE);
init_pair(FG_BG(COLOR_CYAN, COLOR_BLACK),       COLOR_CYAN, COLOR_BLACK);
init_pair(FG_BG(COLOR_CYAN, COLOR_RED),         COLOR_CYAN, COLOR_RED);
init_pair(FG_BG(COLOR_CYAN, COLOR_GREEN),       COLOR_CYAN, COLOR_GREEN);
init_pair(FG_BG(COLOR_CYAN, COLOR_YELLOW),      COLOR_CYAN, COLOR_YELLOW);
init_pair(FG_BG(COLOR_CYAN, COLOR_BLUE),        COLOR_CYAN, COLOR_BLUE);
init_pair(FG_BG(COLOR_CYAN, COLOR_MAGENTA),     COLOR_CYAN, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_CYAN, COLOR_CYAN),        COLOR_CYAN, COLOR_CYAN);
init_pair(FG_BG(COLOR_CYAN, COLOR_WHITE),       COLOR_CYAN, COLOR_WHITE);
init_pair(FG_BG(COLOR_WHITE, COLOR_BLACK),      COLOR_WHITE, COLOR_BLACK);
init_pair(FG_BG(COLOR_WHITE, COLOR_RED),        COLOR_WHITE, COLOR_RED);
init_pair(FG_BG(COLOR_WHITE, COLOR_GREEN),      COLOR_WHITE, COLOR_GREEN);
init_pair(FG_BG(COLOR_WHITE, COLOR_YELLOW),     COLOR_WHITE, COLOR_YELLOW);
init_pair(FG_BG(COLOR_WHITE, COLOR_BLUE),       COLOR_WHITE, COLOR_BLUE);
init_pair(FG_BG(COLOR_WHITE, COLOR_MAGENTA),    COLOR_WHITE, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_WHITE, COLOR_CYAN),       COLOR_WHITE, COLOR_CYAN);
init_pair(FG_BG(COLOR_WHITE, COLOR_WHITE),      COLOR_WHITE, COLOR_WHITE);

    /* Set up colour pairs with default colour -1 */
#ifdef NCURSES_VERSION
    init_pair(FG_BG(COLOR_BLACK, COLOR_DEFAULT),    COLOR_BLACK, -1);
    init_pair(FG_BG(COLOR_RED, COLOR_DEFAULT),      COLOR_RED, -1);
    init_pair(FG_BG(COLOR_GREEN, COLOR_DEFAULT),    COLOR_GREEN, -1);
    init_pair(FG_BG(COLOR_YELLOW, COLOR_DEFAULT),   COLOR_YELLOW, -1);
    init_pair(FG_BG(COLOR_BLUE, COLOR_DEFAULT),     COLOR_BLUE, -1);
    init_pair(FG_BG(COLOR_MAGENTA, COLOR_DEFAULT),  COLOR_MAGENTA, -1);
    init_pair(FG_BG(COLOR_CYAN, COLOR_DEFAULT),     COLOR_CYAN, -1);
    init_pair(FG_BG(COLOR_WHITE, COLOR_DEFAULT),    COLOR_WHITE, -1);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_BLACK),    -1, COLOR_BLACK);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_RED),      -1, COLOR_RED);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_GREEN),    -1, COLOR_GREEN);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_YELLOW),   -1, COLOR_YELLOW);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_BLUE),     -1, COLOR_BLUE);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_MAGENTA),  -1, COLOR_MAGENTA);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_CYAN),     -1, COLOR_CYAN);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_WHITE),    -1, COLOR_WHITE);
    init_pair(FG_BG(COLOR_DEFAULT, COLOR_DEFAULT),  -1, -1);
#endif

    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cbreak(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    cbreak();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    nodelay(BF::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    nodelay(stdscr, BF);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    halfdelay(Tenths::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    halfdelay(Tenths);
    IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    stop(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    endwin();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

session(P, !IO) :-
    start(!IO),
    P(!IO),
    stop(!IO).

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    def_prog_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    def_prog_mode();
    IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    def_shell_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    def_shell_mode();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reset_prog_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    reset_prog_mode();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reset_shell_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    reset_shell_mode();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

suspend(Pred, Res, !IO) :-
    % ncurses programming HOWTO - Temporarily Leaving Curses mode.
    def_prog_mode(!IO),
    stop(!IO),
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        Pred(Res, !IO)
    then
        reset_prog_mode(!IO),
        refresh(!IO)
    catch_any Excp ->
        reset_prog_mode(!IO),
        refresh(!IO),
        exception.throw(Excp)
    ).

%-----------------------------------------------------------------------------%

soft_suspend(Pred, Res, !IO) :-
    % Don't know how to do this properly; this is the best I could achieve
    % through trial and error.
    %
    % We need to prepare for Pred to be able to draw to the screen itself.
    % But if Pred produces no output in most cases, we don't want to see a
    % brief flash as the terminal goes to non-alternate then back to alternate
    % mode, which we would get if we used endwin() to leave curses.
    def_prog_mode(!IO),
    reset_shell_mode(!IO),      % not endwin
    promise_equivalent_solutions [Res, !:IO]
    ( try [io(!IO)]
        Pred(Res, !IO)
    then
        reset_prog_mode(!IO),
        redrawwin_stdscr(!IO)   % not refresh
    catch_any Excp ->
        reset_prog_mode(!IO),
        redrawwin_stdscr(!IO),
        exception.throw(Excp)
    ).

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rows_cols(Rows::out, Cols::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    getmaxyx(stdscr, Rows, Cols);
    IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    move(Row::in, Col::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    move(Row, Col);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    erase(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    erase();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    clear(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    clear();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    redrawwin_stdscr(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    redrawwin(stdscr);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    addch(Attr::in, CharCode::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    addch((chtype)Attr | (chtype)CharCode);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    addstr(Str::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    addstr(Str);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    attr_on(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    attron(Attr);
    IO = IO0;
").

:- pragma foreign_proc("C",
    attr_off(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    attroff(Attr);
    IO = IO0;
").

:- pragma foreign_proc("C",
    attr_set(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    attrset(Attr);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    refresh(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    refresh();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    doupdate(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    doupdate();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getch(CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    CharCode = getch();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_wch(CharCode::out, IsCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    wint_t ch;
    int rc;

    rc = get_wch(&ch);
    if (rc == OK) {
        CharCode = ch;
        IsCode = MR_NO;
    } else if (rc == KEY_CODE_YES) {
        CharCode = ch;
        IsCode = MR_YES;
    } else {
        CharCode = 0;
        IsCode = MR_NO;
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", flushinp(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure], "

    flushinp();
    IO = IO0;

").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    err = (E::out),
    [will_not_call_mercury, promise_pure],
"
    E = ERR;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    key_down = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_DOWN;
").
:- pragma foreign_proc("C",
    key_up = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_UP;
").
:- pragma foreign_proc("C",
    key_left = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_LEFT;
").
:- pragma foreign_proc("C",
    key_right = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_RIGHT;
").
:- pragma foreign_proc("C",
    key_home = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_HOME;
").
:- pragma foreign_proc("C",
    key_backspace = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_BACKSPACE;
").
:- pragma foreign_proc("C",
    key_f(N::in) = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_F(N);
").
:- pragma foreign_proc("C",
    key_del = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_DC;
").
:- pragma foreign_proc("C",
    key_ins = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_IC;
").

:- pragma foreign_proc("C",
    key_pageup = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_PPAGE;
").

:- pragma foreign_proc("C",
    key_pagedown = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_NPAGE;
").

:- pragma foreign_proc("C",
    key_enter = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_ENTER;
").

:- pragma foreign_proc("C",
    key_a1 = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_A1;
").

:- pragma foreign_proc("C",
    key_a3 = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_A3;
").

:- pragma foreign_proc("C",
    key_b2 = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_B2;
").

:- pragma foreign_proc("C",
    key_c1 = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_C1;
").

:- pragma foreign_proc("C",
    key_c3 = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_C3;
").

:- pragma foreign_proc("C",
    key_btab = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_BTAB;
").

:- pragma foreign_proc("C",
    key_end = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_END;
").

:- pragma foreign_proc("C",
    key_resize = (K::out),
    [will_not_call_mercury, promise_pure],
"
    K = KEY_RESIZE;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    acs_block = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_BLOCK;
").

:- pragma foreign_proc("C",
    acs_board = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_BOARD;
").

:- pragma foreign_proc("C",
    acs_btee = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_BTEE;
").

:- pragma foreign_proc("C",
    acs_bullet = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_BULLET;
").

:- pragma foreign_proc("C",
    acs_ckboard = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_CKBOARD;
").

:- pragma foreign_proc("C",
    acs_darrow = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_DARROW;
").

:- pragma foreign_proc("C",
    acs_degree = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_DEGREE;
").

:- pragma foreign_proc("C",
    acs_diamond = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_DIAMOND;
").

:- pragma foreign_proc("C",
    acs_gequal = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_GEQUAL;
").

:- pragma foreign_proc("C",
    acs_hline = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_HLINE;
").

:- pragma foreign_proc("C",
    acs_lantern = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_LANTERN;
").

:- pragma foreign_proc("C",
    acs_larrow = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = ACS_LARROW;
").

:- pragma foreign_proc("C",
    acs_lequal = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_LEQUAL;
").

:- pragma foreign_proc("C", acs_llcorner = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_LLCORNER;
").

:- pragma foreign_proc("C", acs_lrcorner = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_LRCORNER;
").

:- pragma foreign_proc("C", acs_ltee = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_LTEE;
").

:- pragma foreign_proc("C", acs_nequal = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_NEQUAL;
").

:- pragma foreign_proc("C", acs_pi = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_PI;
").

:- pragma foreign_proc("C", acs_plminus = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_PLMINUS;
").

:- pragma foreign_proc("C", acs_plus = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_PLUS;
").

:- pragma foreign_proc("C", acs_rarrow = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_RARROW;
").

:- pragma foreign_proc("C", acs_rtee = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_RTEE;
").

:- pragma foreign_proc("C", acs_s1 = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_S1;
").

:- pragma foreign_proc("C", acs_s3 = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_S3;
").

:- pragma foreign_proc("C", acs_s7 = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_S7;
").

:- pragma foreign_proc("C", acs_s9 = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_S9;
").

:- pragma foreign_proc("C", acs_sterling = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_STERLING;
").

:- pragma foreign_proc("C", acs_ttee = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_TTEE;
").

:- pragma foreign_proc("C", acs_uarrow = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_UARROW;
").

:- pragma foreign_proc("C", acs_ulcorner = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_ULCORNER;
").

:- pragma foreign_proc("C", acs_urcorner = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_URCORNER;
").

:- pragma foreign_proc("C", acs_vline = (C::out),
    [will_not_call_mercury, promise_pure], "
    C = ACS_VLINE;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    curs.((Attr1::in) + (Attr2::in)) = (Attr::out),
    [will_not_call_mercury, promise_pure],
"
    Attr = (chtype)Attr1 | (chtype)Attr2;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", normal = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_NORMAL;
").
:- pragma foreign_proc("C", standout = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_STANDOUT;
").
:- pragma foreign_proc("C", underline = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_UNDERLINE;
").
:- pragma foreign_proc("C", reverse = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_REVERSE;
").
:- pragma foreign_proc("C", blink = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_BLINK;
").
:- pragma foreign_proc("C", dim = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_DIM;
").
:- pragma foreign_proc("C", bold = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_BOLD;
").
:- pragma foreign_proc("C", invis = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = A_INVIS;
").
:- pragma foreign_proc("C", fg_bg(Fg::in, Bg::in) = (A::out),
    [will_not_call_mercury, promise_pure], "
    A = COLOR_PAIR(FG_BG(Fg, Bg));
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    default = (C::out),
    [will_not_call_mercury, promise_pure],
"
#ifdef NCURSES_VERSION
    C = COLOR_DEFAULT;
#else
    C = COLOR_BLACK;
#endif
").

:- pragma foreign_proc("C",
    black = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_BLACK;
").

:- pragma foreign_proc("C",
    red = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_RED;
").

:- pragma foreign_proc("C",
    green = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_GREEN;
").

:- pragma foreign_proc("C",
    yellow = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_YELLOW;
").

:- pragma foreign_proc("C",
    blue = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_BLUE;
").

:- pragma foreign_proc("C",
    magenta = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_MAGENTA;
").

:- pragma foreign_proc("C",
    cyan = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_CYAN;
").

:- pragma foreign_proc("C",
    white = (C::out),
    [will_not_call_mercury, promise_pure],
"
    C = COLOR_WHITE;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    border(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    border(0, 0, 0, 0, 0, 0, 0, 0);
    IO = IO0;
").

:- pragma foreign_proc("C",
    hline(C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    hline(C, N);
    IO = IO0;
").

:- pragma foreign_proc("C",
    vline(C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    vline(C, N);
    IO = IO0;
").

    %-------------------------------------------------------------------------%
    %-------------------------------------------------------------------------%

    :- module panel.

    :- implementation.

    %-------------------------------------------------------------------------%

    :- pragma foreign_decl("C", "

        #include <ncurses.h>
        #include <panel.h>

    ").
    
    :- pragma foreign_type("C", panel, "PANEL *").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        new(Rows::in, Cols::in, Row::in, Col::in, Attr::in, Panel::out,
            IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
            WINDOW *w = newwin(Rows, Cols, Row, Col);
            scrollok(w, TRUE);
            wattrset(w, Attr);
            wcolor_set(w, Attr, NULL);
            wclear(w);
            Panel = new_panel(w);

            IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        delete(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        WINDOW *w = panel_window(Panel);
        del_panel(Panel);
        delwin(w);

        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        scrollok(Panel::in, Bool::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        scrollok(panel_window(Panel), (Bool == MR_YES));
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        getyx(Panel::in, Y::out, X::out, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        getyx(panel_window(Panel), Y, X);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        getmaxyx(Panel::in, Y::out, X::out, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        getmaxyx(panel_window(Panel), Y, X);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        raise(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        top_panel(Panel);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        lower(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        bottom_panel(Panel);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        hide(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        hide_panel(Panel);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        reveal(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        show_panel(Panel);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        relocate(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        move_panel(Panel, Row, Col);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        erase(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        werase(panel_window(Panel));
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        clear(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wclear(panel_window(Panel));
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        move(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wmove(panel_window(Panel), Row, Col);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        addch(Panel::in, Attr::in, CharCode::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        waddch(panel_window(Panel), (chtype)Attr | (chtype)CharCode);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        addstr(Panel::in, Str::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        waddstr(panel_window(Panel), Str);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        attr_on(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wattron(panel_window(Panel), Attr);
        IO = IO0;
    ").
    
    :- pragma foreign_proc("C",
        attr_off(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wattroff(panel_window(Panel), Attr);
        IO = IO0;
    ").

    :- pragma foreign_proc("C",
        attr_set(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wattrset(panel_window(Panel), Attr);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        update_panels(IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        update_panels();
        doupdate();
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%

    :- pragma foreign_proc("C",
        border(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wborder(panel_window(Panel), 0, 0, 0, 0, 0, 0, 0, 0);
        IO = IO0;
    ").

    :- pragma foreign_proc("C",
        hline(Panel::in, C::in, N::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        whline(panel_window(Panel), C, N);
        IO = IO0;
    ").

    :- pragma foreign_proc("C",
        vline(Panel::in, C::in, N::in, IO0::di, IO::uo),
        [will_not_call_mercury, promise_pure],
    "
        wvline(panel_window(Panel), C, N);
        IO = IO0;
    ").

    %-------------------------------------------------------------------------%
    :- end_module panel.
    %-------------------------------------------------------------------------%
    %-------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
