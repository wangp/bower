%-----------------------------------------------------------------------------%

:- module thread_pager.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type thread_pager_info.

:- pred setup_thread_pager(int::in, list(message)::in, thread_pager_info::out)
    is det.

:- pred draw_thread_pager(screen::in, thread_pager_info::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module require.

:- import_module curs.
:- import_module curs.panel.
:- import_module scrollable.

%-----------------------------------------------------------------------------%

:- type thread_pager_info
    --->    thread_pager_info(
                tp_scrollable   :: scrollable(thread_line)
            ).

:- type thread_line
    --->    thread_line(
                tp_graphics :: list(graphic),
                tp_message  :: message
            ).

:- type graphic
    --->    blank
    ;       vert
    ;       tee
    ;       ell.

:- instance scrollable.line(thread_line) where [
    pred(draw_line/5) is draw_thread_line
].

%-----------------------------------------------------------------------------%

setup_thread_pager(_Cols, Messages, ThreadPagerInfo) :-
    append_messages([], [], Messages, cord.init, ThreadCord),
    ThreadLines = list(ThreadCord),
    Scrollable = scrollable.init_with_cursor(ThreadLines, 0),
    ThreadPagerInfo = thread_pager_info(Scrollable).

:- pred append_messages(list(graphic)::in, list(graphic)::in,
    list(message)::in, cord(thread_line)::in, cord(thread_line)::out) is det.

append_messages(_Above, _Below, [], !Cord).
append_messages(Above0, Below0, [Message | Messages], !Cord) :-
    (
        Messages = [],
        Line = thread_line(Above0 ++ [ell], Message),
        snoc(Line, !Cord),
        MessagesCord = cord.empty,
        Below1 = Below0
    ;
        Messages = [_ | _],
        Line = thread_line(Above0 ++ [tee], Message),
        snoc(Line, !Cord),
        append_messages(Above0, Below0, Messages, cord.init, MessagesCord),
        ( get_first(MessagesCord, FollowingLine) ->
            Below1 = FollowingLine ^ tp_graphics
        ;
            unexpected($module, $pred, "empty cord")
        )
    ),
    ( not_blank_at_column(Below1, length(Above0)) ->
        Above1 = Above0 ++ [vert]
    ;
        Above1 = Above0 ++ [blank]
    ),
    append_messages(Above1, Below1, Message ^ m_replies, !Cord),
    !:Cord = !.Cord ++ MessagesCord.

:- pred not_blank_at_column(list(graphic)::in, int::in) is semidet.

not_blank_at_column(Graphics, Col) :-
    list.index0(Graphics, Col, Graphic),
    Graphic \= blank.

draw_thread_pager(Screen, ThreadPagerInfo, !IO) :-
    MainPanels = Screen ^ main_panels,
    ThreadPagerInfo = thread_pager_info(Scrollable),
    scrollable.draw(MainPanels, Scrollable, !IO).

:- pred draw_thread_line(panel::in, thread_line::in, bool::in,
    io::di, io::uo) is det.

draw_thread_line(Panel, Line, _IsCursor, !IO) :-
    Graphics = Line ^ tp_graphics,
    Message = Line ^ tp_message,
    From = Message ^ m_from,
    list.foldl(draw_graphic(Panel), Graphics, !IO),
    my_addstr(Panel, "> ", !IO),
    my_addstr(Panel, From, !IO).

:- pred draw_graphic(panel::in, graphic::in, io::di, io::uo) is det.

draw_graphic(Panel, Graphic, !IO) :-
    my_addstr(Panel, graphic_to_char(Graphic), !IO).

:- func graphic_to_char(graphic) = string.

graphic_to_char(blank) = " ".
graphic_to_char(vert) = "│".
graphic_to_char(tee) = "├".
graphic_to_char(ell) = "└".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
