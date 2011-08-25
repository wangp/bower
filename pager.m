%-----------------------------------------------------------------------------%

:- module pager.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type pager_info.

:- pred setup_pager(int::in, list(message)::in, pager_info::out,
    io::di, io::uo) is det.

:- type pager_action
    --->    continue
    ;       leave_pager.

:- pred pager_input(screen::in, char::in, pager_action::out,
    message_update::out, pager_info::in, pager_info::out) is det.

:- pred draw_pager(screen::in, pager_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.

%-----------------------------------------------------------------------------%

:- type pager_info
    --->    pager_info(
                p_lines     :: list(pager_line),
                p_numlines  :: int,
                p_top       :: int
            ).

:- type pager_line
    --->    header(string, string)
    ;       text(quote_level, string)
    ;       attachment(content)
    ;       message_separator.

:- type quote_level
    --->    quote_level(int).

:- type binding
    --->    scroll_down
    ;       scroll_up
    ;       page_down
    ;       page_up
    ;       half_page_down
    ;       half_page_up
    ;       leave_pager.

:- func default_quote_level = quote_level.

default_quote_level = quote_level(0).

%-----------------------------------------------------------------------------%

setup_pager(Cols, Messages, Info, !IO) :-
    list.foldl(append_message(Cols), Messages, cord.init, LinesCord),
    Lines = list(LinesCord),
    NumLines = list.length(Lines),
    Top = 0,
    Info = pager_info(Lines, NumLines, Top).

:- pred append_message(int::in, message::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_message(Cols, Message, !Lines) :-
    append_header("Subject", Message ^ m_subject, !Lines),
    append_header("From", Message ^ m_from, !Lines),
    append_header("To", Message ^ m_to, !Lines),
    append_header("Date", Message ^ m_date, !Lines),
    snoc(blank_line, !Lines),
    Body = Message ^ m_body,
    ( cord.head_tail(Body, FirstPart, RestParts) ->
        append_content(Cols, yes, FirstPart, !Lines),
        cord.foldl_pred(append_content(Cols, no), RestParts, !Lines)
    ;
        true
    ),
    snoc(message_separator, !Lines),
    snoc(message_separator, !Lines),
    snoc(message_separator, !Lines).

:- pred append_header(string::in, string::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_header(Header, Value, !Lines) :-
    Line = header(Header, Value),
    snoc(Line, !Lines).

:- pred append_content(int::in, bool::in, content::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_content(Cols, IsFirst, Content, !Lines) :-
    Content = content(_Id, Type, MaybeText, _MaybeFilename),
    (
        IsFirst = yes,
        Type = "text/plain"
    ->
        true
    ;
        snoc(blank_line, !Lines),
        snoc(attachment(Content), !Lines)
    ),
    (
        MaybeText = yes(Text),
        Start = 0,
        LastBreak = 0,
        Cur = 0,
        append_text(Cols, Text, Start, LastBreak, Cur, no, !Lines)
    ;
        MaybeText = no,
        snoc(text(default_quote_level, "(not supported)"), !Lines)
    ).

:- pred append_text(int::in, string::in, int::in, int::in, int::in,
    maybe(quote_level)::in, cord(pager_line)::in, cord(pager_line)::out)
    is det.

append_text(Max, String, Start, LastBreak, Cur, QuoteLevel, !Lines) :-
    ( string.unsafe_index_next(String, Cur, Next, Char) ->
        (
            Char = '\n'
        ->
            append_substring(String, Start, Cur, QuoteLevel, _, !Lines),
            append_text(Max, String, Next, Next, Next, no, !Lines)
        ;
            char.is_whitespace(Char)
        ->
            append_text(Max, String, Start, Cur, Next, QuoteLevel, !Lines)
        ;
            % XXX this should actually count with wcwidth
            Next - Start > Max
        ->
            maybe_append_substring(String, Start, LastBreak, QuoteLevel,
                ContQuoteLevel, !Lines),
            skip_whitespace(String, LastBreak, NextStart),
            append_text(Max, String, NextStart, NextStart, Next,
                yes(ContQuoteLevel), !Lines)
        ;
            append_text(Max, String, Start, LastBreak, Next, QuoteLevel,
                !Lines)
        )
    ;
        % End of string.
        maybe_append_substring(String, Start, Cur, QuoteLevel, _, !Lines)
    ).

:- pred maybe_append_substring(string::in, int::in, int::in,
    maybe(quote_level)::in, quote_level::out,
    cord(pager_line)::in, cord(pager_line)::out) is det.

maybe_append_substring(String, Start, End, QuoteLevel, ContQuoteLevel,
        !Lines) :-
    ( End > Start ->
        append_substring(String, Start, End, QuoteLevel, ContQuoteLevel,
            !Lines)
    ;
        (
            QuoteLevel = yes(ContQuoteLevel)
        ;
            QuoteLevel = no,
            ContQuoteLevel = default_quote_level
        )
    ).

:- pred append_substring(string::in, int::in, int::in, maybe(quote_level)::in,
    quote_level::out, cord(pager_line)::in, cord(pager_line)::out) is det.

append_substring(String, Start, End, QuoteLevel, ContQuoteLevel, !Lines) :-
    string.between(String, Start, End, SubString),
    (
        QuoteLevel = yes(Level)
    ;
        QuoteLevel = no,
        LevelInt = detect_quote_level(SubString, 0),
        Level = quote_level(LevelInt)
    ),
    snoc(text(Level, SubString), !Lines),
    ContQuoteLevel = Level.

:- func detect_quote_level(string, int) = int.

detect_quote_level(String, Pos) = QuoteLevel :-
    ( string.unsafe_index_next(String, Pos, NextPos, Char) ->
        ( char.is_whitespace(Char) ->
            QuoteLevel = detect_quote_level(String, NextPos)
        ; Char = ('>') ->
            QuoteLevel = 1 + detect_quote_level(String, NextPos)
        ;
            QuoteLevel = 0
        )
    ;
        QuoteLevel = 0
    ).

:- pred skip_whitespace(string::in, int::in, int::out) is det.

skip_whitespace(String, I0, I) :-
    ( string.unsafe_index_next(String, I0, I1, Char) ->
        ( char.is_whitespace(Char) ->
            skip_whitespace(String, I1, I)
        ;
            I = I0
        )
    ;
        I = I0
    ).

:- func blank_line = pager_line.

blank_line = text(default_quote_level, "").

%-----------------------------------------------------------------------------%

pager_input(Screen, Char, Action, MessageUpdate, !Info) :-
    ( key_binding(Char, Binding) ->
        % XXX should cache the number of rows
        NumRows = list.length(Screen ^ main_panels),
        (
            Binding = scroll_down,
            scroll(NumRows, 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = scroll_up,
            scroll(NumRows, -1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = page_down,
            scroll(NumRows, NumRows - 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = page_up,
            scroll(NumRows, -NumRows + 1, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = half_page_down,
            scroll(NumRows, NumRows//2, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = half_page_up,
            scroll(NumRows, -NumRows//2, MessageUpdate, !Info),
            Action = continue
        ;
            Binding = leave_pager,
            Action = leave_pager,
            MessageUpdate = clear_message
        )
    ;
        Action = continue,
        MessageUpdate = no_change
    ).

:- pred key_binding(char::in, binding::out) is semidet.

key_binding('j', scroll_down).
key_binding('\r', scroll_down).
key_binding('k', scroll_up).
key_binding('\\', scroll_up).
key_binding('\b', scroll_up).
key_binding(' ', page_down).
key_binding('b', page_up).
key_binding(']', half_page_down).
key_binding('[', half_page_up).
key_binding('i', leave_pager).

:- pred scroll(int::in, int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

scroll(NumRows, Delta, MessageUpdate, !Info) :-
    NumLines = !.Info ^ p_numlines,
    Top0 = !.Info ^ p_top,
    TopLimit = int.max(0, NumLines - NumRows),
    Top = clamp(0, Top0 + Delta, TopLimit),
    ( Top = Top0, Delta < 0 ->
        MessageUpdate = set_warning("Top of message is shown.")
    ; Top = Top0, Delta > 0 ->
        MessageUpdate = set_warning("Bottom of message is shown.")
    ;
        MessageUpdate = clear_message
    ),
    !Info ^ p_top := Top.

:- func clamp(int, int, int) = int.

clamp(Min, X, Max) =
    ( X < Min -> Min
    ; X > Max -> Max
    ; X
    ).

%-----------------------------------------------------------------------------%

draw_pager(Screen, Info, !IO) :-
    MainPanels = Screen ^ main_panels,
    Info = pager_info(Lines0, _NumLines, Top),
    ( list.drop(Top, Lines0, Lines1) ->
        Lines = Lines1
    ;
        Lines = []
    ),
    draw_pager_lines(MainPanels, Lines, !IO).

:- pred draw_pager_lines(list(panel)::in, list(pager_line)::in,
    io::di, io::uo) is det.

draw_pager_lines([], _, !IO).
draw_pager_lines([Panel | Panels], Lines, !IO) :-
    panel.erase(Panel, !IO),
    (
        Lines = [Line | RestLines],
        draw_pager_line(Panel, Line, !IO)
    ;
        Lines = [],
        RestLines = []
    ),
    draw_pager_lines(Panels, RestLines, !IO).

:- pred draw_pager_line(panel::in, pager_line::in, io::di, io::uo) is det.

draw_pager_line(Panel, Line, !IO) :-
    (
        Line = header(Header, Value),
        panel.attr_set(Panel, fg_bg(red, black) + bold, !IO),
        my_addstr(Panel, "  ", !IO),
        my_addstr(Panel, Header, !IO),
        my_addstr(Panel, ": ", !IO),
        panel.attr_set(Panel, normal, !IO),
        my_addstr(Panel, Value, !IO)
    ;
        Line = text(QuoteLevel, Text),
        Attr = quote_level_to_attr(QuoteLevel),
        panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, Text, !IO)
    ;
        Line = attachment(Content),
        Content ^ c_type = ContentType,
        Content ^ c_filename = MaybeFilename,
        panel.attr_set(Panel, fg_bg(magenta, black) + bold, !IO),
        my_addstr(Panel, "[-- ", !IO),
        my_addstr(Panel, ContentType, !IO),
        (
            MaybeFilename = yes(Filename),
            my_addstr(Panel, "; ", !IO),
            my_addstr(Panel, Filename, !IO)
        ;
            MaybeFilename = no
        ),
        my_addstr(Panel, " --]", !IO)
    ;
        Line = message_separator,
        panel.attr_set(Panel, fg_bg(blue, black) + bold, !IO),
        my_addstr(Panel, "~", !IO)
    ).

:- func quote_level_to_attr(quote_level) = attr.

quote_level_to_attr(quote_level(QuoteLevel)) =
    ( QuoteLevel = 0 ->
        normal
    ; int.odd(QuoteLevel) ->
        fg_bg(blue, black) + bold
    ;
        fg_bg(green, black)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
