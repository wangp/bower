%-----------------------------------------------------------------------------%

:- module pager.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- import_module curs.
:- import_module curs.panel.
:- import_module data.
:- import_module screen.

%-----------------------------------------------------------------------------%

:- type pager_info.

:- pred setup_pager(int::in, list(message)::in, pager_info::out) is det.

:- pred setup_pager_for_staging(int::in, string::in, pager_info::out) is det.

:- type pager_action
    --->    continue
    ;       leave_pager.

:- pred pager_input(screen::in, char::in, pager_action::out,
    message_update::out, pager_info::in, pager_info::out) is det.

:- pred scroll(int::in, int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- pred next_message(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred prev_message(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred skip_quoted_text(message_update::out, pager_info::in, pager_info::out)
    is det.

:- pred get_top_message(pager_info::in, message::out) is semidet.

:- pred skip_to_message(message_id::in, pager_info::in, pager_info::out)
    is det.

:- type search_kind
    --->    new_search
    ;       continue_search.

:- pred skip_to_search(int::in, search_kind::in, string::in,
    message_update::out, pager_info::in, pager_info::out) is det.

:- pred highlight_attachment(int::in, message_update::out,
    pager_info::in, pager_info::out) is det.

:- pred get_highlighted_attachment(pager_info::in, part::out) is semidet.

:- pred draw_pager(screen::in, pager_info::in, io::di, io::uo) is det.

:- pred draw_pager_lines(list(panel)::in, pager_info::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module version_array.

:- import_module scrollable.
:- import_module string_util.

%-----------------------------------------------------------------------------%

:- type pager_info
    --->    pager_info(
                p_scrollable :: scrollable(pager_line)
            ).

:- type pager_line
    --->    start_message_header(message, string, string)
    ;       header(string, string)
    ;       text(string)
    ;       quoted_text(quote_level, string)
    ;       diff_text(diff_line, string)
    ;       attachment(part)
    ;       message_separator.

:- type quote_level == int.

:- type diff_line
    --->    diff_add
    ;       diff_rem
    ;       diff_hunk
    ;       diff_index.

:- type binding
    --->    leave_pager
    ;       scroll_down
    ;       scroll_up
    ;       page_down
    ;       page_up
    ;       half_page_down
    ;       half_page_up
    ;       next_message
    ;       prev_message
    ;       skip_quoted_text.

:- instance scrollable.line(pager_line) where [
    pred(draw_line/5) is draw_pager_line
].

%-----------------------------------------------------------------------------%

setup_pager(Cols, Messages, Info) :-
    list.foldl(append_message(Cols), Messages, cord.init, LinesCord),
    Lines = list(LinesCord),
    Scrollable = scrollable.init(Lines),
    Info = pager_info(Scrollable).

:- pred append_message(int::in, message::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_message(Cols, Message, !Lines) :-
    Headers = Message ^ m_headers,
    Subject = Headers ^ h_subject,
    StartMessage = start_message_header(Message, "Subject", Subject),
    snoc(StartMessage, !Lines),
    append_header("Date", Headers ^ h_date, !Lines),
    append_header("From", Headers ^ h_from, !Lines),
    append_header("To", Headers ^ h_to, !Lines),
    Cc = Headers ^ h_cc,
    ( Cc = "" ->
        true
    ;
        append_header("Cc", Cc, !Lines)
    ),
    ReplyTo = Headers ^ h_replyto,
    ( ReplyTo = "" ->
        true
    ;
        append_header("Reply-To", ReplyTo, !Lines)
    ),
    snoc(blank_line, !Lines),
    Body = Message ^ m_body,
    ( cord.head_tail(Body, FirstPart, RestParts) ->
        append_part(Cols, yes, FirstPart, !Lines),
        cord.foldl_pred(append_part(Cols, no), RestParts, !Lines)
    ;
        true
    ),
    snoc(message_separator, !Lines),
    snoc(message_separator, !Lines),
    snoc(message_separator, !Lines),
    Replies = Message ^ m_replies,
    list.foldl(append_message(Cols), Replies, !Lines).

:- pred append_header(string::in, string::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_header(Header, Value, !Lines) :-
    Line = header(Header, Value),
    snoc(Line, !Lines).

:- pred append_part(int::in, bool::in, part::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_part(Cols, IsFirst, Part, !Lines) :-
    Part = part(_MsgId, _Part, Type, MaybeText, _MaybeFilename),
    (
        IsFirst = yes,
        strcase_equal(Type, "text/plain")
    ->
        true
    ;
        snoc(blank_line, !Lines),
        snoc(attachment(Part), !Lines)
    ),
    (
        MaybeText = yes(Text),
        Start = 0,
        LastBreak = 0,
        Cur = 0,
        QuoteLevel = no,
        InDiff = no,
        append_text(Cols, Text, Start, LastBreak, Cur, QuoteLevel, InDiff,
            !Lines)
    ;
        MaybeText = no,
        snoc(text("(not supported)"), !Lines)
    ).

:- pred append_text(int::in, string::in, int::in, int::in, int::in,
    maybe(quote_level)::in, bool::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_text(Max, String, Start, LastBreak, Cur, QuoteLevel, !.InDiff, !Lines) :-
    ( string.unsafe_index_next(String, Cur, Next, Char) ->
        (
            Char = '\n'
        ->
            append_substring(String, Start, Cur, QuoteLevel, _, !InDiff, !Lines),
            append_text(Max, String, Next, Next, Next, no, !.InDiff, !Lines)
        ;
            char.is_whitespace(Char)
        ->
            append_text(Max, String, Start, Cur, Next, QuoteLevel, !.InDiff,
                !Lines)
        ;
            % XXX this should actually count with wcwidth
            Next - Start > Max
        ->
            maybe_append_substring(String, Start, LastBreak, QuoteLevel,
                ContQuoteLevel, !InDiff, !Lines),
            skip_whitespace(String, LastBreak, NextStart),
            append_text(Max, String, NextStart, NextStart, Next,
                yes(ContQuoteLevel), !.InDiff, !Lines)
        ;
            append_text(Max, String, Start, LastBreak, Next, QuoteLevel,
                !.InDiff, !Lines)
        )
    ;
        % End of string.
        maybe_append_substring(String, Start, Cur, QuoteLevel, _,
            !.InDiff, _, !Lines)
    ).

:- pred maybe_append_substring(string::in, int::in, int::in,
    maybe(quote_level)::in, quote_level::out, bool::in, bool::out,
    cord(pager_line)::in, cord(pager_line)::out) is det.

maybe_append_substring(String, Start, End, QuoteLevel, ContQuoteLevel,
        !InDiff, !Lines) :-
    ( End > Start ->
        append_substring(String, Start, End, QuoteLevel, ContQuoteLevel,
            !InDiff, !Lines)
    ;
        (
            QuoteLevel = yes(ContQuoteLevel)
        ;
            QuoteLevel = no,
            ContQuoteLevel = 0
        )
    ).

:- pred append_substring(string::in, int::in, int::in, maybe(quote_level)::in,
    quote_level::out, bool::in, bool::out,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_substring(String, Start, End, MaybeLevel, ContQuoteLevel, !InDiff,
        !Lines) :-
    string.unsafe_between(String, Start, End, SubString),
    (
        MaybeLevel = yes(Level)
    ;
        MaybeLevel = no,
        Level = detect_quote_level(SubString, 0)
    ),
    ( Level = 0 ->
        append_unquoted_string(SubString, !InDiff, !Lines)
    ;
        snoc(quoted_text(Level, SubString), !Lines)
    ),
    ContQuoteLevel = Level.

:- pred append_unquoted_string(string::in, bool::in, bool::out,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_unquoted_string(String, !InDiff, !Lines) :-
    (  detect_diff(String, !.InDiff, DiffLine) ->
        !:InDiff = yes,
        snoc(diff_text(DiffLine, String), !Lines)
    ;
        snoc(text(String), !Lines)
    ).

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

:- pred detect_diff(string::in, bool::in, diff_line::out) is semidet.

detect_diff(String, InDiff, Diff) :-
    ( String \= "" ->
        string.unsafe_index(String, 0, Char),
        (
            Char = ('+'),
            InDiff = yes,
            Diff = diff_add
        ;
            Char = ('-'),
            not string.all_match(unify('-'), String),
            InDiff = yes,
            Diff = diff_rem
        ;
            Char = ('@'),
            string.prefix(String, "@@ "),
            Diff = diff_hunk
        ;
            Char = 'd',
            string.prefix(String, "diff -"),
            Diff = diff_index
        ;
            Char = 'I',
            string.prefix(String, "Index: "),
            Diff = diff_index
        )
    ;
        fail
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

blank_line = text("").

%-----------------------------------------------------------------------------%

setup_pager_for_staging(Cols, Text, Info) :-
    Start = 0,
    LastBreak = 0,
    Cur = 0,
    MaybeLevel = no,
    InDiff = no,
    append_text(Cols, Text, Start, LastBreak, Cur, MaybeLevel, InDiff,
        cord.init, LinesCord),
    Lines = list(LinesCord),
    Scrollable = scrollable.init(Lines),
    Info = pager_info(Scrollable).

%-----------------------------------------------------------------------------%

pager_input(Screen, Char, Action, MessageUpdate, !Info) :-
    ( key_binding(Char, Binding) ->
        % XXX should cache the number of rows
        NumRows = list.length(Screen ^ main_panels),
        (
            Binding = leave_pager,
            Action = leave_pager,
            MessageUpdate = clear_message
        ;
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
            Binding = next_message,
            next_message(MessageUpdate, !Info),
            Action = continue
        ;
            Binding = prev_message,
            prev_message(MessageUpdate, !Info),
            Action = continue
        ;
            Binding = skip_quoted_text,
            skip_quoted_text(MessageUpdate, !Info),
            Action = continue
        )
    ;
        Action = continue,
        MessageUpdate = no_change
    ).

:- pred key_binding(char::in, binding::out) is semidet.

key_binding('i', leave_pager).
key_binding('\r', scroll_down).
key_binding('\\', scroll_up).
key_binding('\b', scroll_up).   % XXX doesn't work
key_binding(' ', page_down).
key_binding('b', page_up).
key_binding(']', half_page_down).
key_binding('[', half_page_up).
key_binding('j', next_message).
key_binding('k', prev_message).
key_binding('S', skip_quoted_text).

scroll(NumRows, Delta, MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    scroll(NumRows, Delta, HitLimit, Scrollable0, Scrollable),
    !:Info = pager_info(Scrollable),
    (
        HitLimit = yes,
        ( Delta < 0 ->
            MessageUpdate = set_warning("Top of message is shown.")
        ;
            MessageUpdate = set_warning("Bottom of message is shown.")
        )
    ;
        HitLimit = no,
        MessageUpdate = clear_message
    ).

next_message(MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    Top0 = get_top(Scrollable0),
    ( search_forward(is_message_start, Scrollable0, Top0 + 1, Top, _) ->
        set_top(Top, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("Already at last message.")
    ).

prev_message(MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    Top0 = get_top(Scrollable0),
    ( search_reverse(is_message_start, Scrollable0, Top0 - 1, Top) ->
        set_top(Top, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("Already at first message.")
    ).

:- pred is_message_start(pager_line::in) is semidet.

is_message_start(start_message_header(_, _, _)).

skip_quoted_text(MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    Top0 = get_top(Scrollable0),
    (
        search_forward(is_quoted_text_or_message_start, Scrollable0,
            Top0 + 1, Top1, _),
        search_forward(is_unquoted_text, Scrollable0, Top1, Top, _)
    ->
        set_top(Top, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No more quoted text.")
    ).

:- pred is_quoted_text_or_message_start(pager_line::in) is semidet.

is_quoted_text_or_message_start(Line) :-
    (
        Line = quoted_text(_, _)
    ;
        Line = start_message_header(_, _, _)
    ).

:- pred is_unquoted_text(pager_line::in) is semidet.

is_unquoted_text(Line) :-
    Line \= quoted_text(_, _).

%-----------------------------------------------------------------------------%

get_top_message(Info, Message) :-
    % XXX we could keep an array for binary search
    Info = pager_info(Scrollable),
    Top = get_top(Scrollable),
    Lines = get_lines(Scrollable),
    get_top_message_2(Lines, Top, Message).

:- pred get_top_message_2(version_array(pager_line)::in, int::in, message::out)
    is semidet.

get_top_message_2(Lines, I, Message) :-
    ( I >= 0 ->
        Line = version_array.lookup(Lines, I),
        ( Line = start_message_header(Message0, _, _) ->
            Message = Message0
        ;
            get_top_message_2(Lines, I - 1, Message)
        )
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

skip_to_message(MessageId, !Info) :-
    !.Info = pager_info(Scrollable0),
    ( search_forward(is_message_start(MessageId), Scrollable0, 0, Top, _) ->
        set_top(Top, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable)
    ;
        true
    ).

:- pred is_message_start(message_id::in, pager_line::in) is semidet.

is_message_start(MessageId, start_message_header(Message, _, _)) :-
    Message ^ m_id = MessageId.

%-----------------------------------------------------------------------------%

skip_to_search(NumRows, SearchKind, Search, MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    Top0 = get_top(Scrollable0),
    Bot = Top0 + NumRows,
    (
        SearchKind = continue_search,
        get_cursor(Scrollable0, Cursor0),
        Cursor0 >= Top0,
        Cursor0 < Bot
    ->
        Cursor1 = Cursor0 + 1
    ;
        Cursor1 = Top0
    ),
    (
        search_forward(line_matches_search(Search), Scrollable0,
            Cursor1, Cursor, _MatchLine)
    ->
        (
            % Jump to the message containing the match, if it wasn't already.
            search_reverse(is_message_start, Scrollable0, Cursor + 1,
                NewMsgStart),
            search_reverse(is_message_start, Scrollable0, Top0 + 1,
                OldMsgStart),
            NewMsgStart \= OldMsgStart
        ->
            set_top(NewMsgStart, Scrollable0, Scrollable1)
        ;
            Scrollable1 = Scrollable0
        ),
        set_cursor_visible(Cursor, NumRows, Scrollable1, Scrollable),
        MessageUpdate = clear_message
    ;
        set_cursor_none(Scrollable0, Scrollable),
        MessageUpdate = set_warning("Not found.")
    ),
    !:Info = pager_info(Scrollable).

:- pred line_matches_search(string::in, pager_line::in) is semidet.

line_matches_search(Search, Line) :-
    require_complete_switch [Line]
    (
        ( Line = start_message_header(_, _, String)
        ; Line = header(_, String)
        ; Line = text(String)
        ; Line = quoted_text(_, String)
        ; Line = diff_text(_, String)
        ),
        strcase_str(String, Search)
    ;
        ( Line = attachment(_)
        ; Line = message_separator
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

highlight_attachment(NumRows, MessageUpdate, !Info) :-
    !.Info = pager_info(Scrollable0),
    Top = get_top(Scrollable0),
    Bot = Top + NumRows,
    (
        get_cursor(Scrollable0, Cur0),
        Cur0 >= Top,
        Cur0 < Bot
    ->
        Start = Cur0 + 1
    ;
        Start = Top
    ),
    ( search_forward_limit(is_attachment, Scrollable0, Start, Bot, Cur, _) ->
        set_cursor(Cur, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable),
        MessageUpdate = clear_message
    ; search_forward_limit(is_attachment, Scrollable0, Top, Bot, Cur, _) ->
        set_cursor(Cur, Scrollable0, Scrollable),
        !:Info = pager_info(Scrollable),
        MessageUpdate = clear_message
    ;
        MessageUpdate = set_warning("No attachment visible.")
    ).

:- pred is_attachment(pager_line::in) is semidet.

is_attachment(attachment(_)).

get_highlighted_attachment(Info, Content) :-
    Info = pager_info(Scrollable),
    get_cursor_line(Scrollable, _, Line),
    Line = attachment(Content).

%-----------------------------------------------------------------------------%

draw_pager(Screen, Info, !IO) :-
    MainPanels = Screen ^ main_panels,
    draw_pager_lines(MainPanels, Info, !IO).

draw_pager_lines(Panels, Info, !IO) :-
    Info = pager_info(Scrollable),
    scrollable.draw(Panels, Scrollable, !IO).

:- pred draw_pager_line(panel::in, pager_line::in, bool::in,
    io::di, io::uo) is det.

draw_pager_line(Panel, Line, IsCursor, !IO) :-
    (
        ( Line = start_message_header(_Message, Header, Value)
        ; Line = header(Header, Value)
        ),
        panel.attr_set(Panel, fg_bg(red, black) + bold, !IO),
        my_addstr(Panel, "| ", !IO),
        my_addstr(Panel, Header, !IO),
        my_addstr(Panel, ": ", !IO),
        (
            IsCursor = yes,
            Attr = reverse
        ;
            IsCursor = no,
            Attr = normal
        ),
        panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, Value, !IO)
    ;
        (
            Line = text(Text),
            Attr0 = normal
        ;
            Line = quoted_text(QuoteLevel, Text),
            Attr0 = quote_level_to_attr(QuoteLevel)
        ;
            Line = diff_text(DiffLine, Text),
            Attr0 = diff_line_to_attr(DiffLine)
        ),
        (
            IsCursor = yes,
            Attr = Attr0 + reverse
        ;
            IsCursor = no,
            Attr = Attr0
        ),
        panel.attr_set(Panel, Attr, !IO),
        my_addstr(Panel, Text, !IO)
    ;
        Line = attachment(Part),
        Part ^ pt_type = ContentType,
        Part ^ pt_filename = MaybeFilename,
        (
            IsCursor = yes,
            Attr = fg_bg(magenta, black) + reverse
        ;
            IsCursor = no,
            Attr = fg_bg(magenta, black) + bold
        ),
        panel.attr_set(Panel, Attr, !IO),
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

quote_level_to_attr(QuoteLevel) =
    ( int.odd(QuoteLevel) ->
        fg_bg(blue, black) + bold
    ;
        fg_bg(green, black)
    ).

:- func diff_line_to_attr(diff_line) = attr.

diff_line_to_attr(diff_add) = fg_bg(cyan, black) + bold.
diff_line_to_attr(diff_rem) = fg_bg(red, black) + bold.
diff_line_to_attr(diff_hunk) = fg_bg(yellow, black) + bold.
diff_line_to_attr(diff_index) = fg_bg(green, black) + bold.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
