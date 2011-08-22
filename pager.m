%-----------------------------------------------------------------------------%

:- module pager.
:- interface.

:- import_module io.
:- import_module list.

:- import_module data.

%-----------------------------------------------------------------------------%

:- pred pager(list(message)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type pager_line
    --->    header(string, string)
    ;       text(quote_level, string)
    ;       attachment(content)
    ;       message_separator.

:- type quote_level
    --->    quote_level(int).

:- func default_quote_level = quote_level.

default_quote_level = quote_level(0).

%-----------------------------------------------------------------------------%

pager(Messages, !IO) :-
    list.foldl(append_message, Messages, cord.init, Lines),
    % XXX actual paging behaviour will have to wait
    cord.foldl_pred(print_line, Lines, !IO).

:- pred append_message(message::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_message(Message, !Lines) :-
    append_header("Subject", Message ^ m_subject, !Lines),
    append_header("From", Message ^ m_from, !Lines),
    append_header("To", Message ^ m_to, !Lines),
    append_header("Date", Message ^ m_date, !Lines),
    snoc(blank_line, !Lines),
    Body = Message ^ m_body,
    ( cord.head_tail(Body, FirstPart, RestParts) ->
        append_content(yes, FirstPart, !Lines),
        cord.foldl_pred(append_content(no), RestParts, !Lines)
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

:- pred append_content(bool::in, content::in,
    cord(pager_line)::in, cord(pager_line)::out) is det.

append_content(IsFirst, Content, !Lines) :-
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
        % XXX detect terminal width
        Max = 80,
        Start = 0,
        LastBreak = 0,
        Cur = 0,
        append_text(Max, Text, Start, LastBreak, Cur, no, !Lines)
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

:- pred print_line(pager_line::in, io::di, io::uo) is det.

print_line(Line, !IO) :-
    (
        Line = header(Header, Value),
        io.write_string("  ", !IO),
        io.write_string(ansi_red, !IO),
        io.write_string(Header, !IO),
        io.write_string(": ", !IO),
        io.write_string(ansi_default, !IO),
        io.write_string(Value, !IO),
        io.nl(!IO)
    ;
        Line = text(QuoteLevel, Text),
        Color = quote_level_to_color(QuoteLevel),
        io.write_string(Color, !IO),
        io.write_string(Text, !IO),
        io.write_string(ansi_default, !IO),
        io.nl(!IO)
    ;
        Line = attachment(Content),
        Content ^ c_type = ContentType,
        Content ^ c_filename = MaybeFilename,
        io.write_string(ansi_magenta, !IO),
        io.write_string("[-- ", !IO),
        io.write_string(ContentType, !IO),
        (
            MaybeFilename = yes(Filename),
            io.write_string("; ", !IO),
            io.write_string(Filename, !IO)
        ;
            MaybeFilename = no
        ),
        io.write_string(" --]", !IO),
        io.write_string(ansi_default, !IO),
        io.nl(!IO)
    ;
        Line = message_separator,
        io.write_string(ansi_blue, !IO),
        io.write_string("~", !IO),
        io.write_string(ansi_default, !IO),
        io.nl(!IO)
    ).

:- func quote_level_to_color(quote_level) = string.

quote_level_to_color(quote_level(QuoteLevel)) =
    ( QuoteLevel = 0 ->
        ansi_default
    ; int.odd(QuoteLevel) ->
        ansi_blue
    ;
        ansi_darkgreen
    ).

%-----------------------------------------------------------------------------%

:- func ansi_default = string.
:- func ansi_darkred = string.
:- func ansi_red = string.
:- func ansi_darkgreen = string.
:- func ansi_green = string.
:- func ansi_yellow = string.
:- func ansi_blue = string.
:- func ansi_magenta = string.
:- func ansi_cyan = string.

ansi_default    = "\x1B\[0m".
ansi_darkred    = "\x1B\[31m".
ansi_red        = "\x1B\[31;01m".
ansi_darkgreen  = "\x1B\[32m".
ansi_green      = "\x1B\[32;01m".
ansi_yellow     = "\x1B\[33;01m".
ansi_blue       = "\x1B\[34;01m".
ansi_magenta    = "\x1B\[35;01m".
ansi_cyan       = "\x1B\[36;01m".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
