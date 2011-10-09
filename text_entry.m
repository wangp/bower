%-----------------------------------------------------------------------------%

:- module text_entry.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module screen.

:- type history.

:- func init_history = history.

:- pred add_history_nodup(string::in, history::in, history::out) is det.

:- pred text_entry(screen::in, string::in, history::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred text_entry_initial(screen::in, string::in, history::in, string::in,
    maybe(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module pair.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.

:- type history == list(string). % reverse

%-----------------------------------------------------------------------------%

init_history = [].

add_history_nodup(Candidate, Hist0, Hist) :-
    ( Hist0 = [Candidate | _] ->
        Hist = Hist0
    ;
        Hist = [Candidate | Hist0]
    ).

%-----------------------------------------------------------------------------%

text_entry(Screen, Prompt, History0, Return, !IO) :-
    (
        History0 = [],
        Initial = "",
        History = []
    ;
        History0 = [Initial | History]
    ),
    text_entry_initial(Screen, Prompt, History, Initial, Return, !IO).

text_entry_initial(Screen, Prompt, History, Initial, Return, !IO) :-
    string.to_char_list(Initial, Before0),
    list.reverse(Before0, Before),
    After = [],
    FirstTime = yes,
    text_entry(Screen, Prompt, History - [], Before, After, FirstTime,
        Return, !IO),
    update_message(Screen, clear_message, !IO).

:- pred text_entry(screen::in, string::in, pair(history, history)::in,
    list(char)::in, list(char)::in, bool::in, maybe(string)::out,
    io::di, io::uo) is det.

text_entry(Screen, Prompt, History, Before, After, FirstTime, Return, !IO) :-
    draw_text_entry(Screen, Prompt, Before, After, !IO),
    get_char(Char, !IO),
    (
        ( Char = '\x07\' % BEL (^G)
        ; Char = '\x1b\' % ESC
        )
    ->
        Return = no
    ;
        Char = '\r' % CR
    ->
        String = char_lists_to_string(Before, After),
        Return = yes(String)
    ;
        ( Char = '\x7f\' % DEL
        ; Char = '\x08\' % BS
        )
    ->
        (
            Before = [],
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        ;
            Before = [_ | Before1],
            text_entry(Screen, Prompt, History, Before1, After, Return, !IO)
        )
    ;
        Char = '\x02\' % ^B
    ->
        (
            Before = [B | Before1],
            text_entry(Screen, Prompt, History, Before1, [B | After], Return, !IO)
        ;
            Before = [],
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        )
    ;
        Char = '\x06\' % ^F
    ->
        (
            After = [A | After1],
            Before1 = [A | Before],
            text_entry(Screen, Prompt, History, Before1, After1, Return, !IO)
        ;
            After = [],
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        )
    ;
        Char = '\x01\' % ^A
    ->
        After1 = list.reverse(Before) ++ After,
        text_entry(Screen, Prompt, History, [], After1, Return, !IO)
    ;
        Char = '\x05\' % ^E
    ->
        Before1 = list.reverse(After) ++ Before,
        text_entry(Screen, Prompt, History, Before1, [], Return, !IO)
    ;
        Char = '\x15\' % ^U
    ->
        text_entry(Screen, Prompt, History, [], [], Return, !IO)
    ;
        Char = '\x10\' % ^P
    ->
        History = Pre - Post,
        (
            Pre = [],
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        ;
            Pre = [Edit | Pre1],
            String = char_lists_to_string(Before, After),
            Post1 = [String | Post],
            History1 = Pre1 - Post1,
            Before1 = list.reverse(string.to_char_list(Edit)),
            text_entry(Screen, Prompt, History1, Before1, [], Return, !IO)
        )
    ;
        Char = '\xe\' % ^N
    ->
        History = Pre - Post,
        (
            Post = [],
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        ;
            Post = [Edit | Post1],
            String = char_lists_to_string(Before, After),
            Pre1 = [String | Pre],
            History1 = Pre1 - Post1,
            Before1 = list.reverse(string.to_char_list(Edit)),
            text_entry(Screen, Prompt, History1, Before1, [], Return, !IO)
        )
    ;
        ( isprint(Char) ->
            (
                FirstTime = yes,
                not char.is_whitespace(Char)
            ->
                History = Pre - Post,
                String = char_lists_to_string(Before, After),
                History1 = [String | Pre] - Post,
                Before1 = [Char]
            ;
                Before1 = [Char | Before],
                History1 = History
            ),
            text_entry(Screen, Prompt, History1, Before1, After, Return, !IO)
        ;
            text_entry(Screen, Prompt, History, Before, After, Return, !IO)
        )
    ).

:- pred text_entry(screen::in, string::in, pair(history, history)::in,
    list(char)::in, list(char)::in, maybe(string)::out, io::di, io::uo) is det.

:- pragma inline(text_entry/8).

text_entry(Screen, Prompt, History, Before, After, Return, !IO) :-
    text_entry(Screen, Prompt, History, Before, After, no, Return, !IO).

:- func char_lists_to_string(list(char), list(char)) = string.

char_lists_to_string(Before, After) = BeforeString ++ AfterString :-
    string.from_rev_char_list(Before, BeforeString),
    string.from_char_list(After, AfterString).

:- pred draw_text_entry(screen::in, string::in, list(char)::in, list(char)::in,
    io::di, io::uo) is det.

draw_text_entry(Screen, Prompt, Before, After, !IO) :-
    Panel = Screen ^ msgentry_panel,
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, Prompt, !IO),
    my_addstr(Panel, string.from_rev_char_list(Before), !IO),
    panel.getyx(Panel, Y, X, !IO),
    my_addstr(Panel, string.from_char_list(After), !IO),
    panel.move(Panel, Y, X, !IO),
    panel.update_panels(!IO).

:- pred isprint(char::in) is semidet.

:- pragma foreign_proc("C",
    isprint(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = isprint(Char) || Char >= 0x80;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
