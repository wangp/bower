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
    get_keycode(Key, !IO),
    (
        ( Key = char('\x07\') % BEL (^G)
        ; Key = char('\x1b\') % ESC
        )
    ->
        Return = no
    ;
        Key = char('\r') % CR
    ->
        String = char_lists_to_string(Before, After),
        Return = yes(String)
    ;
        ( Key = char('\x7f\') % DEL
        ; Key = char('\x08\') % BS
        ; Key = code(key_backspace)
        )
    ->
        delete_char(Before, Before1),
        text_entry(Screen, Prompt, History, Before1, After, Return, !IO)
    ;
        ( Key = meta('\x7f\') % DEL
        ; Key = meta('\x08\') % BS
        )
    ->
        delete_word(Before, Before1),
        text_entry(Screen, Prompt, History, Before1, After, Return, !IO)
    ;
        ( Key = char('\x04\') % ^D
        ; Key = code(key_del)
        )
    ->
        delete_char(After, After1),
        text_entry(Screen, Prompt, History, Before, After1, Return, !IO)
    ;
        Key = meta('d')
    ->
        delete_word(After, After1),
        text_entry(Screen, Prompt, History, Before, After1, Return, !IO)
    ;
        ( Key = char('\x02\') % ^B
        ; Key = code(key_left)
        )
    ->
        move_char(Before, Before1, After, After1),
        text_entry(Screen, Prompt, History, Before1, After1, Return, !IO)
    ;
        ( Key = char('\x06\') % ^F
        ; Key = code(key_right)
        )
    ->
        move_char(After, After1, Before, Before1),
        text_entry(Screen, Prompt, History, Before1, After1, Return, !IO)
    ;
        Key = meta('b')
    ->
        back_word(Before, Before1, After, After1),
        text_entry(Screen, Prompt, History, Before1, After1, Return, !IO)
    ;
        Key = meta('f')
    ->
        forward_word(Before, Before1, After, After1),
        text_entry(Screen, Prompt, History, Before1, After1, Return, !IO)
    ;
        ( Key = char('\x01\') % ^A
        ; Key = code(key_home)
        )
    ->
        bol_eol(Before, After, After1),
        text_entry(Screen, Prompt, History, [], After1, Return, !IO)
    ;
        ( Key = char('\x05\') % ^E
        ; Key = code(key_end)
        )
    ->
        bol_eol(After, Before, Before1),
        text_entry(Screen, Prompt, History, Before1, [], Return, !IO)
    ;
        Key = char('\x15\') % ^U
    ->
        text_entry(Screen, Prompt, History, [], [], Return, !IO)
    ;
        Key = char('\x0b\') % ^K
    ->
        text_entry(Screen, Prompt, History, Before, [], Return, !IO)
    ;
        ( Key = char('\x10\') % ^P
        ; Key = code(key_up)
        )
    ->
        History = Pre - Post,
        move_history(Before, Before1, After, After1, Pre, Pre1, Post, Post1),
        History1 = Pre1 - Post1,
        text_entry(Screen, Prompt, History1, Before1, After1, Return, !IO)
    ;
        ( Key = char('\xe\') % ^N
        ; Key = code(key_down)
        )
    ->
        History = Pre - Post,
        move_history(Before, Before1, After, After1, Post, Post1, Pre, Pre1),
        History1 = Pre1 - Post1,
        text_entry(Screen, Prompt, History1, Before1, After1, Return, !IO)
    ;
        (
            Key = char(Char),
            isprint(Char)
        ->
            (
                FirstTime = yes,
                not_whitespace(Char)
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

:- pred delete_char(list(char)::in, list(char)::out) is det.

delete_char([], []).
delete_char([_ | Cs], Cs).

:- pred delete_word(list(char)::in, list(char)::out) is det.

delete_word(Cs0, Cs) :-
    list.takewhile(is_whitespace, Cs0, _, Cs1),
    list.takewhile(not_whitespace, Cs1, _, Cs).

:- pred move_char(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

move_char([], [], Ys, Ys).
move_char([C | Xs], Xs, Ys, [C | Ys]).

:- pred back_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

back_word(Before0, Before, After0, After) :-
    list.takewhile(is_whitespace, Before0, Take0, Before1),
    list.takewhile(not_whitespace, Before1, Take1, Before),
    After = list.reverse(Take0 ++ Take1) ++ After0.

:- pred forward_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

forward_word(Before0, Before, After0, After) :-
    list.takewhile(not_whitespace, After0, Take0, After1),
    list.takewhile(is_whitespace, After1, Take1, After),
    Before = list.reverse(Take0 ++ Take1) ++ Before0.

:- pred bol_eol(list(char)::in, list(char)::in, list(char)::out) is det.

bol_eol(Xs, Ys, reverse(Xs) ++ Ys).

:- pred move_history(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out,
    history::in, history::out, history::in, history::out) is det.

move_history(Before0, Before, After0, After, Hist0, Hist, HistOpp0, HistOpp) :-
    (
        Hist0 = [],
        Before = Before0,
        After = After0,
        Hist = Hist0,
        HistOpp = HistOpp0
    ;
        Hist0 = [Edit | Hist],
        Before = list.reverse(string.to_char_list(Edit)),
        After = [],
        SaveString = char_lists_to_string(Before0, After0),
        HistOpp = [SaveString | HistOpp0]
    ).

%-----------------------------------------------------------------------------%

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

:- pred not_whitespace(char::in) is semidet.

not_whitespace(C) :-
    not char.is_whitespace(C).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
