%-----------------------------------------------------------------------------%

:- module text_entry.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module screen.

:- pred text_entry(screen::in, string::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred text_entry_initial(screen::in, string::in, string::in,
    maybe(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.

%-----------------------------------------------------------------------------%

text_entry(Screen, Prompt, Return, !IO) :-
    text_entry_initial(Screen, Prompt, "", Return, !IO).

text_entry_initial(Screen, Prompt, Initial, Return, !IO) :-
    string.to_char_list(Initial, Before0),
    list.reverse(Before0, Before),
    text_entry(Screen, Prompt, Before, [], Return, !IO),
    update_message(Screen, clear_message, !IO).

:- pred text_entry(screen::in, string::in, list(char)::in, list(char)::in,
    maybe(string)::out, io::di, io::uo) is det.

text_entry(Screen, Prompt, Before, After, Return, !IO) :-
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
        string.from_rev_char_list(Before, BeforeString),
        string.from_char_list(After, AfterString),
        String = BeforeString ++ AfterString,
        Return = yes(String)
    ;
        ( Char = '\x7f\' % DEL
        ; Char = '\x08\' % BS
        )
    ->
        (
            Before = [],
            text_entry(Screen, Prompt, Before, After, Return, !IO)
        ;
            Before = [_ | Before1],
            text_entry(Screen, Prompt, Before1, After, Return, !IO)
        )
    ;
        Char = '\x02\' % ^B
    ->
        (
            Before = [B | Before1],
            text_entry(Screen, Prompt, Before1, [B | After], Return, !IO)
        ;
            Before = [],
            text_entry(Screen, Prompt, Before, After, Return, !IO)
        )
    ;
        Char = '\x06\' % ^F
    ->
        (
            After = [A | After1],
            text_entry(Screen, Prompt, [A | Before], After1, Return, !IO)
        ;
            After = [],
            text_entry(Screen, Prompt, Before, After, Return, !IO)
        )
    ;
        Char = '\x01\' % ^A
    ->
        After1 = list.reverse(Before) ++ After,
        text_entry(Screen, Prompt, [], After1, Return, !IO)
    ;
        Char = '\x05\' % ^E
    ->
        Before1 = list.reverse(After) ++ Before,
        text_entry(Screen, Prompt, Before1, [], Return, !IO)
    ;
        Char = '\x15\' % ^U
    ->
        text_entry(Screen, Prompt, [], [], Return, !IO)
    ;
        ( isprint(Char) ->
            text_entry(Screen, Prompt, [Char | Before], After, Return, !IO)
        ;
            text_entry(Screen, Prompt, Before, After, Return, !IO)
        )
    ).

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
