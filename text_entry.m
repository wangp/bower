% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module text_entry.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- import_module screen.

:- type history.

:- type completion_type
    --->    complete_none
    ;       complete_path.

:- func init_history = history.

:- pred add_history_nodup(string::in, history::in, history::out) is det.

:- pred choose_text_initial(history::in, string::in, string::out) is det.

:- pred text_entry(screen::in, string::in, history::in, completion_type::in,
    maybe(string)::out, io::di, io::uo) is det.

:- pred text_entry_initial(screen::in, string::in, history::in, string::in,
    completion_type::in, maybe(string)::out, io::di, io::uo) is det.

:- pred text_entry_full(screen::in, string::in, history::in, string::in,
    completion_type::in, bool::in, maybe(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.

:- type history == list(string). % reverse

:- type sub_info
    --->    sub_info(
                first_time      :: bool,
                pre_history     :: history,
                post_history    :: history,
                compl_type      :: completion_type,
                compl_choices   :: list(string)
            ).

%-----------------------------------------------------------------------------%

init_history = [].

add_history_nodup(Candidate, Hist0, Hist) :-
    ( Hist0 = [Candidate | _] ->
        Hist = Hist0
    ;
        Hist = [Candidate | Hist0]
    ).

choose_text_initial([], Default, Default).
choose_text_initial([Last | _], _, Last).

%-----------------------------------------------------------------------------%

text_entry(Screen, Prompt, History0, CompleteType, Return, !IO) :-
    (
        History0 = [],
        Initial = "",
        History = []
    ;
        History0 = [Initial | History]
    ),
    FirstTime = yes,
    text_entry_full(Screen, Prompt, History, Initial, CompleteType, FirstTime,
        Return, !IO).

text_entry_initial(Screen, Prompt, History, Initial, CompleteType, Return,
        !IO) :-
    FirstTime = yes,
    text_entry_full(Screen, Prompt, History, Initial, CompleteType, FirstTime,
        Return, !IO).

text_entry_full(Screen, Prompt, History, Initial, CompleteType, FirstTime,
        Return, !IO) :-
    string.to_char_list(Initial, Before0),
    list.reverse(Before0, Before),
    After = [],
    SubInfo = sub_info(FirstTime, History, [], CompleteType, []),
    text_entry_real(Screen, Prompt, Before, After, SubInfo, Return, !IO),
    update_message(Screen, clear_message, !IO).

:- pred text_entry_real(screen::in, string::in, list(char)::in, list(char)::in,
    sub_info::in, maybe(string)::out, io::di, io::uo) is det.

text_entry_real(Screen, Prompt, Before, After, SubInfo, Return, !IO) :-
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
        text_entry(Screen, Prompt, Before1, After, SubInfo, Return, !IO)
    ;
        ( Key = meta('\x7f\') % DEL
        ; Key = meta('\x08\') % BS
        )
    ->
        delete_word(Before, Before1),
        text_entry(Screen, Prompt, Before1, After, SubInfo, Return, !IO)
    ;
        ( Key = char('\x04\') % ^D
        ; Key = code(key_del)
        )
    ->
        delete_char(After, After1),
        text_entry(Screen, Prompt, Before, After1, SubInfo, Return, !IO)
    ;
        Key = meta('d')
    ->
        delete_word(After, After1),
        text_entry(Screen, Prompt, Before, After1, SubInfo, Return, !IO)
    ;
        ( Key = char('\x02\') % ^B
        ; Key = code(key_left)
        )
    ->
        move_char(Before, Before1, After, After1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo, Return, !IO)
    ;
        ( Key = char('\x06\') % ^F
        ; Key = code(key_right)
        )
    ->
        move_char(After, After1, Before, Before1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo, Return, !IO)
    ;
        Key = meta('b')
    ->
        back_word(Before, Before1, After, After1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo, Return, !IO)
    ;
        Key = meta('f')
    ->
        forward_word(Before, Before1, After, After1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo, Return, !IO)
    ;
        ( Key = char('\x01\') % ^A
        ; Key = code(key_home)
        )
    ->
        bol_eol(Before, After, After1),
        text_entry(Screen, Prompt, [], After1, SubInfo, Return, !IO)
    ;
        ( Key = char('\x05\') % ^E
        ; Key = code(key_end)
        )
    ->
        bol_eol(After, Before, Before1),
        text_entry(Screen, Prompt, Before1, [], SubInfo, Return, !IO)
    ;
        Key = char('\x15\') % ^U
    ->
        text_entry(Screen, Prompt, [], [], SubInfo, Return, !IO)
    ;
        Key = char('\x0b\') % ^K
    ->
        text_entry(Screen, Prompt, Before, [], SubInfo, Return, !IO)
    ;
        ( Key = char('\x10\') % ^P
        ; Key = code(key_up)
        )
    ->
        get_history(SubInfo, Pre, Post),
        move_history(Before, Before1, After, After1, Pre, Pre1, Post, Post1),
        set_history(Pre1, Post1, SubInfo, SubInfo1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo1, Return, !IO)
    ;
        ( Key = char('\xe\') % ^N
        ; Key = code(key_down)
        )
    ->
        get_history(SubInfo, Pre, Post),
        move_history(Before, Before1, After, After1, Post, Post1, Pre, Pre1),
        set_history(Pre1, Post1, SubInfo, SubInfo1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo1, Return, !IO)
    ;
        Key = char('\x09\') % Tab
    ->
        bol_eol(After, Before, Before1),
        do_completion(Before1, Before2, SubInfo, MaybeSubInfo1, !IO),
        (
            MaybeSubInfo1 = yes(SubInfo1),
            clear_first_time_flag(SubInfo1, SubInfo2),
            text_entry_real(Screen, Prompt, Before2, [], SubInfo2, Return, !IO)
        ;
            MaybeSubInfo1 = no,
            % XXX type tab?
            text_entry(Screen, Prompt, Before, After, SubInfo, Return, !IO)
        )
    ;
        (
            Key = char(Char),
            isprint(Char)
        ->
            (
                SubInfo ^ first_time = yes,
                not char.is_whitespace(Char)
            ->
                Before1 = [Char],
                get_history(SubInfo, Pre, Post),
                String = char_lists_to_string(Before, After),
                Pre1 = [String | Pre],
                set_history(Pre1, Post, SubInfo, SubInfo1)
            ;
                Before1 = [Char | Before],
                SubInfo1 = SubInfo
            ),
            text_entry(Screen, Prompt, Before1, After, SubInfo1, Return, !IO)
        ;
            text_entry(Screen, Prompt, Before, After, SubInfo, Return, !IO)
        )
    ).

:- pred text_entry(screen::in, string::in, list(char)::in, list(char)::in,
    sub_info::in, maybe(string)::out, io::di, io::uo) is det.

:- pragma inline(text_entry/8).

text_entry(Screen, Prompt, Before, After, !.SubInfo, Return, !IO) :-
    clear_first_time_flag(!SubInfo),
    clear_completion_choices(!SubInfo),
    text_entry_real(Screen, Prompt, Before, After, !.SubInfo, Return, !IO).

:- pred clear_first_time_flag(sub_info::in, sub_info::out) is det.

clear_first_time_flag(!SubInfo) :-
    ( !.SubInfo ^ first_time = yes ->
        !SubInfo ^ first_time := no
    ;
        true
    ).

:- pred clear_completion_choices(sub_info::in, sub_info::out) is det.

clear_completion_choices(!SubInfo) :-
    ( !.SubInfo ^ compl_choices = [_ | _] ->
        !SubInfo ^ compl_choices := []
    ;
        true
    ).

:- func char_lists_to_string(list(char), list(char)) = string.

char_lists_to_string(Before, After) = BeforeString ++ AfterString :-
    string.from_rev_char_list(Before, BeforeString),
    string.from_char_list(After, AfterString).

:- pred delete_char(list(char)::in, list(char)::out) is det.

delete_char([], []).
delete_char([_ | Cs], Cs).

:- pred delete_word(list(char)::in, list(char)::out) is det.

delete_word(Cs0, Cs) :-
    list.takewhile(non_word_char, Cs0, _, Cs1),
    list.takewhile(is_word_char, Cs1, _, Cs).

:- pred move_char(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

move_char([], [], Ys, Ys).
move_char([C | Xs], Xs, Ys, [C | Ys]).

:- pred back_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

back_word(Before0, Before, After0, After) :-
    list.takewhile(non_word_char, Before0, Take0, Before1),
    list.takewhile(is_word_char, Before1, Take1, Before),
    After = list.reverse(Take0 ++ Take1) ++ After0.

:- pred forward_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

forward_word(Before0, Before, After0, After) :-
    list.takewhile(is_word_char, After0, Take0, After1),
    list.takewhile(non_word_char, After1, Take1, After),
    Before = list.reverse(Take0 ++ Take1) ++ Before0.

:- pred bol_eol(list(char)::in, list(char)::in, list(char)::out) is det.

bol_eol(Xs, Ys, reverse(Xs) ++ Ys).

:- pred get_history(sub_info::in, history::out, history::out) is det.

get_history(SubInfo, Pre, Post) :-
    Pre = SubInfo ^ pre_history,
    Post = SubInfo ^ post_history.

:- pred set_history(history::in, history::in, sub_info::in, sub_info::out)
    is det.

set_history(Pre, Post, !SubInfo) :-
    !SubInfo ^ pre_history := Pre,
    !SubInfo ^ post_history := Post.

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

:- pred do_completion(list(char)::in, list(char)::out, sub_info::in,
    maybe(sub_info)::out, io::di, io::uo) is det.

do_completion(Orig, Replacement, SubInfo0, MaybeSubInfo, !IO) :-
    Type = SubInfo0 ^ compl_type,
    Choices0 = SubInfo0 ^ compl_choices,
    (
        Choices0 = [],
        Type = complete_none,
        Choices = []
    ;
        Choices0 = [],
        Type = complete_path,
        string.from_rev_char_list(Orig, OrigString),
        generate_path_choices(OrigString, Choices, !IO)
    ;
        Choices0 = [_ | _],
        Choices = Choices0
    ),
    (
        Choices = [],
        Replacement = Orig,
        MaybeSubInfo = no
    ;
        Choices = [FirstChoice | MoreChoices],
        Replacement = list.reverse(string.to_char_list(FirstChoice)),
        (
            MoreChoices = [],
            SubInfo = SubInfo0 ^ compl_choices := []
        ;
            MoreChoices = [_ | _],
            SubInfo = SubInfo0 ^ compl_choices := MoreChoices ++ [FirstChoice]
        ),
        MaybeSubInfo = yes(SubInfo)
    ).

:- pred generate_path_choices(string::in, list(string)::out,
    io::di, io::uo) is det.

generate_path_choices(OrigString, Choices, !IO) :-
    ( string.suffix(OrigString, "/") ->
        DirName = OrigString,
        Filter = filter_path_nonhidden(DirName)
    ; dir.split_name(OrigString, DirName0, BaseNamePrefix) ->
        DirName = DirName0,
        Filter = filter_path_prefix(DirName / "", BaseNamePrefix)
    ;
        DirName = dir.this_directory,
        Filter = filter_path_prefix("", OrigString)
    ),
    dir.foldl2(Filter, DirName, [], Result, !IO),
    (
        Result = ok(Choices0),
        list.sort(Choices0, Choices)
    ;
        Result = error(_, _),
        Choices = []
    ).

:- pred filter_path_nonhidden(string::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

filter_path_nonhidden(AddPrefix, _DirName, BaseName, FileType, yes,
        !Matches, !IO) :-
    ( string.prefix(BaseName, ".") ->
        true
    ;
        ( FileType = directory ->
            Match = AddPrefix ++ BaseName ++ "/"
        ;
            Match = AddPrefix ++ BaseName
        ),
        list.cons(Match, !Matches)
    ).

:- pred filter_path_prefix(string::in, string::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

filter_path_prefix(AddPrefix, MatchBaseNamePrefix, _DirName, BaseName,
        FileType, yes, !Matches, !IO) :-
    ( string.prefix(BaseName, MatchBaseNamePrefix) ->
        ( FileType = directory ->
            Match = AddPrefix ++ BaseName ++ "/"
        ;
            Match = AddPrefix ++ BaseName
        ),
        list.cons(Match, !Matches)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred draw_text_entry(screen::in, string::in, list(char)::in, list(char)::in,
    io::di, io::uo) is det.

draw_text_entry(Screen, Prompt, Before, After, !IO) :-
    get_msgentry_panel(Screen, Panel),
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

:- pred is_word_char(char::in) is semidet.

is_word_char(C) :-
    not non_word_char(C).

:- pred non_word_char(char::in) is semidet.

non_word_char(C) :-
    char.to_int(C) =< 0x7f,
    not (
        char.is_alnum_or_underscore(C)
    ;
        C = ('-')
    ;
        C = ('.')
    ;
        C = ('~')
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
