% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module text_entry.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module path_expand.
:- import_module prog_config.
:- import_module screen.

:- type history.

:- type completion_type
    --->    complete_none
    ;       complete_path(home)
    ;       complete_limit(
                prog_config,
                % Name of search alias section.
                string,
                % Prefixes to trigger tag completion.
                list(string)
            )
    ;       complete_tags_smart(
                prog_config,
                % All selected messages have these tags.
                and_tags    :: set(string),
                % At least one selected message has these tags.
                or_tags     :: set(string)
            )
    ;       complete_config_key(
                prog_config,
                % Name of config section.
                string
            )
    ;       complete_address(prog_config).

:- func init_history = history.

:- func init_history(string) = history.

:- func init_history_list(list(string)) = history.

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
:- import_module require.
:- import_module string.

:- import_module addressbook.
:- import_module call_system.
:- import_module curs.
:- import_module curs.panel.
:- import_module list_util.
:- import_module quote_arg.
:- import_module string_util.

:- type info
    --->    info(
                prompt          :: string,          % static
                state           :: te_state,
                states          :: te_states,
                first_time      :: bool,
                left_offset     :: int,
                compl_type      :: completion_type, % static
                compl_choices   :: list(string),
                compl_point     :: int
            ).

    % This is a stack implemented as a reverse list.  The stack module
    % predicates changed argument order between Mercury 11.07 and 13.05.
    %
:- type te_states == list(te_state).

:- type te_state
    --->    te_state(
                before          :: list(char),
                after           :: list(char),
                pre_history     :: history,
                post_history    :: history
            ).

:- type history == list(string). % reverse

:- type move_history_dir
    --->    pre
    ;       post.

%-----------------------------------------------------------------------------%

init_history = [].

init_history(X) = [X].

init_history_list(Xs) = Xs.

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

text_entry_initial(Screen, Prompt, History, Initial, CompleteType, Return, !IO)
        :-
    FirstTime = yes,
    text_entry_full(Screen, Prompt, History, Initial, CompleteType, FirstTime,
        Return, !IO).

text_entry_full(Screen, Prompt, History, Initial, CompleteType, FirstTime,
        Return, !IO) :-
    Before = list.reverse(string.to_char_list(Initial)),
    State = te_state(Before, [], History, []),
    States = [],
    LeftOffset = 0,
    Info0 = info(Prompt, State, States, FirstTime, LeftOffset, CompleteType,
        [], 0),
    text_entry_loop(Screen, Return, Info0, _Info, !IO),
    update_message(Screen, clear_message, !IO).

:- pred text_entry_loop(screen::in, maybe(string)::out, info::in, info::out,
    io::di, io::uo) is det.

text_entry_loop(Screen, Return, !Info, !IO) :-
    draw_text_entry(Screen, !Info, !IO),
    get_keycode_blocking(Key, !IO),
    (
        ( Key = char('\x07\') % BEL (^G)
        ; Key = char('\x1b\') % ESC
        )
    ->
        Return = no
    ;
        Key = char('\r') % CR
    ->
        State = !.Info ^ state,
        Before = State ^ before,
        After = State ^ after,
        String = char_lists_to_string(Before, After),
        Return = yes(String)
    ;
        ( Key = char('\x7f\') % DEL
        ; Key = char('\x08\') % BS
        ; Key = code(key_backspace)
        )
    ->
        modify_before(delete_char, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x17\') % ^W
        ; Key = meta('\x7f\') % DEL
        ; Key = meta('\x08\') % BS
        ; Key = metacode(key_backspace)
        )
    ->
        modify_before(delete_word, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x04\') % ^D
        ; Key = code(key_del)
        )
    ->
        modify_after(delete_char, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = meta('d')
    ->
        modify_after(delete_word, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x02\') % ^B
        ; Key = code(key_left)
        )
    ->
        modify_before_after(left_char, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x06\') % ^F
        ; Key = code(key_right)
        )
    ->
        modify_before_after(right_char, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = meta('b')
    ->
        modify_before_after(back_word, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = meta('f')
    ->
        modify_before_after(forward_word, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x01\') % ^A
        ; Key = code(key_home)
        )
    ->
        modify_before_after(bol, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x05\') % ^E
        ; Key = code(key_end)
        )
    ->
        modify_before_after(eol, no, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = char('\x15\') % ^U
    ->
        modify_before_after(kill_all, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = char('\x0b\') % ^K
    ->
        modify_after(kill_eol, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = char('\x14\') % ^T
    ->
        modify_before_after(transpose, yes, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\x10\') % ^P
        ; Key = code(key_up)
        )
    ->
        move_history(pre, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        ( Key = char('\xe\') % ^N
        ; Key = code(key_down)
        )
    ->
        move_history(post, !Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        Key = char('\x09\') % Tab
    ->
        !.Info ^ compl_type = Type,
        !.Info ^ state = State0,
        State0 = te_state(Before0, After0, Pre, Post),
        forward_for_completion(Type, Before0, Before1, After0, After),
        do_completion(Before1, Before, After, !.Info, MaybeInfo, !IO),
        (
            MaybeInfo = yes(!:Info),
            State = te_state(Before, After, Pre, Post),
            enter_state(State, yes, !Info),
            clear_first_time_flag(!Info),
            text_entry_loop(Screen, Return, !Info, !IO)
        ;
            MaybeInfo = no,
            % XXX type tab?
            continue_text_entry(Screen, Return, !Info, !IO)
        )
    ;
        Key = char('\x1f\') % ^_
    ->
        undo(!Info),
        continue_text_entry(Screen, Return, !Info, !IO)
    ;
        (
            Key = char(Char),
            isprint(Char)
        ->
            insert(Char, !Info),
            continue_text_entry(Screen, Return, !Info, !IO)
        ;
            Key = timeout_or_error
        ->
            % Don't clear first time flag on a timeout.
            text_entry_loop(Screen, Return, !Info, !IO)
        ;
            continue_text_entry(Screen, Return, !Info, !IO)
        )
    ).

:- pred continue_text_entry(screen::in, maybe(string)::out,
    info::in, info::out, io::di, io::uo) is det.

:- pragma inline(continue_text_entry/6).

continue_text_entry(Screen, Return, !Info, !IO) :-
    clear_first_time_flag(!Info),
    clear_completion_choices(!Info),
    text_entry_loop(Screen, Return, !Info, !IO).

:- pred clear_first_time_flag(info::in, info::out) is det.

clear_first_time_flag(!Info) :-
    ( !.Info ^ first_time = yes ->
        !Info ^ first_time := no
    ;
        true
    ).

:- pred clear_completion_choices(info::in, info::out) is det.

clear_completion_choices(!Info) :-
    ( !.Info ^ compl_choices = [_ | _] ->
        !Info ^ compl_choices := [],
        !Info ^ compl_point := 0
    ;
        true
    ).

:- func char_lists_to_string(list(char), list(char)) = string.

char_lists_to_string(Before, After) = BeforeString ++ AfterString :-
    string.from_rev_char_list(Before, BeforeString),
    string.from_char_list(After, AfterString).

:- pred enter_state(te_state::in, bool::in, info::in, info::out) is det.

enter_state(State, Push, !Info) :-
    (
        Push = yes,
        OldState = !.Info ^ state,
        States0 = !.Info ^ states,
        States = [OldState | States0],
        !Info ^ states := States
    ;
        Push = no
    ),
    !Info ^ state := State.

:- pred modify_before(pred(list(char), list(char)), bool, info, info).
:- mode modify_before(in(pred(in, out) is semidet), in, in, out) is det.

modify_before(P, Push, !Info) :-
    State0 = !.Info ^ state,
    Before0 = State0 ^ before,
    ( P(Before0, Before) ->
        State = State0 ^ before := Before,
        enter_state(State, Push, !Info)
    ;
        true
    ).

:- pred modify_after(pred(list(char), list(char)), bool, info, info).
:- mode modify_after(in(pred(in, out) is semidet), in, in, out) is det.

modify_after(P, Push, !Info) :-
    State0 = !.Info ^ state,
    After0 = State0 ^ after,
    ( P(After0, After) ->
        State = State0 ^ after := After,
        enter_state(State, Push, !Info)
    ;
        true
    ).

:- pred modify_before_after(pred(list(char), list(char), list(char),
    list(char)), bool, info, info).
:- mode modify_before_after(in(pred(in, out, in, out) is det), in, in, out)
    is det.
:- mode modify_before_after(in(pred(in, out, in, out) is semidet), in, in, out)
    is det.

modify_before_after(P, Push, !Info) :-
    !.Info ^ state = te_state(Before0, After0, Pre, Post),
    ( P(Before0, Before, After0, After) ->
        State = te_state(Before, After, Pre, Post),
        enter_state(State, Push, !Info)
    ;
        true
    ).

:- pred delete_char(list(char)::in, list(char)::out) is semidet.

delete_char([], []) :- fail.
delete_char([_ | Cs], Cs).

:- pred delete_word(list(char)::in, list(char)::out) is semidet.

delete_word(Cs0, Cs) :-
    list_util.take_while(non_word_char, Cs0, _, Cs1),
    list_util.take_while(is_word_char, Cs1, _, Cs),
    Cs \= Cs0.

:- pred left_char(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is semidet.

left_char([], [], Ys, Ys) :- fail.
left_char([C | Xs], Xs, Ys, [C | Ys]).

:- pred right_char(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is semidet.

right_char(!Xs, !Ys) :-
    left_char(!Ys, !Xs).

:- pred back_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

back_word(Before0, Before, After0, After) :-
    list_util.take_while(non_word_char, Before0, Take0, Before1),
    list_util.take_while(is_word_char, Before1, Take1, Before),
    After = list.reverse(Take0 ++ Take1) ++ After0.

:- pred forward_word(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

forward_word(Before0, Before, After0, After) :-
    list_util.take_while(is_word_char, After0, Take0, After1),
    list_util.take_while(non_word_char, After1, Take1, After),
    Before = list.reverse(Take0 ++ Take1) ++ Before0.

:- pred bol(list(char)::in, list(char)::out, list(char)::in, list(char)::out)
    is semidet.

bol(Before, [], After, reverse(Before) ++ After) :-
    Before = [_ | _].

:- pred eol(list(char)::in, list(char)::out, list(char)::in, list(char)::out)
    is semidet.

eol(!Before, !After) :-
    bol(!After, !Before).

:- pred kill_all(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is semidet.

kill_all(Before, [], After, []) :-
    ( Before = [_ | _]
    ; After = [_ | _]
    ).

:- pred kill_eol(list(char)::in, list(char)::out) is semidet.

kill_eol([_ | _], []).

:- pred transpose(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

transpose(Before0, Before, After0, After) :-
    (
        After0 = [],
        Before0 = [B1, B2 | Bs]
    ->
        After = [],
        Before = [B2, B1 | Bs]
    ;
        After0 = [A | As],
        Before0 = [B | Bs]
    ->
        After = As,
        Before = [B, A | Bs]
    ;
        Before = Before0,
        After = After0
    ).

:- pred insert(char::in, info::in, info::out) is det.

insert(Char, !Info) :-
    State0 = !.Info ^ state,
    State0 = te_state(Before0, After0, Pre0, Post),
    (
        !.Info ^ first_time = yes,
        not char.is_whitespace(Char)
    ->
        Before = [Char],
        String = char_lists_to_string(Before0, After0),
        Pre = [String | Pre0]
    ;
        Before = [Char | Before0],
        Pre = Pre0
    ),
    State = te_state(Before, After0, Pre, Post),
    enter_state(State, yes, !Info).

:- pred undo(info::in, info::out) is det.

undo(!Info) :-
    !.Info ^ states = States0,
    (
        States0 = []
    ;
        States0 = [State | States],
        !Info ^ state := State,
        !Info ^ states := States
    ).

:- pred move_history(move_history_dir::in, info::in, info::out) is det.

move_history(Dir, !Info) :-
    !.Info ^ state = State0,
    ( move_history_2(Dir, State0, State) ->
        enter_state(State, yes, !Info)
    ;
        true
    ).

:- pred move_history_2(move_history_dir::in, te_state::in, te_state::out)
    is semidet.

move_history_2(Dir, State0, State) :-
    State0 = te_state(Before0, After0, Pre0, Post0),
    SaveString = char_lists_to_string(Before0, After0),
    (
        Dir = pre,
        Pre0 = [Edit | Pre],
        Post = [SaveString | Post0]
    ;
        Dir = post,
        Post0 = [Edit | Post],
        Pre = [SaveString | Pre0]
    ),
    Before = list.reverse(string.to_char_list(Edit)),
    State = te_state(Before, [], Pre, Post).

%-----------------------------------------------------------------------------%

:- pred forward_for_completion(completion_type::in,
    list(char)::in, list(char)::out, list(char)::in, list(char)::out) is det.

forward_for_completion(Type, Before0, Before, After0, After) :-
    (
        Type = complete_none,
        Before = Before0,
        After = After0
    ;
        Type = complete_path(_),
        Before = reverse(After0) ++ Before0,
        After = []
    ;
        ( Type = complete_limit(_, _, _)
        ; Type = complete_tags_smart(_, _, _)
        ; Type = complete_config_key(_, _)
        ),
        list_util.take_while(non_whitespace, After0, Take, After),
        Before = list.reverse(Take) ++ Before0
    ;
        Type = complete_address(_),
        list_util.take_while(not_comma, After0, Take, After),
        Before = list.reverse(Take) ++ Before0
    ).

:- pred do_completion(list(char)::in, list(char)::out, list(char)::in,
    info::in, maybe(info)::out, io::di, io::uo) is det.

do_completion(Orig, Replacement, After, Info0, MaybeInfo, !IO) :-
    Choices0 = Info0 ^ compl_choices,
    (
        Choices0 = [],
        Type = Info0 ^ compl_type,
        generate_choices(Type, Orig, After, Choices, CompletionPoint,
            !IO),
        IsNew = yes
    ;
        Choices0 = [_ | _],
        Choices = Choices0,
        CompletionPoint = Info0 ^ compl_point,
        IsNew = no
    ),
    (
        Choices = [],
        Replacement = Orig,
        MaybeInfo = no
    ;
        Choices = [_ | _],
        choose_expansion(IsNew, Choices, Expansion, RotateChoices),
        det_take_tail(CompletionPoint, Orig, OrigKeep),
        reverse_onto(Expansion, OrigKeep, Replacement),
        Info1 = Info0 ^ compl_choices := RotateChoices,
        Info = Info1 ^ compl_point := CompletionPoint,
        MaybeInfo = yes(Info)
    ).

:- pred generate_choices(completion_type::in, list(char)::in, list(char)::in,
    list(string)::out, int::out, io::di, io::uo) is det.

generate_choices(Type, Orig, After, Choices, CompletionPoint, !IO) :-
    (
        Type = complete_none,
        Choices = [],
        CompletionPoint = 0
    ;
        Type = complete_path(Home),
        string.from_rev_char_list(Orig, OrigString),
        generate_path_choices(Home, OrigString, Choices, !IO),
        CompletionPoint = 0
    ;
        Type = complete_limit(Config, SearchAliasSection,
            TagCompletionTriggers),
        list_util.take_while(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        string.from_rev_char_list(Word, WordString),
        generate_limit_choices(Config, SearchAliasSection,
            TagCompletionTriggers, WordString, Choices, !IO)
    ;
        Type = complete_tags_smart(Config, AndTags, OrTags),
        list_util.take_while(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        get_entered_tags(Untouched, After, EnteredTags),
        string.from_rev_char_list(Word, WordString),
        generate_smart_tag_choices(Config, AndTags, OrTags, EnteredTags,
            WordString, Choices, !IO)
    ;
        Type = complete_config_key(Config, SectionName),
        list_util.take_while(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        string.from_rev_char_list(Word, WordString),
        generate_config_key_choices(Config, SectionName, WordString, Choices,
            !IO)
    ;
        Type = complete_address(Config),
        list_util.take_while(not_comma, Orig, RevWord0, Prefix),
        list_util.take_while(is_whitespace, reverse(RevWord0), LeadWhiteSpace, Word),
        CompletionPoint = length(Prefix) + length(LeadWhiteSpace),
        WordString = rstrip(from_char_list(Word)),
        generate_config_key_choices(Config, addressbook_section,
            WordString, AliasChoices, !IO),
        ( WordString = "" ->
            Choices = AliasChoices
        ;
            generate_address_choices(Config, WordString, AddressChoices, !IO),
            Choices = AliasChoices ++ AddressChoices
        )
    ).

:- pred choose_expansion(bool::in, list(string)::in(non_empty_list),
    list(char)::out, list(string)::out) is det.

choose_expansion(IsNew, Choices, Expansion, RotateChoices) :-
    (
        Choices = [Choice],
        Expansion = to_char_list(Choice),
        RotateChoices = []
    ;
        Choices = [FirstChoice | MoreChoices],
        MoreChoices = [_ | _],
        string.to_char_list(FirstChoice, FirstChoiceChars),
        (
            IsNew = yes,
            common_prefix_strings(MoreChoices, FirstChoiceChars, CommonPrefix),
            CommonPrefix \= FirstChoiceChars
        ->
            Expansion = CommonPrefix,
            RotateChoices = Choices
        ;
            Expansion = FirstChoiceChars,
            RotateChoices = MoreChoices ++ [FirstChoice]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred generate_path_choices(home::in, string::in, list(string)::out,
    io::di, io::uo) is det.

generate_path_choices(Home, OrigString, Choices, !IO) :-
    ( string.rstrip_pred(unify('/'), OrigString) = "~" ->
        Home = home(HomeDir),
        Choices = [HomeDir / ""]
    ;
        expand_tilde_home(Home, OrigString, ExpandedString),
        generate_path_choices_2(ExpandedString, Choices, !IO)
    ).

:- pred generate_path_choices_2(string::in, list(string)::out, io::di, io::uo)
    is det.

generate_path_choices_2(ExpandedString, Choices, !IO) :-
    ( string.suffix(ExpandedString, "/") ->
        DirName = ExpandedString,
        Filter = filter_path_nonhidden(DirName)
    ; dir.split_name(ExpandedString, DirName0, BaseNamePrefix) ->
        DirName = DirName0,
        Filter = filter_path_prefix(DirName / "", BaseNamePrefix)
    ;
        DirName = dir.this_directory,
        Filter = filter_path_prefix("", ExpandedString)
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

filter_path_nonhidden(AddPrefix, DirName, BaseName, FileType, yes,
        !Matches, !IO) :-
    ( string.prefix(BaseName, ".") ->
        true
    ;
        is_directory(DirName, BaseName, FileType, IsDir, !IO),
        (
            IsDir = yes,
            Match = AddPrefix ++ BaseName ++ "/"
        ;
            IsDir = no,
            Match = AddPrefix ++ BaseName
        ),
        list.cons(Match, !Matches)
    ).

:- pred filter_path_prefix(string::in, string::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

filter_path_prefix(AddPrefix, MatchBaseNamePrefix, DirName, BaseName,
        FileType, yes, !Matches, !IO) :-
    ( string.prefix(BaseName, MatchBaseNamePrefix) ->
        is_directory(DirName, BaseName, FileType, IsDir, !IO),
        (
            IsDir = yes,
            Match = AddPrefix ++ BaseName ++ "/"
        ;
            IsDir = no,
            Match = AddPrefix ++ BaseName
        ),
        list.cons(Match, !Matches)
    ;
        true
    ).

:- pred is_directory(string::in, string::in, io.file_type::in, bool::out,
    io::di, io::uo) is det.

is_directory(DirName, BaseName, FileType0, IsDir, !IO) :-
    ( FileType0 = directory ->
        IsDir = yes
    ; FileType0 = symbolic_link ->
        FollowSymLinks = yes,
        io.file_type(FollowSymLinks, DirName / BaseName, ResFileType, !IO),
        IsDir = ( ResFileType = ok(directory) -> yes ; no )
    ;
        IsDir = no
    ).

%-----------------------------------------------------------------------------%

:- pred generate_limit_choices(prog_config::in, string::in, list(string)::in,
    string::in, list(string)::out, io::di, io::uo) is det.

generate_limit_choices(Config, SearchAliasSection, TagCompletionTriggers,
        OrigString, Choices, !IO) :-
    ( string.remove_prefix("~", OrigString, KeyPrefix) ->
        generate_search_alias_choices(Config, SearchAliasSection, KeyPrefix,
            Choices, !IO)
    ;
        generate_tag_choices(Config, TagCompletionTriggers, OrigString,
            Choices, !IO)
    ).

:- pred generate_search_alias_choices(prog_config::in, string::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_search_alias_choices(Config, SearchAliasSection, KeyPrefix, Choices,
        !IO) :-
    generate_config_key_choices(Config, SearchAliasSection, KeyPrefix,
        Choices0, !IO),
    list.map(append("~"), Choices0) = Choices.

%-----------------------------------------------------------------------------%

:- pred generate_tag_choices(prog_config::in, list(string)::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_tag_choices(_Config, [], _OrigString, [], !IO).
generate_tag_choices(Config, CompletionTriggers, OrigString, Choices, !IO) :-
    CompletionTriggers = [Trigger | Triggers],
    ( string.remove_prefix(Trigger, OrigString, TagPrefix) ->
        get_notmuch_all_tags(Config, TagsList, !IO),
        list.filter_map(filter_tag_choice(Trigger, TagPrefix),
            TagsList, Choices)
    ;
        generate_tag_choices(Config, Triggers, OrigString, Choices, !IO)
    ).

:- pred get_entered_tags(list(char)::in, list(char)::in, set(string)::out)
    is det.

get_entered_tags(Before, After, TagSet) :-
    reverse_onto(Before, After, Chars),
    string.from_char_list(Chars, String),
    Words = string.words(String),
    list.filter_map(is_entered_tag, Words, TagList),
    TagSet = set.from_list(TagList).

:- pred is_entered_tag(string::in, string::out) is semidet.

is_entered_tag(Word, Tag) :-
    ( string.remove_prefix("-", Word, Tag0) ->
        Tag = Tag0
    ; string.remove_prefix("+", Word, Tag0) ->
        Tag = Tag0
    ;
        fail
    ).

:- pred generate_smart_tag_choices(prog_config::in,
    set(string)::in, set(string)::in, set(string)::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_smart_tag_choices(Config, AndTagSet, OrTagSet, EnteredTagSet,
        OrigString, Choices, !IO) :-
    (
        string.remove_prefix("-", OrigString, TagPrefix)
    ->
        set.difference(OrTagSet, EnteredTagSet, CandidateSet),
        set.to_sorted_list(CandidateSet, CandidateList),
        list.filter_map(filter_tag_choice("-", TagPrefix),
            CandidateList, Choices)
    ;
        string.remove_prefix("+", OrigString, TagPrefix)
    ->
        get_notmuch_all_tags(Config, AllTagsList, !IO),
        set.difference(from_list(AllTagsList), AndTagSet, CandidateSet0),
        set.difference(CandidateSet0, EnteredTagSet, CandidateSet),
        set.to_sorted_list(CandidateSet, CandidateList),
        list.filter_map(filter_tag_choice("+", TagPrefix),
            CandidateList, Choices)
    ;
        Choices = []
    ).

:- pred get_notmuch_all_tags(prog_config::in, list(string)::out,
    io::di, io::uo) is det.

get_notmuch_all_tags(Config, TagsList, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, [
        "search", "--output=tags", "--exclude=all", "--", "*"
    ], redirect_input("/dev/null"), no_redirect, Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
    (
        CallRes = ok(TagListString),
        % The empty string following the final newline is not a tag.
        TagsList = string.words_separator(unify('\n'), TagListString)
    ;
        CallRes = error(_),
        TagsList = []
    ).

:- pred filter_tag_choice(string::in, string::in, string::in, string::out)
    is semidet.

filter_tag_choice(Trigger, TagPrefix, Tag, Choice) :-
    string.prefix(Tag, TagPrefix),
    Choice = Trigger ++ Tag.

%-----------------------------------------------------------------------------%

:- pred generate_config_key_choices(prog_config::in, string::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_config_key_choices(Config, SectionName, OrigString, Choices, !IO) :-
    get_notmuch_command(Config, Notmuch),
    make_quoted_command(Notmuch, ["config", "list"],
        redirect_input("/dev/null"), no_redirect, Command),
    call_system_capture_stdout(Command, no, CallRes, !IO),
    (
        CallRes = ok(ItemsString),
        % The empty string following the final newline is not an item.
        ItemsList = string.words_separator(unify('\n'), ItemsString),
        list.filter_map(
            filter_config_key_choice(SectionName ++ ".", OrigString),
            ItemsList, Choices0),
        list.sort(Choices0, Choices)
    ;
        CallRes = error(_),
        Choices = []
    ).

:- pred filter_config_key_choice(string::in, string::in, string::in,
    string::out) is semidet.

filter_config_key_choice(SectionNameDot, OrigString, Input, Key) :-
    string.remove_prefix(SectionNameDot, Input, InputSansSection),
    string.prefix(InputSansSection, OrigString),
    KeyStart = 0,
    find_first_char(InputSansSection, '=', KeyStart, KeyEnd),
    KeyEnd > KeyStart,
    string.between(InputSansSection, KeyStart, KeyEnd, Key).

:- pred find_first_char(string::in, char::in, int::in, int::out) is semidet.

find_first_char(S, FindChar, I0, I) :-
    string.unsafe_index_next(S, I0, I1, C),
    ( C = FindChar ->
        I = I0
    ;
        find_first_char(S, FindChar, I1, I)
    ).

%-----------------------------------------------------------------------------%

:- pred generate_address_choices(prog_config::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_address_choices(Config, OrigString, Choices, !IO) :-
    search_addressbook(Config, OrigString, MaybeFound, !IO),
    (
        MaybeFound = yes(Choice),
        Choices = [Choice]
    ;
        MaybeFound = no,
        search_notmuch_address(Config, OrigString, Choices, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred non_whitespace(char::in) is semidet.

non_whitespace(C) :-
    not char.is_whitespace(C).

:- pred not_comma(char::in) is semidet.

not_comma(C) :-
    C \= (',').

    % Note: Mercury 14.01 list.m also defines this.
    %
:- pred det_take(int::in, list(char)::in, list(char)::out) is det.

det_take(N, List, Start) :-
    ( list.take(N, List, StartPrime) ->
        Start = StartPrime
    ;
        unexpected($module, $pred, "not enough elements")
    ).

:- pred det_take_tail(int::in, list(char)::in, list(char)::out) is det.

det_take_tail(N, Xs, XsTail) :-
    list.length(Xs, Length),
    list.det_drop(Length - N, Xs, XsTail).

:- pred reverse_onto(list(char)::in, list(char)::in, list(char)::out) is det.

reverse_onto([], Acc, Acc).
reverse_onto([X | Xs], Acc0, Acc) :-
    reverse_onto(Xs, [X | Acc0], Acc).

:- pred common_prefix_strings(list(string)::in,
    list(char)::in, list(char)::out) is semidet.

common_prefix_strings([], !Prefix).
common_prefix_strings([String | Strings], Prefix0, Prefix) :-
    string.to_char_list(String, Chars),
    common_prefix(Chars, Prefix0, Prefix1),
    Prefix1 = [_ | _],
    common_prefix_strings(Strings, Prefix1, Prefix).

:- pred common_prefix(list(char)::in, list(char)::in, list(char)::out)
    is det.

common_prefix(A, B, CommonPrefix) :-
    (
        A = [X | As],
        B = [X | Bs]
    ->
        common_prefix(As, Bs, CommonPrefixRest),
        CommonPrefix = [X | CommonPrefixRest]
    ;
        CommonPrefix = []
    ).

%-----------------------------------------------------------------------------%

:- pred draw_text_entry(screen::in, info::in, info::out, io::di, io::uo)
    is det.

draw_text_entry(Screen, !Info, !IO) :-
    !.Info ^ prompt = Prompt,
    !.Info ^ state = te_state(Before, After, _Pre, _Post),
    !.Info ^ left_offset = LeftOffset0,

    get_cols(Screen, Cols),
    RemainCols = Cols - string_wcwidth(Prompt),
    calc_draw(RemainCols, Before, BeforeDraw, After, AfterDraw,
        LeftOffset0, LeftOffset),

    !Info ^ left_offset := LeftOffset,

    % Bit ugly.
    update_message(Screen, set_prompt(Prompt), !IO),
    get_msgentry_panel(Screen, Panel),
    draw(Panel, string.from_rev_char_list(BeforeDraw), !IO),
    panel.getyx(Panel, Y, X, !IO),
    draw(Panel, string.from_char_list(AfterDraw), !IO),
    panel.move(Panel, Y, X, !IO),
    panel.update_panels(!IO).

:- pred calc_draw(int::in, list(char)::in, list(char)::out,
    list(char)::in, list(char)::out, int::in, int::out) is det.

calc_draw(TotalCols, Before, BeforeDraw, After, AfterDraw, !LeftOffset) :-
    NumBefore = list.length(Before),
    % Check if the cursor moved past the previous left offset.
    !:LeftOffset = min(!.LeftOffset, NumBefore),
    (
        After = [],
        ReservedAfterCols = 1 % cursor
    ;
        After = [AfterChar | _],
        ReservedAfterCols = wcwidth(AfterChar)
    ),
    % Check if code points from Before after LeftOffset will fit in the
    % available screen columns.
    text_entry.det_take(NumBefore - !.LeftOffset, Before, BeforeDraw0),
    BeforeCols0 = list_wcwidth(BeforeDraw0),
    ( BeforeCols0 =< TotalCols - ReservedAfterCols ->
        % BeforeDraw0 fits.
        BeforeDraw = BeforeDraw0,
        AfterCols = TotalCols - BeforeCols0
    ;
        % BeforeDraw0 is too wide.
        take_upto_width(Before, BeforeDraw,
            TotalCols - ReservedAfterCols, RemainingCols),
        AfterCols = RemainingCols + ReservedAfterCols,
        !:LeftOffset = NumBefore - length(BeforeDraw)
    ),
    % Draw as many After code points as will fit in the remaining columns.
    take_upto_width(After, AfterDraw, AfterCols, _).

:- pred take_upto_width(list(char)::in, list(char)::out, int::in, int::out)
    is det.

take_upto_width([], [], !RemainCols).
take_upto_width([C | Cs], TakeChars, RemainCols0, RemainCols) :-
    RemainCols1 = RemainCols0 - wcwidth(C),
    ( RemainCols1 < 0 ->
        TakeChars = [],
        RemainCols = RemainCols0
    ;
        take_upto_width(Cs, TakeChars1, RemainCols1, RemainCols),
        TakeChars = [C | TakeChars1]
    ).

%-----------------------------------------------------------------------------%

:- pred isprint(char::in) is semidet.

:- pragma foreign_proc("C",
    isprint(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* The argument to isprint must be representable by an unsigned char
     * or equal to EOF.
     */
    SUCCESS_INDICATOR = (Char >= 0x80) || isprint(Char);
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
