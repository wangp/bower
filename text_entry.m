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
:- import_module screen.

:- type history.

:- type completion_type
    --->    complete_none
    ;       complete_path(home)
    ;       complete_limit(
                % Name of search alias section.
                string,
                % Prefixes to trigger tag completion.
                list(string)
            )
    ;       complete_tags_smart(
                % All selected messages have these tags.
                and_tags    :: set(string),
                % At least one selected message has these tags.
                or_tags     :: set(string)
            )
    ;       complete_config_key(string). % config section name

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
:- import_module require.
:- import_module string.

:- import_module curs.
:- import_module curs.panel.
:- import_module path_expand.
:- import_module call_system.
:- import_module prog_config.
:- import_module quote_arg.
:- import_module string_util.

:- type history == list(string). % reverse

:- type sub_info
    --->    sub_info(
                first_time      :: bool,
                left_offset     :: int,
                pre_history     :: history,
                post_history    :: history,
                compl_type      :: completion_type,
                compl_choices   :: list(string),
                compl_point     :: int
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
    LeftOffset = 0,
    string.to_char_list(Initial, Before0),
    list.reverse(Before0, Before),
    After = [],
    SubInfo = sub_info(FirstTime, LeftOffset, History, [],
        CompleteType, [], 0),
    text_entry_real(Screen, Prompt, Before, After, SubInfo, Return, !IO),
    update_message(Screen, clear_message, !IO).

:- pred text_entry_real(screen::in, string::in, list(char)::in, list(char)::in,
    sub_info::in, maybe(string)::out, io::di, io::uo) is det.

text_entry_real(Screen, Prompt, Before, After, SubInfo0, Return, !IO) :-
    LeftOffset0 = SubInfo0 ^ left_offset,
    draw_text_entry(Screen, Prompt, Before, After, LeftOffset0, LeftOffset,
        !IO),
    SubInfo = SubInfo0 ^ left_offset := LeftOffset,

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
        ( Key = char('\x17\') % ^W
        ; Key = meta('\x7f\') % DEL
        ; Key = meta('\x08\') % BS
        ; Key = metacode(key_backspace)
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
        Key = char('\x14\') % ^T
    ->
        transpose(Before, Before1, After, After1),
        text_entry(Screen, Prompt, Before1, After1, SubInfo, Return, !IO)
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
        Type = SubInfo ^ compl_type,
        forward_for_completion(Type, Before, Before1, After, After1),
        do_completion(Before1, Before2, After1, SubInfo, MaybeSubInfo1,
            !IO),
        (
            MaybeSubInfo1 = yes(SubInfo1),
            clear_first_time_flag(SubInfo1, SubInfo2),
            text_entry_real(Screen, Prompt, Before2, After1, SubInfo2, Return,
                !IO)
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
            Key = timeout_or_error
        ->
            % Don't clear first time flag on a timeout.
            text_entry_real(Screen, Prompt, Before, After, SubInfo, Return,
                !IO)
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
        !SubInfo ^ compl_choices := [],
        !SubInfo ^ compl_point := 0
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

:- pred forward_for_completion(completion_type::in,
    list(char)::in, list(char)::out, list(char)::in, list(char)::out) is det.

forward_for_completion(Type, Before0, Before, After0, After) :-
    (
        Type = complete_none,
        Before = Before0,
        After = After0
    ;
        Type = complete_path(_),
        bol_eol(After0, Before0, Before),
        After = []
    ;
        ( Type = complete_limit(_, _)
        ; Type = complete_tags_smart(_, _)
        ; Type = complete_config_key(_)
        ),
        list.takewhile(non_whitespace, After0, Take, After),
        Before = list.reverse(Take) ++ Before0
    ).

:- pred do_completion(list(char)::in, list(char)::out, list(char)::in,
    sub_info::in, maybe(sub_info)::out, io::di, io::uo) is det.

do_completion(Orig, Replacement, After, SubInfo0, MaybeSubInfo, !IO) :-
    Type = SubInfo0 ^ compl_type,
    Choices0 = SubInfo0 ^ compl_choices,
    (
        Choices0 = [],
        Type = complete_none,
        Choices = [],
        CompletionPoint = 0
    ;
        Choices0 = [],
        Type = complete_path(Home),
        string.from_rev_char_list(Orig, OrigString),
        generate_path_choices(Home, OrigString, Choices, !IO),
        CompletionPoint = 0
    ;
        Choices0 = [],
        Type = complete_limit(SearchAliasSection, TagCompletionTriggers),
        list.takewhile(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        string.from_rev_char_list(Word, WordString),
        generate_limit_choices(SearchAliasSection, TagCompletionTriggers,
            WordString, Choices, !IO)
    ;
        Choices0 = [],
        Type = complete_tags_smart(AndTags, OrTags),
        list.takewhile(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        get_entered_tags(Untouched, After, EnteredTags),
        string.from_rev_char_list(Word, WordString),
        generate_smart_tag_choices(AndTags, OrTags, EnteredTags, WordString,
            Choices, !IO)
    ;
        Choices0 = [],
        Type = complete_config_key(SectionName),
        list.takewhile(non_whitespace, Orig, Word, Untouched),
        list.length(Untouched, CompletionPoint),
        string.from_rev_char_list(Word, WordString),
        generate_config_key_choices(SectionName, WordString, Choices, !IO)
    ;
        Choices0 = [_ | _],
        Choices = Choices0,
        CompletionPoint = SubInfo0 ^ compl_point
    ),
    (
        Choices = [],
        Replacement = Orig,
        MaybeSubInfo = no
    ;
        Choices = [FirstChoice | MoreChoices],
        det_take_tail(CompletionPoint, Orig, OrigKeep),
        reverse_onto(string.to_char_list(FirstChoice), OrigKeep, Replacement),
        (
            MoreChoices = [],
            SubInfo = SubInfo0 ^ compl_choices := []
        ;
            MoreChoices = [_ | _],
            RotateChoices = MoreChoices ++ [FirstChoice],
            SubInfo1 = SubInfo0 ^ compl_choices := RotateChoices,
            SubInfo = SubInfo1 ^ compl_point := CompletionPoint
        ),
        MaybeSubInfo = yes(SubInfo)
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

:- pred generate_limit_choices(string::in, list(string)::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_limit_choices(SearchAliasSection, TagCompletionTriggers, OrigString,
        Choices, !IO) :-
    ( string.remove_prefix("~", OrigString, KeyPrefix) ->
        generate_search_alias_choices(SearchAliasSection, KeyPrefix, Choices,
            !IO)
    ;
        generate_tag_choices(TagCompletionTriggers, OrigString, Choices, !IO)
    ).

:- pred generate_search_alias_choices(string::in, string::in,
    list(string)::out, io::di, io::uo) is det.

generate_search_alias_choices(SearchAliasSection, KeyPrefix, Choices, !IO) :-
    generate_config_key_choices(SearchAliasSection, KeyPrefix, Choices0, !IO),
    list.map(append("~"), Choices0) = Choices.

%-----------------------------------------------------------------------------%

:- pred generate_tag_choices(list(string)::in, string::in, list(string)::out,
    io::di, io::uo) is det.

generate_tag_choices([], _OrigString, [], !IO).
generate_tag_choices(CompletionTriggers, OrigString, Choices, !IO) :-
    CompletionTriggers = [Trigger | Triggers],
    ( string.remove_prefix(Trigger, OrigString, TagPrefix) ->
        get_notmuch_all_tags(TagsList, !IO),
        list.filter_map(filter_tag_choice(Trigger, TagPrefix),
            TagsList, Choices)
    ;
        generate_tag_choices(Triggers, OrigString, Choices, !IO)
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

:- pred generate_smart_tag_choices(set(string)::in, set(string)::in,
    set(string)::in, string::in, list(string)::out, io::di, io::uo) is det.

generate_smart_tag_choices(AndTagSet, OrTagSet, EnteredTagSet, OrigString,
        Choices, !IO) :-
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
        get_notmuch_all_tags(AllTagsList, !IO),
        set.difference(from_list(AllTagsList), AndTagSet, CandidateSet0),
        set.difference(CandidateSet0, EnteredTagSet, CandidateSet),
        set.to_sorted_list(CandidateSet, CandidateList),
        list.filter_map(filter_tag_choice("+", TagPrefix),
            CandidateList, Choices)
    ;
        Choices = []
    ).

:- pred get_notmuch_all_tags(list(string)::out, io::di, io::uo) is det.

get_notmuch_all_tags(TagsList, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    args_to_quoted_command(["search", "--output=tags", "--", "*"], Command),
    call_system_capture_stdout(Notmuch ++ Command, no, CallRes, !IO),
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

:- pred generate_config_key_choices(string::in, string::in, list(string)::out,
    io::di, io::uo) is det.

generate_config_key_choices(SectionName, OrigString, Choices, !IO) :-
    get_notmuch_prefix(Notmuch, !IO),
    args_to_quoted_command(["config", "list"], Command),
    call_system_capture_stdout(Notmuch ++ Command, no, CallRes, !IO),
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

:- pred non_whitespace(char::in) is semidet.

non_whitespace(C) :-
    not char.is_whitespace(C).

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

%-----------------------------------------------------------------------------%

:- pred draw_text_entry(screen::in, string::in, list(char)::in, list(char)::in,
    int::in, int::out, io::di, io::uo) is det.

draw_text_entry(Screen, Prompt, Before, After, LeftOffset0, LeftOffset, !IO) :-
    get_cols(Screen, Cols),
    RemainCols = Cols - string_wcwidth(Prompt),
    calc_draw(RemainCols, Before, BeforeDraw, After, AfterDraw,
        LeftOffset0, LeftOffset),

    get_msgentry_panel(Screen, Panel),
    panel.erase(Panel, !IO),
    panel.attr_set(Panel, normal, !IO),
    my_addstr(Panel, Prompt, !IO),
    my_addstr(Panel, string.from_rev_char_list(BeforeDraw), !IO),
    panel.getyx(Panel, Y, X, !IO),
    my_addstr(Panel, string.from_char_list(AfterDraw), !IO),
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
