% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module string_util.
:- interface.

:- import_module char.
:- import_module list.

:- func wcwidth(char) = int.

:- func list_wcwidth(list(char)) = int.

:- func string_wcwidth(string) = int.

:- pred strcase_equal(string::in, string::in) is semidet.

:- pred strcase_prefix(string::in, string::in) is semidet.

:- pred unsafe_substring_prefix(string::in, int::in, string::in) is semidet.

:- pred strcase_str(string::in, string::in) is semidet.

:- pred strrchr(string::in, char::in, int::out) is semidet.

:- pred unsafe_strstr(string::in, string::in, int::in, int::out) is semidet.

:- pred advance_while(pred(char)::in(pred(in) is semidet), string::in,
    int::in, int::out) is det.

:- pred skip_whitespace(string::in, int::in, int::out) is det.

:- pred until_whitespace(string::in, int::in, int::out) is det.

:- pred get_extension(string::in, string::out) is semidet.

:- pred fix_utf8(string::in, string::out) is det.

:- type pieces
    --->    empty
    ;       literal(string, pieces)
    ;       substring(string, int, int, pieces). % base, start, end

:- pred string_from_rev_pieces(pieces::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module char.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    #include <wchar.h>
").

:- pragma foreign_proc("C",
    wcwidth(C::in) = (Width::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Width = (C < 256) ? 1 : wcwidth(C);
").

list_wcwidth([]) = 0.
list_wcwidth([C | Cs]) = wcwidth(C) + list_wcwidth(Cs).

string_wcwidth(S) = Width :-
    string.foldl(string_wcwidth_2, S, 0, Width).

:- pred string_wcwidth_2(char::in, int::in, int::out) is det.

string_wcwidth_2(C, Width, Width + wcwidth(C)).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    strcase_equal(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasecmp(SA, SB) == 0);
").

:- pragma foreign_proc("C",
    strcase_prefix(String::in, Prefix::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strncasecmp(String, Prefix, strlen(Prefix)) == 0);
").

:- pragma foreign_proc("C",
    unsafe_substring_prefix(String::in, Index::in, Prefix::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strncmp(String + Index, Prefix, strlen(Prefix)) == 0);
").

:- pragma foreign_proc("C",
    strcase_str(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasestr(SA, SB) != 0);
").

:- pragma foreign_proc("C",
    strrchr(S::in, C::in, I::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const char *p;

    p = strrchr(S, C);
    if (p != NULL) {
        SUCCESS_INDICATOR = MR_TRUE;
        I = (p - S);
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
        I = -1;
    }
").

:- pragma foreign_proc("C",
    unsafe_strstr(Haystack::in, Needle::in, BeginAt::in, Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const char *p;

    p = strstr(Haystack + BeginAt, Needle);
    if (p != NULL) {
        SUCCESS_INDICATOR = MR_TRUE;
        Index = (p - Haystack);
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
        Index = -1;
    }
").

%-----------------------------------------------------------------------------%

advance_while(Pred, String, I0, I) :-
    (
        string.unsafe_index_next(String, I0, I1, Char),
        Pred(Char)
    ->
        advance_while(Pred, String, I1, I)
    ;
        I = I0
    ).

skip_whitespace(String, I0, I) :-
    advance_while(char.is_whitespace, String, I0, I).

until_whitespace(String, I0, I) :-
    advance_while(isnt(char.is_whitespace), String, I0, I).

%-----------------------------------------------------------------------------%

get_extension(FileName, Ext) :-
    string.length(FileName, End),
    get_extension_2(FileName, End, Start),
    string.between(FileName, Start, End, Ext).

:- pred get_extension_2(string::in, int::in, int::out) is semidet.

get_extension_2(FileName, !Index) :-
    string.unsafe_prev_index(FileName, !Index, C),
    ( C = ('.') ->
        true
    ; C = ('/') ->
        fail
    ; C = ('\\') ->
        fail
    ;
        get_extension_2(FileName, !Index)
    ).

%-----------------------------------------------------------------------------%

fix_utf8(String0, String) :-
    fix_utf8_loop(String0, length(String0), 0, 0, empty, Pieces),
    ( Pieces = substring(_, _, _, empty) ->
        % Common case.
        String = String0
    ;
        string_from_rev_pieces(Pieces, String)
    ).

:- pred fix_utf8_loop(string::in, int::in, int::in, int::in,
    pieces::in, pieces::out) is det.

fix_utf8_loop(S, Max, Anchor, I0, !Pieces) :-
    ( I0 < Max ->
        string.unsafe_index_code_unit(S, I0, C0),
        I1 = I0 + 1,
        ( C0 =< 0x7f ->
            % Plain ASCII.
            fix_utf8_loop(S, Max, Anchor, I1, !Pieces)
        ;
            ( C0 =< 0xDF ->
                C0 > 0xC1,
                % 2-byte sequence.
                Imax = I0 + 2,
                C1 = C0 /\ 0x1F,
                MinC = 0x80
            ; C0 =< 0xEF ->
                % 3-byte sequence.
                Imax = I0 + 3,
                C1 = C0 /\ 0x0F,
                MinC = 0x800
            ;
                C0 =< 0xF4,
                % 4-byte sequence.
                Imax = I0 + 4,
                C1 = C0 /\ 0x07,
                MinC = 0x10000
            ),
            Imax =< Max,
            check_multibyte_seq(S, I1, Imax, C1, MinC)
        ->
            fix_utf8_loop(S, Max, Anchor, Imax, !Pieces)
        ;
            % Otherwise invalid.
            add_substring_piece(S, Anchor, I0, !Pieces),
            add_bad_code_unit(C0, !Pieces),
            fix_utf8_loop(S, Max, I1, I1, !Pieces)
        )
    ;
        add_substring_piece(S, Anchor, I0, !Pieces)
    ).

:- pred check_multibyte_seq(string::in, int::in, int::in, int::in, int::in)
    is semidet.

check_multibyte_seq(S, I, Imax, C, MinC) :-
    ( I = Imax ->
        MinC =< C
    ;
        string.unsafe_index_code_unit(S, I, D),
        D /\ 0xC0 = 0x80,
        C1 = (C `unchecked_left_shift` 6) \/ (D /\ 0x3F),
        check_multibyte_seq(S, I + 1, Imax, C1, MinC)
    ).

:- pred add_substring_piece(string::in, int::in, int::in,
    pieces::in, pieces::out) is det.

add_substring_piece(BaseString, Start, End, Pieces0, Pieces) :-
    ( End > Start ->
        Pieces = substring(BaseString, Start, End, Pieces0)
    ;
        Pieces = Pieces0
    ).

:- pred add_bad_code_unit(int::in, pieces::in, pieces::out) is det.

add_bad_code_unit(C, Pieces0, Pieces) :-
    % Treat code unit as iso-8859-1 character code.
    Literal = string.from_char(char.det_from_int(C)),
    Pieces = literal(Literal, Pieces0).

%-----------------------------------------------------------------------------%

string_from_rev_pieces(Pieces, String) :-
    pieces_length(Pieces, 0, Length),
    allocate_string(Length, String0),
    copy_rev_pieces(Pieces, Length, String0, String).

:- pred pieces_length(pieces::in, int::in, int::out) is det.

pieces_length(Pieces, Length0, Length) :-
    (
        Pieces = empty,
        Length = Length0
    ;
        Pieces = literal(Literal, Rest),
        string.length(Literal, PieceLength),
        pieces_length(Rest, Length0 + PieceLength, Length)
    ;
        Pieces = substring(_BaseString, Start, End, Rest),
        % We trust that PieceLength =< length(BaseString).
        PieceLength = End - Start,
        expect(PieceLength >= 0, $module, $pred,
            "substring has negative length"),
        pieces_length(Rest, Length0 + PieceLength, Length)
    ).

:- pred allocate_string(int::in, string::uo) is det.

:- pragma foreign_proc("C",
    allocate_string(Length::in, String::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    MR_allocate_aligned_string_msg(String, Length, MR_ALLOC_ID);
    String[Length] = '\\0';
").

:- pred copy_rev_pieces(pieces::in, int::in, string::di, string::uo) is det.

copy_rev_pieces(Pieces, EndPos, !String) :-
    (
        Pieces = empty,
        expect(unify(EndPos, 0), $module, $pred, "EndPos != 0")
    ;
        Pieces = literal(Literal, RestPieces),
        PieceLength = length(Literal),
        StartPos = EndPos - PieceLength,
        expect(StartPos >= 0, $module, $pred, "StartPos < 0"),
        do_copy(Literal, 0, PieceLength, StartPos, !String),
        copy_rev_pieces(RestPieces, StartPos, !String)
    ;
        Pieces = substring(BaseString, BaseStart, BaseEnd, RestPieces),
        PieceLength = BaseEnd - BaseStart,
        StartPos = EndPos - PieceLength,
        expect(StartPos >= 0, $module, $pred, "StartPos < 0"),
        do_copy(BaseString, BaseStart, PieceLength, StartPos, !String),
        copy_rev_pieces(RestPieces, StartPos, !String)
    ).

:- pred do_copy(string::in, int::in, int::in,
    int::in, string::di, string::uo) is det.

:- pragma foreign_proc("C",
    do_copy(Src::in, SrcStart::in, SrcLength::in,
        DestStart::in, Dest0::di, Dest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    memcpy(Dest0 + DestStart, Src + SrcStart, SrcLength);
    Dest = Dest0;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
