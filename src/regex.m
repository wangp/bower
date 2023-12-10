% Bower - a frontend for the Notmuch email system
% Copyright (C) 2023 Peter Wang

:- module regex.
:- interface.

:- import_module list.
:- import_module maybe.

:- type regex.

:- type cflag
    --->    reg_extended
    ;       reg_icase
    ;       reg_nosub
    ;       reg_newline.

:- type eflag
    --->    reg_notbol
    ;       reg_noteol.

    % regcomp(Regex, CFlags, Res):
    % Compile a regular expression.
    %
:- pred regcomp(string::in, list(cflag)::in, maybe_error(regex)::out) is det.

:- type regexec_result
    --->    have_match(list(regmatch))
    ;       no_match
    ;       error(string).

:- type regmatch
    --->    regmatch(
                rm_start_offset :: int, % can be -1
                rm_end_offset   :: int  % can be -1
            ).

    % regexec(Reg, String, EFlags, Res):
    % Match String against the compiled pattern Reg.
    %
    % Warning: not thread-safe.
    % Do not use the same regex from multiple threads simultaneously.
    %
:- pred regexec(regex::in, string::in, list(eflag)::in, regexec_result::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.

:- pragma foreign_decl("C", "#include <regex.h>").

:- pragma foreign_type("C", regex, "regex_t *").

:- pragma foreign_decl("C", local, "
static void
REGEX_finalize_regex(void *reg0, void *client_data)
{
    regex_t *reg = reg0;
    (void) client_data;

    regfree(reg);
}
").

%-----------------------------------------------------------------------------%

:- type cflags == int.
:- type eflags == int.

:- pragma foreign_enum("C", cflag/0,
    [
        reg_extended    - "REG_EXTENDED",
        reg_icase       - "REG_ICASE",
        reg_nosub       - "REG_NOSUB",
        reg_newline     - "REG_NEWLINE"
    ]).

:- pragma foreign_enum("C", eflag/0,
    [
        reg_notbol      - "REG_NOTBOL",
        reg_noteol      - "REG_NOTEOL"
    ]).

:- func cflag_to_int(cflag) = int.

:- pragma foreign_proc("C",
    cflag_to_int(CFlag::in) = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = CFlag;
").

:- func eflag_to_int(eflag) = int.

:- pragma foreign_proc("C",
    eflag_to_int(EFlag::in) = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = EFlag;
").

:- func or_list(func(T) = int, list(T)) = int.

or_list(_F, []) = 0.
or_list(F, [X | Xs]) = F(X) \/ or_list(F, Xs).

%-----------------------------------------------------------------------------%

regcomp(Str, CFlags, Res) :-
    CFlagsInt = or_list(cflag_to_int, CFlags),
    regcomp0(Str, CFlagsInt, ErrCode, Reg),
    ( if ErrCode = 0 then
        Res = ok(Reg)
    else
        regerror(ErrCode, Reg, Error),
        Res = error(Error)
    ).

:- pred regcomp0(string::in, cflags::in, int::out, regex::out) is det.

:- pragma foreign_proc("C",
    regcomp0(Str::in, CFlags::in, ErrCode::out, Reg::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    Reg = MR_GC_NEW_ATTRIB(regex_t, MR_ALLOC_ID);
    ErrCode = regcomp(Reg, Str, CFlags);
    if (ErrCode == 0) {
        MR_GC_register_finalizer(Reg, REGEX_finalize_regex, 0);
    }
").

%-----------------------------------------------------------------------------%

regexec(Reg, Str, EFlags, Res) :-
    EFlagsInt = or_list(eflag_to_int, EFlags),
    regexec0(Reg, Str, EFlagsInt, ErrCode, HaveMatch, Matches),
    ( if ErrCode = 0 then
        (
            HaveMatch = yes,
            Res = have_match(Matches)
        ;
            HaveMatch = no,
            Res = no_match
        )
    else
        % Even if regexec() is thread-safe, this call would not be.
        regerror(ErrCode, Reg, Error),
        Res = error(Error)
    ).

:- pred regexec0(regex::in, string::in, eflags::in, int::out, bool::out,
    list(regmatch)::out) is det.

:- pragma foreign_proc("C",
    regexec0(Reg::in, Str::in, EFlags::in, ErrCode::out, HaveMatch::out,
        MatchList::out),
    [may_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    regmatch_t *matches;
    int nmatch;
    int rc;
    int i;

    // If we had cflags, we could test for REG_NOSUB and skip this.
    nmatch = 1 + Reg->re_nsub;
    matches = MR_GC_NEW_ARRAY_ATTRIB(regmatch_t, nmatch, MR_ALLOC_ID);

    rc = regexec(Reg, Str, nmatch, matches, EFlags);

    if (rc == 0) {
        ErrCode = 0;
        HaveMatch = MR_YES;
        MatchList = MR_list_empty();
        for (i = nmatch - 1; i >= 0; i--) {
            MR_Word m = REGEX_make_regmatch(matches[i].rm_so, matches[i].rm_eo);
            MatchList = MR_list_cons(m, MatchList);
        }
    } else if (rc == REG_NOMATCH) {
        ErrCode = 0;
        HaveMatch = MR_NO;
        MatchList = MR_list_empty();
    } else {
        ErrCode = rc;
        HaveMatch = MR_NO;
        MatchList = MR_list_empty();
    }
").

:- func make_regmatch(int, int) = regmatch.
:- pragma foreign_export("C", make_regmatch(in, in) = out,
    "REGEX_make_regmatch").

make_regmatch(StartOfs, EndOfs) = regmatch(StartOfs, EndOfs).

%-----------------------------------------------------------------------------%

:- pred regerror(int::in, regex::in, string::out) is det.

:- pragma foreign_proc("C",
    regerror(ErrCode::in, Reg::in, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    char errbuf[256];

    regerror(ErrCode, Reg, errbuf, sizeof(errbuf)); // null terminated
    MR_make_aligned_string_copy_msg(Error, errbuf, MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
