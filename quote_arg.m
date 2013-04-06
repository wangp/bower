% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module quote_arg.
:- interface.

:- import_module list.

:- type redirect_input
    --->    no
    ;       redirect_input(string).

:- type redirect_output
    --->    no
    ;       redirect_output(string)
    ;       redirect_append(string).

:- pred args_to_quoted_command(list(string)::in, string::out) is det.

:- pred args_to_quoted_command(list(string)::in,
    redirect_input::in, redirect_output::in, string::out) is det.

:- func quote_arg(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

args_to_quoted_command(Args, Command) :-
    args_to_quoted_command(Args, no, no, Command).

args_to_quoted_command(Args, MaybeRedirectInput, MaybeRedirectOutput,
        Command) :-
    QuotedArgs0 = list.map(quote_arg, Args),
    (
        MaybeRedirectInput = redirect_input(RedirectInput),
        QuotedInput = quote_arg(RedirectInput),
        QuotedArgs1 = QuotedArgs0 ++ ["<", QuotedInput]
    ;
        MaybeRedirectInput = no,
        QuotedArgs1 = QuotedArgs0
    ),
    (
        MaybeRedirectOutput = redirect_output(TargetFile),
        QuotedArgs = QuotedArgs1 ++ [">", quote_arg(TargetFile)]
    ;
        MaybeRedirectOutput = redirect_append(TargetFile),
        QuotedArgs = QuotedArgs1 ++ [">>", quote_arg(TargetFile)]
    ;
        MaybeRedirectOutput = no,
        QuotedArgs = QuotedArgs1
    ),
    Command = string.join_list(" ", QuotedArgs).

quote_arg(String) = QuotedString :-
    string.foldr(quote_char, String, [], RevChars),
    string.from_char_list(RevChars, QuotedString).

:- pred quote_char(char::in, list(char)::in, list(char)::out) is det.

quote_char(Char, Acc0, Acc) :-
    ( shell_special_char(Char) ->
        Acc = ['\\', Char | Acc0]
    ;
        Acc = [Char | Acc0]
    ).

:- pred shell_special_char(char::in) is semidet.

shell_special_char(' ').
shell_special_char('!').
shell_special_char('"').
shell_special_char('#').
shell_special_char('$').
shell_special_char('%').
shell_special_char('&').
shell_special_char('''').
shell_special_char('(').
shell_special_char(')').
shell_special_char('*').
shell_special_char(';').
shell_special_char('<').
shell_special_char('=').
shell_special_char('>').
shell_special_char('?').
shell_special_char('[').
shell_special_char('\\').
shell_special_char(']').
shell_special_char('^').    % maybe
shell_special_char('`').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
