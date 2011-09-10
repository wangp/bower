%-----------------------------------------------------------------------------%

:- module quote_arg.
:- interface.

:- import_module list.

:- pred args_to_quoted_command(list(string)::in, string::out) is det.

:- pred args_to_quoted_command_with_redirect(list(string)::in, string::in,
    string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

args_to_quoted_command(Args, Command) :-
    QuotedArgs = list.map(quote_arg, Args),
    Command = string.join_list(" ", QuotedArgs).

args_to_quoted_command_with_redirect(Args, RedirectOutput, Command) :-
    QuotedArgs = list.map(quote_arg, Args),
    QuotedOutput = quote_arg(RedirectOutput),
    Command = string.join_list(" ", QuotedArgs ++ [">", QuotedOutput]).

:- func quote_arg(string) = string.

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
