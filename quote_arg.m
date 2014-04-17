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

%-----------------------------------------------------------------------------%

% Same algorithm as Python shlex.quote module - should be well tested.

quote_arg(String) = QuotedString :-
    ( String = "" ->
        QuotedString = "''"
    ; string.all_match(safe, String) ->
        QuotedString = String
    ;
        string.replace_all(String, "'", "'\"'\"'", QuotedString0),
        QuotedString = "'" ++ QuotedString0 ++ "'"
    ).

:- pred safe(char::in) is semidet.

safe(C) :- char.is_alnum_or_underscore(C).
safe('%').
safe('+').
safe(',').
safe('-').
safe('.').
safe('/').
safe(':').
safe('=').
safe('@').

% unsafe:
% I =< 0x20
% ! " # $ & ' ( ) * ; < = > ? [ \ ] ^ ` { | }

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
