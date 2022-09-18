% Bower - a frontend for the Notmuch email system
% Copyright (C) 2011 Peter Wang

:- module quote_arg.
:- interface.

:- func quote_arg(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

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
