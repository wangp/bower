% Bower - a frontend for the Notmuch email system
% Copyright (C) 2020 Peter Wang

:- module char_util.
:- interface.

:- import_module char.

:- pred is_printable(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
    is_printable(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* The argument to isprint must be representable by an unsigned char
     * or equal to EOF.
     */
    SUCCESS_INDICATOR = (Char >= 0x80) || isprint(Char);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
