%-----------------------------------------------------------------------------%

:- module ansi_color.
:- interface.

:- func ansi_reset = string.
:- func ansi_bold = string.     % bright
:- func ansi_underline = string.
:- func ansi_inverse = string.
:- func ansi_positive = string.

:- func ansi_red = string.
:- func ansi_bright_red = string.
:- func ansi_green = string.
:- func ansi_bright_green = string.
:- func ansi_yellow = string.
:- func ansi_bright_yellow = string.
:- func ansi_blue = string.
:- func ansi_bright_blue = string.
:- func ansi_magenta = string.
:- func ansi_bright_magenta = string.
:- func ansi_cyan = string.
:- func ansi_bright_cyan = string.
:- func ansi_white = string.
:- func ansi_bright_white = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

ansi_reset          = "\x1B\[0m".
ansi_bold           = "\x1B\[1m".
ansi_underline      = "\x1B\[4m".
ansi_inverse        = "\x1B\[7m".
ansi_positive       = "\x1B\[22m".

ansi_red            = "\x1B\[31m".
ansi_bright_red     = "\x1B\[31;01m".
ansi_green          = "\x1B\[32m".
ansi_bright_green   = "\x1B\[32;01m".
ansi_yellow         = "\x1B\[33m".
ansi_bright_yellow  = "\x1B\[33;01m".
ansi_blue           = "\x1B\[34m".
ansi_bright_blue    = "\x1B\[34;01m".
ansi_magenta        = "\x1B\[35m".
ansi_bright_magenta = "\x1B\[35;01m".
ansi_cyan           = "\x1B\[36m".
ansi_bright_cyan    = "\x1B\[36;01m".
ansi_white          = "\x1B\[37m".
ansi_bright_white   = "\x1B\[37;01m".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
