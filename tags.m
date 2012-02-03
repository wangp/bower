% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module tags.
:- interface.

:- pred standard_tag(string::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

standard_tag("deleted").
standard_tag("flagged").
standard_tag("inbox").
standard_tag("new").
standard_tag("replied").
standard_tag("sent").
standard_tag("signed").
standard_tag("unread").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
