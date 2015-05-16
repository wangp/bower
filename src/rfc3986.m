% Bower - a frontend for the Notmuch email system
% Copyright (C) 2015 Peter Wang

:- module rfc3986.
:- interface.

:- import_module char.

:- pred valid_uri_char(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

valid_uri_char('!').    % sub-delims
valid_uri_char('*').    % sub-delims
valid_uri_char('''').   % sub-delims
valid_uri_char('(').    % sub-delims
valid_uri_char(')').    % sub-delims
valid_uri_char(';').    % sub-delims
valid_uri_char(':').    % gen-delims
valid_uri_char('@').    % gen-delims
valid_uri_char('&').    % sub-delims
valid_uri_char('=').    % sub-delims
valid_uri_char('+').    % sub-delims
valid_uri_char('$').    % sub-delims
valid_uri_char(',').    % sub-delims
valid_uri_char('/').    % gen-delims
valid_uri_char('?').    % gen-delims
valid_uri_char('#').    % gen-delims
valid_uri_char('[').    % gen-delims
valid_uri_char(']').    % gen-delims
valid_uri_char('A').    % unreserved
valid_uri_char('B').
valid_uri_char('C').
valid_uri_char('D').
valid_uri_char('E').
valid_uri_char('F').
valid_uri_char('G').
valid_uri_char('H').
valid_uri_char('I').
valid_uri_char('J').
valid_uri_char('K').
valid_uri_char('L').
valid_uri_char('M').
valid_uri_char('N').
valid_uri_char('O').
valid_uri_char('P').
valid_uri_char('Q').
valid_uri_char('R').
valid_uri_char('S').
valid_uri_char('T').
valid_uri_char('U').
valid_uri_char('V').
valid_uri_char('W').
valid_uri_char('X').
valid_uri_char('Y').
valid_uri_char('Z').
valid_uri_char('a').
valid_uri_char('b').
valid_uri_char('c').
valid_uri_char('d').
valid_uri_char('e').
valid_uri_char('f').
valid_uri_char('g').
valid_uri_char('h').
valid_uri_char('i').
valid_uri_char('j').
valid_uri_char('k').
valid_uri_char('l').
valid_uri_char('m').
valid_uri_char('n').
valid_uri_char('o').
valid_uri_char('p').
valid_uri_char('q').
valid_uri_char('r').
valid_uri_char('s').
valid_uri_char('t').
valid_uri_char('u').
valid_uri_char('v').
valid_uri_char('w').
valid_uri_char('x').
valid_uri_char('y').
valid_uri_char('z').
valid_uri_char('0').
valid_uri_char('1').
valid_uri_char('2').
valid_uri_char('3').
valid_uri_char('4').
valid_uri_char('5').
valid_uri_char('6').
valid_uri_char('7').
valid_uri_char('8').
valid_uri_char('9').
valid_uri_char('-').    % unreserved
valid_uri_char('_').    % unreserved
valid_uri_char('.').    % unreserved
valid_uri_char('~').    % unreserved
valid_uri_char('%').    % percent-encoding

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
