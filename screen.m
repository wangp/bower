%-----------------------------------------------------------------------------%

:- module views.
:- interface.

:- import_module curs.
:- import_module curs.panel.

:- type panels
    --->    panels(
                rows            :: int,
                cols            :: int,
                main_panel      :: panel,
                bar_panel       :: panel,
                msgentry_panel  :: panel
            ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
