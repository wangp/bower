bower
=====

Bower is a curses frontend for the [Notmuch] email system.
I wrote it for me, but you might like it, too.

bower is written in [Mercury].

[Notmuch]: http://notmuchmail.org/
[Mercury]: http://www.cs.mu.oz.au/research/mercury/


Requirements
============

bower makes use of standard Linux utilities, so will likely
require some work to work on other systems.

At compile time:

* Mercury compiler (11.07 beta or release-of-the-day)

At run time:

* notmuch
* notmuch-deliver (for adding draft and sent messages)
* ncurses with wide character support
* GNU coreutils: date, base64
* SMTP client to send messages (configurable)
* lynx to dump HTML emails (configurable)
* file(1) to detect MIME types when adding attachments


Compiling
=========

Firstly, to install Mercury from source you can follow these steps:

    tar xf mercury-compiler-VERSION.tar.gz
    cd mercury-compiler-VERSION
    ./configure --prefix=/path/to/mercury
    make install PARALLEL=-j6 LIBGRADES=
    export PATH=$PATH:/path/to/mercury/bin

where PARALLEL=-j6 reflects the number of parallel jobs to use.

With Mercury installed and `mmc` in your PATH, run:

    make PARALLEL=-j6

You may want to edit `Mercury.options` to suit your system.
If successful, you will get a binary named `bower`.


Configuration
=============

The configuration file is called `~/.config/bower/bower.conf`.
See `bower.conf.sample` for details.


Usage
=====

There are two main views: the index, and the combined thread/pager.

Index view
----------

This view shows the notmuch search results for the current query.
The keys are:

    j,k,g,G, etc.   move around
    l               change search terms ("limit")
    =               refresh search results
    Enter           open thread
    m               compose new message
    R               recall postponed message
    q               quit

The `l` command slightly extends the notmuch search term syntax with these
macros:

    ~F              tag:flagged
    ~U              tag:unread
    ~D              tag:deleted
    ~lw             last week
    ~lm             last month
    ~yesterday
    ~today
    ~dDATE..DATE    (no spaces)
    ~dDATE
    ~dDATE..
    ~d..DATE

Date strings are passed to the date(1) utility so you can write any date that
it understands, e.g. "~d2011-06-01..", as long as it doesn't require spaces.
This is an interim solution until notmuch gains a date parser.

bower will call notmuch count every minute to check for new results.
This is pretty crude.


Thread/pager view
-----------------

This view pages through an entire thread.  The keys are:

    j, k            next, previous message
    b, space        previous, next page
    \, Enter        previous, next line
    [, ]            previous, next half page
    S               skip quoted text
    /, n            search, next
    =               refresh search results

    N               toggle 'unread' tag on current message
    F               toggle 'flagged' tag on current message
    d               add 'deleted' tag on current message
    u               remove 'deleted' tag on current message

    r               reply to sender
    g               reply to group
    L               reply to list

    v               highlight next visible message or part
    V               highlight next visible URL
    s               save highlighted message/part
    o               open highlighted message/part/URL with external program

    i, q            return to index
    I               return to index, removing 'unread' tag on all messages

Tag updates are only applied when returning to the index.


Simple addressbook
------------------

When entering an email address, bower will try to expand any simple words
containing only alphanumeric, underscore or '-' or '+' characters.
The expansions should be added to the notmuch config file `~/.notmuch-config`
in a section called [addressbook], e.g.

    [addressbook]
    someone = Someone <someone@example.org>
    someoneelse = someoneelse@example.org


Contact
=======

Contact me at novalazy@gmail.com with any feedback or suggestions!

