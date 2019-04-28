bower
=====

Bower is a curses frontend for the [Notmuch] email system.
I wrote it for me, but you might like it, too.

bower is written in [Mercury].

See some [screen shots](screenshots).

[Notmuch]: http://notmuchmail.org/
[Mercury]: http://mercurylang.org/


Requirements
============

bower makes use of standard Linux utilities, so will likely
require some work to work on other systems.

At compile time:

* Mercury compiler (11.07 or later, including release-of-the-day);
  currently available at <http://dl.mercurylang.org/index.html>

At run time:

* notmuch
* ncurses with wide character support
* gpgme (GnuPG Made Easy)
* GNU coreutils: base64
* SMTP client to send messages (configurable)
* lynx to dump HTML emails (configurable)
* file(1) to detect MIME types when adding attachments

To create a man page from this readme, you will need:

* [pandoc] universal document converter
* awk

[pandoc]: https://github.com/jgm/pandoc


Compiling
=========

Firstly, to install Mercury from source you can follow these steps:

    tar xf mercury-srcdist-VERSION.tar.gz
    cd mercury-srcdist-VERSION
    ./configure --prefix=/path/to/mercury
    make install PARALLEL=-j6 LIBGRADES=
    export PATH=$PATH:/path/to/mercury/bin

where PARALLEL=-j6 reflects the number of parallel jobs to use.

With Mercury installed and `mmc` in your PATH, run:

    make PARALLEL=-j6

You may want to edit `Mercury.options` to suit your system.
If successful, you will get a binary named `bower`.

To generate a man page, run:

    make man

This will produce `bower.1` for installation into your `man` search path.


Configuration
=============

You can configure bower by creating a file in one of the following locations,
in order of priority:

  - the path specified in the environment variable `BOWER_CONFIG` (if set)

  - the path `$XDG_CONFIG_HOME/bower/bower.conf`
    (if the environment variable `XDG_CONFIG_HOME` is set and non-empty)

  - the path `$HOME/.config/bower/bower.conf`
    (if the environment variable `HOME` is set)

  - a path `$basedir/bower/bower.conf` where `$basedir` is an element of the
    colon-separated list given in the environment variable `XDG_CONFIG_DIRS`
    (defaults to `/etc/xdg`)

The details of the configuration file are given in `bower.conf.sample`.

Bower is designed such that it can be run on the local
machine but call out to notmuch on a remote machine (that holds your
mail) via ssh.  The advantage is that you can start helper programs on
the local machine (e.g. a web browser or image viewer), and add or save
attachments on the local filesystem, even if your mail archive is stored
on a different machine.

Bower also keeps some centralised information in the notmuch configuration
file.  You may wish to tell bower about your Maildir hierarchy, namely
where to place draft and sent messages.  The defaults are given here:

    [bower:maildir]
    drafts_folder = Drafts
    sent_folder = Sent

It is recommended that your `.notmuch-config` file contains at least
these search exclusions:

    [search]
    exclude_tags=deleted;draft


Usage
=====

Run `bower` or `bower SEARCH-TERMS` to start.  By default, Bower displays the
last week's worth of mail.  You can change that by setting the `~default`
search alias; see below.


Views
=====

There are two main views: the index, and the combined thread/pager.

Index view keys
---------------

This view shows the notmuch search results for the current query.
The keys are:

    j, k            next, previous thread
    g, G            first, last thread
    [, ]            scroll half page up, down
    Tab, comma      go to next unread thread
    Enter           open thread
    l, ~            change search terms ("limit")
    =               refresh search results
    z               toggle authors column

    /, ?            search for string within results
    n               skip to next search result

    N               toggle 'unread' tag on current thread
    F               toggle 'flagged' tag on current thread
    a               toggle 'inbox', 'unread' tags on current thread (archive)
    d               set 'deleted' tag on current thread
    u               unset 'deleted' tag on current thread
    +, -            add/remove arbitrary tags
    t               select/unselect thread
    T               unselect all threads
    '               bulk tag changes on selected threads
    "               same as ' but retain selections afterwards

    m               compose new message
    r               reply to sender
    e               reply to everyone
    L               reply to list
    R               recall postponed message
    @               add to address book

    |               pipe thread IDs to command

    q               quit

Thread/pager view keys
----------------------

This view pages through an entire thread.  The keys are:

    j, k            next, previous message
    g, G            first, last message
    b, space        previous, next page
    \, Enter        previous, next line
    [, ]            previous, next half page
    Tab, comma      go to next unread message
    p               go to parent message
    S               skip quoted text
    O               toggle ordering of messages (threaded or flat)
    =               refresh search results

    /, ?            search for string
    n               skip to next search result

    J               mark current message read and go to next message
    K               mark current message read and go to previous message
    N               toggle 'unread' tag on current message
    ^R              remove 'unread' tag on preceding messages
    F               toggle 'flagged' tag on current message
    a               toggle 'inbox', 'unread' tags on current message (archive)
    d               add 'deleted' tag on current message
    u               remove 'deleted' tag on current message
    +, -            add/remove arbitrary tags
    t               select/unselect message
    T               unselect all messages
    '               bulk tag changes on selected threads
    "               same as ' but retain selections afterwards

    r               reply to sender
    e               reply to everyone
    L               reply to list
    W               forward message
    B               resend message to another address ("bounce")
    E               use current message as a template for a new message
    R               recall postponed message
    @               add to address book

    v               highlight next visible attachment or URL or folded text
    V               highlight next visible attachment or top of message
    s               save highlighted message/part
    o               open highlighted message/part/URL with external program
    z               cycle alternative parts / toggle inline display / decrypt
    Z               toggle inline display of part / decrypt
    z, o            expand/collapse folded text
    y               verify signed part

    |               pipe thread or message IDs to command

    i, q            return to index
    I               return to index, removing 'unread' tag on all messages
    A               return to index, removing 'inbox' and 'unread' on messages

The 'o' command, which opens parts and URLs, takes a command using Unix shell
quoting syntax.  If the command ends with an unquoted '&' character then the
command will be run in the background.

Note: forward (W) and edit-as-new (E) currently will not create a proper
message template for messages that are manually decrypted in the thread view.
A workaround is to set `crypto.decrypt_by_default = true` in the bower
configuration so that encrypted messages are automatically decrypted upon
opening a thread.


Limit command syntax extensions
===============================

The `l` (limit) command in the index view slightly extends the notmuch search
term syntax with these macros:

    ~A              disable default cap on number of search results
    ~F              tag:flagged
    ~U              tag:unread
    ~D              tag:deleted
    ~d DATE..DATE
    ~d DATE..
    ~d ..DATE
    ~d DATE

By default, a maximum of 300 search results will be displayed.
Add ~A to the search string to get all results.

The ~d syntax passes dates through to notmuch as a "date:" range;
see `notmuch-search-terms`(7) for the date range syntax.
You may use curly brackets to surround date strings containing spaces.
The single date form ~d DATE is equivalent to ~d DATE..DATE.

Some examples:

    ~d 2011-06-01..
    ~d {2 weeks ago}..{last week}
    ~d yesterday


Search term aliases
-------------------

In addition to the built-in macros, bower will try to expand tokens of the
form `~NAME` in two ways:

  - if there is a search term alias called `NAME` then the token will
    be replaced with its expansion

  - if there is a notmuch named query called `NAME` then the token will
    be replaced with `query:NAME`

Search term aliases are defined in the notmuch config file in a section
called `[bower:search_alias]`. Expansions may make use of other
(non-recursive) expansions.  For example:

    [bower:search_alias]
    default = ~lw
    lw = ~d {last week}..
    2w = ~d {2 weeks ago}..
    lm = ~d {last month}..
    ly = ~d {last year}..
    notmuch = to:notmuch
    bower = ~notmuch AND bower
    bower_recent = ~lm ~bower

As mentioned earlier, the `~default` alias sets the initial search query
if you run `bower` without command-line arguments.

The differences between search term aliases and notmuch named queries are:

  - search term aliases are specific to bower
  - search term aliases may use bower-specific syntax, and refer to other
    search term aliases
  - named queries are stored in the notmuch database instead of the notmuch
    config file

You can tab complete search term aliases and notmuch named queries.


Tag modification behaviour
==========================

Tag modifications in the index view are (mostly) performed asynchronously
to minimise stutter.  Remember to quit properly using 'q' to ensure all
tag modifications are completed before exiting.  Tag modifications will
be retried a limited number of times on failure, e.g. because the notmuch
database is locked (this was a problem with old notmuch versions).

Unlike the index view, tag modifications in the thread/pager view will only
applied upon returning to the index view. If bower is terminated before
returning to the index view then any tag modifications made in the thread
view will be lost.


Polling for new messages
========================

By default, bower will call `notmuch count` every 60 seconds in both the index
and thread views in order to notify you of new unread messages matching the
search terms in the index, or new messages in the current thread. You can
change the polling frequency or disable it with the configuration option
`ui.poll_period_secs`.

You can also configure a command to run when new messages are found
using the `command.poll_notify` option.


Simple address book
===================

Bower can use `notmuch address` command to look up email addresses where
required, e.g. when composing a new message. Only addresses that appear
in the `From` header of messages from the last year will be considered;
this restriction improves performance and avoids finding stale email addresses.

Address aliases can also be kept in the notmuch config file in a section
called `[bower:addressbook]`.  For example:

    [bower:addressbook]
    someone = Someone <someone@example.org>
    someoneelse = someoneelse@example.org

You can add to the address book using '@' in the index or thread views.

Tab completion will prefer address aliases over addresses found by
`notmuch address`.


Sending mail
============

You can send mail through one or more accounts defined in `bower.conf`.
An account is selected by matching the From address on the message to the
address on the account.  Bower performs two steps when sending a message:

 1. Run the configured `sendmail` command with the message on standard input.
    The command should pass the message onto an SMTP server.
    If the `sendmail` command fails (exits with non-zero status) then the
    message is considered not sent, and the next step does not run.

 2. Run the "post-sendmail" step.

    The default behaviour (if `post_sendmail` is not set) is to use
    `notmuch insert` to add the message to the mail store, and to the
    database with the `sent` tag and without the `unread` tag.

    Otherwise, if `post_sendmail` is set to a non-empty command
    then the command is run with the message on standard input.

    Otherwise, if `post_sendmail` is set to the empty string
    then no command is run.

If both the `sendmail` and `post_sendmail` commands will run on the same
remote server then there is a slight inefficiency because a single
message would need to be transferred to the remote server twice.
If that is a concern, you could combine the two steps into a single script
to be run at the `sendmail` step, and disable the `post_sendmail` command.


Encryption and signing (beta)
=============================

Bower can produce encrypted and/or signed PGP/MIME messages through GnuPG.
Message decryption and signature verification are performed by notmuch.
It is up to the user to configure GnuPG on the machines running bower
or notmuch.


Author
======

Peter Wang <novalazy@gmail.com>

Feel free to contact me with feedback or suggestions.

