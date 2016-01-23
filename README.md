bower
=====

Bower is a curses frontend for the [Notmuch] email system.
I wrote it for me, but you might like it, too.

bower is written in [Mercury].

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
* GNU coreutils: base64
* SMTP client to send messages (configurable)
* lynx to dump HTML emails (configurable)
* file(1) to detect MIME types when adding attachments


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


Configuration
=============

The bower configuration file is located at `~/.config/bower/bower.conf`.
See `bower.conf.sample` for details.

In particular, bower is designed such that it can be run locally,
calling out to notmuch on a remote machine (that holds your mail) via ssh.
The advantage is that you can start helper programs locally (e.g. web browser),
and add or save attachments on the local filesystem.

Bower also keeps some centralised information in the notmuch configuration
file `~/.notmuch-config`.  You may wish to tell bower about your Maildir
hierarchy, namely where to place draft and sent messages.
The defaults are given here:

    [bower:maildir]
    drafts_folder = Drafts
    sent_folder = Sent

It is recommended that your `.notmuch-config` file contains at least
these search exclusions:

    [search]
    exclude_tags=deleted;draft


Usage
=======

Run `bower` or `bower SEARCH-TERMS` to start.  By default, Bower displays the
last week's worth of mail.  You can change that by setting the `~default`
search alias; see below.


Views
=====

There are two main views: the index, and the combined thread/pager.

Index view
----------

This view shows the notmuch search results for the current query.
The keys are:

    j,k,g,G, etc.   move around
    [, ]            scroll half page up, down
    l, ~            change search terms ("limit")
    =               refresh search results
    Enter           open thread
    m               compose new message
    r               reply to sender
    e               reply to everyone
    L               reply to list
    R               recall postponed message
    a               add to addressbook
    /, ?            search for string within results
    n               skip to next search result
    N               toggle unread tag on current thread
    F               toggle flagged tag on current thread
    d               set deleted tag on current thread
    u               unset deleted tag on current thread
    +, -            add/remove arbitrary tags
    t               select/unselect thread
    T               unselect all threads
    '               bulk tag changes on selected threads
    "               same as ' but retain selections afterwards
    q               quit

Index view limit command
------------------------

The `l` (limit) command slightly extends the notmuch search term syntax with
these macros:

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

In addition to the built-in macros, you can add your own search term
expansions.  See below for "Search term aliases".


Index view behaviour
--------------------

Tag modifications are performed (mostly) asynchronously to minimise stutter.
Remember to quit properly using 'q' to flush all changes.  Tag updates will
be retried a limited number of time on failure, e.g. because the notmuch
database is locked.

By default, bower will call notmuch count every 60 seconds in the index
view, to notify you of new unread messages matching the current search
terms.


Thread/pager view
-----------------

This view pages through an entire thread.  The keys are:

    j, k            next, previous message
    g, G            first, last message
    b, space        previous, next page
    \, Enter        previous, next line
    [, ]            previous, next half page
    p               go to parent message
    S               skip quoted text
    /, ?, n         search, next
    O               change ordering of messages
    =               refresh search results

    N               toggle 'unread' tag on current message
    ^R              remove 'unread' tag on preceding messages
    F               toggle 'flagged' tag on current message
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
    B               resend message to another address ("bounce")
    E               use current message as a template for a new message
    R               recall postponed message
    a               add to addressbook

    v               highlight next visible attachment or URL or folded text
    V               highlight next visible attachment or top of message
    s               save highlighted message/part
    o               open highlighted message/part/URL with external program
    z               cycle alternative parts / toggle inline display
    Z               toggle inline display of part
    z, o            expand/collapse folded text

    i, q            return to index
    I               return to index, removing 'unread' tag on all messages

Tag updates are only applied when returning to the index.

The 'o' command, which opens parts and URLs, takes a command using Unix shell
quoting syntax.  If the command ends with an unquoted '&' character then the
command will be run in the background.


Search term aliases
-------------------

Bower will try to expand search terms written with the syntax `~WORD`.
The expansions should be added to the notmuch config file `~/.notmuch-config`
in a section called `[bower:search_alias]`.  Expansions may make use of other
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


Simple addressbook
------------------

When entering an email address, bower will try to expand any simple words
containing only alphanumeric, underscore, '-', '+', '.' or non-ASCII
characters.  The expansions should be added to the notmuch config file
`~/.notmuch-config` in a section called `[bower:addressbook]`, e.g.

    [bower:addressbook]
    someone = Someone <someone@example.org>
    someoneelse = someoneelse@example.org

You can add to the addressbook using 'a' in the index or thread views.


Sending mail
------------

Bower performs two steps when sending a message:

1. Call the configured `sendmail` command with the message on standard
   input.  This command should pass the message onto an SMTP server.
   If the command fails (exits with non-zero status) then stop immediately.

2. Call the configured `post_sendmail` command with the message on
   standard input.  When no command is set, the default behaviour is to
   use `notmuch insert` to add the sent message to the mail store, and
   into the database with the `sent` tag and without the `unread` tag.
   You may replace this with a custom command, or bypass the step by
   setting the command to the empty string.

If the `sendmail` and `post_sendmail` commands both run on the same remote
server, then there is an inefficiency because a single message would need
to be transferred to the remote server twice.  You could combine the two
steps into a single script run at the `sendmail` step, and disable the
`post_sendmail` step.


Author
======

Peter Wang <novalazy@gmail.com>

Feel free to contact me with feedback or suggestions.

