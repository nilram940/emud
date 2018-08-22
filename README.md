This project contains the elisp files for an emacs mud client, emud.

# Installation

This code is in early development and has several paths hard-coded. In
particular it expects all files to be installed in `~/emud`.  (You can
change the path by changing line 6 of emud.el to point to the correct
path)

In order to install emud place all files in `~/emud` and byte compile
them.  Inside emacs type `M-x byte-recompile-directory`. When prompted
for a path enter `~/emud`.

# Use

In order to start emud. First load the code this most easily
accomplished by adding something like the following to your `.emacs`:

```lisp
(setq emud-host-alist
      (list 
        ;name        host                 port   login    password
       '("nanny"    "mud.lysator.liu.se" "2000" "MYCHAR" "MYPASS")))
(load "~/emud/emud.elc")
(load "~/emud/emud-map.elc")
(add-hook 'emud-mode-hook 'emud-map-start)
```

This will load the emud code and the mapping code and start the
mapping code when emud starts.

To start emud type M-x emud and when prompted enter a name from the list (i.e. nanny)

# XML

Emud works by parsing xml code emitted from the mud. This has only
been tested on nannyMUD (`mud.lysator.liu.se 2000`) and I know of no
other mud that supports xml in any form. This client will work best
when xml is enabled on nannymud. The command is:

```
toggle xml
```

# BUGS

Keep in mind this code is in early development and has many bugs. It
could leave your character stranded at the most inopportune time and
get your character killed.

## Global variables

emud.el currently uses a number of global variables. They are kept
that way to facilitate debugging. As a result **multi-playing is not
possible**
