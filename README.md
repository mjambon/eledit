Overview
--------

Eledit is a line editor, allowing to use control commands like in emacs
or in shells (bash, tcsh). To be used with interactive commands.

Installation
------------

Building eledit requires only a standard installation of OCaml.
It no longer relies on Camlp4 or Camlp5.
It is compiled into a standalone executable that doesn't require an
OCaml installation to run properly.

```
$ make
$ make install
```

By default it installs into $HOME/bin and $HOME/man/man1.
The usual variables PREFIX, BINDIR and MANDIR can be overridden
during the install phase:

```
$ make PREFIX=/usr/local install
```

Copyright
---------

Eledit is a fork of ledit 2.03, written by Daniel de Rauglaudre.

Up to ledit 2.03, all files are copyright 2001-2012 Institut
National de Recherche en Informatique et Automatique (Inria).

See LICENSE file.
