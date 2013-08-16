ifndef PREFIX
  PREFIX = $(HOME)
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif

ifndef MANDIR
  MANDIR = $(PREFIX)/man/man1
endif

.PHONY: default
default: eledit eledit.1

SOURCES = fstream.ml cursor.mli cursor.ml ledit.mli ledit.ml go.ml

eledit: $(SOURCES)
	ocamlopt -o eledit unix.cmxa \
          fstream.ml cursor.mli cursor.ml ledit.mli ledit.ml go.ml

eledit.1: eledit.1.tpl go.ml
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/ELEDIT_VERSION/$$VERSION/ eledit.1.tpl > eledit.1

.PHONY: install
install:
	-mkdir -p "$(BINDIR)"
	-cp eledit "$(BINDIR)/"
	-mkdir -p "$(MANDIR)"
	-cp eledit.1 "$(MANDIR)/"

.PHONY: uninstall
uninstall:
	rm -f "$(BINDIR)"/eledit
	rm -f "$(MANDIR)"/eledit.1

.PHONY: reinstall
reinstall: install

.PHONY: clean
clean:
	rm -f *.cm* *.o *~ eledit eledit.1
