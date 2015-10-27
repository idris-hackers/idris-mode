# Makefile for idris-mode, to run tests and ensure dependencies are in order
# Portions based on the Makefile for Proof General

EMACS = emacs
EMACSFLAGS =
CASK = cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS   := $(shell $(CASK) files)
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

build: $(OBJECTS)

dist:
	$(CASK) package

test: $(OBJECTS)
	$(CASK) exec $(EMACSBATCH) -L . -l ert -l idris-tests.el -f ert-run-tests-batch-and-exit

getdeps: $(PKGDIR)

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

clean:
	-$(RM) $(OBJECTS)
	-$(RM) test-data/*ibc

.PHONY: clean build test
