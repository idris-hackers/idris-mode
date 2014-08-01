# Makefile for idris-mode, to run tests and ensure dependencies are in order
# Portions based on the Makefile for Proof General

EMACS=emacs

BATCHEMACS=$(EMACS) --batch --no-site-file -q -eval '(add-to-list (quote load-path) "${PWD}/")'

BYTECOMP = $(BATCHEMACS) -eval '(progn (require (quote bytecomp)) (setq byte-compile-warnings t) (setq byte-compile-error-on-warn t))' -f batch-byte-compile

OBJS =	idris-commands.elc		\
	idris-common-utils.elc		\
	idris-compat.elc		\
	idris-core.elc			\
	idris-events.elc		\
	idris-info.elc			\
	idris-ipkg-mode.elc		\
	idris-keys.elc			\
	idris-log.elc			\
	idris-metavariable-list.elc	\
	idris-mode.elc			\
	idris-prover.elc		\
	idris-repl.elc			\
	idris-settings.elc		\
	idris-simple-indent.elc		\
	idris-syntax.elc		\
	idris-warnings.elc		\
	idris-warnings-tree.elc		\
	inferior-idris.elc

.el.elc:
	$(BYTECOMP) $<

build: $(OBJS)

test:
	$(BATCHEMACS) -l ert -l idris-tests.el -f ert-run-tests-batch-and-exit

clean:
	-rm $(OBJS)
	-rm test-data/*ibc

.PHONY: clean build test
