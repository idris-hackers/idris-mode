
# Makefile for idris-mode, to run tests and ensure dependencies are in order
# Portions based on the Makefile for Proof General

EMACS=$(shell if [ -z "`which emacs`" ]; then echo "Emacs executable not found"; exit 1; else echo emacs; fi)

BATCHEMACS=${EMACS} --batch --no-site-file -q -eval '(add-to-list (quote load-path) "${PWD}/")'

EL=$(ls *.el)

ELC=$(EL:.el=.elc)

BYTECOMP = $(BATCHEMACS) -eval '(progn (require (quote bytecomp)) (setq byte-compile-warnings t) (setq byte-compile-error-on-warn t))' -f batch-byte-compile

default:

clean:
	-rm *.elc

%.elc : %.el
	$(BYTECOMP) $<

compile: clean idris-commands.elc idris-common-utils.elc idris-compat.elc idris-core.elc idris-events.elc idris-info.elc idris-ipkg-mode.elc idris-keys.elc idris-log.elc idris-metavariable-list.elc idris-mode.elc idris-prover.elc idris-repl.elc idris-settings.elc idris-simple-indent.elc idris-syntax.elc idris-warnings.elc idris-warnings-tree.elc inferior-idris.elc

test:
	$(BATCHEMACS) -l ert -l idris-tests.el -f ert-run-tests-batch-and-exit

### Rules for each file (done this way for BSD make compat)
idris-commands.elc: idris-commands.el
	$(BYTECOMP) idris-commands.el

idris-common-utils.elc: idris-common-utils.el
	$(BYTECOMP) idris-common-utils.el

idris-compat.elc: idris-compat.el
	$(BYTECOMP) idris-compat.el

idris-core.elc: idris-core.el
	$(BYTECOMP) idris-core.el

idris-events.elc: idris-events.el
	$(BYTECOMP) idris-events.el

idris-info.elc: idris-info.el
	$(BYTECOMP) idris-info.el

idris-ipkg-mode.elc: idris-ipkg-mode.el
	$(BYTECOMP) idris-ipkg-mode.el

idris-keys.elc: idris-keys.el
	$(BYTECOMP) idris-keys.el

idris-log.elc: idris-log.el
	$(BYTECOMP) idris-log.el

idris-metavariable-list.elc: idris-metavariable-list.el
	$(BYTECOMP) idris-metavariable-list.el

idris-mode.elc: idris-mode.el
	$(BYTECOMP) idris-mode.el

idris-prover.elc: idris-prover.el
	$(BYTECOMP) idris-prover.el

idris-repl.elc: idris-repl.el
	$(BYTECOMP) idris-repl.el

idris-settings.elc: idris-settings.el
	$(BYTECOMP) idris-settings.el

idris-simple-indent.elc: idris-simple-indent.el
	$(BYTECOMP) idris-simple-indent.el

idris-syntax.elc: idris-syntax.el
	$(BYTECOMP) idris-syntax.el

idris-tests.elc: idris-tests.el
	$(BYTECOMP) idris-tests.el

idris-warnings.elc: idris-warnings.el
	$(BYTECOMP) idris-warnings.el

idris-warnings-tree.elc: idris-warnings-tree.el
	$(BYTECOMP) idris-warnings-tree.el

inferior-idris.elc: inferior-idris.el
	$(BYTECOMP) inferior-idris.el
