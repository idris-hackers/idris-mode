# Makefile for idris-mode, to run tests and ensure dependencies are in order
# Portions based on the Makefile for Proof General

EMACS ?= emacs

NEED_PKGS=prop-menu

BATCHEMACS=$(EMACS) --batch --no-site-file -q \
	-eval '(add-to-list (quote load-path) "${PWD}/")' \
	-eval '(require (quote package))' \
	-eval '(add-to-list (quote package-archives) (quote ("melpa" . "http://melpa.org/packages/")) t)' \
	-eval '(package-initialize)'

BYTECOMP = $(BATCHEMACS) \
	-eval '(require (quote bytecomp))' \
	-eval '(setq byte-compile-warnings t)' \
	-eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile

OBJS =	idris-commands.elc		\
	idris-common-utils.elc		\
	idris-compat.elc		\
	idris-core.elc			\
	idris-events.elc		\
	idris-highlight-input.elc	\
	idris-info.elc			\
	idris-ipkg-mode.elc		\
	idris-keys.elc			\
	idris-log.elc			\
	idris-hole-list.elc		\
	idris-mode.elc			\
	idris-prover.elc		\
	idris-repl.elc			\
	idris-settings.elc		\
	idris-simple-indent.elc		\
	idris-tree-info.elc             \
	idris-syntax.elc		\
	idris-warnings.elc		\
	idris-warnings-tree.elc		\
	inferior-idris.elc

.el.elc:
	$(BYTECOMP) $<

build: getdeps $(OBJS)

test: getdeps build
	$(BATCHEMACS) -L . -l ert -l idris-tests.el -f ert-run-tests-batch-and-exit

test2: getdeps build
	$(BATCHEMACS) -L . \
		-eval '(setq idris-interpreter-path (executable-find "idris2"))' \
		-l ert -l idris-tests.el -f ert-run-tests-batch-and-exit

test3: getdeps build
	$(BATCHEMACS) -L . \
		-eval '(setq idris-interpreter-path (executable-find "idris2"))' \
		-eval '(setq idris-repl-history-file "idris2-history.eld")' \
		-eval '(setq idris-log-events t)' \
		-l ert -l idris-tests3.el -f ert-run-tests-batch-and-exit

clean:
	-${RM} -f $(OBJS)
	-${RM} -f test-data/*ibc
	-${RM} -rf test-data/build/
	-${RM} -r docs/auto docs/*.aux docs/*.log docs/*.pdf
getdeps:
	$(BATCHEMACS) -eval \
		"(let* \
		    ((need-pkgs '($(NEED_PKGS))) \
		     (want-pkgs (seq-remove #'package-installed-p need-pkgs))) \
		  (unless (null want-pkgs) \
		    (package-initialize) \
		    (package-refresh-contents) \
		    (mapcar #'package-install want-pkgs)))"

docs: docs/documentation.tex
	-@( cd docs/ && latexmk -xelatex documentation.tex )

.PHONY: clean build test getdeps docs
