EMACS ?= emacs
CASK  ?= cask

.PHONY: clean-elc clean compile test-el test-elc test-compilation test

clean-elc:
	-rm ./*.elc

clean: clean-elc

compile: clean-elc
	${CASK} exec ${EMACS} -Q -batch -L ./ -f batch-byte-compile ./*.el

test-el: clean-elc
	${CASK} exec ert-runner ./sloth-test.el

test-elc: compile
	${CASK} exec ert-runner ./sloth-test.elc

test-compilation: clean-elc
	${CASK} exec ${EMACS} -Q -batch -L ./ -eval \
	"(progn \
	   (when (version<= \"24.4\" emacs-version) \
	     (setq byte-compile-error-on-warn t)) \
	   (batch-byte-compile))" ./*.el

test: test-el test-elc test-compilation
