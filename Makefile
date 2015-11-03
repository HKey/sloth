EMACS ?= emacs
CASK  ?= cask

.PHONY: clean-elc clean compile test-el test-elc test-compilation test coverage

clean-elc:
	-rm ./*.elc
	-rm ./test/*.elc

clean: clean-elc

compile: clean-elc
	${CASK} exec ${EMACS} -Q -batch -L ./ -L ./test -f batch-byte-compile \
	./*.el \
	./test/*.el

test-el: clean-elc
	${CASK} exec ert-runner ./test/sloth-test.el
	${CASK} exec ert-runner ./test/sloth-test-dash.el

test-elc: compile
	${CASK} exec ert-runner ./test/sloth-test.elc
	${CASK} exec ert-runner ./test/sloth-test-dash.elc

# Ert on Emacs 24.3 or older makes many unused value warnings.
# That is the reason why only Emacs 24.4 or newer versions are tested with
# byte-compile-error-on-warn = t.
test-compilation: clean-elc
	${CASK} exec ${EMACS} -Q -batch -L ./ -L ./test -eval \
	"(progn \
	   (when (version<= \"24.4\" emacs-version) \
	     (setq byte-compile-error-on-warn t)) \
	   (batch-byte-compile))" ./*.el ./test/*.el

test: test-el test-elc test-compilation

coverage: clean-elc
	${CASK} exec ert-runner ./test/sloth-test-coverage.el
