EMACS ?= emacs
LISP := $(wildcard lisp/*.el)
TESTS := $(wildcard test/*-test.el)

.PHONY: test compile clean check-docs

test:
	$(EMACS) -Q --batch -L lisp -L test \
	  $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L lisp \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(LISP)

clean:
	rm -f lisp/*.elc test/*.elc

check-docs:
	@! grep -rn "$(HOME)" --include='*.md' . || (echo "home-directory path in docs" && exit 1)
