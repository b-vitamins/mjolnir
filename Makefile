EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L lisp

# Source files
ALL_SOURCES = $(wildcard lisp/*.el)

# Byte-compiled files
ELC_FILES = $(ALL_SOURCES:.el=.elc)

.PHONY: all clean compile check lint-package

all: compile check

clean:
	rm -f $(ELC_FILES)

compile: clean
	$(BATCH) -f batch-byte-compile $(ALL_SOURCES)

check: compile
	$(BATCH) \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -f mjolnir-run-all-tests

lint-package:
	$(BATCH) \
	  --eval "(require 'package)" \
	  --eval "(package-initialize)" \
	  -l package-lint \
	  -f package-lint-batch-and-exit lisp/mjolnir.el
