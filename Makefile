EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L . -L core -L features -L ui

# Source files
EL_SOURCES = mjolnir.el
CORE_SOURCES = $(wildcard core/*.el)
FEATURE_SOURCES = $(wildcard features/*.el)
UI_SOURCES = $(wildcard ui/*.el)
ALL_SOURCES = $(EL_SOURCES) $(CORE_SOURCES) $(FEATURE_SOURCES) $(UI_SOURCES)

# Byte-compiled files
ELC_FILES = $(ALL_SOURCES:.el=.elc)

.PHONY: all clean compile test lint-package

all: compile test

clean:
	rm -f $(ELC_FILES)

compile: clean
	$(BATCH) -f batch-byte-compile $(ALL_SOURCES)

test: compile
	$(BATCH) \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -f mjolnir-run-all-tests

lint-package:
	$(BATCH) \
	  -l package-lint \
	  -f package-lint-batch-and-exit mjolnir.el
