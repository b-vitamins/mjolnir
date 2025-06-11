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

.PHONY: all clean compile compile-strict test test-coverage test-stress \
        test-memory test-performance test-minimal lint-all check-doc \
        verify-package test-integration-vanilla test-integration-doom \
        test-integration-spacemacs test-integration-prelude

all: compile test

clean:
	rm -f $(ELC_FILES)
	rm -f test/*.elc
	rm -f coverage.json
	rm -rf test-results/

# Standard compilation
compile: clean
	$(BATCH) -f batch-byte-compile $(ALL_SOURCES)

# Strict compilation - warnings are errors
compile-strict: clean
	$(BATCH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq byte-compile-warnings '(all))" \
	  -f batch-byte-compile $(ALL_SOURCES)

# Run standard tests
test: compile
	$(BATCH) \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -f mjolnir-run-all-tests

# Run tests with coverage
test-coverage: compile
	$(BATCH) \
	  --eval "(require 'coverage)" \
	  --eval "(coverage-start)" \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -f mjolnir-run-all-tests \
	  --eval "(coverage-stop)" \
	  --eval "(coverage-save \"coverage.json\")"

# Run stress tests
test-stress: compile
	$(BATCH) \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -l test/mjolnir-stress-test.el \
	  -f mjolnir-run-stress-tests

# Check for memory leaks
test-memory: compile
	$(BATCH) \
	  --eval "(setq gc-cons-threshold 1000)" \
	  -l test/mjolnir-memory-test.el \
	  -f mjolnir-check-memory-leaks

# Performance tests
test-performance: compile
	$(BATCH) \
	  -l test/mjolnir-performance-test.el \
	  -f mjolnir-run-performance-tests

# Test without optional dependencies
test-minimal: clean
	$(BATCH) \
	  --eval "(setq mjolnir-minimal-test t)" \
	  -l ert \
	  -l test/mjolnir-test.el \
	  -f mjolnir-run-minimal-tests

# Run all linters
lint-all: lint-package lint-elisp lint-regexp lint-checkdoc

lint-package:
	$(BATCH) \
	  -l package-lint \
	  -f package-lint-batch-and-exit $(ALL_SOURCES)

lint-elisp:
	$(BATCH) \
	  -l elisp-lint \
	  --eval "(elisp-lint-files-batch \"$(ALL_SOURCES)\")"

lint-regexp:
	$(BATCH) \
	  -l relint \
	  -f relint-batch $(ALL_SOURCES)

lint-checkdoc:
	$(BATCH) \
	  --eval "(checkdoc-file \"mjolnir.el\")" \
	  --eval "(dolist (f '($(CORE_SOURCES) $(FEATURE_SOURCES) $(UI_SOURCES))) \
	            (checkdoc-file f))"

# Check documentation
check-doc:
	$(BATCH) \
	  --eval "(require 'checkdoc)" \
	  --eval "(setq checkdoc-force-docstrings-flag t)" \
	  --eval "(setq checkdoc-arguments-in-order-flag t)" \
	  --eval "(setq checkdoc-verb-check-experimental-flag t)" \
	  -f checkdoc-batch $(ALL_SOURCES)

# Verify package format
verify-package:
	$(BATCH) \
	  --eval "(require 'package)" \
	  --eval "(package-buffer-info)" \
	  --eval "(package-lint-buffer)" \
	  -f kill-emacs

# Integration tests
test-integration-vanilla:
	$(BATCH) \
	  -l test/integration/vanilla-config.el \
	  -l test/mjolnir-integration-test.el \
	  -f mjolnir-test-vanilla-integration

test-integration-doom:
	@echo "Testing with Doom Emacs configuration..."
	$(BATCH) \
	  -l test/integration/doom-config.el \
	  -l test/mjolnir-integration-test.el \
	  -f mjolnir-test-doom-integration

test-integration-spacemacs:
	@echo "Testing with Spacemacs configuration..."
	$(BATCH) \
	  -l test/integration/spacemacs-config.el \
	  -l test/mjolnir-integration-test.el \
	  -f mjolnir-test-spacemacs-integration

test-integration-prelude:
	@echo "Testing with Prelude configuration..."
	$(BATCH) \
	  -l test/integration/prelude-config.el \
	  -l test/mjolnir-integration-test.el \
	  -f mjolnir-test-prelude-integration
