# =============================================================================
# Environment Configuration - Single source of truth for Emacs invocation
# =============================================================================

SHELL := /bin/bash
CONFIG_ROOT := $(shell pwd)
RUNTIME_DIR := $(CONFIG_ROOT)/runtime
BIN_DIR := $(CONFIG_ROOT)/bin

# Detect Emacs binary (macOS app bundle or system installation)
EMACS_APP := /Applications/Emacs.app/Contents/MacOS/Emacs
EMACS := $(shell if [ -f "$(EMACS_APP)" ]; then echo "$(EMACS_APP)"; elif command -v emacs >/dev/null 2>&1; then command -v emacs; else echo ""; fi)

# Verify Emacs is available
ifeq ($(EMACS),)
$(error Emacs not found. Install Emacs or add it to PATH)
endif

# Isolated Emacs configuration paths
EARLY_INIT := $(CONFIG_ROOT)/early-init.el
INIT := $(CONFIG_ROOT)/init.el
TESTING_MODULE := $(CONFIG_ROOT)/config/core/testing.el

# Emacs batch mode with testing framework loaded
# This is the core invocation pattern - single source of truth
EMACS_TEST_BATCH := EMACS_USER_DIRECTORY="$(RUNTIME_DIR)" $(EMACS) -q \
	--load "$(EARLY_INIT)" \
	--load "$(INIT)" \
	-batch \
	-l "$(TESTING_MODULE)"

# =============================================================================
# Low-level targets - Core building blocks for scripts
# =============================================================================

.PHONY: emacs-isolated
# Run Emacs in isolated mode (GUI or custom args)
# Usage: make emacs-isolated EMACS_ARGS="--batch --eval '(message \"hello\")'"
emacs-isolated:
	@EMACS_USER_DIRECTORY="$(RUNTIME_DIR)" $(EMACS) -q \
		--load "$(EARLY_INIT)" \
		--load "$(INIT)" \
		$(EMACS_ARGS)

.PHONY: emacs-test-eval
# Run ERT tests with custom elisp evaluation
# Usage: make emacs-test-eval EVAL_CMD="(jf/test-run-all-batch)"
emacs-test-eval:
	@$(EMACS_TEST_BATCH) --eval '$(EVAL_CMD)'

# =============================================================================
# High-level test targets - User-facing convenience wrappers
# =============================================================================

.PHONY: test test-verbose test-pattern test-directory test-snapshot test-buttercup test-buttercup-directory help

# Default target
help:
	@echo "Emacs Configuration - Make Targets"
	@echo ""
	@echo "Environment:"
	@echo "  EMACS:       $(EMACS)"
	@echo "  CONFIG_ROOT: $(CONFIG_ROOT)"
	@echo "  RUNTIME_DIR: $(RUNTIME_DIR)"
	@echo ""
	@echo "Low-level targets:"
	@echo "  make emacs-isolated EMACS_ARGS='...'  Run isolated Emacs with custom args"
	@echo "  make emacs-test-eval EVAL_CMD='...'   Run tests with elisp eval"
	@echo ""
	@echo "Testing (both frameworks via run-tests.sh):"
	@echo "  make test                       Run all tests (both frameworks)"
	@echo "  make test-verbose               Run all tests with verbose output"
	@echo "  make test-pattern PATTERN=X     Run ERT tests matching pattern"
	@echo "  make test-directory DIR=X       Run tests in specific directory"
	@echo "  make test-snapshot DIR=X        Run tests and capture snapshot"
	@echo ""
	@echo "Testing (Buttercup - direct):"
	@echo "  make test-buttercup             Run all Buttercup tests"
	@echo "  make test-buttercup-directory DIR=X  Run Buttercup tests in specific directory"
	@echo ""
	@echo "Examples:"
	@echo "  make test-pattern PATTERN='^test-glob-'"
	@echo "  make test-directory DIR=config/bash-parser"
	@echo "  make test-snapshot DIR=config/bash-parser"
	@echo ""

# Run all tests (both frameworks)
test:
	@./bin/run-tests.sh

# Run tests with verbose output
test-verbose:
	@./bin/run-tests.sh -v

# Run tests matching pattern
test-pattern:
	@./bin/run-tests.sh -p "$(PATTERN)"

# Run tests in specific directory
test-directory:
	@./bin/run-tests.sh -d "$(DIR)"

# Run tests and capture snapshot
test-snapshot:
	@./bin/run-tests.sh -d "$(DIR)" --snapshot

# Run all Buttercup tests
test-buttercup:
	@$(EMACS_TEST_BATCH) --eval '(jf/test-run-all-buttercup-batch)'

# Run Buttercup tests in specific directory
test-buttercup-directory:
	@$(EMACS_TEST_BATCH) --eval '(jf/test-run-buttercup-directory-batch "$(DIR)")'
