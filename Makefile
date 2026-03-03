.PHONY: test test-verbose test-pattern test-directory test-bash-parser test-gptel test-snapshot help

# Default target
help:
	@echo "Emacs Configuration - Make Targets"
	@echo ""
	@echo "Testing:"
	@echo "  make test                       Run all tests (auto-discovery)"
	@echo "  make test-verbose               Run all tests with verbose output"
	@echo "  make test-pattern PATTERN=X     Run tests matching pattern"
	@echo "  make test-directory DIR=X       Run tests in specific directory"
	@echo "  make test-snapshot DIR=X        Run tests and capture snapshot"
	@echo ""
	@echo "Module shortcuts:"
	@echo "  make test-bash-parser           Run bash-parser tests"
	@echo "  make test-bash-parser-snapshot  Run and capture bash-parser snapshot"
	@echo "  make test-gptel                 Run gptel tests (when available)"
	@echo ""
	@echo "Examples:"
	@echo "  make test-pattern PATTERN='^test-glob-'"
	@echo "  make test-directory DIR=config/experiments/bash-parser"
	@echo "  make test-snapshot DIR=config/experiments/bash-parser"
	@echo "  make test-bash-parser-snapshot"
	@echo ""

# Run all tests
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

# Shortcuts for common modules
test-bash-parser:
	@./bin/run-tests.sh -d config/experiments/bash-parser

test-bash-parser-snapshot:
	@./bin/run-tests.sh -d config/experiments/bash-parser --snapshot

test-gptel:
	@./bin/run-tests.sh -d config/gptel
