.PHONY: test test-verbose test-pattern test-directory test-bash-parser test-gptel help

# Default target
help:
	@echo "Emacs Configuration - Make Targets"
	@echo ""
	@echo "Testing:"
	@echo "  make test                       Run all tests (auto-discovery)"
	@echo "  make test-verbose               Run all tests with verbose output"
	@echo "  make test-pattern PATTERN=X     Run tests matching pattern"
	@echo "  make test-directory DIR=X       Run tests in specific directory"
	@echo ""
	@echo "Module shortcuts:"
	@echo "  make test-bash-parser           Run bash-parser tests"
	@echo "  make test-gptel                 Run gptel tests (when available)"
	@echo ""
	@echo "Examples:"
	@echo "  make test-pattern PATTERN='^test-glob-'"
	@echo "  make test-directory DIR=config/experiments/bash-parser"
	@echo "  make test-directory DIR=config/gptel"
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

# Shortcuts for common modules
test-bash-parser:
	@./bin/run-tests.sh -d config/experiments/bash-parser

test-gptel:
	@./bin/run-tests.sh -d config/gptel
