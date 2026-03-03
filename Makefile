.PHONY: test test-verbose test-pattern help

# Default target
help:
	@echo "Emacs Configuration - Make Targets"
	@echo ""
	@echo "Testing:"
	@echo "  make test                    Run all tests (auto-discovery)"
	@echo "  make test-verbose            Run all tests with verbose output"
	@echo "  make test-pattern PATTERN=X  Run tests matching pattern"
	@echo ""
	@echo "Examples:"
	@echo "  make test-pattern PATTERN='^test-glob-'"
	@echo "  make test-pattern PATTERN='^test-extraction-'"
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
