#!/usr/bin/env bash
# Emacs Test Runner with automatic test discovery
#
# Usage:
#   ./bin/run-tests.sh                    # Run all tests
#   ./bin/run-tests.sh -p "^test-glob-"   # Run tests matching pattern
#   ./bin/run-tests.sh -h                 # Show help

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

# Parse command line arguments
PATTERN=""
DIRECTORY=""
VERBOSE=false

show_help() {
    cat << EOF
Emacs Test Runner - Automatic test discovery and execution

USAGE:
    $(basename "$0") [OPTIONS]

OPTIONS:
    -d, --directory DIR     Run tests only in specified directory
    -p, --pattern PATTERN   Run tests matching regexp pattern
    -v, --verbose           Show verbose output
    -h, --help              Show this help message

EXAMPLES:
    # Run all tests (discovers all *-test.el files)
    $(basename "$0")

    # Run tests in specific directory (module)
    $(basename "$0") -d config/experiments/bash-parser
    $(basename "$0") -d config/gptel

    # Run tests matching pattern
    $(basename "$0") -p "^test-glob-"

    # Combine directory and pattern
    $(basename "$0") -d config/experiments/bash-parser -p "^test-glob-"

    # Verbose mode
    $(basename "$0") -v

DISCOVERY:
    Automatically discovers all *-test.el files in config/ directory.
    Use -d to limit discovery to a specific module directory.

TEST ORGANIZATION:
    Place test files alongside modules with -test.el suffix:
    - config/experiments/bash-parser/test/test-glob-matching.el
    - config/core/testing-test.el (future)
    - config/gptel/sessions-test.el (future)

EOF
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--directory)
            DIRECTORY="$2"
            shift 2
            ;;
        -p|--pattern)
            PATTERN="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

cd "$REPO_ROOT"

echo "========================================"
echo "Emacs Configuration Test Runner"
echo "========================================"
echo ""

# Build the elisp command
if [ -n "$DIRECTORY" ] && [ -n "$PATTERN" ]; then
    echo "Running tests in: $DIRECTORY"
    echo "Matching pattern: $PATTERN"
    echo ""
    TEST_COMMAND="(progn (jf/test-load-all-test-files \"$DIRECTORY\") (ert-run-tests-batch-and-exit \"$PATTERN\"))"
elif [ -n "$DIRECTORY" ]; then
    echo "Running tests in: $DIRECTORY"
    echo ""
    TEST_COMMAND="(jf/test-run-directory-batch \"$DIRECTORY\")"
elif [ -n "$PATTERN" ]; then
    echo "Running tests matching: $PATTERN"
    echo ""
    TEST_COMMAND="(jf/test-run-pattern-batch \"$PATTERN\")"
else
    echo "Running all tests (auto-discovery)"
    echo ""
    TEST_COMMAND="(jf/test-run-all-batch)"
fi

# Run tests using emacs-isolated.sh
if [ "$VERBOSE" = true ]; then
    "$REPO_ROOT/bin/emacs-isolated.sh" -batch \
        -l "$REPO_ROOT/config/core/testing.el" \
        --eval "$TEST_COMMAND" 2>&1
else
    "$REPO_ROOT/bin/emacs-isolated.sh" -batch \
        -l "$REPO_ROOT/config/core/testing.el" \
        --eval "$TEST_COMMAND"
fi

EXIT_CODE=$?

echo ""
echo "========================================"
if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ All tests passed"
else
    echo "✗ Some tests failed (exit code: $EXIT_CODE)"
fi
echo "========================================"

exit $EXIT_CODE
