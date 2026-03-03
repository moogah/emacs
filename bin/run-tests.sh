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
SNAPSHOT=false
SNAPSHOT_FILE=""

show_help() {
    cat << EOF
Emacs Test Runner - Automatic test discovery and execution

USAGE:
    $(basename "$0") [OPTIONS]

OPTIONS:
    -d, --directory DIR     Run tests only in specified directory
    -p, --pattern PATTERN   Run tests matching regexp pattern
    -s, --snapshot [FILE]   Capture output to snapshot file (for git tracking)
                           Default: test-results.txt in test directory
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

    # Capture output to snapshot file
    $(basename "$0") -d config/experiments/bash-parser --snapshot
    $(basename "$0") -s custom-results.txt

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
        -s|--snapshot)
            SNAPSHOT=true
            # Check if next arg is a filename (not a flag)
            if [[ -n "$2" && ! "$2" =~ ^- ]]; then
                SNAPSHOT_FILE="$2"
                shift 2
            else
                shift
            fi
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

# Determine snapshot file location
if [ "$SNAPSHOT" = true ]; then
    if [ -z "$SNAPSHOT_FILE" ]; then
        # Default location based on directory or config root
        if [ -n "$DIRECTORY" ]; then
            SNAPSHOT_FILE="$REPO_ROOT/$DIRECTORY/test-results.txt"
        else
            SNAPSHOT_FILE="$REPO_ROOT/test-results.txt"
        fi
    elif [[ ! "$SNAPSHOT_FILE" = /* ]]; then
        # Make relative paths absolute
        SNAPSHOT_FILE="$REPO_ROOT/$SNAPSHOT_FILE"
    fi

    # Ensure directory exists
    mkdir -p "$(dirname "$SNAPSHOT_FILE")"

    echo "Capturing output to: $(realpath --relative-to="$REPO_ROOT" "$SNAPSHOT_FILE" 2>/dev/null || basename "$SNAPSHOT_FILE")"
    echo ""
fi

# Run tests using emacs-isolated.sh
if [ "$SNAPSHOT" = true ]; then
    # Snapshot mode: capture output with tee
    "$REPO_ROOT/bin/emacs-isolated.sh" -batch \
        -l "$REPO_ROOT/config/core/testing.el" \
        --eval "$TEST_COMMAND" \
        2>&1 | tee "$SNAPSHOT_FILE"
    EXIT_CODE=${PIPESTATUS[0]}
elif [ "$VERBOSE" = true ]; then
    "$REPO_ROOT/bin/emacs-isolated.sh" -batch \
        -l "$REPO_ROOT/config/core/testing.el" \
        --eval "$TEST_COMMAND" 2>&1
    EXIT_CODE=$?
else
    "$REPO_ROOT/bin/emacs-isolated.sh" -batch \
        -l "$REPO_ROOT/config/core/testing.el" \
        --eval "$TEST_COMMAND"
    EXIT_CODE=$?
fi

echo ""
echo "========================================"

# Extract summary if in snapshot mode
if [ "$SNAPSHOT" = true ] && [ -f "$SNAPSHOT_FILE" ]; then
    echo "Test Results Summary"
    echo "========================================"
    echo ""

    # Extract summary statistics
    TOTAL_TESTS=$(grep -E "^Ran [0-9]+ tests" "$SNAPSHOT_FILE" | head -1 | awk '{print $2}')
    EXPECTED=$(grep -E "^Ran [0-9]+ tests" "$SNAPSHOT_FILE" | head -1 | awk '{print $4}')
    UNEXPECTED=$(grep -E "^Ran [0-9]+ tests" "$SNAPSHOT_FILE" | head -1 | awk '{print $8}')

    if [ -n "$TOTAL_TESTS" ]; then
        echo "Total tests run: $TOTAL_TESTS"
        echo "Expected results: $EXPECTED"
        echo "Unexpected results: $UNEXPECTED"
        echo ""
    fi

    if [ $EXIT_CODE -eq 0 ]; then
        echo "✓ All tests passed"
    else
        echo "✗ Some tests failed"
        if [ -n "$UNEXPECTED" ] && [ "$UNEXPECTED" != "0" ]; then
            echo ""
            echo "Failed tests:"
            grep "^   FAILED" "$SNAPSHOT_FILE" | head -10 || true
        fi
    fi

    echo ""
    echo "Results saved to: $(realpath --relative-to="$REPO_ROOT" "$SNAPSHOT_FILE" 2>/dev/null || basename "$SNAPSHOT_FILE")"
    echo "Use 'git diff $(realpath --relative-to="$REPO_ROOT" "$SNAPSHOT_FILE" 2>/dev/null || basename "$SNAPSHOT_FILE")' to see changes"
else
    if [ $EXIT_CODE -eq 0 ]; then
        echo "✓ All tests passed"
    else
        echo "✗ Some tests failed (exit code: $EXIT_CODE)"
    fi
fi

echo "========================================"

exit $EXIT_CODE
