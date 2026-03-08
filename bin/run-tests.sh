#!/usr/bin/env bash
# Emacs Test Runner with automatic test discovery
#
# This script provides a user-friendly CLI for running both ERT and Buttercup tests.
# It delegates Emacs invocation to Makefile targets (single source of truth).
#
# Usage:
#   ./bin/run-tests.sh                    # Run all tests (both frameworks)
#   ./bin/run-tests.sh -f buttercup       # Run only Buttercup tests
#   ./bin/run-tests.sh -p "^test-glob-"   # Run ERT tests matching pattern
#   ./bin/run-tests.sh -h                 # Show help

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

# Parse command line arguments
PATTERN=""
DIRECTORY=""
FRAMEWORK="auto"  # auto, ert, buttercup, or both
VERBOSE=false
SNAPSHOT=false
SNAPSHOT_FILE=""
TEST_TYPE=""  # unit, integration, behavioral
CAPABILITY=""  # schema, cloud-auth, pipelines, file-paths

show_help() {
    cat << EOF
Emacs Test Runner - Automatic test discovery and execution

USAGE:
    $(basename "$0") [OPTIONS]

OPTIONS:
    -f, --framework FRAMEWORK
                           Test framework: ert, buttercup, both, or auto (default)
                           auto: auto-detects based on file extension or runs both
    -d, --directory DIR    Run tests only in specified directory
    -p, --pattern PATTERN  Run ERT tests matching regexp pattern (ERT only)
    -t, --type TYPE        Filter by test type: unit, integration, behavioral
    -c, --capability CAP   Filter by capability: schema, cloud-auth, pipelines, file-paths
    -s, --snapshot [FILE]  Capture output to snapshot file (for git tracking)
                           Default: test-results.txt in test directory
    -v, --verbose          Show verbose output
    -h, --help             Show this help message

FRAMEWORKS:
    ERT (legacy):          Built-in Emacs testing with *-test.el files
    Buttercup (preferred): BDD testing with *-spec.el files

EXAMPLES:
    # Run all tests (auto-detects framework or runs both)
    $(basename "$0")

    # Run only Buttercup tests
    $(basename "$0") -f buttercup

    # Run only ERT tests
    $(basename "$0") -f ert

    # Run both frameworks explicitly
    $(basename "$0") -f both

    # Run tests in specific directory (auto-detects framework)
    $(basename "$0") -d config/experiments/bash-parser
    $(basename "$0") -d config/gptel

    # Run Buttercup tests in specific directory
    $(basename "$0") -f buttercup -d config/gptel

    # Run ERT tests matching pattern
    $(basename "$0") -f ert -p "^test-glob-"

    # Filter by test type
    $(basename "$0") -t unit -d config/gptel/tools/test
    $(basename "$0") -f buttercup -t behavioral

    # Filter by capability
    $(basename "$0") -c schema -d config/gptel/tools/test/integration
    $(basename "$0") -c cloud-auth

    # Capture output to snapshot file
    $(basename "$0") -d config/experiments/bash-parser --snapshot
    $(basename "$0") -s custom-results.txt

    # Verbose mode
    $(basename "$0") -v

DISCOVERY:
    Automatically discovers test files in config/ directory:
    - ERT: *-test.el files
    - Buttercup: *-spec.el files
    Use -d to limit discovery to a specific module directory.

TEST ORGANIZATION:
    Buttercup (preferred): config/MODULE/test/*-spec.el
    ERT (legacy):          config/MODULE/*-test.el or config/MODULE/test/test-*.el

EOF
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--framework)
            FRAMEWORK="$2"
            if [[ ! "$FRAMEWORK" =~ ^(ert|buttercup|both|auto)$ ]]; then
                echo "Error: Invalid framework '$FRAMEWORK'. Must be: ert, buttercup, both, or auto"
                exit 1
            fi
            shift 2
            ;;
        -d|--directory)
            DIRECTORY="$2"
            shift 2
            ;;
        -p|--pattern)
            PATTERN="$2"
            shift 2
            ;;
        -t|--type)
            TEST_TYPE="$2"
            if [[ ! "$TEST_TYPE" =~ ^(unit|integration|behavioral)$ ]]; then
                echo "Error: Invalid test type '$TEST_TYPE'. Must be: unit, integration, or behavioral"
                exit 1
            fi
            shift 2
            ;;
        -c|--capability)
            CAPABILITY="$2"
            if [[ ! "$CAPABILITY" =~ ^(schema|cloud-auth|pipelines|file-paths)$ ]]; then
                echo "Error: Invalid capability '$CAPABILITY'. Must be: schema, cloud-auth, pipelines, or file-paths"
                exit 1
            fi
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

# Apply test type filtering
if [ -n "$TEST_TYPE" ]; then
    if [ -n "$DIRECTORY" ]; then
        # Append type subdirectory to existing directory
        DIRECTORY="$DIRECTORY/$TEST_TYPE"
    else
        echo "Error: --type requires --directory to be specified"
        exit 1
    fi
fi

# Apply capability filtering
if [ -n "$CAPABILITY" ]; then
    if [ -n "$DIRECTORY" ]; then
        # Look for capability file in directory
        CAPABILITY_FILE=""
        case "$CAPABILITY" in
            schema)
                CAPABILITY_FILE="schema.el"
                ;;
            cloud-auth)
                CAPABILITY_FILE="cloud-auth.el"
                ;;
            pipelines)
                CAPABILITY_FILE="pipelines.el"
                ;;
            file-paths)
                CAPABILITY_FILE="file-paths.el"
                ;;
        esac

        # Check if capability file exists
        if [ -f "$REPO_ROOT/$DIRECTORY/$CAPABILITY_FILE" ]; then
            # Update directory to point to specific file
            DIRECTORY="$DIRECTORY/$CAPABILITY_FILE"
        else
            echo "Warning: Capability file $CAPABILITY_FILE not found in $DIRECTORY"
            echo "Searching for pattern in directory instead..."
        fi
    else
        echo "Error: --capability requires --directory to be specified"
        exit 1
    fi
fi

# Auto-detect framework if needed
if [ "$FRAMEWORK" = "auto" ]; then
    if [ -n "$DIRECTORY" ]; then
        # Check if directory has Buttercup or ERT tests
        HAS_BUTTERCUP=$(find "$REPO_ROOT/$DIRECTORY" -name "*-spec.el" 2>/dev/null | wc -l)
        HAS_ERT=$(find "$REPO_ROOT/$DIRECTORY" -name "*-test.el" -o -name "test-*.el" 2>/dev/null | wc -l)

        if [ "$HAS_BUTTERCUP" -gt 0 ] && [ "$HAS_ERT" -gt 0 ]; then
            FRAMEWORK="both"
        elif [ "$HAS_BUTTERCUP" -gt 0 ]; then
            FRAMEWORK="buttercup"
        elif [ "$HAS_ERT" -gt 0 ]; then
            FRAMEWORK="ert"
        else
            FRAMEWORK="both"  # Default to both if no tests found
        fi
    else
        FRAMEWORK="both"  # Run both when discovering all tests
    fi
fi

# Pattern only works with ERT
if [ -n "$PATTERN" ] && [ "$FRAMEWORK" != "ert" ]; then
    echo "Warning: Pattern matching only works with ERT framework"
    echo "Setting framework to 'ert'"
    FRAMEWORK="ert"
fi

echo "Framework: $FRAMEWORK"
if [ -n "$TEST_TYPE" ]; then
    echo "Test type: $TEST_TYPE"
fi
if [ -n "$CAPABILITY" ]; then
    echo "Capability: $CAPABILITY"
fi
echo ""

# Build the elisp command based on framework
run_tests() {
    local framework=$1
    local test_command=""

    case "$framework" in
        ert)
            if [ -n "$DIRECTORY" ] && [ -n "$PATTERN" ]; then
                echo "Running ERT tests in: $DIRECTORY" >&2
                echo "Matching pattern: $PATTERN" >&2
                echo "" >&2
                test_command="(progn (jf/test-load-all-test-files \"$DIRECTORY\") (ert-run-tests-batch-and-exit \"$PATTERN\"))"
            elif [ -n "$DIRECTORY" ]; then
                echo "Running ERT tests in: $DIRECTORY" >&2
                echo "" >&2
                test_command="(jf/test-run-directory-batch \"$DIRECTORY\")"
            elif [ -n "$PATTERN" ]; then
                echo "Running ERT tests matching: $PATTERN" >&2
                echo "" >&2
                test_command="(jf/test-run-pattern-batch \"$PATTERN\")"
            else
                echo "Running all ERT tests (auto-discovery)" >&2
                echo "" >&2
                test_command="(jf/test-run-all-batch)"
            fi
            ;;
        buttercup)
            if [ -n "$DIRECTORY" ]; then
                echo "Running Buttercup tests in: $DIRECTORY" >&2
                echo "" >&2
                test_command="(jf/test-run-buttercup-directory-batch \"$DIRECTORY\")"
            else
                echo "Running all Buttercup tests (auto-discovery)" >&2
                echo "" >&2
                test_command="(jf/test-run-all-buttercup-batch)"
            fi
            ;;
    esac

    echo "$test_command"
}

if [ "$FRAMEWORK" = "both" ]; then
    # Run both frameworks sequentially
    ERT_COMMAND=$(run_tests "ert")
    BUTTERCUP_COMMAND=$(run_tests "buttercup")
    TEST_COMMAND="(progn $ERT_COMMAND $BUTTERCUP_COMMAND)"
else
    TEST_COMMAND=$(run_tests "$FRAMEWORK")
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

# Run tests using make (eliminates dependency on emacs-isolated.sh)
# Makefile provides emacs-test-eval target with proper environment setup
if [ "$SNAPSHOT" = true ]; then
    # Snapshot mode: capture output with tee
    make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$TEST_COMMAND" 2>&1 | tee "$SNAPSHOT_FILE"
    EXIT_CODE=${PIPESTATUS[0]}
elif [ "$VERBOSE" = true ]; then
    make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$TEST_COMMAND" 2>&1
    EXIT_CODE=$?
else
    make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$TEST_COMMAND"
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
