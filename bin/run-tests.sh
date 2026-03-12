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
REPORT=false
REPORT_FILE=""

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
    -s, --snapshot [FILE]  Capture output to snapshot file (for git tracking)
                           Default: test-results.txt in test directory
    -r, --report [FILE]   Generate concise test report (counts + failures only)
                           Default: test-report.txt in test directory
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
    $(basename "$0") -d config/bash-parser
    $(basename "$0") -d config/gptel

    # Run Buttercup tests in specific directory
    $(basename "$0") -f buttercup -d config/gptel

    # Run ERT tests matching pattern
    $(basename "$0") -f ert -p "^test-glob-"

    # Capture output to snapshot file
    $(basename "$0") -d config/bash-parser --snapshot
    $(basename "$0") -s custom-results.txt

    # Generate concise test report (counts + failures)
    $(basename "$0") --report
    $(basename "$0") -d config/gptel --report

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
        -r|--report)
            REPORT=true
            # Check if next arg is a filename (not a flag)
            if [[ -n "$2" && ! "$2" =~ ^- ]]; then
                REPORT_FILE="$2"
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
echo ""

# Build the elisp command for a single framework
build_test_command() {
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

# Temp file for capturing output when report mode is active
if [ "$REPORT" = true ]; then
    REPORT_RAW_OUTPUT=$(mktemp "${TMPDIR:-/tmp}/test-output.XXXXXX")
    trap 'rm -f "$REPORT_RAW_OUTPUT"' EXIT
fi

# Run a single framework in its own Emacs process
# Returns exit code via the EXIT_CODE_VAR named in $2
run_single_framework() {
    local framework=$1
    local test_command
    test_command=$(build_test_command "$framework")

    if [ "$SNAPSHOT" = true ] && [ "$REPORT" = true ]; then
        make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$test_command" 2>&1 | tee -a "$SNAPSHOT_FILE" | tee -a "$REPORT_RAW_OUTPUT"
        return ${PIPESTATUS[0]}
    elif [ "$SNAPSHOT" = true ]; then
        make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$test_command" 2>&1 | tee -a "$SNAPSHOT_FILE"
        return ${PIPESTATUS[0]}
    elif [ "$REPORT" = true ]; then
        make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$test_command" 2>&1 | tee -a "$REPORT_RAW_OUTPUT"
        return ${PIPESTATUS[0]}
    elif [ "$VERBOSE" = true ]; then
        make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$test_command" 2>&1
        return $?
    else
        make -C "$REPO_ROOT" emacs-test-eval EVAL_CMD="$test_command"
        return $?
    fi
}

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

    # Clear snapshot file before appending
    > "$SNAPSHOT_FILE"
fi

# Determine report file location
if [ "$REPORT" = true ]; then
    if [ -z "$REPORT_FILE" ]; then
        if [ -n "$DIRECTORY" ]; then
            REPORT_FILE="$REPO_ROOT/$DIRECTORY/test-report.txt"
        else
            REPORT_FILE="$REPO_ROOT/test-report.txt"
        fi
    elif [[ ! "$REPORT_FILE" = /* ]]; then
        REPORT_FILE="$REPO_ROOT/$REPORT_FILE"
    fi
    mkdir -p "$(dirname "$REPORT_FILE")"
fi

# Run tests - separate Emacs processes for "both" mode
EXIT_CODE=0

if [ "$FRAMEWORK" = "both" ]; then
    # Run ERT and Buttercup as separate Emacs processes
    # This fixes the critical bug where ert-run-tests-batch-and-exit
    # calls kill-emacs before Buttercup can run
    ERT_EXIT=0
    BUTTERCUP_EXIT=0

    set +e
    run_single_framework "ert"
    ERT_EXIT=$?

    echo ""
    echo "----------------------------------------"
    echo ""

    run_single_framework "buttercup"
    BUTTERCUP_EXIT=$?
    set -e

    # Fail if either framework failed
    if [ $ERT_EXIT -ne 0 ] || [ $BUTTERCUP_EXIT -ne 0 ]; then
        EXIT_CODE=1
    fi
else
    set +e
    run_single_framework "$FRAMEWORK"
    EXIT_CODE=$?
    set -e
fi

echo ""
echo "========================================"

# Extract summary if in snapshot mode
if [ "$SNAPSHOT" = true ] && [ -f "$SNAPSHOT_FILE" ]; then
    echo "Test Results Summary"
    echo "========================================"
    echo ""

    # Extract ERT summary statistics
    ERT_SUMMARY=$(grep -E "^Ran [0-9]+ tests" "$SNAPSHOT_FILE" | head -1)
    if [ -n "$ERT_SUMMARY" ]; then
        TOTAL_TESTS=$(echo "$ERT_SUMMARY" | awk '{print $2}')
        EXPECTED=$(echo "$ERT_SUMMARY" | awk '{print $4}')
        UNEXPECTED=$(echo "$ERT_SUMMARY" | awk '{print $8}')
        echo "ERT: $TOTAL_TESTS tests, $EXPECTED expected, $UNEXPECTED unexpected"
    fi

    # Extract Buttercup summary statistics
    BUTTERCUP_SUMMARY=$(grep -E "^Ran [0-9]+ specs," "$SNAPSHOT_FILE" | head -1)
    if [ -n "$BUTTERCUP_SUMMARY" ]; then
        SPEC_COUNT=$(echo "$BUTTERCUP_SUMMARY" | awk '{print $2}')
        FAILED_COUNT=$(echo "$BUTTERCUP_SUMMARY" | awk '{print $4}')
        echo "Buttercup: $SPEC_COUNT specs, $FAILED_COUNT failed"
    fi

    echo ""

    if [ $EXIT_CODE -eq 0 ]; then
        echo "✓ All tests passed"
    else
        echo "✗ Some tests failed"
        # Show ERT failures
        if [ -n "$UNEXPECTED" ] && [ "$UNEXPECTED" != "0" ]; then
            echo ""
            echo "Failed ERT tests:"
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

# Generate concise test report
if [ "$REPORT" = true ] && [ -f "$REPORT_RAW_OUTPUT" ]; then
    # Strip ANSI color codes for reliable parsing
    CLEAN_OUTPUT=$(sed 's/\x1b\[[0-9;]*m//g' "$REPORT_RAW_OUTPUT")

    {
        echo "Test Report"
        echo "==========="
        echo ""

        # --- ERT ---
        ERT_SUMMARY=$(echo "$CLEAN_OUTPUT" | grep -E "^Ran [0-9]+ tests," | head -1)
        if [ -n "$ERT_SUMMARY" ]; then
            ERT_TOTAL=$(echo "$ERT_SUMMARY" | awk '{print $2}')
            ERT_EXPECTED=$(echo "$ERT_SUMMARY" | awk '{print $4}')
            ERT_UNEXPECTED=$(echo "$ERT_SUMMARY" | awk '{print $8}')
            ERT_PASSED=$ERT_EXPECTED

            echo "ERT: $ERT_TOTAL ran, $ERT_PASSED passed"

            if [ "$ERT_UNEXPECTED" != "0" ]; then
                # Extract failed test names from ERT output
                # ERT format: "   FAILED  test-name" or "   failed  NNN/NNN  test-name"
                echo ""
                echo "ERT failures:"
                echo "$CLEAN_OUTPUT" | grep -E "^[[:space:]]+(FAILED|failed)[[:space:]]+" | \
                    sed -E 's/^[[:space:]]+(FAILED|failed)[[:space:]]+([0-9]+\/[0-9]+[[:space:]]+)?/  /' | \
                    sed -E 's/[[:space:]]+\([0-9.]+ sec\)$//' | \
                    sort -u
            fi
        else
            echo "ERT: not run"
        fi

        echo ""

        # --- Buttercup ---
        BUTTERCUP_SUMMARY=$(echo "$CLEAN_OUTPUT" | grep -E "^Ran [0-9]+ specs," | head -1)
        if [ -n "$BUTTERCUP_SUMMARY" ]; then
            BC_TOTAL=$(echo "$BUTTERCUP_SUMMARY" | awk '{print $2}')
            BC_FAILED=$(echo "$BUTTERCUP_SUMMARY" | awk '{print $4}')
            BC_PASSED=$((BC_TOTAL - BC_FAILED))

            echo "Buttercup: $BC_TOTAL ran, $BC_PASSED passed"

            if [ "$BC_FAILED" != "0" ]; then
                # Extract failed spec names from Buttercup output
                # Buttercup format: spec name on line before "FAILED:"
                echo ""
                echo "Buttercup failures:"
                echo "$CLEAN_OUTPUT" | grep -B1 "^FAILED:" | grep -v "^FAILED:" | grep -v "^--$" | \
                    sed 's/^/  /' | \
                    sort -u
            fi
        else
            echo "Buttercup: not run"
        fi

        echo ""
        echo "==========="

        # Overall summary
        TOTAL_RAN=0
        TOTAL_PASSED=0
        [ -n "$ERT_TOTAL" ] && TOTAL_RAN=$((TOTAL_RAN + ERT_TOTAL)) && TOTAL_PASSED=$((TOTAL_PASSED + ERT_PASSED))
        [ -n "$BC_TOTAL" ] && TOTAL_RAN=$((TOTAL_RAN + BC_TOTAL)) && TOTAL_PASSED=$((TOTAL_PASSED + BC_PASSED))

        echo "Total: $TOTAL_RAN ran, $TOTAL_PASSED passed"
    } > "$REPORT_FILE"

    echo ""
    # Also print report to stdout
    cat "$REPORT_FILE"
    echo ""
    REPORT_REL=$(realpath --relative-to="$REPO_ROOT" "$REPORT_FILE" 2>/dev/null || basename "$REPORT_FILE")
    echo "Report saved to: $REPORT_REL"
fi

exit $EXIT_CODE
