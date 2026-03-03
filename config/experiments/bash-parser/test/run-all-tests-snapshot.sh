#!/bin/bash
# Run all bash-parser tests and capture output as a status snapshot

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARSER_DIR="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(cd "$PARSER_DIR/../../.." && pwd)"
TREESIT_DIR="${REPO_ROOT}/runtime/tree-sitter"

# Output file (tracked in git)
OUTPUT_FILE="${SCRIPT_DIR}/test-results.txt"

echo "=========================================="
echo "Bash Parser - Comprehensive Test Suite"
echo "=========================================="
echo ""
echo "Running all tests and capturing output..."
echo "Output file: ${OUTPUT_FILE}"
echo ""

if [ ! -f "$TREESIT_DIR/libtree-sitter-bash.dylib" ]; then
    echo "Error: tree-sitter bash library not found at $TREESIT_DIR"
    echo "Expected: $TREESIT_DIR/libtree-sitter-bash.dylib"
    exit 1
fi

# Run tests and capture output
"${REPO_ROOT}/bin/emacs-isolated.sh" -batch \
    --eval "(setq treesit-extra-load-path '(\"$TREESIT_DIR\"))" \
    -l "${PARSER_DIR}/bash-parser.el" \
    -l "${PARSER_DIR}/bash-parser-test.el" \
    -l "${SCRIPT_DIR}/test-glob-matching.el" \
    -l "${SCRIPT_DIR}/test-command-semantics.el" \
    -l "${SCRIPT_DIR}/test-file-operations.el" \
    -l "${SCRIPT_DIR}/test-parser-extension.el" \
    -l "${SCRIPT_DIR}/test-security-validator.el" \
    -l "${SCRIPT_DIR}/test-file-operations-corpus.el" \
    -f ert-run-tests-batch-and-exit \
    2>&1 | tee "$OUTPUT_FILE"

echo ""
echo "=========================================="
echo "Test Results Summary"
echo "=========================================="
echo ""

# Extract summary statistics from output
TOTAL_TESTS=$(grep -E "^Ran [0-9]+ tests" "$OUTPUT_FILE" | head -1 | awk '{print $2}')
EXPECTED=$(grep -E "^Ran [0-9]+ tests" "$OUTPUT_FILE" | head -1 | awk '{print $4}')
UNEXPECTED=$(grep -E "^Ran [0-9]+ tests" "$OUTPUT_FILE" | head -1 | awk '{print $9}')

if [ -n "$TOTAL_TESTS" ]; then
    echo "Total tests run: $TOTAL_TESTS"
    echo "Expected results: $EXPECTED"
    echo "Unexpected results: $UNEXPECTED"

    if [ "$UNEXPECTED" = "0" ]; then
        echo ""
        echo "✓ All tests passed!"
        EXIT_CODE=0
    else
        echo ""
        echo "✗ Some tests failed"
        echo ""
        echo "Failed tests:"
        grep -A 1 "^   FAILED" "$OUTPUT_FILE" || true
        EXIT_CODE=1
    fi
else
    echo "Could not parse test results"
    EXIT_CODE=1
fi

echo ""
echo "Results saved to: ${OUTPUT_FILE}"
echo "Use 'git diff ${OUTPUT_FILE}' to see what changed"
echo ""

exit $EXIT_CODE
