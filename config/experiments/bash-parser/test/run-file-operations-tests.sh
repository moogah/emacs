#!/bin/bash
# Run file operations extraction tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARSER_DIR="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(cd "$PARSER_DIR/../../.." && pwd)"
EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/Emacs"

echo "================================================"
echo "Bash Parser - File Operations Extraction Tests"
echo "================================================"
echo ""

# Check if bash-parser.el exists
if [ ! -f "$PARSER_DIR/bash-parser.el" ]; then
    echo "ERROR: bash-parser.el not found at $PARSER_DIR/bash-parser.el"
    echo "Please tangle bash-parser.org first:"
    echo "  cd $PARSER_DIR && emacs --batch bash-parser.org -f org-babel-tangle"
    exit 1
fi

echo "Parser module: $PARSER_DIR/bash-parser.el"
echo "Test file: $SCRIPT_DIR/test-file-operations.el"
echo "Tree-sitter path: $REPO_ROOT/runtime/tree-sitter"
echo ""

# Run tests
echo "Running tests..."
echo ""

"$EMACS_BIN" --batch \
    --eval "(setq treesit-extra-load-path '(\"$REPO_ROOT/runtime/tree-sitter\"))" \
    -l "$PARSER_DIR/bash-parser.el" \
    -l "$SCRIPT_DIR/test-file-operations.el" \
    --eval '(ert-run-tests-batch-and-exit "^test-")'

EXIT_CODE=$?

echo ""
echo "================================================"
echo "Test run complete"
if [ $EXIT_CODE -eq 0 ]; then
    echo "Result: ALL TESTS PASSED"
else
    echo "Result: SOME TESTS FAILED (exit code: $EXIT_CODE)"
    echo ""
    echo "Known issues (follow-up beads created):"
    echo "  - emacs-xbsj: Fix unresolved variable marking"
    echo "  - emacs-zppa: Fix variable assignment tracking"
    echo "  - emacs-qhtn: Fix touch command semantics"
    echo "  - emacs-wxn1: Fix pipeline redirection extraction"
fi
echo "================================================"

exit $EXIT_CODE
