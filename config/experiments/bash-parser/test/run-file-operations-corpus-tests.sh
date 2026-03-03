#!/bin/bash
# Run file operations corpus tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARSER_DIR="$(dirname "$SCRIPT_DIR")"
EMACS="${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}"

# Set tree-sitter library path (relative to repo root)
REPO_ROOT="$(cd "$PARSER_DIR/../../.." && pwd)"
TREESIT_DIR="${REPO_ROOT}/runtime/tree-sitter"

if [ ! -f "$TREESIT_DIR/libtree-sitter-bash.dylib" ]; then
    echo "Error: tree-sitter bash library not found at $TREESIT_DIR"
    echo "Expected: $TREESIT_DIR/libtree-sitter-bash.dylib"
    exit 1
fi

echo "Running file operations corpus tests..."
echo "Using Emacs: $EMACS"
echo "Using tree-sitter from: $TREESIT_DIR"
echo ""

"$EMACS" -batch \
    -l ert \
    --eval "(setq treesit-extra-load-path '(\"$TREESIT_DIR\"))" \
    -l "$SCRIPT_DIR/test-file-operations-corpus.el" \
    -f ert-run-tests-batch-and-exit

echo ""
echo "✓ All file operations corpus tests passed!"
