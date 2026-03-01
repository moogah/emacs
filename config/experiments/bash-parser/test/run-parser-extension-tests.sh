#!/bin/bash
# Run parser extension tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARSER_DIR="$(dirname "$SCRIPT_DIR")"
EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/Emacs"

echo "================================================"
echo "Bash Parser - Extension Tests (Wave 4)"
echo "================================================"
echo ""

# Check if bash-parser.el exists
if [ ! -f "$PARSER_DIR/bash-parser.el" ]; then
    echo "ERROR: bash-parser.el not found at $PARSER_DIR/bash-parser.el"
    echo "Please ensure bash-parser.el is present"
    exit 1
fi

echo "Parser module: $PARSER_DIR/bash-parser.el"
echo "Test file: $SCRIPT_DIR/test-parser-extension.el"
echo ""

# Run tests
echo "Running tests..."
echo ""

"$EMACS_BIN" -batch \
    -l "$PARSER_DIR/bash-parser.el" \
    -l "$SCRIPT_DIR/test-parser-extension.el" \
    --eval '(ert-run-tests-batch-and-exit "^test-parser-extension-")'

echo ""
echo "================================================"
echo "Test run complete"
echo "================================================"
