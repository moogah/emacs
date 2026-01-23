#!/bin/bash
# Run gptel schema serialization tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "========================================="
echo "GPtel Schema Serialization Tests"
echo "========================================="
echo

# Find Emacs binary
if [[ -x "/Applications/Emacs.app/Contents/MacOS/Emacs" ]]; then
    EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
elif command -v emacs &> /dev/null; then
    EMACS="emacs"
else
    echo "Error: Could not find Emacs binary"
    exit 1
fi

echo "Using Emacs: $EMACS"
echo

cd "$REPO_ROOT"

# Run tests - need to load gptel and dependencies
"$EMACS" --batch \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/compat\")" \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/map\")" \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/gptel\")" \
    --load "$SCRIPT_DIR/test-schema-serialization.el" \
    --funcall test-schema-run-all

echo
echo "========================================="
echo "Tests complete"
echo "========================================="
