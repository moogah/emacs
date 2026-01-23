#!/bin/bash
# Run incremental tests in batch mode

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "========================================="
echo "Incremental Tests (Batch Mode)"
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

# Run tests
"$EMACS" --batch \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/cond-let\")" \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/compat\")" \
    --eval "(add-to-list 'load-path \"$REPO_ROOT/runtime/straight/build/transient\")" \
    --load "$SCRIPT_DIR/test-incremental.el" \
    --funcall test-inc-run-all

echo
echo "========================================="
echo "Tests complete"
echo "========================================="
