#!/usr/bin/env bash
# Run coverage analysis on bash parser research corpus

set -euo pipefail

# Find repository root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

cd "$REPO_ROOT"

echo "Running coverage analysis on corpus..."
echo ""

EMACS_USER_DIRECTORY="$REPO_ROOT/runtime" \
  /Applications/Emacs.app/Contents/MacOS/Emacs -q \
  --load "$REPO_ROOT/early-init.el" \
  --load "$REPO_ROOT/init.el" \
  -batch \
  --eval "(add-to-list 'load-path \"$REPO_ROOT/config/bash-parser/research\")" \
  --eval "(require 'analyze-with-coverage)" \
  --eval "(jf/bash-coverage-research-analyze)"

echo ""
echo "Analysis complete! Results written to:"
echo "  config/bash-parser/research/coverage-analysis.tsv"
