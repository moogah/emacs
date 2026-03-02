#!/usr/bin/env bash
# Run all bash-parser tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

cd "$REPO_ROOT"

echo "Running all bash-parser tests..."
echo "================================"
echo ""

./bin/emacs-isolated.sh -batch \
  -l config/experiments/bash-parser/bash-parser.el \
  -l config/experiments/bash-parser/test/test-glob-matching.el \
  -l config/experiments/bash-parser/test/test-command-semantics.el \
  -l config/experiments/bash-parser/test/test-file-operations.el \
  -l config/experiments/bash-parser/test/test-parser-extension.el \
  -l config/experiments/bash-parser/test/test-security-validator.el \
  -f ert-run-tests-batch-and-exit

echo ""
echo "All tests completed!"
