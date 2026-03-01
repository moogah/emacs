#!/usr/bin/env bash
# Run bash-parser tests

cd "$(dirname "$0")"

/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(setq treesit-extra-load-path '(\"/Users/jefffarr/emacs/runtime/tree-sitter\"))" \
  --eval "(require 'ert)" \
  --load bash-parser.el \
  --load test-corpus.el \
  --load bash-parser-test.el \
  --eval '(ert-run-tests-batch-and-exit "jf/bash-parser-test-")'
