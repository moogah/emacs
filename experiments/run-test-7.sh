#!/bin/bash
# run-test-7.sh - Execute Test 7: Dynamic Suffix Generation

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-7-dynamic-suffixes.el \
  --eval "(test-7-main)" \
  2>&1 | tee experiments/test-7-output.txt

echo "Exit code: $?"
