#!/bin/bash
# run-test-8.sh - Execute Test 8: Argument Handling

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-8-argument-handling.el \
  --eval "(test-8-main)" \
  2>&1 | tee experiments/test-8-output.txt

echo "Exit code: $?"
