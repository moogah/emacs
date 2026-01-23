#!/bin/bash
# run-test-3.sh - Execute Test 3: Callback Invocation

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-3-callback.el \
  --eval "(test-3-main)" \
  2>&1 | tee experiments/test-3-output.txt

echo "Exit code: $?"
