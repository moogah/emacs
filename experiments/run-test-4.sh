#!/bin/bash
# run-test-4.sh - Execute Test 4: Custom Infix Class

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-4-custom-infix.el \
  --eval "(test-4-main)" \
  2>&1 | tee experiments/test-4-output.txt

echo "Exit code: $?"
