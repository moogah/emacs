#!/bin/bash
# run-test-6.sh - Execute Test 6: JSON Building with Backquote

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-6-json-building.el \
  --eval "(test-6-main)" \
  2>&1 | tee experiments/test-6-output.txt

echo "Exit code: $?"
