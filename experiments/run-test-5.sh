#!/bin/bash
# run-test-5.sh - Execute Test 5: Custom Infix with Plist Data

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-5-infix-with-plist.el \
  --eval "(test-5-main)" \
  2>&1 | tee experiments/test-5-output.txt

echo "Exit code: $?"
