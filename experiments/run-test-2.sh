#!/bin/bash
# run-test-2.sh - Execute Test 2: Hash Table Storage

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-2-hash-storage.el \
  --eval "(test-2-main)" \
  2>&1 | tee experiments/test-2-output.txt

echo "Exit code: $?"
