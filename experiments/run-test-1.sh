#!/bin/bash
# run-test-1.sh - Execute Test 1: Basic Transient Menu

/Applications/Emacs.app/Contents/MacOS/Emacs \
  --batch \
  --eval "(setq debug-on-error t)" \
  --load ./experiments/test-1-basic-transient.el \
  --eval "(test-1-main)" \
  2>&1 | tee experiments/test-1-output.txt

echo "Exit code: $?"
