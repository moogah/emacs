---
name: simplify-stream-error-messages
description: Strip function-name prefix from stream module error messages
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sanitize-chunks
---

## Files to modify
- `config/gptel/chat/stream.org`
- `config/gptel/chat/stream.el` (tangled)

## Implementation steps
1. Find error signals in `gptel-chat--make-stream-closure` (and
   peers) that look like
   `(error "gptel-chat--make-stream-closure: INSERTION-MARKER must be a live marker")`.
2. Remove the `gptel-chat--make-stream-closure:` prefix — Emacs's
   backtrace already shows the function name.
3. Apply the same cleanup to any other `error` calls in the file
   that prefix the function name.

## Design rationale
Idiomatic Elisp avoids redundant function-name prefixes in error
messages. Cosmetic cleanup, but worth doing now while the file is
small.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes
  (any tests asserting on error message text are updated).

## Context
- Review of `sanitize-chunks` task Finding #6.
