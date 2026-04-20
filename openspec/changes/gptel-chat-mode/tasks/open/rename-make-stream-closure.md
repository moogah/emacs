---
name: rename-make-stream-closure
description: Rename gptel-chat--make-stream-closure to describe its role
change: gptel-chat-mode
status: blocked
relations:
  - discovered-from:sanitize-chunks
  - blocked-by:expose-tool-marker-setter
---

## Files to modify
- `config/gptel/chat/stream.org`
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/*.el` (call-site updates)

## Implementation steps
1. Pick a role-describing name; recommended:
   `gptel-chat--make-chunk-inserter` or
   `gptel-chat--make-stream-inserter`.
2. Rename the function and all call sites (currently only tests
   and the future `stream-callback` task).
3. Update any docstrings or comments that reference the old name.

## Design rationale
"Closure" is implementation detail. A name like
`make-chunk-inserter` describes what the thing does — useful for
future readers. Worth doing while the API is being refactored
(see `expose-tool-marker-setter`) so all callers move together.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- `grep -rn "gptel-chat--make-stream-closure" config/gptel/chat`
  is empty.

## Context
- Review of `sanitize-chunks` task Finding #7.
