---
name: marker-insertion-type-contract
description: Make the stream closure's insertion-marker insertion-type contract explicit
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sanitize-chunks
---

## Files to modify
- `config/gptel/chat/stream.org` (factory docstring + optional
  guard or coercion)
- `config/gptel/chat/stream.el` (tangled)

## Implementation steps
1. `gptel-chat--stream-insert-line` does
   `(goto-char marker) (insert ...)`. Markers default to insertion-type
   nil (do NOT advance through inserted text). Callers passing
   `(copy-marker pos)` would get reversed-order line inserts because
   the marker stays put.
2. Pick ONE:
   - **A**: Add to the factory docstring "INSERTION-MARKER must have
     insertion-type t" and add a runtime guard:
     `(unless (marker-insertion-type insertion-marker) (error "..."))`.
   - **B**: Coerce inside the factory:
     `(set-marker-insertion-type insertion-marker t)`.
3. Add a Buttercup spec that asserts the contract — e.g., calling
   the factory with a default-insertion-type marker triggers the
   guard (option A) or auto-corrects (option B).

## Design rationale
Every existing test uses `(copy-marker pos t)` so the bug is
invisible. The send-task caller will probably get this right, but
implicit contracts are how regressions sneak in.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- New spec asserts the chosen behaviour.

## Context
- Review of `sanitize-chunks` task Finding #3.
