---
name: stream-callback-orphan-result-loud-fail
description: Fail loudly (cl-assert or visible marker) when a tool-result arrives without a pending tool-marker, instead of silently dropping
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:stream-callback
---

## Files to modify
- `config/gptel/chat/stream.org` (`:707-714` tool-result arm)
- `config/gptel/chat/stream.el` (re-tangled)
- `config/gptel/chat/test/stream/tool-call-spec.el` (add orphan-result
  spec)

## Implementation steps
1. Replace the current `(when marker ...)` guard with either:
   - `(cl-assert marker nil "Orphan tool-result with no pending marker")`
     for a loud failure, or
   - `(unless marker (insert (format "<!-- orphan tool-result: %S -->\n"
     result)))` for a user-visible diagnostic that survives into the
     buffer for debugging.
   Pick whichever matches the rest of the file's error-handling style
   (the `(_ (error ...))` defensive guard at the end of the pcase
   suggests loud-fail is preferred).
2. Re-tangle.
3. Add a spec that drives the callback with a `(tool-result . ...)` but
   no prior `(tool-call . ...)`; assert the chosen behavior.

## Design rationale
The rest of `stream.org` fails loudly on unexpected shapes (defensive
`(_ (error ...))` arm). Silently dropping an orphan tool-result is
inconsistent and makes any future regression that desynchronises the
FIFO invisible. Loud fail keeps the contract strict.

Non-blocking — does not gate downstream tasks, but worth landing
before `verify-change` runs end-to-end.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes with
  the new orphan-result spec.
- `grep -n "when marker" config/gptel/chat/stream.el` returns no hits
  (or only inside the expected assertion).

## Context
- Review of `stream-callback` (2026-04-21, orch-review-1776774164),
  Finding #5 (non-blocking code-quality).
