---
name: tool-marker-routing-tests
description: Add tests that exercise tool-marker routing in the stream closure
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sanitize-chunks
  - blocked-by:expose-tool-marker-setter
  - enables:stream-callback
---

## Files to modify
- `config/gptel/chat/test/stream/streaming-spec.el` (or a new
  `tool-marker-spec.el`)

## Implementation steps
1. Add a direct unit test for `gptel-chat--stream-active-marker`:
   - nil tool-marker → returns insertion-marker.
   - live tool-marker → returns tool-marker.
   - dead tool-marker (cleared via `set-marker nil`) → returns
     insertion-marker.
2. Add an integration test that uses the new tool-marker setter
   (delivered by `expose-tool-marker-setter`) to alternate inserts
   between assistant marker and tool marker across multiple
   chunks, asserting the resulting buffer text has each line at
   the correct position. The scenarios must include:
   - **Reroute case:** set → insert → set-to-a-different-live-marker
     → insert. The second insert must land at the new tool marker,
     not the first. (Expose review finding #2: pins that
     `setq tool-marker …` in the setter lambda works on the second
     call of the same closure instance, not just the first.)
3. Add a negative test for `set-tool-marker` input validation:
   `(funcall (gptel-chat-stream-set-tool-marker handle) nil)`
   must signal an error — clearing routing is the
   `clear-tool-marker` slot's job, not `set-tool-marker`'s. (Expose
   review finding #5: the current implementation already rejects
   nil, but no test pins that contract, so a future refactor could
   silently weaken it.)

## Design rationale
The routing branch in `gptel-chat--stream-active-marker` and the
helper's `if`-on-marker-live arm are currently unreached by any
test. The commentary in `streaming-spec.el` explicitly admits
"direct surgery on the captured environment" is required — a tell
that the design isn't tractable to test as-merged.

Severity: **blocking** — companion to `expose-tool-marker-setter`.
Without coverage, regressions in routing will surface only at
runtime.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- `gptel-chat--stream-active-marker` is invoked under all three
  marker states by the test suite.
- The reroute case (set → insert → set-to-different-marker →
  insert) is covered and asserts text lands at the new marker.
- `(funcall (gptel-chat-stream-set-tool-marker handle) nil)`
  signals an error in a dedicated test.

## Context
- Review of `sanitize-chunks` task Finding #2 (BLOCKING).
- Extended 2026-04-20 to absorb findings #2 and #5 from the
  `expose-tool-marker-setter` adversarial review (orchestrator
  session `orch-1776713064`).
