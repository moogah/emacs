---
name: stream-active-marker-unit-test
description: Add direct unit test for gptel-chat--stream-active-marker helper
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sanitize-chunks
---

## Resolution

**Superseded by `tool-marker-routing-tests`** step 1, which adds the exact same
three `describe "gptel-chat--stream-active-marker"` cases (nil / live / dead
tool-marker) to `streaming-spec.el`. The superseding task was merged and is
in needs-review. Closed as done to clear the ready queue; no code change is
needed here. If `tool-marker-routing-tests` review surfaces that its step 1
is insufficient, a fresh task should be raised rather than reopening this.

## Original task (preserved for history)

### Files to modify
- `config/gptel/chat/test/stream/streaming-spec.el`

### Implementation steps
1. Add a `describe "gptel-chat--stream-active-marker"` block with
   three `it` cases:
   - nil tool-marker → returns insertion-marker.
   - live tool-marker → returns tool-marker.
   - tool-marker set to a now-dead position (e.g.,
     `(set-marker m nil)`) → returns insertion-marker.
2. Use real Emacs markers in temp buffers; no mocking required.

### Design rationale
The helper was extracted "for testability" but no test invokes it
directly. Either delete it (inline back into the closure) or test
it. This task adds the missing tests; if the helper is later
inlined, these tests become obsolete and can be removed in the
same change.

### Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.

### Context
- Review of `sanitize-chunks` task Finding #4.
- Companion to `tool-marker-routing-tests` (which goes one level
  higher, exercising the helper through the closure).
