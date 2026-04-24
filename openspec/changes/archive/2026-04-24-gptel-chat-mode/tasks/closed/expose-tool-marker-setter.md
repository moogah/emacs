---
name: expose-tool-marker-setter
description: Expose tool-marker setter on stream closure so stream-callback can wire it
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sanitize-chunks
  - enables:stream-callback
---

## Files to modify
- `config/gptel/chat/stream.org` (closure factory and active-marker
  helper)
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/streaming-spec.el` (or new spec) —
  exercise the setter

## Implementation steps
1. Today `tool-marker` is a lexical variable captured inside the
   closure with no accessor. The downstream `stream-callback` task
   needs to set/clear it as tool blocks open and close, and the
   closure as merged provides no API for that.
2. Refactor `gptel-chat--make-stream-closure` to return a
   `cl-defstruct gptel-chat-stream` with
   `insert`, `set-tool-marker`, and `clear-tool-marker` slots,
   per Decision 3b. Options A (plist) and C (caller-owned
   cell) previously listed here are superseded — see
   `design.md` Decision 3b Alternatives.
3. Update existing call sites (none in production yet — only tests).
4. Tests covered by companion task `tool-marker-routing-tests`.

## Design rationale
Per the sanitize-chunks task body, "when [tool-marker is] non-nil,
inserts go here instead of the assistant marker (wired up in task
`stream-callback`)". The merged implementation honours the routing
logic but provides no way to flip the slot, so the slot is dead
code under YAGNI. This must be addressed before `stream-callback`
can implement its TOOL state.

Severity: **blocking** — `stream-callback`'s premise depends on
this slot being externally usable.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- Calling code can set tool-marker without `cl-letf` surgery on the
  closure.

## Context
- Review of `sanitize-chunks` task Finding #1 (BLOCKING).
- See `revisit-decision-3b-tool-marker` for the spec-level decision
  to make first.
