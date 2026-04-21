---
name: nav-regenerate-in-flight-guard
description: Prevent gptel-chat-regenerate from deleting an in-flight assistant response and wrap its delete+send in atomic-change-group
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:nav-commands
---

## Files to modify
- `config/gptel/chat/nav.org` (regenerate command)
- `config/gptel/chat/nav.el` (re-tangled)
- `config/gptel/chat/test/nav/` (new spec for in-flight guard)

## Implementation steps
1. Add a guard at the top of `gptel-chat-regenerate`:
   ```elisp
   (when gptel-chat--lifecycle-state
     (user-error "Chat request in progress; cannot regenerate"))
   ```
   This reads the buffer-local state that `fsm-handlers` writes
   (`'waiting` / `'streaming` / `'tool-running`) and refuses to
   proceed when any of those states are active. The state is nil
   between requests, so steady-state regeneration is unaffected.

2. Wrap the `delete-region` + `gptel-chat-send` in
   `atomic-change-group` so a `C-/` after a failed regenerate
   (e.g., preset missing, auth error) restores the pre-regenerate
   state in one undo step rather than leaving the user with a
   deleted assistant turn and an unhelpful error.

3. Add Buttercup specs:
   - With `gptel-chat--lifecycle-state` bound to `'streaming`,
     calling `gptel-chat-regenerate` signals `user-error` and
     does not delete any buffer text.
   - With `gptel-chat--lifecycle-state` nil, regenerate deletes
     the trailing assistant block and calls `gptel-chat-send`
     (use a spy on `gptel-chat-send`).
   - `atomic-change-group` is present: inspect undo boundaries
     or use a spy on the `gptel-chat-send` spy to force a
     throw/signal and verify `C-/` restores the assistant block.

## Design rationale
The `nav-commands` review flagged that `gptel-chat-regenerate`
unconditionally deletes the trailing assistant block and calls
`gptel-chat-send`. If invoked mid-stream, the delete corrupts the
currently-streaming write (stream handlers are writing to markers
inside the deleted range) and double-fires a request against
upstream.

The guard depends on `fsm-handlers`'s buffer-local state accessor,
which is already in place (the `fsm-handlers` task is in
`needs-review` pending unrelated blocking findings, but the state
machinery exists). The guard is cheap and high-value.

Atomic-change-group wrapping addresses a separate polish concern:
if the follow-up send errors, the user has lost their prior
response with no single-keystroke undo path.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/nav` passes.
- Manual: start a long-running request, invoke
  `gptel-chat-regenerate` during `TYPE`, confirm `user-error`
  ("Chat request in progress").

## Context
- Review of `nav-commands` (2026-04-21, orch-review-1776770835),
  Findings 1 and 2.
- State accessor: `config/gptel/chat/send.el:63` (or wherever
  `gptel-chat--lifecycle-state` is declared after fsm-handlers
  review close).
