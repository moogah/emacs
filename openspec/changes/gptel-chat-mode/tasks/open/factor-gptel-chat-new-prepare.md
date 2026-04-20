---
name: factor-gptel-chat-new-prepare
description: Factor gptel-chat-new into pure prepare and interactive switch wrappers
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/mode.org`
- `config/gptel/chat/mode.el` (tangled)
- `config/gptel/chat/test/parser/buffer-format-spec.el` (test of the
  pure path)

## Implementation steps
1. Define `gptel-chat--prepare-new-buffer`: pure, returns a fresh
   buffer with `gptel-chat-mode` active, initial content inserted,
   and point positioned. No window mutation.
2. Redefine `gptel-chat-new` (interactive wrapper) as:
   `(let ((buf (gptel-chat--prepare-new-buffer))) (switch-to-buffer buf))`.
3. Update tests to call `gptel-chat--prepare-new-buffer` directly
   so they don't mutate window state.
4. Optional follow-up: this factoring also enables a future
   `gptel-chat-new-other-window` variant.

## Design rationale
The current command switches to the new buffer before activating
the mode and inserting content, leaking window-config mutation
into any test that calls `(gptel-chat-new)`. Tests handle buffer
cleanup but not window state. Splitting makes tests side-effect-free.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- New tests use the pure helper and assert on buffer state, not
  window state.

## Context
- Review of `mode-definition` task Finding #7.
