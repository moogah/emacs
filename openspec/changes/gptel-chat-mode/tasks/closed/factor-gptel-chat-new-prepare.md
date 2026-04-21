---
name: factor-gptel-chat-new-prepare
description: Factor gptel-chat-new into pure prepare and interactive switch wrappers
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Review (2026-04-21, orch session `orch-review-1776796835`)

Reviewer agent (`a823370178cb51e17`) inspected diff `5515cc3`, tangled
`mode.el`, the updated `buffer-format-spec.el`, and all callers of
`gptel-chat-new` repo-wide. Verified the pure helper
`gptel-chat--prepare-new-buffer` contains no `(interactive)` and no
`switch-to-buffer`/`pop-to-buffer`/`display-buffer`/`select-window`
calls; it operates only through `with-current-buffer`. The public
wrapper is three lines, with `;;;###autoload` on the public command
only. The new "does not mutate window configuration" spec snapshots
`(window-buffer (selected-window))` pre- and post-call and asserts
equality — a load-bearing assertion that would catch a regression
re-inlining `switch-to-buffer` into the pure helper. Naming
(`gptel-chat--prepare-new-buffer`, double-dash) matches the project's
Elisp private-helper convention.

**Findings:** none.

Ruled out:
- Sub-par code: clean 6-line body, no copy-paste, no dead branches.
- Window-mutation leakage: grep of `switch-to-buffer|pop-to-buffer|
  display-buffer|select-window` in `mode.el` hits only the wrapper.
- Test quality: window-config spec asserts on window state directly,
  not re-stating buffer state.
- Stragglers: repo-wide grep finds no test calls to `gptel-chat-new`.
- Drift: design.md §Decision 9 fixes initial content + point; does
  not prescribe monolithic vs factored implementation.

**Spec-level signals:** none.

**Follow-ups:** none.

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
