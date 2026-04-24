---
name: keymap-activation-active-map-test
description: Strengthen keymap-activation test to assert the active local map
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (the
  "installs the chat-mode keymap" `it`)

## Implementation steps
1. Today the test calls `(gptel-chat-mode)` and asserts on
   `(lookup-key gptel-chat-mode-map (kbd "C-c C-c"))`. That
   verifies the defvar's contents but not that
   `define-derived-mode` actually wired it onto the buffer.
2. Add (or replace with) an assertion on the active map:
   - `(expect (eq (current-local-map) gptel-chat-mode-map) :to-be-truthy)`,
     OR
   - `(expect (lookup-key (current-local-map) (kbd "C-c C-c")) :to-equal 'gptel-chat-send)`.
3. Wrap in `with-temp-buffer` so the active map is the chat-mode
   one.

## Design rationale
The spec scenario is "chat-mode keybindings are active". A
regression that renamed the keymap variable, or omitted the
`-map`-suffix wiring `define-derived-mode` performs, would leave
the existing assertion green while `C-c C-c` does nothing in a
chat buffer.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- The test now asserts on `current-local-map`, not just the
  defvar.

## Context
- Review of `mode-definition` task Finding #4.

## Review
- **Session:** orch-review-1776789773 (2026-04-21), agent `a0514552d76096657`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - **Failure-mode claim**: if `define-derived-mode` rename-convention
    wiring regressed (e.g., keymap var renamed off the `-map` suffix),
    the old `lookup-key gptel-chat-mode-map` assertion would pass
    green while the new `(eq (current-local-map) gptel-chat-mode-map)`
    would fail. New assertion genuinely closes the claimed gap.
  - **Activation scope**: `with-temp-buffer` + direct `(gptel-chat-mode)`
    call + assertions inside the same form — `current-local-map` is
    evaluated in the live buffer where the mode ran.
  - **Adjacent weaker patterns**: remaining defvar-based assertions
    (lines 121-132) sit directly below the new active-map check,
    already covered by the `eq` assertion. No other `it` blocks in
    the file use a weaker pattern.
  - **Downstream impact**: terminal leaf; no open task depends.
- **Verification:** `./bin/run-tests.sh -d config/gptel/chat/test/parser`
  passes 86/86 specs.
- **Follow-ups:** none
- **Dependents repointed:** none
