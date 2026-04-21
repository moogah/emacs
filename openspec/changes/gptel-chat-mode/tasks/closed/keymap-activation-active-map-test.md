---
name: keymap-activation-active-map-test
description: Strengthen keymap-activation test to assert the active local map
change: gptel-chat-mode
status: needs-review
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
