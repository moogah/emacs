---
name: declare-function-library-names
description: Fix declare-function library hints in mode.el to match real file names
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/mode.org` (declare-function block)
- `config/gptel/chat/mode.el` (tangled)

## Implementation steps
1. Five sibling `declare-function` calls reference library names
   like `"gptel-chat-send"`, `"gptel-chat-nav"`, `"gptel-chat-display"`,
   etc. Actual files are `send.el`, `nav.el`, `display.el`.
   `locate-library` returns nil for the mismatched names, so
   `M-x check-declare` reports "Library not found" for each.
2. Fix EITHER:
   - **A (recommended)**: Use the single-arg form
     `(declare-function gptel-chat-send nil ())` — drop the library
     hint entirely. Cleanest.
   - **B**: Use the actual file basename:
     `(declare-function gptel-chat-send "send" ())`.
3. Apply the chosen form to all five sibling declarations.

## Design rationale
`declare-function`'s library hint is purely for `check-declare`'s
benefit. With wrong names, the rationale comment about
"`check-declare` is happier" is half-wrong. The byte-compiler is
unaffected either way (it uses the symbol name only).

## Verification
- `M-x check-declare-file config/gptel/chat/mode.el` reports no
  "Library not found" entries.
- `./bin/run-tests.sh -d config/gptel/chat` passes.

## Context
- Review of `mode-definition` task Finding #1.
