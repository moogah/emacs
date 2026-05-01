---
name: add-parser-heading-unescape
description: Add inverse-of-sanitizer un-escape for column-0 headings on send
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:extend-stream-sanitizer-heading-rule
---

## Files to modify

- `config/gptel/chat/parser.org` (and tangled `parser.el`)
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` (extend round-trip specs)

## Implementation steps

1. In `parser.org`, add a function `gptel-chat--unescape-headings` that takes a string and returns it with leading whitespace stripped from any line matching `^[ \t]+\*+ `. Strip exactly the amount of leading whitespace required to leave the line as `^\*+ ...`. Lines not matching are unchanged.
2. Document that the un-escape is *robust against indent width* — it strips any leading whitespace before `*+ `, not exactly `gptel-chat-content-indentation` characters. This makes round-trip survive defcustom changes and legacy content with different indent widths. See design.md Decision 8.
3. Wire the new un-escape into `gptel-chat--segment-to-messages` (parser.el:494) alongside the existing `gptel-chat--unescape-end-delimiters` call. Apply both un-escapes to user content, assistant text segments, and tool result content.
4. Add round-trip specs in `escape-round-trip-spec.el`:
   - Body containing ` * Heading` → message contains `* Heading`.
   - Body containing `  ** Deep` → message contains `** Deep` (regardless of how many leading spaces).
   - Body containing both `,#+end_assistant` and ` * Heading` → message contains `#+end_assistant` and `* Heading` (both un-escapes apply independently).
   - Body containing `* Heading` with NO leading whitespace (the unescaped legacy form) → message contains `* Heading` (no-op, the un-escape is idempotent on already-unescaped content).
   - Negative: a line `text* asterisk` (asterisk not at column 0) → unchanged.

## Design rationale

The un-escape is the symmetric inverse of the sanitizer's heading rule. The robustness-against-indent-width property keeps round-trip stable across config changes. See design.md Decisions 1 and 8.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- `grep -n 'gptel-chat--unescape-headings' config/gptel/chat/parser.el` shows the new function and the wiring in `gptel-chat--segment-to-messages`.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decisions 1 and 8.
- Existing inverse: `gptel-chat--unescape-end-delimiters` at `config/gptel/chat/parser.el:436`.
- Wiring point: `gptel-chat--segment-to-messages` at `config/gptel/chat/parser.el:494`.
