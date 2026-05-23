---
name: add-parser-heading-unescape
description: Add inverse-of-sanitizer un-escape for column-0 headings on send
change: gptel-chat-heading-scoping
status: done
merge_commit: 75432038894bd188dcbece92cc0c510e4908cc78
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

## Observations

- Implementation closely mirrors the existing `gptel-chat--unescape-end-delimiters`
  pattern: a sibling `defconst` for documentation/discoverability
  (`gptel-chat--escaped-heading-regexp`) plus the function itself, both
  living in their own `parser.org` subsection ("Heading un-escape"),
  immediately after the comma-un-escape subsection.
- The function uses `replace-regexp-in-string` with the boundary anchor
  `\\(\\`\\|\n\\)` to handle the first line and interior lines
  uniformly — same pattern as `gptel-chat--unescape-end-delimiters`.
  The match alternative has to be `[ \t]+` (NOT `[ \t]*`) so a column-0
  star never matches and the un-escape stays idempotent on already
  un-escaped content.
- Wiring in `gptel-chat--segment-to-messages` and
  `gptel-chat--turn-to-messages` composes the two un-escapers as
  `(unescape-headings (unescape-end-delimiters x))`. They operate on
  disjoint line shapes, so order is irrelevant in principle; I picked
  delimiter-first (innermost) only because that's what was already
  there. The new "composes with unescape-end-delimiters" describe block
  in the test file explicitly verifies order-independence.
- Test count: 173 specs in `config/gptel/chat/test/parser` (was ~140
  before; added ~30 new heading specs covering line-level shape,
  idempotence, indent-width robustness, sanitize/un-escape inverse pair,
  and full-pipeline round-trips for both assistant and user content
  plus a combined heading+delimiter body). Full chat suite still 487
  green.

## Discoveries

- The cited register entries `register/boundary/chat-heading-collision-escape`
  and `register/invariant/chat-heading-escape-roundtrip-transparent` are
  not present in `interfaces.org` at this commit. The boundary's stage 2
  language quoted in the briefing was apparently authored speculatively
  but never actually written into the register file. This is not a
  blocker for the implementation — the design.md Decision 8 text and
  the existing `gptel-chat--unescape-end-delimiters` sibling are
  sufficient to specify the contract — but the register entries
  themselves should be added or reconciled by whoever owns the register
  during integrate. Filing this as a Discovery rather than a new task,
  per "push-backs go in Discoveries."
- The asymmetry callout (sanitize inserts exactly N spaces; un-escape
  strips any amount) ended up cleanly testable with a dedicated
  "round-trip survives indent-width mismatches" describe block
  (sanitize@1 → un-escape@2 and the reverse). This makes the design.md
  Decision 8 robustness property a regression-pinned invariant rather
  than just a docstring claim.
