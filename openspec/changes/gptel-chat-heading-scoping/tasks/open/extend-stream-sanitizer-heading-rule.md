---
name: extend-stream-sanitizer-heading-rule
description: Add column-0 heading escape to gptel-chat--sanitize-chunk
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:add-content-indentation-defcustom
  - enables:add-parser-heading-unescape
---

## Files to modify

- `config/gptel/chat/stream.org` (and tangled `stream.el`)
- `config/gptel/chat/test/stream/streaming-spec.el` (extend existing specs)
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` (extend round-trip specs)

## Implementation steps

1. In `stream.org`, modify `gptel-chat--sanitize-chunk` to apply two rules per line, in this order:
   a. If the line matches `gptel-chat--end-delimiter-regexp` (existing rule), prepend `,`.
   b. Else if the line matches `^\*+ `, prepend `(make-string gptel-chat-content-indentation ?\s)`.
   c. Else return the line unchanged.
2. The two rules are mutually exclusive (a line cannot start with both `#+end_` and `*`), so the `else` chain is correct.
3. Update the function's docstring to describe both rules and their inverses.
4. Add Buttercup specs in `streaming-spec.el` covering:
   - Single-line `* Heading` chunk → escaped output.
   - Multi-level `*** Deep` → escaped with one prefix (the prefix doesn't change with star count).
   - Mixed chunk: `* Heading\n#+end_assistant\nplain text` → first two lines escaped per their respective rules, third unchanged.
   - Heading split across chunks: `\n* He` + `ading\n` → completed line escaped before insertion.
   - Negative: `not* a heading` (not column 0) → unchanged.
   - Negative: `*no-space` (no trailing space) → unchanged (the heading regex requires a space).
5. Add round-trip specs in `escape-round-trip-spec.el` mirroring the existing pattern: a body containing escaped headings round-trips back to the original through sanitize → parse → message.

## Design rationale

The streaming sanitizer is the high-frequency write path; per-line scan with two predicates is the cheapest design. See design.md Decision 4.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes (existing + new specs).
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes (existing + new round-trip specs).
- Manual check: `grep -n 'gptel-chat--sanitize-chunk' config/gptel/chat/stream.el` shows the function with both rules.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 4.
- Existing sanitizer: `config/gptel/chat/stream.el:46`.
- Existing round-trip tests: `config/gptel/chat/test/parser/escape-round-trip-spec.el`.
