---
name: add-point-in-block-body-predicate
description: Fast predicate for whether point is inside a chat-block body
change: gptel-chat-heading-scoping
status: ready
relations:
  - enables:add-user-typed-heading-escape
  - enables:add-paste-heading-escape
---

## Files to modify

- `config/gptel/chat/parser.org` (and tangled `parser.el`)
- New test file: `config/gptel/chat/test/parser/point-in-block-spec.el`

## Implementation steps

1. Add a function `gptel-chat--point-in-block-body-p` to `parser.org`. Signature: `(&optional pos buffer)`. Returns non-nil when POS (default `(point)`) is strictly inside the body of an outer `#+begin_user` / `#+begin_assistant` / `#+begin_tool` block — i.e., between the opening delimiter line and the matching closing delimiter line, exclusive of both delimiter lines.
2. Implementation: scan backward from POS using `re-search-backward` against the existing `gptel-chat--block-opener-regexp` (or define a small helper if it doesn't already exist) to find the nearest `#+begin_*` opener. Then scan forward from POS for the nearest `#+end_*` closer. Return non-nil iff opener exists, closer exists, and POS lies strictly between the two delimiter lines (column 0 of the line after the opener through column 0 of the closing delimiter line, exclusive).
3. Edge cases:
   - POS on a delimiter line itself (opener or closer) → return nil.
   - POS at `point-min` or `point-max` outside any block → return nil.
   - Mismatched opener/closer types (e.g., opener is `#+begin_user` but next closer is `#+end_assistant`) → fall back to a stricter pairing scan or return nil. Document the chosen behavior in the docstring.
4. Write Buttercup specs covering: inside body, on opener line, on closer line, in nested tool block inside assistant, between blocks, outside any block, in pre-block metadata drawer.

## Design rationale

The user-typed and paste hooks (Decisions 2 and 3) call this predicate on every keystroke / change. It must be sub-millisecond on typical chat buffers. A backward `re-search-backward` is O(distance to nearest opener), which for typical turn sizes (KB-scale) is well under a millisecond. Avoid `org-element-at-point` here — too expensive for the hot path. See design.md Decisions 2 and 3.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes (existing + new specs).
- `grep -n 'gptel-chat--point-in-block-body-p' config/gptel/chat/parser.el` shows the new function.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decisions 2 and 3.
- Existing parser scanner functions in `config/gptel/chat/parser.el`: `gptel-chat--scan-user-body`, `gptel-chat--scan-assistant-body`, `gptel-chat-parse-buffer`.
