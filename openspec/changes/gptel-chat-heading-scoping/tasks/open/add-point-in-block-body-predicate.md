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

## Observations

- **Helper regex name.** The task body and parent design.md cited a `gptel-chat--block-opener-regexp` that did not exist. I introduced
  `gptel-chat--re-block-delimiter` (matches ANY of the six delimiter
  variants — `#+begin_user`, `#+begin_assistant`, `#+begin_tool`,
  `#+end_user`, `#+end_assistant`, `#+end_tool`) instead of an
  opener-only regex. The reason: a single-shot backward scan for
  openers alone is unsound when assistants contain closed tool blocks
  (a `#+end_tool` already-closed sub-block sits between POS and the
  enclosing `#+begin_assistant`). I needed to walk both sides to keep
  a closer-stack and pop matched pairs while scanning backward.
- **Stack-walking algorithm.** Backward scan maintains a closer-stack
  count: each `#+end_*` pushes (`closer-stack++`), each `#+begin_*`
  either pops (already-closed inner block) or, if the stack is empty,
  is the actual enclosing opener. This correctly handles
  `#+begin_assistant ... #+begin_tool ... #+end_tool ... POS ...
  #+end_assistant` — POS is in assistant body, not tool body.
  Forward scan from the opener uses a same-kind depth counter so
  hypothetical sibling/nested same-kind blocks don't fool the closer
  match.
- **Mismatched opener/closer behavior.** The task body asks for a
  decision on mismatched pairs. With the stack-walk + same-kind
  forward scan, pure mismatches (opener kind != next closer kind) are
  impossible to generate from a well-formed parser-accepted buffer.
  In a malformed buffer mid-edit, the most likely failure is "no
  matching closer found before EOF" — for that case the predicate
  returns nil. This is the safe-by-default failure mode for the
  escape boundary: when the predicate is uncertain it returns nil,
  causing the user-typed escape to leave the user's text alone.
  Documented in the docstring.
- **Tool-block scoping.** The existing `gptel-chat--re-begin-turn`
  regex covers only `user|assistant`. The task body explicitly asks
  the predicate to treat `#+begin_tool` as an enclosing block too
  (a `*` heading inside a tool body would break the surrounding
  assistant block just as much as one in assistant prose). I picked
  this interpretation in code; see the new docstring and the
  "nested tool block inside assistant" describe block in
  `point-in-block-spec.el`.
- **Adjacent code looked correct.** No drift noticed in
  `gptel-chat--scan-assistant-body`, `gptel-chat--scan-user-body`,
  or the existing delimiter regexes. The new predicate is read-only
  and does not touch any of them.
- **Performance.** Per design.md the predicate must be sub-millisecond
  on typical chat buffers. Buttercup test timings show every spec
  completes in 0.03ms in batch mode (warm Emacs), well within budget.

## Discoveries

- discovery_id: disc-add-point-in-block-body-predicate-1
  class: vocabulary-mismatch
  description: |
    The task body and design.md reference a
    `gptel-chat--block-opener-regexp` that does not exist in
    `parser.el`. Existing names are `gptel-chat--re-begin-turn`
    (user/assistant only), `gptel-chat--re-begin-tool`, and the
    union `gptel-chat--re-outer-opener` (turn+tool, openers only).
    None of those alone is sufficient for the predicate's
    backward-stack scan, which needs ALL six delimiter forms. I
    added a sibling `gptel-chat--re-block-delimiter` matching any
    chat-block delimiter (begin or end, user|assistant|tool).
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    When future tasks reference parser regexes from design docs,
    use the actual symbol names from parser.org. The new
    `gptel-chat--re-block-delimiter` is the right primitive for
    "delimiter line, any kind, any side" checks; downstream
    consumers (e.g. paste-heading-escape, migration-on-read)
    should reuse it rather than re-rolling a similar union.

- discovery_id: disc-add-point-in-block-body-predicate-2
  class: invariant-gap
  description: |
    The predicate's "delimiter line returns nil" guard depends on
    the chat-block-delimiter-lines-stay-at-column-0 invariant
    holding — i.e., that delimiters never appear indented or
    mid-line. The current implementation matches `^#\+begin_*`
    anchored to BOL, so an indented "fake" delimiter (which the
    parser would already ignore) cannot fool the predicate. But
    an INDENTED line that happens to look like a delimiter and
    sits inside a real block body will return non-nil for the
    predicate (correctly — it's body content). No gap, but worth
    noting: the predicate's correctness rests on the column-0
    invariant being enforced upstream by the parser and stream
    sanitizer.
  affected_register_entry: register/invariant/chat-block-delimiter-lines-stay-at-column-0
  recommendation: |
    No action required. The invariant is upheld by the parser's
    column-0-anchored regexes and by the stream sanitizer's
    `,`-prefix escape. If a future change relaxes column-0
    anchoring in the parser, this predicate will need re-review.
