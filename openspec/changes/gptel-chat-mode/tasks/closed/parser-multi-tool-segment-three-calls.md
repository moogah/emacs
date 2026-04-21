---
name: parser-multi-tool-segment-three-calls
description: Extend parser multi-tool-segment fixture from two to three sequential tool calls per spec
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:parser-multi-tool-segment-test
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (extend the
  multi-tool-segment scenario at lines 564-590)

## Implementation steps
1. Extend the existing `it` block fixture with a third
   `#+begin_tool (tool_c :z 3) ... #+end_tool` segment and a
   trailing `prose4`.
2. Update the shape assertion from
   `(text tool-call text tool-call text)` to
   `(text tool-call text tool-call text tool-call text)` — seven
   segments.
3. Add assertions on the third tool-call's name, args, and result,
   and the third/fourth prose text content.

## Design rationale
`spec.md:249-252` enumerates "three sequential tool calls with
interleaved prose" as the multi-tool scenario; the current fixture
covers only two. The loop-advancement invariant is already pinned
by the two-tool case, so this task is about closing the spec/test
divergence rather than catching a new bug.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- The extended scenario asserts segment count = 7 and the full
  type sequence.

## Context
- Review of `parser-multi-tool-segment-test` (2026-04-21,
  orch-review-1776789773) Finding #1.
