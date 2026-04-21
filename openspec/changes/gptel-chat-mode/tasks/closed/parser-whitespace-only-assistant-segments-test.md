---
name: parser-whitespace-only-assistant-segments-test
description: Pin parser output for whitespace-only assistant block (`:segments` list with whitespace text segment)
change: gptel-chat-mode
status: done
relations:
  - discovered-from:empty-assistant-turn-contract
---

## Review (2026-04-21, orch session `orch-review-1776796835`)

Diff (`6037ff6`) adds one `it` spec adjacent to the `:segments nil`
sibling spec, parsing `#+begin_assistant\n   \n#+end_assistant\n` and
asserting:

- exactly one turn, role `assistant`
- `:segments` is a list with `>= 1` elements
- every segment's `:type` is `text`
- every segment's `:content` contains no non-whitespace chars

**Verified against the task's stated intent:**
- Matches the task's step-1 assertions one-for-one (length ≥ 1, text
  segments, whitespace-only content).
- Inline comment explains why both halves of the empty-turn contract
  need parser-side pinning (end-to-end message list is nil either way,
  so trim regressions would go invisible). Matches the design rationale
  in the task body.
- No production code changed (consistent with "characterization /
  pin" intent).

**Regression coverage check:** a parser regression that collapses to
`:segments nil` fails on `(>= (length segs) 1)` (since `(length nil)`
is 0). A regression that drops segments entirely fails on the same
assertion. Coverage is sound.

**Findings:** none.

Note: the agent initially flipped the task file to `status: done`
directly; the prior orchestrator corrected it back to `needs-review`
before merge so this review step was not skipped. Recorded in
`orch-1776795320` state-prev for audit.

**Follow-ups:** none.

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (add one Buttercup spec)

## Implementation steps
1. Add an `it` spec alongside the existing "emits an assistant turn
   with `:segments nil` for an empty block" spec. Parse input
   `#+begin_assistant\n   \n#+end_assistant\n` and assert:
   - `:segments` is a list of length ≥ 1.
   - The segment(s) are `(text …)` segments whose payload is
     whitespace-only (no non-whitespace characters).
2. No production code changes expected.

## Design rationale
The `empty-assistant-turn-contract` spec scenarios define the
"empty assistant turn" contract as `:segments nil` OR all-whitespace
`text` segments. The existing parser test pins the `:segments nil`
branch; a regression that silently *trims* whitespace segments (turning
them into `:segments nil` or dropping them) would desync from the
contract without being caught at the parser layer. The consumer-side
`message-construction-spec.el` asserts the end-to-end message list is
nil either way, so drift on the whitespace shape would go invisible.

Non-blocking: no known regression risk today, and the observable
end-to-end behaviour is already locked in. Pin the parser-side
whitespace shape explicitly so the contract is testable on both sides.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes with the
  new spec.

## Context
- Review of `empty-assistant-turn-contract` (2026-04-21,
  orch-review-1776789773), non-blocking finding #1.
- Contract spec: `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md` §"Message construction from buffer" (empty-assistant-turn scenarios).
