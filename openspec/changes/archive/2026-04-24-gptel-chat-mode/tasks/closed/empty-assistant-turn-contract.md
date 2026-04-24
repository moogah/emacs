---
name: empty-assistant-turn-contract
description: Define explicitly what counts as empty assistant turn for the messages task
change: gptel-chat-mode
status: done
relations:
  - discovered-from:parser
  - enables:messages
---

## Files to modify
- `openspec/changes/gptel-chat-mode/tasks/open/messages.md` (clarify
  contract in the task body)
- OR `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  §"Message construction from buffer" (add scenario language)

## Implementation steps
1. The parser emits an assistant turn with `:segments nil` for an
   empty assistant block (`#+begin_assistant\n#+end_assistant\n`),
   and an assistant turn with all-whitespace text segments for
   `#+begin_assistant\n   \n#+end_assistant\n`. Neither case has
   the `:content` field that the user-turn empty-skip rule keys off.
2. Define what "empty assistant turn" means — choices:
   - `:segments nil` only
   - `:segments nil` OR all segments are whitespace-only `text`
3. Update the chosen artifact (likely the `messages` task body, since
   it's the consumer) with the explicit definition.
4. Optionally add a parser test that pins `:segments nil` for an
   empty block (so the contract is testable from both sides).

## Design rationale
The `messages` task body says "Skip empty user/assistant turns
(whitespace-only content emits no message)" but the parser-output
contract has no `:content` field on assistant turns, making
"whitespace-only content" ambiguous. Pin the definition before
`messages` lands.

## Verification
- Chosen artifact has a one-paragraph definition of "empty
  assistant turn".
- If parser test added: `./bin/run-tests.sh -d config/gptel/chat/test/parser`
  passes.

## Context
- Review of `parser` task Finding #8.

## Review (orch-review-1776792000, 2026-04-21)

Reviewer verified the end-to-end contract:
- Spec wording (`specs/gptel-chat-mode/spec.md §Message construction from buffer`) matches parser behaviour: `gptel-chat--flush-text-segment` never emits an empty-string segment, so `:segments nil` is guaranteed for a fully empty assistant block; whitespace-only body produces a `(text "   \n")` segment because `flush` only checks `string-empty-p`.
- `gptel-chat--segment-to-messages` always returns a `(tool . …)` cons for any `tool-call` segment regardless of `:result`/`:args`/`:name` content, so "assistant turn with any tool-call segment is never empty" is enforced by construction.
- Consumer coverage in `message-construction-spec.el:217-255` already exercises all four empty scenarios; the new parser test at `buffer-format-spec.el:510-515` is complementary.
- Spec-over-task-body was the correct home (specs are canonical).

### Findings
- **Non-blocking:** Parser test pins `:segments nil` for a fully empty block but does not pin the whitespace-only branch (`#+begin_assistant\n   \n#+end_assistant\n`). A regression that silently trimmed whitespace text segments into `:segments nil` would pass at the parser layer and go invisible end-to-end (consumer-side asserts nil message-list either way). Follow-up task: `parser-whitespace-only-assistant-segments-test` (discovered-from: this task).
