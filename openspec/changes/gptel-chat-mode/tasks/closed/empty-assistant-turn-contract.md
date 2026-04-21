---
name: empty-assistant-turn-contract
description: Define explicitly what counts as empty assistant turn for the messages task
change: gptel-chat-mode
status: needs-review
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
