---
name: messages
description: Convert turn list to gptel-request :prompt shape with comma-unescape
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:parser
---

## Files to modify
- `config/gptel/chat/parser.org` (modify — add `gptel-chat--turns-to-messages`)
- `config/gptel/chat/parser.el` (tangled)
- `config/gptel/chat/test/parser/message-construction-spec.el` (new)
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` (new)

## Implementation steps
1. Implement `gptel-chat--turns-to-messages` that consumes the parser's turn
   list and produces a list matching `gptel-request`'s documented `:prompt`
   contract. Shape per turn:
   - **User turn** → a `(role . content)` cons (role `"user"`) with the block's
     body joined and comma-unescaped.
   - **Assistant turn** → expand to a sequence in segment order: assistant-text
     segments become `(role . content)` cons (role `"assistant"`); tool-call
     segments expand to a tool-call message followed by a tool-result message
     matching upstream gptel's tool-history format.
2. Comma-unescape: for every body line matching
   `^,#\+end_\(user\|assistant\|tool\)\b` (case-insensitive), strip the
   leading comma before inclusion. This is the inverse of the stream
   sanitizer (task `sanitize-chunks`) — tests must cover the round trip.
3. Skip empty user/assistant turns (whitespace-only content emits no message;
   see spec scenario "Empty blocks are skipped").
4. **Verify tool-call message shape against `persistent-agent.org`.**
   `config/gptel/tools/persistent-agent.org` passes full tool history through
   `:prompt` and is the canonical in-repo worked example. Extract the exact
   shape from there — this resolves Open Question 1 in design.md.
5. Tests: every "Message construction from buffer" scenario in the spec, plus
   the escape round-trip explicitly (`,#+end_assistant` body line round-trips
   through parse → message → text → sanitize → parse).

## Design rationale
Decision 2 commits to *matching* `gptel-request`'s documented `:prompt`
contract rather than inventing a parallel message protocol. `gptel-request` is
the single public API we couple to. Emitting internal gptel message structs
directly would couple us to gptel internals; sending a single flat user
message would lose role distinction in multi-turn context.

The un-escape step pairs with Decision 4: the stream sanitizer adds a leading
comma to any body line that would collide with our three `#+end_*` delimiters;
this function strips those commas so the model never sees the escape. Round-
tripping through text (save/reopen) is preserved by construction.

## Design pattern
`persistent-agent.org` is the canonical reference. Start there for both the
message shape and the tool-history format — design.md §"Reference
implementation" for Decision 10 makes this explicit.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- Scenarios covered (from spec §"Message construction from buffer"):
  - Single user-assistant turn
  - Assistant turn with one tool call → expands to assistant+tool_call+tool_result+assistant sequence
  - Multiple tool calls in one assistant turn
  - Delimiter escape round-trip
  - Empty blocks are skipped
  - Turns distributed across org headings
  - User prompt with org structural features preserved verbatim

## Context
- design.md §Decision 2 (message list shape)
- design.md §Decision 4 (sanitization — inverse of this task's un-escape)
- design.md §Open Questions #1 — tool-history shape verification
- `config/gptel/tools/persistent-agent.org` — canonical reference
  implementation
- architecture.md §`gptel-chat-parser` — data contract
