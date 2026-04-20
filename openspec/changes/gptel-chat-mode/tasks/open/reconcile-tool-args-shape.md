---
name: reconcile-tool-args-shape
description: Reconcile design.md Decision 10 vs spec.md examples on tool-block :args header shape
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:parser
  - enables:messages
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (Decision 10)
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md` (Buffer Format examples — verify only)
- `config/gptel/chat/parser.org` (docstring of `gptel-chat--parse-tool-header` — clarify final shape)

## Implementation steps
1. Decision: pick ONE of the two shapes the artifacts currently
   disagree on for the `#+begin_tool` opening line:
   - **A**: `(<name> :args <plist>)` — Decision 10's wording. Parser
     would emit `:args (cdr (cdr parsed))` (skip the `:args` keyword).
   - **B**: `(<name> <plist...>)` — spec.md example and current test
     fixture. Parser emits `:args (cdr parsed)`. **Recommended** —
     matches the implementation as merged and is one fewer level of
     wrapping for the messages-task consumer.
2. Update `design.md` Decision 10 wording to match the chosen shape.
3. Update `spec.md` §"Buffer Format" example to be consistent (one
   form only).
4. Add a one-line note to `parser.org`'s `gptel-chat--parse-tool-header`
   docstring stating which shape is the contract.
5. If shape **A** is chosen, change parser code AND test fixtures to
   match (the implementation as merged uses shape B).

## Design rationale
The two artifacts currently disagree. The `messages` task will
consume `:args` to build `gptel-request`'s tool-call message; without
a single source of truth, the read path (parser + messages) and the
write path (stream — which writes the `#+begin_tool` line on
agent-completion) will silently diverge.

## Verification
- `grep -n "begin_tool" openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md openspec/changes/gptel-chat-mode/design.md` shows the same shape in all examples.
- Parser docstring documents the shape.
- If shape change required: `./bin/run-tests.sh -d config/gptel/chat/test/parser` still passes.

## Context
- Review of `parser` task Finding #3 (severity: recommended, but
  reviewer flagged "before messages lands" — included as a hard
  blocker on `messages` to force the reconciliation up front).
