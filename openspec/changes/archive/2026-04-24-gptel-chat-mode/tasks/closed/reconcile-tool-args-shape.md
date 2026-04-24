---
name: reconcile-tool-args-shape
description: Reconcile design.md Decision 10 vs spec.md examples on tool-block :args header shape
change: gptel-chat-mode
status: done
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

## Review (2026-04-21, orch-review session)

- Reviewer agent `a6f3edea8dcae27a6`. Verdict: FINDINGS (inline-fixed).
- The Path B adoption covered `parser.org` + `parser.el` docstring and
  `design.md` §Decision 10 callback-dispatch table + narrative, but
  missed three other places that still described Path A language. The
  reconciliation was incomplete by the single-source-of-truth goal
  stated in the task itself.
- Findings and inline fixes applied during this review:
  1. `config/gptel/chat/stream.org:396` dispatch-table row still said
     `(<name> :args <sexp>)`. Updated to `(<name> <plist...>)` and
     re-tangled to `stream.el`.
  2. `config/gptel/chat/stream.org:411-414` narrative described Path A
     contract. Rewrote to state the Path B header contract
     explicitly, with the writer's current `(:args <sexp>)` plist
     shape framed as a writer choice, not a parser contract.
  3. `specs/gptel-chat-mode/spec.md:83` schema row said
     `#+begin_tool (call-id args)` — misleading (no call-id in header,
     `args` suggests a single arg rather than a plist tail). Rewrote
     to `#+begin_tool (<name> <plist...>)` with contract description
     and a back-pointer to design.md Decision 10.
  4. `specs/gptel-chat-mode/spec.md:151` Message Construction section
     had the same stale `(call-id args)` phrasing. Updated to match.
  5. Sibling task `spec-tool-header-shape-alignment.md` was an
     open task that prescribed the OPPOSITE shape (Path A) and
     called the current Path B example "wrong." Moved to
     `tasks/closed/` with a Resolution note — see that file.
- Re-ran verification: gptel-chat suite 260/260 specs pass; full
  regression matches baseline (599 ERT, 9 pre-existing unexpected;
  1528 buttercup, 3 pre-existing failed).
- No follow-up tasks. Flipped to `done`.
