---
name: chat-spec-t-signal-round-semantics
description: Capture the per-round-not-per-turn semantics of the `t` stream-completion signal in the chat-mode spec so the multi-round gating fix is regression-guarded by the spec
change: gptel-chat-mode
status: done
relations:
  - discovered-from:stream-callback-multi-round-t-signal
---

## Files to modify
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (Requirement: Response streaming and sanitization, around line 208)

## Implementation steps

1. Add a scenario under "Response streaming and sanitization" (or
   add a companion Requirement — choose whichever reads more
   naturally; scenario is lighter-weight) capturing the `t`
   completion signal's per-round semantics. Draft:

   > #### Scenario: Multi-round tool-use preserves the assistant
   > block across rounds
   >
   > - **WHEN** the model completes a tool-use round and upstream
   >   fires the `t` completion signal with `:tool-use` set on the
   >   INFO plist
   > - **THEN** the callback flushes any holdback characters into
   >   the open assistant block
   > - **AND** the assistant block remains open — no
   >   `#+end_assistant` is appended and no new empty user block
   >   is started
   > - **AND** subsequent `(tool-result . ...)` events and the
   >   next request's streamed text land inside the same
   >   assistant block
   > - **WHEN** the model completes the final round (no further
   >   tool use pending) and upstream fires `t` with `:tool-use`
   >   unset or absent
   > - **THEN** the holdback is flushed, `#+end_assistant` is
   >   appended, and an empty `#+begin_user`/`#+end_user` block
   >   is started with point positioned for the next human turn

2. Optionally mirror this into the persisted spec path
   (`openspec/specs/gptel/` if/when this change's delta is synced
   back). For this task, edit only the change's delta spec.

3. Cross-reference design.md Decision 10 (dispatch /
   streaming-callback) with a one-line note that the callback's
   `'t` arm gates end-of-turn close on `(null (plist-get info
   :tool-use))` — an implementation-facing hint that a future
   reader needs in order to implement the scenario correctly.

## Design rationale

The multi-round gating fix landed correctly in code and tests, but
the load-bearing invariant — upstream's `t` signal fires once per
HTTP round-trip, not once per turn — currently lives only in code
comments, `design.md` Decision 10, and the task body. A future
change that "simplifies" the `'t` arm without knowing this will
regress buffer corruption in tool-using turns. The spec is where
this invariant belongs because it's a behavioural contract, not an
implementation choice. The scenario also documents the
canonical-pattern shape (matches `config/gptel/tools/persistent-
agent.org:733`) so the chat-mode and persistent-agent subsystems
stay aligned.

## Verification

- `grep -nE "tool-use|multi-round" openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  has at least one hit under the streaming-and-sanitization
  requirement block.
- `grep -n "#+end_assistant" openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  — the new scenario explicitly mentions the close happening only
  on the final `t`.

No code or test changes. Spec-only task.

## Context
- Review of `stream-callback-multi-round-t-signal` (orch session
  `orch-1776779279`, reviewer agent `a4dce24c78a3f6469`, 2026-04-21),
  Spec-level finding on missing `t`-per-round scenario.
- Canonical pattern reference:
  `config/gptel/tools/persistent-agent.org:733`.
- Upstream reference: `gptel-request.el:2669` (where the `t`
  callback is issued by `gptel-curl--stream-cleanup`).

## Review (2026-04-21, orch-review session)

- Reviewer agent `ab6c15959cebf83f1`. Verdict: CLEAN.
- Findings: none.
- The scenario is placed under "Response streaming and sanitization",
  names the `:tool-use` plist-key exactly as the code inspects it, and
  covers both `:tool-use` set (keep-open) and unset/absent (close)
  arms. Design.md Decision 10 cross-reference was applied. Canonical-
  pattern alignment with `persistent-agent.org:733` holds. Verification
  greps both pass.
- No follow-up tasks. Flipped to `done`.
