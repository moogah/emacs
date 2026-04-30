---
name: chat-mode-tool-confirm-ui-missing
description: gptel-chat-mode lacks a confirmation UI for :confirm t tool calls; the FSM hangs forever after the tool block is rendered because chat-mode's stream callback ignores the continuation upstream supplies for the user-approval path.
status: ready
source: openspec/changes/persistent-agent-rebuild
relations:
  - "discovered-from:verify-end-to-end"
---

## Symptom

When an LLM running through `gptel-chat-mode` invokes a tool registered with `:confirm t` (e.g. `PersistentAgent`):

1. The LLM emits a `tool_use` content block — observable in `*gptel-logs*`.
2. Chat-mode's stream callback opens an empty `#+begin_tool ... #+end_tool` block in the assistant turn.
3. The FSM enters TOOL state.
4. Nothing further happens. No confirmation prompt appears (no overlay, no transient, no minibuffer y/n). The session is stuck forever.

The only escape is `M-x gptel-abort`. Globally setting `(setq gptel-confirm-tool-calls nil)` works around the bug by routing every tool through the auto-approved (inline) path; this is what the `persistent-agent-rebuild` smoke test had to do to make progress.

This is **pre-existing in chat-mode** — not caused by the persistent-agent-rebuild. The rebuild surfaced it because `PersistentAgent` is a `:confirm t` tool. Any `:confirm t` tool called from chat-mode would behave identically.

## Root cause

Two missing pieces in chat-mode:

1. **Stream callback ignores the continuation.** `config/gptel/chat/stream.el:617-627` (the `(tool-call . ,calls)` arm of the chat-mode stream callback's `pcase` dispatch) destructures each pending call as `(,tool-spec ,args ,_cb)` and discards the third element. The comment claims `"we only render here, so CB is ignored"`. This is correct only for the auto-approved path: upstream's `gptel--handle-tool-use` runs the tool inline before firing the event, so by the time chat-mode sees `tool-call` the result is already coming. On the `:confirm t` path, upstream emits the same event with `pending-calls` as a list of `(TOOL-SPEC ARGS PROCESS-TOOL-RESULT)` 3-lists, and the third element IS the continuation that has to be invoked with the result string when the user approves. Chat-mode never invokes it, so the FSM never advances.

2. **TOOL handler chain has no confirmation UI handler.** `config/gptel/chat/send.el` defines:

   ```elisp
   (TOOL ,#'gptel-chat--on-tool ,#'gptel--handle-tool-use)
   ```

   Compare to upstream gptel-mode (`runtime/straight/repos/gptel/gptel.el:1097`):

   ```elisp
   (TOOL ,#'gptel--update-tool-call ,#'gptel--handle-tool-use ,#'gptel--update-tool-ask)
   ```

   Upstream's `gptel--update-tool-ask` only updates the modeline in gptel-mode — it does not display the actual confirm UI itself. The actual UI comes from upstream's default `gptel-curl--stream-insert-response` callback dispatcher, which on a `(tool-call . ,tool-calls)` event calls `gptel--display-tool-calls`. That function puts an overlay on the assistant text with a keymap (`gptel-tool-call-actions-map`: `C-c C-c` accept, `C-c C-k` reject, `C-c C-i` inspect). Accepting calls `gptel--accept-tool-calls`, which iterates the pending calls and invokes each tool's function, then feeds the result into the stored continuation.

   Chat-mode bypasses upstream's stream-insert callback in favour of its own (correctly so — chat-mode's buffer format is different), so the upstream UI never fires.

## Fix sketch

The chat-mode-native fix should:

- In `gptel-chat-stream-callback`'s `(tool-call . ,calls)` arm:
  - Render the empty tool blocks (already does).
  - For each pending call, store the continuation alongside the tool block's marker so the user-approval path can find it.
- Decide where the confirm UI lives. Two reasonable options:
  1. **Overlay-on-assistant pattern (mirrors upstream).** Put an overlay over the rendered tool blocks with a keymap (`C-c C-c` accept, `C-c C-k` reject, `C-c C-i` inspect). Simple. Matches the existing `gptel-tool-call-actions-map`. Could likely reuse `gptel--accept-tool-calls` / `gptel--reject-tool-calls` directly if the pending-call shape is preserved.
  2. **Transient menu** popped at the time of the event. Heavier but more discoverable. The existing scope-validation flow uses a transient (`jf/gptel-scope-prompt-expansion`); pattern is established.
- On approval: invoke the stored continuations. Each call's result then triggers a `(tool-result . ,results)` event back through chat-mode's existing handler at `stream.el:642`, closing the empty tool block with the result text.
- On rejection: invoke the continuation with a deny-shaped result string (mirror `gptel--reject-tool-calls`), so the FSM still advances and the LLM sees a deny acknowledgement.

The work is bounded — the data is all already in the event payload; chat-mode just needs to plumb the continuation through to a visible action surface.

## Why this is a `.tasks/` follow-up rather than in `persistent-agent-rebuild`

Per CLAUDE.md's "In-change vs cross-cutting externalisation" rule, externalise when **all** of:

- The finding is genuinely external to the active change's scope. ✓ The bug is in `gptel-chat-mode`'s tool-confirm path; the persistent-agent-rebuild correctly composes onto chat-mode's documented public API. The `:confirm t` flag on `PersistentAgent` matches the established gptel tool-registration shape.
- Holding the active change open for it adds delay disproportionate to the change's own scope. ✓ The persistent-agent-rebuild's verify-end-to-end already smoke-passed via the workaround; the chat-mode UI work is a meaningful chunk that deserves its own design conversation (overlay vs. transient).
- The fix can stand on its own. ✓ It needs chat-mode context, not persistent-agent context.

The persistent-agent-rebuild change can archive once verify-end-to-end is recorded. PersistentAgent will work for users who set `gptel-confirm-tool-calls` to `nil` (or whose tools register with `:confirm nil`). Once this `.tasks/` follow-up lands, PersistentAgent (and every other `:confirm t` tool) becomes usable from chat-mode without that workaround.

## Verification

A `:confirm t` tool, called from gptel-chat-mode, results in a visible approval surface (overlay or transient) at the time the tool block is rendered. Pressing the accept binding runs the tool, the result lands inside the rendered block, and the FSM advances to DONE / further tool calls. Pressing reject feeds a deny message back to the LLM, which sees the rejection and (typically) explains.

**Workaround in current tree:** `:confirm t` was removed from the `PersistentAgent` registration in `config/gptel/tools/persistent-agent.org` (during the `gptel-scope-in-org-properties` smoke pass on 2026-04-30) so the smoke test could exercise the agent path. When this `.tasks/` item lands, restore `:confirm t` on that registration to verify the new UI works on the agent path.

Concretely:
- Smoke: with `gptel-confirm-tool-calls` set to its default (`'auto`), drive a chat-mode session that uses `PersistentAgent`. Approve. Agent should run and return its text. Repeat with reject; LLM should see the deny.
- Test (Buttercup): a spec under `config/gptel/chat/test/` driving the stream callback with a synthetic `(tool-call . ((tool-spec args cb)))` event, asserting that the UI surface fires and that calling its accept entry-point invokes `cb`.

## Context

`config/gptel/chat/stream.el:617-627` — the `(tool-call . ,calls)` arm that ignores the continuation.
`config/gptel/chat/send.el` — `gptel-chat-fsm-handlers` defvar; missing the equivalent of upstream's `gptel--update-tool-ask`.
`runtime/straight/repos/gptel/gptel.el:1097` — upstream's TOOL handler chain.
`runtime/straight/repos/gptel/gptel.el:1729+` — upstream's `gptel--display-tool-calls` and the `gptel-tool-call-actions-map` keymap; reusable shape if the overlay-on-assistant approach is taken.
`runtime/straight/repos/gptel/gptel-request.el:~1730` — where pending-calls are emitted to the callback as `(tool-call . pending-calls)`; documents the 3-list shape `(TOOL-SPEC ARG-VALUES PROCESS-TOOL-RESULT)`.
Smoke test that surfaced this: `openspec/changes/persistent-agent-rebuild/tasks/closed/verify-end-to-end.md` step 4 (manual interactive run, 2026-04-28).
