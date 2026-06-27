---
name: migrate-persistent-agent-fsm-for-tpre-tret-states
description: When vendored gptel gains the split tool-lifecycle FSM (TPRE/TRET states, gptel-send--transitions ≠ gptel-request--transitions), teach gptel-chat-fsm-handlers the new states and confirm persistent-agent's --build-fsm-handlers propagates them. Dormant trip-wire until the next gptel bump introduces those states; karthink's gptel-agent already adapted (commit 1aa1fdb).
status: blocked
source: architecture review of config/gptel/tools/persistent-agent.org (2026-06-13)
---

> Surfaced 2026-06-13 while reviewing `persistent-agent.org` against the
> underlying gptel package and karthink's upstream `gptel-agent`. Upstream
> gptel split the single `TOOL` FSM state into a three-beat tool lifecycle
> (`TPRE` → `TOOL` → `TRET`) and split the transition table in two
> (`gptel-request--transitions` ≠ `gptel-send--transitions`). karthink's
> `gptel-agent` already tracked this (commit `1aa1fdb`, 2026-03-22). Our
> vendored gptel pin (`42fde46`, 2026-04-25) does **not** have those states
> yet, so we are insulated today — this is a canary for the *next* gptel
> bump, not an active bug.

## The risk in one sentence

`persistent-agent`'s `--build-fsm-handlers` composes its handlers by
mapping over `gptel-chat-fsm-handlers` (chat-mode's alist, which currently
covers only `WAIT/TYPE/TOOL/DONE/ERRS/ABRT`); if a future vendored gptel
introduces `TPRE`/`TRET` and the FSM begins entering those states, chat-mode
(and therefore persistent-agent) will silently lag — tool-phase overlay
updates, insertion, and autosave can fire at the wrong beat or not at all,
with no crash to signal the drift.

## Why this is currently latent

Confirmed against the tree as of 2026-06-13:

- `runtime/straight/repos/gptel/gptel-request.el` defines only
  `gptel-request--transitions` (old graph: `INIT → WAIT → TYPE →
  {TOOL ↺ | DONE | ERRS}`, plus `ABRT`). No `TPRE`, no `TRET`, no
  `gptel--handle-pre-tool` / `gptel--handle-post-tool`, no
  `gptel-send--transitions`.
- `config/gptel/chat/send.org` (`gptel-chat-fsm-handlers`) covers the
  matching old state set; persistent-agent's `--build-fsm-handlers` is
  internally consistent with it.
- `config/gptel/tools/persistent-agent.org` calls
  `(gptel-make-fsm :handlers handlers)` with **no `:table`**, so it inherits
  the default `gptel-request--transitions`. Fine today; see step 3 below for
  the bump.

Activation requires: a vendored-gptel bump to a line that contains the
TPRE/TRET split. Until then there is nothing to migrate — the new states do
not exist to handle.

## What upstream did (the reference migration)

karthink's `runtime/straight/repos/gptel-agent` is the worked example.
`git show 1aa1fdb` (and follow-ups `0e574a6`, `c3612ae`) made exactly two
kinds of change, which we will mirror at the chat-mode layer:

1. Handler alist gained the new states (`gptel-agent-request--handlers`):

   ```elisp
   (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
   (TOOL ,#'gptel-agent--indicate-tool-call ,#'gptel--handle-tool-use)
   (TRET ,#'gptel--handle-post-tool ,#'gptel--handle-tool-result)
   ```

2. The sub-agent FSM switched to the send-side table:

   ```elisp
   :fsm (gptel-make-fsm :table gptel-send--transitions
                        :handlers gptel-agent-request--handlers)
   ```

   Commit message: *"Use gptel-send's transition table, as this is now
   different from that of gptel-request in gptel."*

Note the asymmetry that makes our migration heavier than his: karthink
*replaces* the whole sub-agent handler alist, so he edits one `defvar`.
We *compose* over chat-mode's alist, so the fix lands in **two** layers —
`gptel-chat-fsm-handlers` (shared pipeline) and a confirmation that
`--build-fsm-handlers` carries the new states through.

## Files to modify (at bump time)

- `config/gptel/chat/send.org` — add `TPRE`/`TRET` entries to
  `gptel-chat-fsm-handlers` (chat-mode-native pre/post-tool behaviour +
  the upstream `gptel--handle-pre-tool` / `gptel--handle-post-tool` /
  `gptel--handle-tool-result`).
- `config/gptel/tools/persistent-agent.org` —
  `jf/gptel-persistent-agent--build-fsm-handlers`: ensure the `pcase` over
  `(car entry)` either passes `TPRE`/`TRET` through untouched (default arm
  already does) **or** prepends the overlay updater on the correct beat;
  decide whether the `gptel-make-fsm` call needs
  `:table gptel-send--transitions`.
- Overlay placement: `--indicate-tool-call` currently fires on `TOOL`.
  Confirm that is still the right beat once `TPRE`/`TRET` exist (upstream
  keeps the tool-call indicator on `TOOL`; the count/"+N" accounting may
  need a look against `TRET`).

## Implementation steps

0. **(Can do now) Add the trip-wire.** Before any bump, add a guard test
   that converts silent lag into a loud failure. A buttercup spec under
   `config/gptel/chat/test/` (or `config/gptel/tools/test/persistent-agent/`)
   that asserts the vendored gptel's transition graph matches what
   chat-mode's handler alist covers — e.g. fail if
   `(boundp 'gptel-send--transitions)` or if a `TPRE`/`TRET` symbol appears
   in the active transition table while `gptel-chat-fsm-handlers` has no
   entry for it. This is the single highest-value action here and is
   actionable today; everything below is gated on the bump.

1. **At bump time, diff the gptel FSM first.** Before bumping the straight
   pin, `grep -n 'TPRE\|TRET\|gptel-send--transitions\|gptel--handle-pre-tool'`
   the candidate gptel checkout. If absent, no migration needed — just
   re-run the suite. If present, proceed.

2. **Teach chat-mode the new states** (mirror `1aa1fdb` at the
   `gptel-chat-fsm-handlers` layer). Keep chat-mode's own lifecycle
   handlers (`--on-tool` etc.) on the correct beat and chain upstream's
   pre/post-tool handlers after them.

3. **Decide the transition table for persistent-agent.** If chat-mode's
   send path now requires `gptel-send--transitions`, persistent-agent's
   `(gptel-make-fsm :handlers handlers)` likely needs the matching
   `:table gptel-send--transitions` so the agent FSM walks the same graph
   chat-mode does. Verify the return-path terminal handlers (`DONE`/`ERRS`/
   `ABRT`) still fire exactly once.

4. **Re-run the suites.**

   ```bash
   ./bin/run-tests.sh -d config/gptel/chat
   ./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent
   make test
   ```

## Design rationale

The pipeline-conformance benefit that justifies persistent-agent streaming
(it rides chat-mode's send path so agent `session.org` files are identical
to interactive ones, keeping the log format stable and restore free) has a
flip side: persistent-agent is coupled to chat-mode tracking gptel's FSM. We
accept that coupling deliberately, but it means FSM churn in gptel is a
chat-mode-layer obligation with persistent-agent as a downstream consumer —
hence this lives in `.tasks/` (cross-cutting, chat-mode-owned), not inside
any persistent-agent change.

## Verification

- Trip-wire spec (step 0) passes against the current pin and would fail if a
  TPRE/TRET-bearing gptel were vendored without the chat-mode update.
- Post-migration: a chat-mode session that drives a tool loop renders tool
  calls and results in the correct order; persistent-agent's overlay shows
  "Calling Tools…(+N)" with correct counts; the agent returns its final
  segment on `DONE`.
- Negative control: bumping gptel to a non-TPRE line requires no source
  change and leaves all suites green.

## Activation precondition

Do not perform steps 1–4 until a vendored-gptel bump actually introduces the
TPRE/TRET split. Step 0 (the trip-wire) may be done at any time and is the
recommended immediate action. Performing the migration speculatively against
the current pin would add handlers for states the FSM never enters.

## Context

- Reviewed module: `config/gptel/tools/persistent-agent.org`
  (`--build-fsm-handlers`; `gptel-make-fsm` call with no `:table`).
- Chat-mode handler alist: `config/gptel/chat/send.org`
  (`gptel-chat-fsm-handlers`).
- Current vendored gptel pin: `runtime/straight/repos/gptel`
  HEAD `42fde46` (2026-04-25) — old single-`TOOL` FSM, no split.
- Reference migration upstream: `runtime/straight/repos/gptel-agent`
  commits `1aa1fdb` (Task tool FSM handler + transitions), `0e574a6`
  (handler for tool-result state), `c3612ae` (agent tool pre/post
  handlers). These require a gptel with `TPRE`/`TRET` and
  `gptel-send--transitions`, so karthink's current `gptel-agent` master is
  itself incompatible with our gptel pin — independent confirmation that the
  gptel FSM is actively churning.
- Related parallel-evolution note (not part of this task): upstream gptel-
  agent also added prompt compaction (`1020a05`, `54126e3`) and sub-agent
  pre-specified context (`b7ef5d5`) — input-side context-budget work that
  complements our output-side final-segment return policy.
</content>
</invoke>
