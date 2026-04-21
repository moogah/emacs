---
name: fsm-handlers
description: Chained FSM handlers for UI state updates on WAIT/TYPE/TOOL/DONE/ERRS
change: gptel-chat-mode
status: done
relations:
  - blocked-by:mode-definition
---

## Files to modify
- `config/gptel/chat/send.org` (new — handler-alist section)
- `config/gptel/chat/send.el` (tangled)
- `config/gptel/chat/test/send/backend-invocation-spec.el` (new — verify
  handler chaining)

## Implementation steps
1. Define `gptel-chat--fsm-handlers` as the handler alist passed to
   `gptel-make-fsm`. Our handlers must **chain before** the upstream
   handlers — this is the pattern used by
   `config/gptel/tools/persistent-agent.org`:

   ```elisp
   (defvar gptel-chat--fsm-handlers
     `((WAIT ,#'gptel-chat--on-wait  ,#'gptel--handle-wait)
       (TYPE ,#'gptel-chat--on-type)
       (TOOL ,#'gptel-chat--on-tool  ,#'gptel--handle-tool-use)
       (DONE ,#'gptel-chat--on-done)
       (ERRS ,#'gptel-chat--on-errs)))
   ```

2. Implement the five `gptel-chat--on-*` handlers. Each is passed the FSM
   and does **UI-only** work; they do NOT call `gptel--fsm-transition`
   directly — transitions are driven by the chained upstream handlers
   (`gptel--handle-wait`, `gptel--handle-tool-use`) that remain in the
   alist. v1 bodies:
   - `gptel-chat--on-wait` — set a buffer-local indicator (e.g. mode-line
     hint variable) to "waiting".
   - `gptel-chat--on-type` — set it to "streaming".
   - `gptel-chat--on-tool` — set it to "tool-running".
   - `gptel-chat--on-done` — clear the indicator.
   - `gptel-chat--on-errs` — set it to "error" and log a message.

   The display-layer task (`display-layer`) can later subscribe to these
   via a hook if needed; v1 just writes a buffer-local var that the mode
   line (or a future overlay) can read. Keep handler bodies tiny.
3. Expose a public accessor (e.g. `gptel-chat--state`) that returns the
   current lifecycle state, reading `gptel--fsm-last` — used by the
   send-guard in task `send-command`.
4. Tests: stub `gptel-request` to fire a scripted sequence of handler
   invocations via a mock FSM; assert our handlers are called and the
   buffer-local indicator transitions correctly. Verify our handlers do
   not call `gptel--fsm-transition` directly (use a spy).

## Design rationale
Decision 3: upstream's `gptel-fsm` (defined at `gptel-request.el:1570-1636`)
is the one durable lifecycle abstraction gptel gives us. Plugging in at the
handler layer keeps our UI reactive to state changes while letting upstream
own transition correctness, tool dispatch, and error propagation. If
upstream adds new intermediate states (e.g. a `REASONING` phase), we
inherit them for free.

Alternatives rejected:
- **Hand-rolled per-request state machine**: would parallel, not integrate
  with, upstream — we'd be stranded when the upstream tool loop advances
  (`TOOL → WAIT` for multi-turn agentic tool use) and we'd have to
  re-derive it.
- **Ignoring the FSM, relying only on `:callback`**: the callback exposes
  wire events (chunks, tool-call/result cons cells, completion sentinels)
  but not higher-level lifecycle state like "between tool calls during a
  multi-turn assistant turn." The FSM exposes that directly via
  `gptel-fsm-state`.

## Design pattern
`config/gptel/tools/persistent-agent.org` is the canonical in-repo worked
example of chained FSM handlers. Start from the shape there — don't invent
a parallel structure.

Chaining contract: the handler alist for a state is `(STATE OUR_FN
UPSTREAM_FN ...)`. Upstream's `gptel--fsm-transition` iterates the list in
order; our function runs first and should not return a value the upstream
handler depends on (they're side-effect-only UI updates).

## Verification
- `./bin/tangle-org.sh config/gptel/chat/send.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/send` passes.
- Tests assert:
  - Handler alist has the right shape (our handler before upstream for WAIT
    and TOOL; our-only for TYPE/DONE/ERRS).
  - Each handler fires when the FSM transitions to its state.
  - Buffer-local state indicator tracks the transitions.
  - Our handlers do not invoke `gptel--fsm-transition`.

## Context
- design.md §Decision 3 (upstream FSM via `:fsm` and custom handlers)
- design.md §State-machine taxonomy (table at the top of design.md)
- `config/gptel/tools/persistent-agent.org` — canonical reference pattern
- architecture.md §`gptel-chat-send` (send module)

## Review (2026-04-21, orch-review-1776770835)

Implementation is faithful to Decision 3 shape but **two blocking
findings** were discovered — both design-drift from upstream's contract
that will surface as hard-to-debug silent failures once `send-command`
lands:

1. The handler alist REPLACES (rather than chains) upstream's
   `gptel--handle-post` on DONE/ERRS. Any caller-supplied `:post` hook is
   silently dropped. The persistent-agent reference cited as canonical
   only chains WAIT/TOOL and leaves DONE/ERRS to upstream defaults, so
   this task's pattern deviates.
2. The ABRT state is unhandled. `gptel-abort` transitions the FSM to ABRT
   (upstream `gptel-request.el:2124`); `gptel-chat--lifecycle-state` is
   left stuck at `waiting` / `streaming` / `tool-running` forever,
   wedging the send-guard that `send-command` will install.

Non-blocking findings (cl-struct-p guard looseness, missing INIT/ABRT/
unknown-state test coverage) folded in.

Blocking follow-up: `fsm-handlers-upstream-integration` (stays at
`needs-review` until that task closes; `send-command` remains blocked by
this task).

## Closeout (2026-04-21, orch-review-1776774164)

`fsm-handlers-upstream-integration` reviewed with two non-blocking
findings (folded into separate follow-ups) and closed. Flipping this
task to `done`. `send-command` remains blocked only by `stream-callback`.
