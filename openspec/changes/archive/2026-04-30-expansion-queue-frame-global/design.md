## Context

This change addresses two `current-buffer`-divergence bugs in the expansion-UI flow that combine to break PersistentAgent tool runs. They share a root cause: the expansion UI runs across multiple buffer contexts (the buffer that fires the prompt, the buffer the user answers from, the buffer the action handler runs in) and several pieces of code read state from `(current-buffer)` rather than from a captured handle.

### Bug 1 — Buffer-local queue and flag

`config/gptel/scope/scope-expansion.org` declares two pieces of state that govern the expansion-UI prompt sequence:

```elisp
(defvar-local jf/gptel-scope--expansion-queue nil ...)
(defvar-local jf/gptel-scope--expansion-active nil ...)
```

Both are buffer-local. The UI itself is a `transient-setup` popup, which is **frame-modal** by construction: it draws once and consumes keystrokes until the user picks an action or quits. Only one transient can be on screen per frame.

The buffer-local design assumed concurrency isolation between sessions. In practice the path that flips the flag (`jf/gptel-scope-prompt-expansion`) and the path that clears it (`jf/gptel-scope--process-expansion-queue`, called from each transient action handler) run in different `current-buffer` contexts:

- `prompt-expansion` is invoked synchronously from a tool wrapper, with `(current-buffer)` bound to the buffer that owns the in-flight `gptel-request` (e.g. an agent's invisible `session.org`).
- The transient menu pops in the frame; the user answers it from wherever they're focused (typically the parent session's overlay buffer, sometimes a different gptel buffer).
- The action handler runs in the focused buffer and calls `--process-expansion-queue` there, which `setq`s `expansion-active` to `nil` *in that buffer* — leaving the original buffer's `t` untouched.

A diagnostic trace (`config/gptel/tools/persistent-agent-trace.org`) captured the failure end-to-end on the agent-test session of 2026-04-30. PersistentAgents make this fatal because `find-file-noselect` opens the agent's `session.org` without ever displaying it; the user can't switch to that buffer to drain the queue, even if they knew it was there.

The drawer-write side of the same expansion already routes via `:chat-buffer` carried on each queue entry — the writer reads it through `jf/gptel-scope--current-chat-buffer`. That mechanism is unaffected by this change and remains the source of truth for "which session does this approval belong to?".

### Bug 2 — Re-validation runs in the wrong buffer

After Bug 1's fix landed, a follow-up trace surfaced a second drift. `jf/gptel-scope-authorize-tool-call`'s wrapper-callback (the lambda passed to `--trigger-inline-expansion`) recurses into `authorize-tool-call` to re-validate after add-to-scope. That callback runs synchronously inside the transient action handler, whose `current-buffer` is the buffer the user clicked from — for PersistentAgents, the parent's overlay buffer, not the agent's session buffer.

`--load-config` reads buffer-local `jf/gptel--branch-dir` to choose which session's drawer to validate against. With the wrong `current-buffer`, re-validation loads the *parent's* drawer, doesn't find the just-added pattern, denies, and triggers another `prompt-expansion` — which captures the parent buffer as `:chat-buffer`. The user re-approves; the writer routes to the parent. Every approved path gets duplicated to both drawers, and the parent accumulates scope it shouldn't have.

Trace evidence: `expansion-prompt buf=<agent>` (correct first prompt) → `expansion-action buf=<main>` (action runs in parent) → `auth-entry buf=<main> args=("ls -la /tmp")` (re-validation in WRONG buffer) → `expansion-prompt buf=<main> QUEUE …` (second prompt, `:chat-buffer = <main>`).

## Goals / Non-Goals

**Goals:**

- Eliminate the buffer-divergence hang class entirely by making queue state frame-global.
- Preserve existing serialization semantics: one prompt at a time, FIFO order, callbacks routed back to the buffer that owns each entry.
- Keep the public surface (`jf/gptel-scope-prompt-expansion`, `--process-expansion-queue`, action handlers, JSON callback shapes) bit-identical so callers (`scope-validation`, `request_scope_expansion`) need no changes.
- Land a regression test that pins the specific agent-buffer / parent-buffer drift this change exists to fix.

**Non-Goals:**

- No changes to the transient menu, action labels, or six-choice layout.
- No changes to the inline-vs-pre-emptive entry-point split.
- No persistent-agent source changes. Persistent agents benefit transitively.
- No changes to drawer routing — `:chat-buffer` per-entry stays the canonical handle.
- No changes to the `request_scope_expansion` LLM tool surface.
- No removal or modification of the diagnostic trace module added on this branch. It stays as standing infrastructure (matches `drawer-trace.el` precedent).

## Decisions

### Decision 0: Both bugs land in one change

The two bugs share a root pattern (`current-buffer` drift across the expansion UI's async resolution). Bug 1 was diagnosed first; Bug 2 surfaced only after Bug 1's fix unblocked the second tool call. Splitting them across two changes would force a second cycle for what is the same conceptual fix — "expansion UI must consistently route state through captured handles, not through `(current-buffer)`". Landing them together also gives both regression specs a single home and avoids leaving the codebase in an intermediate state where Bug 1 is fixed but every PA approval still contaminates the parent's drawer.

### Decision 1: `defvar` over advice-based routing

Two viable shapes were considered for the fix:

- **A — Route per-buffer state through captured `:chat-buffer`.** Wrap each action handler's tail in `(with-current-buffer chat-buffer ...)` so the queue pump operates on the same buffer that flipped the flag. Preserves `defvar-local`.
- **B — Make queue + flag global with `defvar`.** Single source of truth; `(setq …)` from any buffer mutates the global binding.

We chose **B** because:

1. The transient is frame-modal; only one prompt can be in flight per frame regardless of how many sessions exist. Per-buffer queues never enabled actual concurrency — they only created an illusion of isolation that masked the buffer-divergence bug.
2. PersistentAgents make A awkward: the agent buffer is never displayed, so its queue/flag state is invisible. Even with A's correct routing, the conceptual model "expansion state lives in this invisible buffer" is harder to reason about than "expansion state is global, prompts route writes via `:chat-buffer`".
3. A requires every action handler (and any future one) to remember the wrap. Forgetting it reintroduces the bug. B is unforgettable — the flag is global by declaration.
4. Code change for B is two `defvar-local` → `defvar`. A would touch every action handler call-site (≥5 known, more in `--add-to-scope--handle-nil-operation`).

### Decision 2: Per-entry `:chat-buffer` retained

Each queued plist already carries `:chat-buffer`, captured as `(current-buffer)` at `prompt-expansion` time. `jf/gptel-scope--current-chat-buffer` reads it from the active transient's `:scope`. Drawer writers (`--write-pattern-to-drawer`, `--write-provider-to-drawer`) target that buffer. None of that pipeline depends on the queue or active flag being buffer-local — it routes via the plist field, not via `(current-buffer)`. So the writer pipeline is bit-identical after this change.

### Decision 3: Docstring updates document frame-modality

The two `defvar` docstrings need to explicitly call out:

- Frame-modality of the underlying transient (the architectural reason these are global).
- That `:chat-buffer` per-entry is the routing handle for drawer writes.
- That the flag/queue serialize prompts across all sessions / agents in the frame.

This keeps a future reader from re-introducing `defvar-local` because it "looks like a per-session thing".

### Decision 4: Test surface

Two tests pin the new contract under `config/gptel/scope/test/expansion/`:

- **Existing assertion review.** Any existing spec that asserted the buffer-local invariant must be replaced or removed. The trace evidence shows the only callers reading these vars are `prompt-expansion` and `--process-expansion-queue` — tests should now treat them as ordinary global state.
- **Regression spec for buffer-divergence.** New Buttercup spec: simulate two `prompt-expansion` calls from buffer A while focused-action-handler runs in buffer B. Assert that buffer B's queue pump observes buffer A's flag and drains the queue. Use `with-current-buffer` to flip context between calls. Ideally: also assert the drawer write still targets buffer A via the per-entry `:chat-buffer`.

Buttercup is preferred per project policy; existing expansion tests use Buttercup.

### Decision 5: Origin-buffer captured at `authorize-tool-call` entry, not threaded through args

For Bug 2, two implementation shapes were considered:

- **A — Add a `&optional origin-buffer` argument to `authorize-tool-call`.** Callers pass it; the recursive call passes it through.
- **B — Capture `(current-buffer)` at the top of `authorize-tool-call` and use it as origin for the wrapper-callback.**

We chose **B** because:

1. The first call comes from inside the wrapper macro, which by gptel's contract runs in the request's buffer. So `(current-buffer)` at first entry IS the request's owning buffer — exactly what we want as origin.
2. A new arg ripples through the wrapper macro's call signature and every existing caller (production + tests). B is a single-function change.
3. The recursive call doesn't need to thread origin-buffer through — by the time the recursion fires inside `(with-current-buffer origin-buffer …)`, the inner `authorize-tool-call`'s own `(current-buffer)` capture sees origin-buffer correctly. The discipline is local to each invocation.

### Decision 6: Trace module stays

`config/gptel/tools/persistent-agent-trace.org` ships as part of this change. Two reasons: (a) it's the diagnostic that motivated and confirmed the fix; if a similar drift bug surfaces later we want it pre-built. (b) Pattern matches `drawer-trace.el` already in-tree. It's loaded but not auto-installed; activated via `M-x jf/gptel-pa-trace-start`.

## Risks / Trade-offs

**[Risk] Latent reliance on buffer-local-ness elsewhere.** Some other module may bind `jf/gptel-scope--expansion-active` buffer-locally for its own purposes. → **Mitigation**: ripgrep the symbols across the repo before landing; the only sites that should appear are `scope-expansion.org` (the declarations + the prompt + the pump) and any tests. Trace evidence confirms only the prompt sets it and only the pump clears it; any other appearance is already a smell.

**[Risk] Test buffer-pollution across runs.** With `defvar-local` a `kill-buffer` resets state. With `defvar`, the global binding survives. Tests that previously relied on buffer kill to reset must explicitly `(setq jf/gptel-scope--expansion-active nil jf/gptel-scope--expansion-queue nil)` in `before-each`. → **Mitigation**: add a shared fixture in `config/gptel/scope/test/expansion/helpers-spec.el` (or update the existing helpers) so every spec resets cleanly.

**[Trade-off] Loses (theoretical) per-session UI isolation.** Two concurrent sessions both denied at the same instant will now serialize through one global queue rather than two parallel buffer-local queues. This is the same effective UX as today (transient is frame-modal; the second session would block on the first's transient anyway), but the queue-ordering across sessions is now interleaved instead of per-session FIFO. Acceptable: the user sees prompts in the order the underlying tool calls were authorized, which is what they'd expect.

**[Risk] Spec drift.** Anyone reviewing the archived `gptel/scope-expansion` spec history will see a buffer-local invariant that no longer holds. → **Mitigation**: the spec MODIFIED block records the rationale; future readers walk forward, not backward through archive.
