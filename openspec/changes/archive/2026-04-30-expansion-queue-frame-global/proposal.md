## Why

Two `current-buffer`-divergence bugs in the expansion-UI flow combine to break PersistentAgent tool runs.

**Bug 1 — buffer-local queue and flag.** `jf/gptel-scope--expansion-queue` and `jf/gptel-scope--expansion-active` are `defvar-local`, intended for per-buffer concurrency isolation. In practice the transient menu is *frame-modal* — only one prompt can be on screen at a time across the whole frame — so per-buffer queues never enable concurrent UI; they only create a latent bug where the buffer that flips the flag and the buffer that clears it diverge. PersistentAgents make the failure fatal: the agent's `session.org` is opened with `find-file-noselect` and never displayed, so its buffer-local `expansion-active=t queued=N` is unreachable from the user's overlay-driven view.

**Bug 2 — `authorize-tool-call`'s expansion re-entry runs in the wrong buffer.** When the user chooses "add to scope", the dispatcher's wrapper-callback recurses into `jf/gptel-scope-authorize-tool-call` to re-validate against the updated drawer. That callback fires from inside the action handler, whose `current-buffer` is the buffer the user clicked from (the parent's overlay buffer), NOT the buffer that owns the pending tool call (the agent's session buffer). `jf/gptel-scope--load-config` reads buffer-local `jf/gptel--branch-dir`, so re-validation loads the *parent's* drawer instead of the agent's. The just-added pattern isn't there, so re-validation denies; the new prompt fires from the parent buffer, capturing `:chat-buffer = parent`; the writer routes the next add-to-scope into the parent's drawer. Every approved path ends up in both drawers, and the parent accumulates scope it shouldn't have.

We confirmed both bugs with a diagnostic trace (added on this branch). Bug 1's signature: round 1 expansion fires in the agent buffer, the action-handler's queue-pump clears the flag in the parent buffer, round 2 hangs because the agent buffer's flag is still `t`. Bug 2's signature: the `auth-entry` for re-validation runs in `<main>` rather than `<agent>`, the second prompt's `:chat-buffer` is `<main>`, and the writer mutates the parent's drawer.

## What Changes

**Bug 1 fix:**

- Convert `jf/gptel-scope--expansion-queue` and `jf/gptel-scope--expansion-active` from `defvar-local` to `defvar` in `config/gptel/scope/scope-expansion.org`. Update their docstrings to document frame-global serialization.
- Each queue entry continues to carry `:chat-buffer` so drawer writes route to the correct session buffer (no behaviour change for `--current-chat-buffer` or for the writers).
- Update `openspec/specs/gptel/scope-expansion.md` § "Expansion queue serialization": replace the **Queue is buffer-local** scenario with one that contracts frame-global serialization, and explain that `:chat-buffer` per-entry handles drawer routing.

**Bug 2 fix:**

- In `config/gptel/scope/scope-validation.org` § Multi-violation expansion loop, capture `origin-buffer = (current-buffer)` at the top of `jf/gptel-scope-authorize-tool-call` and re-establish it via `(with-current-buffer …)` around the wrapper-callback that fires after the user resolves the expansion UI. This makes on-allow, on-deny, and the recursive `authorize-tool-call` all run in the same buffer the original call entered — so re-validation reads the right session's drawer and any follow-up `prompt-expansion` captures the correct `:chat-buffer`.
- Update `openspec/specs/gptel/scope.md` § "Authorization entrypoint": add a scenario contracting that the expansion-UI re-entry runs in the original entry buffer.

**Tests:**

- Update tests under `config/gptel/scope/test/expansion/` to pin frame-global queue/flag.
- Add a regression spec under `config/gptel/scope/test/validation/` asserting that the recursive `authorize-tool-call` runs with `current-buffer` restored to the original entry buffer (Bug 2).

**Diagnostic infrastructure:** Retain `config/gptel/tools/persistent-agent-trace.org` (added on this branch) as standing infrastructure modelled on `drawer-trace.org`.

No breaking changes to public APIs. The transient menu, action handlers, callback shapes, and writer contracts all stay identical.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gptel/scope-expansion`: queue + active flag become frame-global; the buffer-local concurrency-isolation requirement is replaced with a frame-modal serialization requirement. Per-entry `:chat-buffer` retains the existing drawer-routing semantics.
- `gptel/scope`: add a contracted scenario to "Authorization entrypoint" that the expansion-UI re-entry preserves the original entry buffer's `current-buffer`, so re-validation loads the right session's drawer and any follow-up `prompt-expansion` captures the right `:chat-buffer`.

## Impact

- **Code**:
  - `config/gptel/scope/scope-expansion.org`: two `defvar-local` → `defvar`, docstring updates. All call-sites that read/write the queue/flag stay unchanged because the symbol names don't change; the semantics shift from "current buffer's binding" to "the global binding". `(setq …)` continues to work; only the previously-implicit buffer scoping is removed.
  - `config/gptel/scope/scope-validation.org`: `jf/gptel-scope-authorize-tool-call` captures `origin-buffer` at entry and wraps the expansion wrapper-callback in `(with-current-buffer origin-buffer …)`.
- **Specs**: `openspec/specs/gptel/scope-expansion.md` § Expansion queue serialization rewrite; `openspec/specs/gptel/scope.md` § Authorization entrypoint adds an origin-buffer-preservation scenario.
- **Tests**:
  - `config/gptel/scope/test/expansion/expansion-queue-spec.el`: replace the buffer-local assertion with frame-global ones plus a regression spec for the queue-pump-from-different-buffer drift.
  - `config/gptel/scope/test/validation/authorize-tool-call-spec.el`: add a regression spec asserting the recursive `authorize-tool-call` runs with `current-buffer` restored to the original entry buffer.
- **Persistent-agent**: no source change required. The fix transitively unsticks PA flows. `openspec/specs/gptel/persistent-agent.md` does not contract these flows and does not need a delta.
- **Diagnostic**: `config/gptel/tools/persistent-agent-trace.org` ships as part of this change, registered (but not auto-installed) via `gptel.org`.
