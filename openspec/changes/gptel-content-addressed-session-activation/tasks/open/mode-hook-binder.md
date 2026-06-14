---
name: mode-hook-binder
description: Add a guarded gptel-chat-mode-hook binder that sets the four buffer-local session vars, registers the buffer, and enables autosave using drawer identity and the ancestor-walk session-dir.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:drawer-identity-resolver"
  - "blocked-by:session-dir-ancestor-walk"
  - "blocked-by:magic-mode-alist-activation"
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — add `jf/gptel--bind-session-buffer`; register it on `gptel-chat-mode-hook`.
- `config/gptel/sessions/test/commands/mode-hook-binder-spec.el` (new) — Buttercup specs for binding, the non-session no-op, and branch-dir derivation.

## Implementation steps

1. Write the spec first. Cover:
   - activating `gptel-chat-mode` in a signature-bearing buffer → the four buffer-locals (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`) are set from drawer-resolved identity, the buffer is registered in `jf/gptel--session-registry`, and `jf/gptel-autosave-enabled` is t;
   - activating chat-mode in a buffer with no `:GPTEL_` drawer (scratch chat) → binder is a no-op (no registry entry, no session vars);
   - `jf/gptel--branch-dir` equals `(file-name-directory (buffer-file-name))` — no `../..` walk.
2. Implement `jf/gptel--bind-session-buffer`:
   - Guard: return early unless `(buffer-file-name)` and `(jf/gptel--session-signature-p)` (signature in the buffer) — NOT a path-layout test.
   - `branch-dir` = `(file-name-directory (buffer-file-name))`.
   - `session-id` / `branch-name` via the resolvers (drawer-identity-resolver); `session-type` via `jf/gptel--session-type`.
   - `session-dir` via `jf/gptel--session-dir-from-branch-dir` (session-dir-ancestor-walk).
   - `setq-local` the four vars (after mode activation; the hook fires after `kill-all-local-variables`).
   - Register via `jf/gptel--register-session`; set `jf/gptel-autosave-enabled` t.
   - Wrap registry work in `condition-case` so a registry failure does not abort var-setting (mirror the old auto-init isolation). Do NOT enable `gptel-mode`; do NOT call `gptel--save-state` / `gptel--restore-state`.
3. Register `(add-hook 'gptel-chat-mode-hook #'jf/gptel--bind-session-buffer)`. Ordering: it is an independent hook entry alongside `gptel-chat--apply-declared-preset`; neither depends on the other's success.
4. Tangle `commands.org`; run the new spec.

## Design rationale

Binding belongs to the mode, not to a global file-open hook. The signature guard (drawer carries a `:GPTEL_` key) lets the same hook serve real sessions and scratch chat buffers without a path test. `branch-dir` is the file's own directory — derived, never reverse-engineered. The hook-entry pattern matches the codebase's existing `apply-startup-visibility` / `install-preset-hooks` conventions and the documented reason buffer-local setup must run after the mode body. (design.md §Decision D4; specs `sessions-persistence` Requirement "Content-addressed activation and binding".)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — green, including the non-session no-op.
- Done = opening a session activates+binds via the hook; scratch chat buffers are unaffected.

## Context

design.md § Decision "D4. Binding"; specs `sessions-persistence` Requirement "Content-addressed activation and binding" and "Buffer-local session state".

## Cycle 1 updates (cycle-1781448273)

- The guard is `jf/gptel--session-signature-p` (merged) — drawer-carries-a-:GPTEL_-key, NOT a path
  test; a scratch chat buffer (no drawer) is a no-op by construction. `register/boundary/session-content-signature`
  is **reconciled** and safe to depend on.
- Identity resolution depends on `drawer-identity-resolver` (blocker, not yet built) and session-dir
  on `session-dir-ancestor-walk` (deferred-ready, not yet built) — both still open.

## Cycle 2 updates (cycle-1781451784)

### Cited register entries
- `register/boundary/drawer-first-identity-resolution`: speculated → **confirmed** (resolvers merged 1ec479f). Use `jf/gptel--resolve-session-id` / `jf/gptel--resolve-branch-name` / `jf/gptel--session-type` directly; do not re-derive identity.
- `register/boundary/session-content-signature`: reconciled (cycle-1). The guard `jf/gptel--session-signature-p` is safe to depend on; a scratch chat buffer (no drawer) is a no-op by construction.
- `register/invariant/activation-and-identity-are-content-not-path`: still speculated; this task supplies the BINDING half (no path test in the guard).

### Blocker status
- Two of three blockers are now done: `drawer-identity-resolver` (resolvers) and `magic-mode-alist-activation` (the hook fires via content-addressed activation). **Remaining blocker: `session-dir-ancestor-walk`** (provides `jf/gptel--session-dir-from-branch-dir`, cycle-3). This task unblocks once the walk lands.

## Cycle 3 updates (cycle-1781453946)

### Now fully unblocked — top cycle-4 critical-path pick
- **All three blockers are done:** `drawer-identity-resolver` (cycle-2, 1ec479f), `magic-mode-alist-activation` (cycle-2, 7e524af), and `session-dir-ancestor-walk` (cycle-3, df3dcf2). This task is the natural next critical-path task.

### Cited register entries
- `register/boundary/session-dir-marker-walk`: speculated → **confirmed** (cycle-3). Consume `jf/gptel--session-dir-from-branch-dir` for `session-dir`. **Producer signature is pinned `(branch-dir type)`** — pass the resolved TYPE symbol from `jf/gptel--session-type` (`branch`|`agent`), NOT a drawer-alist. Return value is NOT truename-normalized; compare paths with `file-equal-p`, never `string=`. See `.orchestrator/cycles/cycle-1781453946/reconciliations/boundary-session-dir-marker-walk.md`.
- `register/boundary/drawer-first-identity-resolution`: **caller obligation documented** (cycle-3). When you resolve branch-name, feed the resolver a path that carries the trailing `branches/<branch>/` segment (the session.org file path), not a bare branch-dir — else the fallback regex yields nil for legacy (no-`GPTEL_BRANCH`) sessions.

### Carried meta-discovery (cycle-2, still load-bearing here)
- **Cross-subsystem reference:** this binder is chat-side code calling sessions symbols. Use a forward `declare-function` + call-time resolution (the hook fires only after both subsystems load), **never** a `require`/autoload of sessions from chat — `jf/load-module` loads by absolute path, so a feature-name autoload won't resolve and it would invert the one-directional chat→sessions dependency.
