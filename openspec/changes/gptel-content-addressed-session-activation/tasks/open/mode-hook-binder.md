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
