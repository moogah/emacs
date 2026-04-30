---
name: decouple-auto-init-state-from-preset-application
description: Tighten condition-case granularity in jf/gptel--auto-init-session-buffer so a failing preset-application does not abort the buffer-local session-state setup
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:final-verify-and-archive-prep
---

## Cites register entries

- Boundary: `scope-config-loader` (auto-init is the entry point that bridges drawer-resident config into the running buffer; today a preset-application failure leaves the loader's downstream consumers — session-id, branch-dir, registry — uninitialized)

## Files to modify

- `config/gptel/sessions/commands.org` — restructure the `condition-case` inside `jf/gptel--auto-init-session-buffer` (around line 363) so mode activation has its own narrow error handler, and the four `setq-local` calls plus the registry/symlink work always run when path-detection + directory-validation succeeded.
- `config/gptel/sessions/test/` — add a regression spec covering the "drawer declares a preset that errors at apply time" case.

## Implementation steps

1. **Restructure `--auto-init-session-buffer`.** Within the existing `(when session-type ...)` / validation `when`, replace the broad `condition-case` with a narrower one around the mode-activation step. The four buffer-local `setq-local` calls and the registry/symlink work then always run when validation passed:

   ```elisp
   (when session-type
     (when (if (eq session-type 'branch)
               (and (jf/gptel--valid-session-directory-p session-dir)
                    (jf/gptel--valid-branch-directory-p branch-dir))
             (jf/gptel--valid-branch-directory-p branch-dir))

       (jf/gptel--log 'debug "Auto-initializing %s session: %s/%s"
                      session-type session-id branch-name)

       ;; Mode flip is its own concern. A failing preset (e.g. one that
       ;; references a tool not registered in this build) must not abort
       ;; the buffer-local state-setting that follows. Mode is left in
       ;; whatever state mode-activation reached; preset is NOT applied;
       ;; session vars ARE set.
       (condition-case err
           (jf/gptel--ensure-mode-once)
         (error
          (jf/gptel--log 'warn
                         "Mode activation/preset failed for %s/%s: %s; \
session vars set, preset NOT applied"
                         session-id branch-name (error-message-string err))))

       ;; Buffer-local session vars: always set if validation passed.
       ;; These do not depend on preset application succeeding.
       (setq-local jf/gptel--session-id session-id)
       (setq-local jf/gptel--session-dir session-dir)
       (setq-local jf/gptel--branch-name branch-name)
       (setq-local jf/gptel--branch-dir branch-dir)

       ;; Registry registration + current-symlink update: same scope as
       ;; before — wrapped so they cannot abort one another, but
       ;; independent of preset-application state.
       (condition-case err
           (progn
             ;; existing registry registration
             ;; existing current-symlink update (skipped for legacy flat layout)
             )
         (error
          (jf/gptel--log 'warn "Post-init registry/symlink work failed for %s/%s: %s"
                         session-id branch-name (error-message-string err))))

       (jf/gptel--log 'info "Auto-initialized %s session: %s/%s"
                      session-type session-id branch-name)))
   ```

   Preserve the existing log strings used by other callers / tests that may grep them. Confirm the existing behaviour for the legacy flat agent layout is unchanged.

2. **Add regression spec.** New buttercup spec at `config/gptel/sessions/test/auto-init-resilience-spec.el`:

   - **Setup**: tmpdir-rooted session layout with a `:GPTEL_PRESET: <test-broken-preset>` drawer where `<test-broken-preset>` is registered to reference an unregistered tool name. Use `cl-letf` to install the broken preset on `gptel--known-presets` for the duration of the spec.
   - **Action**: `find-file` the `session.org` (which fires `find-file-hook` and runs `--auto-init-session-buffer`).
   - **Assert**: after `find-file` returns, the buffer's `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir` are all populated; `major-mode` is `gptel-chat-mode`. (The preset is not applied — that's expected — but session detection survived.)
   - **Negative control**: with a known-good preset, `find-file`, assert the same four buffer-locals are set AND the preset's tool list is reachable via `gptel-get-preset`.
   - **Independent of**: any specific tool name. The test should construct a synthetic broken preset that errors deterministically at apply time so the spec doesn't depend on whether `TodoWrite` exists.

3. **Run the suite.**

   ```bash
   ./bin/run-tests.sh -d config/gptel/sessions
   make test
   ```

## Design rationale

This change introduced the drawer-resident-preset path (Decisions 5/6/9 in `design.md`). Pre-change, `:GPTEL_PRESET:` lived in `metadata.yml` and was applied via a different code path. Post-change, mode activation in auto-init is what triggers preset application from the drawer (via `gptel-chat-mode-hook` running `gptel-chat--apply-declared-preset`).

That entanglement coupled two concerns that should be independent:

1. **Making the buffer recognize itself as a session** — essential, safe, depends only on path-shape and directory existence.
2. **Applying its declared preset** — fallible for many reasons unrelated to session-detection (missing tool, removed provider, schema drift, partial registration on init failure).

The current `condition-case` is too coarse: it wraps both, so a preset-application error swallows session detection as collateral damage. Once buffer-locals are nil, downstream consumers (`jf/gptel-persistent-agent--task`'s parent-session check, scope-validation lookups via `jf/gptel--session-dir`, the session registry) all behave as if the buffer is not a session at all.

The fix is structural: narrow the `condition-case` so only the mode-flip-and-preset-application step is wrapped, and the buffer-local `setq-local` work always runs when validation passed. A preset that fails to apply is an *advisory* condition (worth logging at WARN), not a *fatal* condition for session detection.

## Design pattern

Same shape as the change-wide pattern: distinguish failure modes by their blast radius. Drawer-detection failure (path doesn't match, dir doesn't validate) → silently no-op (existing behaviour). Mode/preset-application failure → log WARN, do not abort. Buffer-local session vars → always set when path-detection succeeded.

## Verification

- `./bin/run-tests.sh -d config/gptel/sessions` — all specs pass, including the new resilience spec.
- Manual: with the executor preset's `TodoWrite` reference still missing (per the existing `.tasks/` ticket), kill and reopen any executor-preset session.org. Confirm via eval:

   ```elisp
   (with-current-buffer (get-buffer "session.org")
     (list :session-id   jf/gptel--session-id
           :session-dir  jf/gptel--session-dir
           :branch-dir   jf/gptel--branch-dir
           :major-mode   major-mode))
   ```

   All four are populated and `major-mode` is `gptel-chat-mode`. `*Messages*` carries a single WARN line about preset application failing; no ERROR line about auto-init aborting.
- Downstream check: from that reopened buffer, dispatch `PersistentAgent` with a clean preset (e.g. `system-explorer`) and `allowed_paths`. The agent spawns successfully — no hung state in the parent. (Independent of `harden-persistent-agent-callback-on-error`, which protects against a different class of trigger.)

## Context

Discovered during smoke testing of `final-verify-and-archive-prep`. Symptom chain:

1. Parent buffer has `:GPTEL_PRESET: executor` in its drawer.
2. Buffer is killed and reopened from disk.
3. `find-file-hook` fires `--auto-init-session-buffer`.
4. `--ensure-mode-once` activates `gptel-chat-mode`; the mode hook applies the executor preset.
5. Executor's tool list references `TodoWrite`, which is not registered in this build (separately tracked in `.tasks/`).
6. `gptel-chat--apply-declared-preset` errors with "Cannot find tool 'TodoWrite'".
7. The error propagates up through auto-init's `condition-case`, which catches and logs `[GPTEL-ERROR] Failed to auto-initialize branch session: ...`.
8. The four `setq-local` calls never run.
9. Buffer-local session state is nil.
10. PersistentAgent dispatch later user-errors on the parent-session check; no `condition-case` to translate to callback (see `harden-persistent-agent-callback-on-error`); parent FSM hangs.

This task addresses link 7-9 of the chain — the auto-init function should not lose session-state as collateral damage of an unrelated preset failure. Even after the missing-tool issue is fixed, the same class of bug could recur for any preset that fails to apply for any reason; this fix is the durable one.

The complementary task `harden-persistent-agent-callback-on-error` addresses link 10 — the silent-hang amplifier.

Together these two tasks block `final-verify-and-archive-prep`.
