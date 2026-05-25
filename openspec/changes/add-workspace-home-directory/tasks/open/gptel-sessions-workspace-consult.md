---
name: gptel-sessions-workspace-consult
description: Add workspace-sessions-dir function; modify gptel sessions to consult via featurep; add prefix-arg force-global escape hatch
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:add-home-slot-to-data-model
---

## Files to modify
- `config/workspaces/workspaces.org` (modify — export `workspace-sessions-dir` function)
- `config/gptel/sessions/*.org` (modify — the module containing the session-dir resolution / new-session entry point; locate during apply)
- `config/workspaces/test/gptel-integration-spec.el` (new — Buttercup; tests the workspaces side of the contract)
- `config/gptel/sessions/test/workspace-routing-spec.el` (new — Buttercup; tests the gptel side)

## Implementation steps

1. **Locate the gptel session-dir resolution.** Search
   `config/gptel/sessions/` for the function that computes where a new
   session file goes. Likely candidates: a `gptel-sessions-directory`
   defcustom referenced inside a `gptel-sessions-new` or
   `gptel-sessions-create` function. Identify:
   - The variable / function that resolves the target directory.
   - The user-facing command that creates a new session.

   Document the actual function names you find inline at the top of
   this task as you work — design.md flags this as Open Question Q1.

2. **In `config/workspaces/workspaces.org`**, add the exported
   workspace-consult function:

   ```elisp
   (defun workspace-sessions-dir ()
     "Return the sessions/ directory of the current workspace, or nil.
   Returns nil when no workspace is the current tab's owner, or when
   the workspace's :home is missing/broken."
     (when-let* ((name (workspace--current-name))
                 (ws   (gethash name workspace--registry))
                 ((not (workspace--broken-p ws)))
                 (home (workspace--home ws)))
       (let ((dir (expand-file-name "sessions" home)))
         (when (file-directory-p dir)
           dir))))
   ```

   Notes:
   - Returns nil rather than signalling — gptel needs a value-or-nil
     to fall through cleanly.
   - Broken-state check: do not route into a broken workspace.
   - `file-directory-p` check guards against the rare case where
     `sessions/` was deleted by the user after creation.

3. **In the located gptel sessions module**, modify the session-dir
   resolver to consult the workspace function. Pattern:

   ```elisp
   (defun gptel-sessions--target-dir (&optional force-global)
     "Return the directory where a new session file should be created.
   When a workspace is active and FORCE-GLOBAL is nil, returns the
   workspace's sessions/ dir. Otherwise returns the global default."
     (or (and (not force-global)
              (featurep 'workspaces)
              (fboundp 'workspace-sessions-dir)
              (workspace-sessions-dir))
         gptel-sessions-default-directory))   ;; or whatever the existing var is
   ```

   (Replace `gptel-sessions-default-directory` with the actual
   variable name discovered in step 1.)

4. **Add the prefix-arg escape hatch** to the user-facing
   session-creation command:

   ```elisp
   (defun gptel-sessions-new (&optional force-global)
     "Create a new gptel session file.
   With prefix arg FORCE-GLOBAL, write to the global session directory
   even when a workspace is active."
     (interactive "P")
     (let ((dir (gptel-sessions--target-dir force-global)))
       ;; ... existing creation logic, now using DIR
       ))
   ```

   Adapt to the actual existing command shape. The two changes:
   `interactive "P"` for the prefix arg, and routing through
   `gptel-sessions--target-dir`.

5. **Create `config/workspaces/test/gptel-integration-spec.el`** —
   tests for the workspaces side (no dependency on gptel being loaded):

   - `workspace-sessions-dir` returns nil when not on a workspace tab
     (stub `workspace--current-name` to return nil).
   - Returns nil when on a broken workspace.
   - Returns `<:home>/sessions/` for a healthy workspace whose
     sessions dir exists.
   - Returns nil when the workspace exists but `sessions/` was
     deleted (`file-directory-p` false).

6. **Create `config/gptel/sessions/test/workspace-routing-spec.el`** —
   tests the gptel side of the contract:

   - With `workspaces` loaded and a current workspace: new session
     file lands under `<:home>/sessions/`. Use a real tmp workspace
     created via `workspace-new` or build the registry/tab manually.
   - With prefix arg (`force-global=t`): new session file lands in
     `gptel-sessions-default-directory` even when on a workspace.
   - With `workspaces` NOT loaded (`unfeaturep`): new session file
     lands in the global default. Simulate by `cl-letf` on `featurep`
     to return nil for `'workspaces`.
   - On a tab not owned by a workspace: new session file lands in
     global default.

7. Tangle and test both sides:
   ```bash
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/tangle-org.sh config/gptel/sessions/<changed-file>.org
   ./bin/run-tests.sh -d config/workspaces -p gptel-integration-spec
   ./bin/run-tests.sh -d config/gptel/sessions -p workspace-routing-spec
   ```

## Design rationale

Soft dependency via `(featurep 'workspaces)` keeps the gptel sessions
module loadable in configurations without workspaces. The dependency
direction is one-way: gptel optionally consults workspaces; workspaces
never references gptel. This matches the project's general
loose-coupling pattern for cross-subsystem features.

Returning nil (rather than signalling) from `workspace-sessions-dir`
lets gptel use a clean `(or workspace-dir global-default)` fallback.
The function does its own validity checks (broken, missing sessions/)
so callers don't have to.

The prefix arg as the escape hatch matches Emacs convention — users
already understand `C-u M-x command` as "do the thing differently."
Naming it `force-global` in the function signature makes the call site
self-documenting.

## Design pattern

Cross-module integration via published function: workspaces exports
`workspace-sessions-dir` as a stable contract; gptel calls it. This
keeps the per-module interfaces narrow and visible.

`fboundp` check in addition to `featurep`: defensive for the case where
the feature is provided but the specific function was renamed in a
future workspaces version. Cheap and prevents nondescript "void
function" errors.

## Verification

- Tangle: both `.org` files.
- Specs: both new spec files pass.
- Full test runs: `./bin/run-tests.sh -d config/workspaces` and
  `./bin/run-tests.sh -d config/gptel/sessions` — no regressions.
- Manual:
  - On a workspace tab: `M-x gptel-sessions-new` → assert the new file
    lives under `<home>/sessions/`.
  - `C-u M-x gptel-sessions-new` → assert it lives in the global dir.
  - On a non-workspace tab (e.g., a tab created via `M-x tab-bar-new-tab`
    that is not registered as a workspace): `M-x gptel-sessions-new`
    → assert global dir.

## Context

design.md § Decisions / D3 — gptel integration: soft dependency via featurep + named function
design.md § Open Questions Q1 — locate exact gptel session-dir resolution function during apply
specs/workspaces/spec.md § ADDED "Workspace-aware gptel session creation"
specs/workspaces/spec.md § ADDED "Filesystem-authoritative session inventory"
