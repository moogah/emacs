---
name: workspace-delete-and-purge
description: Make workspace-delete unregister-only; add workspace-purge with yes-or-no-p + scope safeguard; bind C-x w D and C-x w P
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:workspace-new-default-path
---

## Files to modify
- `config/workspaces/workspaces.org` (modify — add `workspace-delete`, `workspace-purge`, bindings)
- `config/workspaces/test/workspace-delete-purge-spec.el` (new — Buttercup)

## Implementation steps

1. In `workspaces.org`, add `workspace-delete` (unregister-only). If a
   prior incarnation of `workspace-delete` exists with destructive
   semantics, replace it — the spec is explicit that `workspace-delete`
   is now non-destructive by default:

   ```elisp
   (defun workspace-delete (name)
     "Remove workspace NAME from the registry and close its tab.
   The home directory and all its contents (including .git/, home.org,
   and sessions/) remain on disk untouched. To delete the directory
   too, use `workspace-purge'.

   Interactively, completes over registered workspace names."
     (interactive
      (list (completing-read "Delete workspace (unregister only): "
                             (workspace--registered-names) nil t)))
     (unless (gethash name workspace--registry)
       (user-error "No workspace named %s" name))
     (let ((tab-idx (workspace--tab-index-for name)))
       (when tab-idx
         (tab-bar-close-tab (1+ tab-idx))))    ;; tab-bar is 1-indexed
     (remhash name workspace--registry)
     (workspace--persistence-save)              ;; synchronous flush
     (message "Workspace %s unregistered (home directory left on disk)" name))
   ```

2. Add `workspace-purge` with the safeguard:

   ```elisp
   (defun workspace-purge (name)
     "Unregister workspace NAME and recursively delete its home directory.
   Prompts for `yes-or-no-p' confirmation showing the absolute path.

   Refuses to operate when :home is not a descendant of
   `workspaces-default-parent-directory' unless a prefix argument is
   supplied. This guards against accidentally purging an anchored
   external project (e.g., ~/code/myproj/).

   For broken-state workspaces (`:home' missing on disk), no
   filesystem deletion is attempted — the registry entry is still
   removed."
     (interactive
      (list (completing-read "Purge workspace (DELETES HOME DIR): "
                             (workspace--registered-names) nil t)))
     (let* ((ws (gethash name workspace--registry))
            (home (and ws (workspace--home ws))))
       (unless ws
         (user-error "No workspace named %s" name))
       (unless home
         (user-error "Workspace %s has no :home (data corruption?)" name))
       ;; Scope safeguard — refuse to nuke a dir outside the default parent
       ;; unless the user explicitly opts in via prefix arg.
       (unless (or current-prefix-arg
                   (file-in-directory-p home workspaces-default-parent-directory))
         (user-error
          "Refusing to purge %s; outside %s. Use `C-u M-x workspace-purge' to override."
          home workspaces-default-parent-directory))
       ;; Final confirmation.
       (unless (yes-or-no-p (format "Recursively delete %s? " home))
         (user-error "Cancelled"))
       ;; Unregister first; even if rm fails the registry is clean.
       (let ((tab-idx (workspace--tab-index-for name)))
         (when tab-idx
           (tab-bar-close-tab (1+ tab-idx))))
       (remhash name workspace--registry)
       (workspace--persistence-save)
       ;; Filesystem delete (best-effort; broken-home case has nothing to do).
       (when (file-directory-p home)
         (delete-directory home t))
       (message "Workspace %s purged" name)))
   ```

3. Add key bindings to the workspaces keymap (locate the existing
   keymap setup — likely a `define-prefix-command` followed by
   `define-key` calls under `C-x w`):

   ```elisp
   (define-key workspace-prefix-map (kbd "D") #'workspace-delete)
   (define-key workspace-prefix-map (kbd "P") #'workspace-purge)
   ```

   Uppercase deliberately, per design D10 — keeps lowercase prefix
   space for buffer-membership commands and visually flags destructive
   intent.

4. Create `test/workspace-delete-purge-spec.el`. Patterns:

   **workspace-delete**:
   - Setup: create a workspace via `workspace-new` (or build a
     registry entry + tab manually) with a tmp home dir containing
     real files.
   - Invoke `(workspace-delete "name")`. Assert: registry no longer
     contains the entry; tab is closed; home dir on disk is unchanged
     (`directory-files` count unchanged; `home.org` content
     unchanged).
   - `user-error` when name is not registered.

   **workspace-purge happy path**:
   - Setup: workspace whose `:home` is inside the tmp parent dir.
     Stub `workspaces-default-parent-directory` to the tmp parent.
     Stub `yes-or-no-p` to return t.
   - Invoke. Assert: registry no longer contains the entry; home dir
     no longer exists on disk; persistence file no longer mentions it.

   **workspace-purge cancellation**:
   - Stub `yes-or-no-p` to return nil. Invoke. Assert: `user-error`
     "Cancelled"; registry unchanged; home dir still exists.

   **workspace-purge external-dir safeguard**:
   - Setup: workspace anchored at `/tmp/external/...` (outside
     `workspaces-default-parent-directory`). No `current-prefix-arg`.
     Invoke. Assert: `user-error` mentions the safeguard; NO
     `yes-or-no-p` prompt fires; registry and disk unchanged.

   **workspace-purge external-dir with prefix arg**:
   - Same setup; bind `current-prefix-arg` to `'(4)` (one C-u). Stub
     `yes-or-no-p` to t. Invoke. Assert: succeeds; dir deleted.

   **workspace-purge broken-home**:
   - Setup: registry entry whose `:home` points at a path that does
     not exist. `current-prefix-arg` set so safeguard passes (or move
     the path inside the default parent). Stub `yes-or-no-p` to t.
     Invoke. Assert: no error; registry entry removed; no attempt
     made to delete a nonexistent path.

5. Tangle and test:
   ```bash
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/run-tests.sh -d config/workspaces -p workspace-delete-purge-spec
   ```

## Design rationale

Splitting destruction across two commands matches the spec's "safe
default, explicit destructive command" contract. The cost is one extra
command to remember; the benefit is that `M-x workspace-delete` never
loses data. Users coming from prior `kill-buffer`-style
mental-models get the safe behavior by default.

The scope safeguard (refuse to purge outside
`workspaces-default-parent-directory` without a prefix arg) protects
the anchored-existing-project case: someone anchors `~/code/myproj/`
and later `M-x workspace-purge`s it without thinking — without the
safeguard, that wipes a real project. The prefix-arg override gives a
deliberate two-step opt-in.

Unregister-first / delete-after: if `delete-directory` errors (e.g.,
permissions, busy file), the registry is already clean, so the
workspace appears removed from the user's perspective and they can
investigate the leftover files separately. Inverse order (delete then
unregister) could leave the registry referencing a half-deleted dir
which is worse.

Uppercase key bindings (`D`, `P`) flag destructive intent visually and
keep lowercase free for future non-destructive ops. `C-x w D` aligns
loosely with `M-x dired-do-flagged-delete` (D for delete) and is
distinct from any buffer-membership command.

## Design pattern

`tab-bar-close-tab` takes a 1-indexed argument. `workspace--tab-index-for`
returns 0-indexed (or nil). `(1+ tab-idx)` is the bridge — write a
small comment if it would surprise a future reader.

`file-in-directory-p`: Emacs built-in, returns t if the first arg is a
descendant of the second. Both args should be expanded.

`current-prefix-arg` inside an interactive function: available without
declaring it in the `interactive` spec. Bind it directly in tests via
`let`.

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/workspaces.org`
- Spec: `./bin/run-tests.sh -d config/workspaces -p workspace-delete-purge-spec`
- Manual:
  - `M-x workspace-new test1` → `M-x workspace-delete test1` → verify
    `~/emacs-workspaces/test1/` still exists.
  - `M-x workspace-purge test1` → confirm yes → verify dir is gone.
  - `C-u M-x workspace-new` anchored at `/tmp/external-test/` →
    `M-x workspace-purge external-test` → verify safeguard message
    and that the dir is unchanged.

## Context

design.md § Decisions / D8 — workspace-purge safeguard implementation
design.md § Decisions / D10 — Key bindings (C-x w D, P)
specs/workspaces/spec.md § ADDED "workspace-delete is unregister-only by default"
specs/workspaces/spec.md § ADDED "workspace-purge as the destructive deletion command"
