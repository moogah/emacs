---
name: broken-home-tolerance
description: Activation guards in workspace-switch/workspace-restore; add workspace-re-anchor; bind C-x w R
change: add-workspace-home-directory
status: ready
relations: []
---

## Files to modify
- `config/workspaces/tabs.org` (modify — add broken-state guard to `workspace-switch`)
- `config/workspaces/persistence.org` (modify — add broken-state guard to `workspace-restore`)
- `config/workspaces/workspaces.org` (modify — add `workspace-re-anchor` command, `C-x w R` binding)
- `config/workspaces/test/broken-home-runtime-spec.el` (new — Buttercup)

## Implementation steps

1. In `tabs.org`, add a guard at the top of `workspace-switch`:

   ```elisp
   (defun workspace-switch (name)
     "Select the tab for workspace NAME.

   Signals `user-error' if NAME is in a broken state (its :home no
   longer exists on disk). Use `workspace-re-anchor' to point it at a
   new path or `workspace-purge' to remove the registry entry."
     (interactive
      (list (completing-read
             "Switch to workspace: "
             (seq-filter #'workspace--tab-for (workspace--registered-names))
             nil t)))
     (let ((ws (gethash name workspace--registry)))
       (when (and ws (workspace--broken-p ws))
         (user-error
          "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
          name (workspace--home ws))))
     (let ((idx (workspace--tab-index-for name)))
       (unless idx
         (user-error "No tab found for workspace %s" name))
       (tab-bar-select-tab idx)))
   ```

2. In `persistence.org`, add the same guard at the top of
   `workspace-restore` (locate the existing command, which handles
   live-tab-already-exists and create-new-tab branches):

   ```elisp
   (let ((ws (gethash name workspace--registry)))
     (when (and ws (workspace--broken-p ws))
       (user-error
        "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
        name (workspace--home ws))))
   ```

   Insert near the top of `workspace-restore`, after the
   `unless (gethash name ...)` "unknown name" guard.

3. In `workspaces.org`, add `workspace-re-anchor`:

   ```elisp
   (defun workspace-re-anchor (name new-home)
     "Point broken workspace NAME at NEW-HOME on the filesystem.
   Clears the broken-state tag and updates `:home'. If
   `(file-name-nondirectory (directory-file-name NEW-HOME))' differs
   from NAME, the registry entry is renamed to match the new basename
   (per the registry-name-equals-basename invariant).

   The new path must exist and must be a directory."
     (interactive
      (list (completing-read "Re-anchor workspace: "
                             (workspace--registered-names) nil t)
            (file-name-as-directory
             (read-directory-name "New home directory: " nil nil t))))
     (let ((ws (gethash name workspace--registry)))
       (unless ws
         (user-error "No workspace named %s" name))
       (unless (file-directory-p new-home)
         (user-error "Not a directory: %s" new-home))
       (let* ((new-name (file-name-nondirectory
                         (directory-file-name new-home)))
              (next-ws (workspace--clear-broken
                        (workspace--set-home ws new-home))))
         ;; If basename changed, rename the registry key too.
         (cond
          ((string= name new-name)
           (puthash name next-ws workspace--registry))
          (t
           (when (gethash new-name workspace--registry)
             (user-error
              "Cannot rename to %s — a workspace with that name already exists"
              new-name))
           (remhash name workspace--registry)
           (puthash new-name next-ws workspace--registry)
           ;; Update tab label if a live tab exists.
           (let ((tab-idx (workspace--tab-index-for name)))
             (when tab-idx
               (tab-bar-rename-tab new-name (1+ tab-idx))))))
         (workspace--persistence-save)
         (message "Workspace re-anchored: %s → %s" name new-home))))
   ```

4. Add the binding to the workspaces keymap:

   ```elisp
   (define-key workspace-prefix-map (kbd "R") #'workspace-re-anchor)
   ```

5. Create `test/broken-home-runtime-spec.el`:

   - **`workspace-switch` refuses broken**: build a registry entry
     and mark it broken via `workspace--mark-broken`. Create a tab
     (or skip the tab requirement by also stubbing
     `workspace--tab-index-for` to a number — the broken-state guard
     fires before the tab lookup). Invoke `(workspace-switch
     "broken-ws")`. Assert: `user-error` mentioning the missing path
     and naming both remediation commands. Assert no tab selection
     occurred.

   - **`workspace-restore` refuses broken**: similar setup; invoke
     `workspace-restore` on the broken name; assert the same
     `user-error` and no new tab created.

   - **`workspace-re-anchor` happy path (same basename)**: setup
     broken workspace `foo` with `:home /tmp/old-foo/` (gone). Create
     `/tmp/new-foo/` and `/tmp/foo/`. Stub `read-directory-name` to
     return `/tmp/foo/`. Invoke `(workspace-re-anchor "foo")`.
     Assert: registry contains `foo` with `:home` `/tmp/foo/`;
     broken tag cleared; persistence file flushed.

   - **`workspace-re-anchor` rename basename**: broken workspace
     `foo`. Stub `read-directory-name` to return `/tmp/renamed/`.
     Assert registry key is now `renamed` (not `foo`); old key removed.

   - **`workspace-re-anchor` rename collision**: pre-populate `foo`
     (broken) and `bar` (healthy). Try to re-anchor `foo` to a path
     basenamed `bar`. Assert `user-error` and registry unchanged.

   - **`workspace-re-anchor` non-existent target**: stub
     `read-directory-name` to return a path that does not exist.
     Assert `user-error` and registry unchanged.

6. Tangle and test:
   ```bash
   ./bin/tangle-org.sh config/workspaces/tabs.org
   ./bin/tangle-org.sh config/workspaces/persistence.org
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/run-tests.sh -d config/workspaces -p broken-home-runtime-spec
   ```

## Design rationale

Broken-state is a runtime tag (per D5/D6): set on load when `:home`
does not exist, cleared on `workspace-re-anchor`, never persisted.
Activation guards fail fast — better a clear error than a silently
mis-restored workspace pointing at the wrong files.

The rename-on-re-anchor branch upholds the registry-name-equals-basename
invariant from the spec. Otherwise a user could re-anchor `foo` at
`/tmp/bar/` and end up with registry entry `foo` whose `:home`
basename is `bar` — confusing and inconsistent.

The "rename collision" guard prevents accidentally clobbering a
different existing workspace by re-anchoring through its name.

## Design pattern

Error-message-naming-remediation: both guards name the two recovery
commands (`workspace-re-anchor` and `workspace-purge`). This is good
ergonomics — the user sees what's wrong AND how to fix it without
opening the manual.

`tab-bar-rename-tab` is destructive on the index; pair it with the
registry update so the user-visible state is consistent.

## Verification

- Tangle: all three .org files
- Spec: `./bin/run-tests.sh -d config/workspaces -p broken-home-runtime-spec`
- Manual:
  - Create workspace `test-broken` via `workspace-new`.
  - From a shell: `rm -rf ~/emacs-workspaces/test-broken/`.
  - Restart Emacs. Verify `*Messages*` notice naming `test-broken`
    and the missing path.
  - `M-x workspace-switch test-broken` → assert user-error with
    remediation commands named.
  - `M-x workspace-re-anchor test-broken` → pick a fresh dir → verify
    re-anchoring works.

## Context

design.md § Decisions / D5 — broken tag is runtime-only
design.md § Decisions / D6 — broken-state semantics in commands
design.md § Decisions / D10 — Key bindings (C-x w R)
specs/workspaces/spec.md § ADDED "Broken home directory tolerated on restore"
specs/workspaces/spec.md § ADDED "Required home directory and identity coupling" (basename invariant for re-anchor renames)


## Cycle 2 updates (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. Blocker `persistence-schema-v3`
  closed at merge `f21f592`. The persistence-side broken-tag work is
  live: `workspace--persistence-deserialize-state` now sets `:broken`
  when `(file-directory-p :home)` is nil and logs to `*Messages*`. Your
  task's job is the COMMAND-LAYER guards (workspace-switch /
  workspace-restore refuse; workspace-re-anchor / workspace-purge /
  workspace-delete permit) plus the user-facing `workspace-re-anchor`
  command — the persistence half is done.

### Cycle-2 register-diff hits relevant to this task

- `register/invariant/broken-tag-runtime-only`: speculated →
  **reconciled**. Mutations of `:broken` now ALWAYS route through
  `workspace--mark-broken` / `workspace--clear-broken` (cycle 1) and
  the parallel `workspace--mark-restore-pending` /
  `workspace--clear-restore-pending` (cycle 2). Your
  `workspace-re-anchor` implementation MUST use
  `workspace--clear-broken` on success — never inline `(plist-put ws
  :broken nil)`. Cycle-2 architect finding
  `arch-cycle-20260525-213500-03` formalises this contract for ALL
  runtime-only tags. See
  `.orchestrator/cycles/cycle-20260525-213500/reconciliations/invariant-broken-tag-runtime-only.md`.

- `register/invariant/home-required-no-floating-workspaces`:
  re-confirmed cycle 2. **NEW WORK ABSORBED HERE**: architect finding
  `arch-cycle-20260525-213500-04` (advisory, invariant-gap) flagged
  that the invariant requires `:home` be absolute, but no code path
  currently enforces `(file-name-absolute-p home)`. Add an `it`
  clause to your activation-guard spec asserting that a workspace
  whose `:home` is a relative path is treated as broken (or rejected
  at workspace--make / deserializer with a notice). Extend the
  deserializer filter at `persistence.el:83-87` with a
  `(not (file-name-absolute-p home))` arm emitting:
  ```
  "Workspaces: skipping persisted entry %S — :home %S is not absolute"
  ```
  Optionally add a defensive `cl-check-type` or `error` in
  `workspace--make` so the constructor enforces it at every entry
  point. See finding file at
  `.orchestrator/cycles/cycle-20260525-213500/findings/arch-cycle-20260525-213500-04.md`.

- `register/shape/workspace-plist-v3`: re-confirmed cycle 2 (now
  enumerates `:restore-pending` alongside `:broken` per finding 01).
  Your guards consult `workspace--broken-p`; the new
  `workspace--restore-pending-p` predicate is also available but is
  consumed by the activation-restore hook, not by your re-anchor
  command.

### Cycle-2 inline-fix hits

The persistence-side deserializer was inline-fixed at commit
`cd1d721` to route `:restore-pending` through helpers. Your task
does not touch the deserializer directly, but if you read it for
reference your inspiration patterns now show `(workspace--mark-broken ws)`
and `(workspace--mark-restore-pending ws)` instead of inline
`plist-put`.
