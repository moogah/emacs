---
name: two-state-layout
description: Bump the persistence schema to v2 and split each layout's window-state into :saved-state (explicit) and :working-state (autosave). Re-enable autosave-on-tab-switch (writing only to :working-state, never clobbering explicit saves), add workspace-revert command, make explicit workspace-save clear :working-state. Bundles foundation F1 (schema v2) + improvement B (B1-B3).
change: refine-workspaces-two-state-layout
status: ready
---

## Files to modify

- `config/workspaces/data-model.org` (modify) — change `workspace--layout-make` to construct `(:timestamp T :saved-state W :working-state nil :etc nil)`. Replace `workspace--layout-frameset` with `workspace--layout-saved-state` and `workspace--layout-working-state`. Add `workspace--layout-effective-state` that returns `:working-state` when non-nil else `:saved-state`. Update `workspace--layout-make` callers throughout the package to choose which slot they're writing.
- `config/workspaces/persistence.org` (modify) — bump `workspace--state-version` from 1 to 2. The reader checks `:version` and emits an `*Messages*` notice + returns nil on mismatch. Re-introduce advice on `tab-bar-select-tab` and `tab-bar-switch-to-tab` calling `workspace--autosave-current-layout` for the outgoing workspace (writes `:working-state`). `workspace--kill-emacs-flush` captures the current workspace's `:working-state` before flushing. Add the `workspace-revert` command + `C-x w r` binding.
- `config/workspaces/layouts.org` (modify) — `workspace--autosave-current-layout` now takes a slot argument (`:saved-state` or `:working-state`) and routes the captured state to it. `workspace-save-layout` and `workspace-switch-layout` write `:working-state` for the outgoing slot (matching the autosave model). `workspace--apply-saved-layout` uses `workspace--layout-effective-state`.
- `config/workspaces/workspaces.org` (modify) — bind `C-x w r` to `workspace-revert`. Update the prefix-help string if there's one.
- `config/workspaces/test/data-model-spec.el` (modify) — extend layout-shape specs for the new slots; `workspace--layout-effective-state` precedence.
- `config/workspaces/test/persistence-spec.el` (modify) — version-mismatch notice + nil-return scenario; autosave-on-tab-switch routes through `:working-state`; `kill-emacs-flush` captures `:working-state` before write.
- `config/workspaces/test/layouts-spec.el` (modify) — autosave goes to `:working-state`; explicit save clears `:working-state`; restore prefers `:working-state` then `:saved-state`.
- `config/workspaces/test/revert-spec.el` (NEW) — the four `workspace-revert` scenarios from the spec delta.

## Implementation steps

1. **Schema bump.** In `persistence.org`:
   ```elisp
   (defconst workspace--state-version 2)
   ```
   In `workspace--read-state`, after `read`ing the form, check `(plist-get state :version)`. If it is not 2, emit `(message "Workspaces: ignoring persistence file %s (unsupported :version %S)" file v)` and return nil. The downstream `workspace--restore` already no-ops on nil state.

2. **Layout shape.** In `data-model.org`, change:
   ```elisp
   (defun workspace--layout-make (saved-state &optional timestamp)
     (list :timestamp (or timestamp (time-convert nil 'integer))
           :saved-state saved-state
           :working-state nil
           :etc nil))
   ```
   Rename `workspace--layout-frameset` → `workspace--layout-saved-state`. Add `workspace--layout-working-state` and `workspace--layout-effective-state` (working-then-saved). Audit all callers of the old accessor.

3. **Autosave routing.** In `layouts.org`, change the signature:
   ```elisp
   (defun workspace--autosave-current-layout (&optional slot)
     "Capture current frame into the recent layout-group's SLOT.
   SLOT defaults to :working-state."
     ...)
   ```
   The captured state goes to the named slot via a `plist-put` on the layout. `workspace-save-layout` explicitly passes `:saved-state`. `workspace-switch-layout` (outgoing snapshot) passes `:working-state`. The `kill-emacs-hook` flush passes `:working-state`.

4. **Tab-switch advice.** In `persistence.org`, add advice on `tab-bar-select-tab` (and `tab-bar-switch-to-tab` for completeness) that, *before* the switch fires, calls `(workspace--autosave-current-layout :working-state)` if the current tab is a workspace tab. The `:before` advice variant ensures we capture the outgoing workspace, not the incoming one.

   **Critical**: the advice must be a no-op when the outgoing tab is not a workspace tab (test with `workspace--current-name`). Otherwise it fires on tab switches in unrelated tabs and is purely overhead.

5. **Explicit save clears working state.** `workspace-save` in `persistence.org`:
   ```elisp
   (workspace--autosave-current-layout :saved-state)
   ;; Clear any drift now that we have a new explicit baseline.
   (let* ((ws (gethash ws-name workspace--registry))
          (group-name (workspace--recent-group ws))
          (group (and group-name (workspace--find-group ws group-name)))
          (layout (and group (workspace--group-recent-layout group))))
     (when layout
       (plist-put layout :working-state nil)))
   ```
   Then `workspace--sync-registry-from-bufferlo` and `workspace--flush-state` as today.

6. **Restore precedence.** `workspace--apply-saved-layout` in `persistence.org` calls `workspace--layout-effective-state` instead of the old `workspace--layout-frameset` (the renamed accessor in step 2). The deferred-restore + generation-counter machinery from the `bookmark-reincarnation` task is untouched.

7. **`workspace-revert` command.** New `defun` in `persistence.org`:
   ```elisp
   (defun workspace-revert ()
     "Clear the current workspace's :working-state and re-apply :saved-state."
     (interactive)
     (let ((name (workspace--current-name)))
       (unless name (user-error "Not on a workspaces-managed tab"))
       (let* ((ws (gethash name workspace--registry))
              (group-name (workspace--recent-group ws))
              (group (workspace--find-group ws group-name))
              (layout (workspace--group-recent-layout group)))
         (when layout
           (plist-put layout :working-state nil)
           (workspace--flush-state)
           (workspace--apply-saved-layout name)))))
   ```
   Bind in `workspaces.org` under the `C-x w` prefix as `r`.

8. **Kill-emacs working-state capture.** `workspace--kill-emacs-flush` in `persistence.org`:
   ```elisp
   (defun workspace--kill-emacs-flush ()
     (when workspace--save-timer (cancel-timer workspace--save-timer) ...)
     ;; Capture working-state of current workspace before flushing.
     (ignore-errors (workspace--autosave-current-layout :working-state))
     (ignore-errors (workspace--write-state (workspace--serialize-registry))))
   ```

9. **Specs**:
   - `data-model-spec.el`: layout has both new slots; effective-state precedence: prefers working then saved; nil on both returns nil.
   - `persistence-spec.el`: version-mismatch returns nil + message; tab-switch advice writes `:working-state` not `:saved-state`; kill-emacs-flush captures `:working-state` once.
   - `layouts-spec.el`: explicit save writes `:saved-state` AND clears `:working-state`; switch-layout writes `:working-state` of outgoing.
   - `revert-spec.el`: the four scenarios from the spec delta (Revert restores saved-state; Revert errors off-workspace; Revert with nil working-state is no-op; Revert clears file-on-disk working-state).

## Design rationale

See `design.md` §D3 (schema v2 shape), §D4 (two-state semantics), §D9 (keybindings). The :working-state-clear on explicit save (step 5) is what makes the explicit save "feel" definitive — without it, a `:working-state` from before the explicit save would still be preferred on restart. Catalog patterns 6, 7.

The tab-switch advice has a documented MVP-blocking history (archived task `fix-restoration-roundtrip`, bug 4): in v1 the same advice fired during `tab-bar-close-tab`'s internal switch and clobbered explicit saves with post-kill state. With separate `:saved-state` / `:working-state` slots the clobber is structurally impossible because the advice can only touch `:working-state`. Anti-save predicates (sibling task `anti-save-predicates`) add a second line of defense.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/data-model.org
./bin/tangle-org.sh config/workspaces/persistence.org
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/tangle-org.sh config/workspaces/workspaces.org
./bin/run-tests.sh -d config/workspaces
```

Specific assertions:

- `grep -n "workspace--state-version 2" config/workspaces/persistence.el` shows the bump.
- `grep -n ":saved-state\|:working-state" config/workspaces/data-model.el` shows both slot names used.
- `grep -n "workspace--layout-effective-state" config/workspaces/persistence.el` shows the restore picks via the helper.
- `grep -n "workspace-revert" config/workspaces/persistence.el config/workspaces/workspaces.el` shows the command defined and bound.
- `grep -n "tab-bar-select-tab" config/workspaces/persistence.el` shows the (re-introduced) advice.
- `grep -n "workspace--layout-frameset" config/workspaces/*.el` returns nothing — the old accessor is fully replaced.

## Context

- `runtime/straight/repos/activities.el/activities.el` — `cl-defstruct activities-activity` (lines 73-80, `default` and `last` slots). The MVP wraps this pattern as a list-of-states; we use named slots instead because we never need more than two states per layout.
- `openspec/changes/refine-workspaces-two-state-layout/design.md` §D3, §D4, §D9.
- `openspec/changes/refine-workspaces-two-state-layout/notes/activities-patterns-catalog.md` patterns 6, 7.
- This task **does not** depend on `bookmark-reincarnation` and can land first. If it lands first, the layout slots are renamed but the leaves still hold buffer-name references; reincarnation is added by the sibling task.
- Sibling tasks `anti-save-predicates` and `idle-save-mode` consume this task's autosave path; they depend on it.
