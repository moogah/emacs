---
name: persistence
description: Save/restore workspaces to a per-machine elisp form via frameset. Auto-save on layout switch, save on kill-emacs, restore on startup. Missing file is non-fatal.
change: add-workspaces-package
status: blocked
relations:
  - "blocked-by:layout-commands"
  - "blocked-by:buffer-membership"
---

## Files to modify

- `config/workspaces/persistence.org` (new) — disk I/O, save/restore commands, hooks.
- `config/workspaces/workspaces.org` (modify) — load `persistence.el` last; install `kill-emacs-hook`; trigger restore at end of module load.
- `config/workspaces/layouts.org` (modify) — call the disk-save trigger from `workspace--autosave-current-layout` (with debounce; see step 5).
- `config/workspaces/test/persistence-spec.el` (new) — Buttercup behavioral tests.

## Implementation steps

1. In `persistence.org`, define the per-machine path:
   ```elisp
   (defun workspace--state-directory ()
     (let ((subdir (if (and (boundp 'jf/machine-role) jf/machine-role)
                       (concat "state/workspaces/" jf/machine-role "/")
                     "state/workspaces/default/")))
       (expand-file-name subdir jf/emacs-dir)))

   (defun workspace--state-file ()
     (expand-file-name "workspaces.eld" (workspace--state-directory)))
   ```
   Use `.eld` (Emacs Lisp Data) extension consistent with modern serialized-state conventions.
2. Serializer `workspace--serialize-registry`:
   - Walk `workspace--registry` and produce the persistent shape from design.md §D5. Plist with `:version 1`, list of workspaces.
   - For each workspace: name, `:buffer-files` (filter to live file buffers via the workspace plist's `:buffer-files`), `:recent-layout-group`, `:layout-groups` (each with its `:layouts` — list, even though MVP has length 1).
   - For each layout: `:timestamp`, `:frameset` (already a serializable frameset object from frameset-save), `:git-state nil`.
3. Disk write `workspace--write-state`:
   - `make-directory (workspace--state-directory) t`.
   - `(with-temp-file (workspace--state-file) (let ((print-length nil) (print-level nil)) (prin1 form (current-buffer))))`.
   - Use `pp` for readability in debugging — performance impact is negligible since this runs on debounced timers and kill-emacs.
4. Restore `workspace--read-state`:
   - If `(file-exists-p (workspace--state-file))` is nil, return nil (non-fatal; spec Scenario: Missing persistence file is non-fatal).
   - `(with-temp-buffer (insert-file-contents (workspace--state-file)) (read (current-buffer)))`.
   - Tolerate read errors with a `condition-case` that emits an `*Messages*` notice and returns nil (do NOT signal an error to the user beyond `message`).
5. Debounced write trigger `workspace-save-state`:
   - Public command (interactive, prefix-bindable).
   - Also installable as a function for the auto-save hook.
   - Use `run-with-idle-timer` with a small idle delay (e.g. 2 seconds) to coalesce bursts of context switches into one disk write. Cancel any pending timer before scheduling a new one.
6. Restore-on-startup `workspace--restore`:
   - Called once at the end of `workspaces.org` module load (or via a delayed `run-with-idle-timer` from there).
   - For each persisted workspace:
     - Insert plist into `workspace--registry`.
     - `tab-bar-new-tab`; `tab-bar-rename-tab` to the name; set `:workspace-name` parameter.
     - Do NOT immediately restore the frameset — defer until the user first selects that tab (lazy activation). Mark the workspace's `:restore-pending` flag.
     - On first `tab-bar-switch-to-tab` to a `:restore-pending` workspace, restore the recent group's layout via `frameset-restore`, then re-acquire file buffers in `:buffer-files` that exist on disk (issue a quiet `find-file-noselect` for each; bufferlo's display-time membership will pick them up as the user navigates).
   - The active tab at startup time is NOT changed by restore — restore is additive (spec Scenario: Workspaces survive restart).
7. Install `kill-emacs-hook` (synchronous disk write):
   ```elisp
   (add-hook 'kill-emacs-hook
             (lambda ()
               ;; Cancel any pending debounce timer first and flush now.
               (when workspace--save-timer
                 (cancel-timer workspace--save-timer)
                 (setq workspace--save-timer nil))
               (ignore-errors (workspace--write-state (workspace--serialize-registry)))))
   ```
8. In `layouts.org`, modify `workspace--autosave-current-layout` to schedule the debounced disk save after the in-memory mutation lands.
9. Buttercup specs in `persistence-spec.el`, mapping 1:1 to scenarios:
   - **Persistence directory is per-machine** — `let`-bind `jf/machine-role` to a fixture value and assert the resulting path.
   - **Workspaces survive restart** — create two workspaces with multiple layouts each; call `workspace--serialize-registry` and `workspace--write-state` directly; clear `workspace--registry`; call `workspace--restore`; assert tab count, workspace names, recent-layout pointers, and `:buffer-files` round-trip.
   - **Missing persistence file is non-fatal** — point `workspace--state-file` to a non-existent path; call `workspace--restore`; assert no error, registry empty, no tab churn.
   - **Frameset round-trip preserves window topology** — create a workspace, split windows in a particular layout, save, clear, restore, assert window count and the buffers they show.
   - **Debounce coalescing** — call `workspace-save-state` three times in rapid succession; assert disk file is written exactly once (use a counter on `workspace--write-state` via spy).
10. Smoke / manual test:
    ```
    ./bin/emacs-isolated.sh --eval "(progn
      (workspace-new \"alpha\")
      (find-file \"~/.emacs.d/init.el\")
      (workspace-save-layout \"reading\")
      (workspace-save-state)
      (message \"state file: %s exists=%s\" (workspace--state-file) (file-exists-p (workspace--state-file)))
      (kill-emacs))"
    # Then re-launch and confirm 'alpha' tab is back with 'reading' layout active.
    ```

## Design rationale

Per-machine paths (`state/workspaces/<machine-role>/`) follow the same convention `activities` uses today (referenced in `config/core/window-management.org`), keeping state organized by machine identity (design.md §D5).

`frameset.el` is the well-trodden serialization path (used by `desktop.el` and `activities.el`) and handles window topology + buffer references reliably. Non-file buffers are lossy across restarts — documented in spec and design.md §D4. The `:buffer-files` list (file paths only) is the authoritative source of "what to re-acquire on first activation."

Debouncing the disk write avoids one `prin1` per context-switch keystroke when the user is rapidly cycling through layouts. `kill-emacs-hook` cancels the timer and flushes synchronously so nothing is lost on exit.

Lazy restore (deferred until first tab selection) keeps Emacs startup time bounded. The cost is a one-time pause on first selection of each restored workspace — acceptable, and avoidable later with a background pre-warm timer if measurement justifies it.

Story A's kill-buffer semantics interact cleanly: dead buffers are filtered out at serialize time, so a buffer killed before the next save is simply absent from disk — no stale references.

## Verification

- `./bin/tangle-org.sh config/workspaces/persistence.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/tangle-org.sh config/workspaces/layouts.org`
- `./bin/run-tests.sh -d config/workspaces` — all `persistence-spec.el` specs pass; previously-passing specs still pass.
- Smoke flow in step 10: state file appears, second launch restores the `alpha` tab and `reading` layout.
- `grep -n "kill-emacs-hook" config/workspaces/persistence.el` — hook is installed.
- `grep -rn "workspace--write-state\|workspace--read-state" config/workspaces/` — only `persistence.el` defines them; `layouts.el` and `tabs.el` may *call* them via the debounce trigger but not re-define them.

## Context

- design.md §D4 "Persist via `frameset.el` for window configs"
- design.md §D5 "Persistence schema is forward-compatible by design"
- design.md §D7 "Auto-save on context switch; explicit save still available"
- specs/workspaces/spec.md Requirement: Per-machine persistence and restoration
