---
name: anti-save-predicates
description: Add workspace-anti-save-predicates defcustom (default skips autosave during minibuffer activity and when a *Backtrace* window is visible) and guard workspace--autosave-current-layout on it. Explicit workspace-save bypasses the predicates. Closes the safety gap that re-enabling autosave-on-tab-switch (in the two-state-layout task) opened.
change: refine-workspaces-two-state-layout
status: ready
relations:
  - "blocked-by:two-state-layout"
---

## Files to modify

- `config/workspaces/workspaces.org` (modify) — add the `workspace-anti-save-predicates` defcustom on the `workspaces` defgroup. Add the helper `workspace--backtrace-visible-p`.
- `config/workspaces/layouts.org` (modify) — guard `workspace--autosave-current-layout` with `run-hook-with-args-until-success` over the predicate list. The guard skips autosave on success (predicate returns non-nil). The explicit `workspace-save` path in `persistence.org` is *not* gated.
- `config/workspaces/test/anti-save-spec.el` (NEW) — three scenarios from the spec delta: minibuffer activity blocks autosave; backtrace visibility blocks autosave; explicit save bypasses the predicates.

## Implementation steps

1. **Defcustom.** In `workspaces.org`, near the other defcustoms (the defgroup is already declared):

   ```elisp
   (defcustom workspace-anti-save-predicates
     '(active-minibuffer-window
       workspace--backtrace-visible-p)
     "List of nullary predicates consulted before any workspace autosave.
   If any predicate returns non-nil, the autosave is skipped silently.
   The explicit `workspace-save' command never consults this list."
     :type '(repeat function)
     :group 'workspaces)
   ```

2. **Backtrace predicate.** In `workspaces.org` (or wherever helpers live):

   ```elisp
   (defun workspace--backtrace-visible-p ()
     "Return non-nil if a *Backtrace* window is visible on the current frame."
     (cl-some (lambda (w)
                (string= (buffer-name (window-buffer w)) "*Backtrace*"))
              (window-list nil 'no-mini)))
   ```

   Reference: `activities--backtrace-visible-p` (`activities.el:1010-1016`).

3. **Guard the autosave.** In `layouts.org`, wrap `workspace--autosave-current-layout`'s body:

   ```elisp
   (defun workspace--autosave-current-layout (&optional slot)
     (let ((slot (or slot :working-state)))
       (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
         ;; ... existing body ...
         )))
   ```

   Note `workspace-anti-save-predicates` is a quoted symbol because `run-hook-with-args-until-success` reads the hook list from the symbol. `active-minibuffer-window` is built-in and nullary.

4. **Explicit save bypass.** `workspace-save` in `persistence.org` already does its capture via a direct call to `workspace--autosave-current-layout :saved-state`. The bypass requires that path *not* consult the predicates. Two ways:
   - Add an internal flag `workspace--skip-anti-save-predicates`, let-bound to `t` in `workspace-save`. The autosave checks it.
   - Split the implementation: a private `workspace--capture-into-slot` that does the work; `workspace--autosave-current-layout` is the guarded wrapper. `workspace-save` calls the private function directly.

   The split is cleaner. Refactor `workspace--autosave-current-layout` into:

   ```elisp
   (defun workspace--capture-into-slot (slot)
     "Capture the current frame into the recent layout-group's SLOT.
   Does NOT consult `workspace-anti-save-predicates'."
     ...)

   (defun workspace--autosave-current-layout (&optional slot)
     "Like `workspace--capture-into-slot' but gated on the anti-save predicates."
     (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
       (workspace--capture-into-slot (or slot :working-state))))
   ```

   `workspace-save` calls `workspace--capture-into-slot :saved-state`. Tab-switch advice, idle save, kill-emacs hook, `workspace-switch-layout`, `workspace-save-layout` all call `workspace--autosave-current-layout`.

5. **Specs**:
   - `anti-save-spec.el`:
     - Minibuffer scenario: `cl-letf` `active-minibuffer-window` to return a sentinel non-nil; assert `workspace--autosave-current-layout` does not mutate the layout's `:working-state`.
     - Backtrace scenario: stub `window-list` (or use `with-temp-buffer`'s display) to put a `*Backtrace*` in the visible list; assert the same no-mutate.
     - Bypass scenario: stub the predicates to all return non-nil; call `workspace-save`; assert `:saved-state` IS mutated and `:working-state` is cleared.
     - Default list shape: assert the value of `workspace-anti-save-predicates` contains exactly `active-minibuffer-window` and `workspace--backtrace-visible-p` (regression on the default).

## Design rationale

See `design.md` §D5. Catalog pattern 5.

The split between `workspace--capture-into-slot` (raw) and `workspace--autosave-current-layout` (guarded) keeps the bypass mechanism mechanical and discoverable. A flag variable would be invisible at the call site; a separate function is grep-able.

The `-predicates` suffix matches activities and matches the Emacs convention for predicate lists (vs. `-functions` for hooks called with arguments). Catalog Q5.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/workspaces.org
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/run-tests.sh -d config/workspaces
```

Specific assertions:

- `grep -n "workspace-anti-save-predicates" config/workspaces/workspaces.el config/workspaces/layouts.el` shows both the defcustom and the guard call.
- `grep -n "workspace--capture-into-slot\|workspace--backtrace-visible-p" config/workspaces/*.el` shows both helpers.
- `grep -n "run-hook-with-args-until-success" config/workspaces/layouts.el` shows the guard application.
- `workspace-save` calls `workspace--capture-into-slot`, not `workspace--autosave-current-layout`. Confirm with `grep -n "workspace--capture-into-slot\|workspace--autosave-current-layout" config/workspaces/persistence.el`.

## Context

- `runtime/straight/repos/activities.el/activities.el:302-310` — defcustom shape.
- `activities.el:580` — guard application via `run-hook-with-args-until-success`.
- `activities.el:1010-1016` — backtrace predicate reference impl.
- `openspec/changes/refine-workspaces-two-state-layout/design.md` §D5.
- `openspec/changes/refine-workspaces-two-state-layout/notes/activities-patterns-catalog.md` pattern 5.
- **Depends on**: `two-state-layout`, because the guard only matters once tab-switch autosave is on. Without that task, this task's predicates would have nothing to skip.
