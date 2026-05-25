---
name: idle-save-mode
description: Add workspaces-mode global minor mode with a workspaces-mode-idle-frequency-seconds idle timer that captures the current workspace's :working-state. Off by default; user opts in. Closes the crash-safety gap.
change: refine-workspaces-two-state-layout
status: done
relations:
  - "blocked-by:two-state-layout"
---

## Register entries cited by this task

- `register/invariant/idle-tick-no-op-off-workspace` (speculated, NEW
  cycle 2). The idle-tick callback must `(when (workspace--current-name)
  ...)`-guard before calling `workspace--autosave-current-layout`.
  Scaffolded failing spec at
  `openspec/changes/refine-workspaces-two-state-layout/scaffolding/
  invariants/idle-tick-no-op-off-workspace.test.el`. The implementor
  may revise the scaffold; satisfying its four scenarios is the
  expected disposition.
- `register/invariant/autosave-never-writes-saved-state` (reconciled).
  Scenario "workspaces-mode idle timer does not modify :saved-state"
  in the scaffolded spec must remain passable.
- `register/boundary/autosave-guard-pipeline` (reconciled). The idle-
  tick is stage 1 entry-point #4 (alongside the three persistence
  advice / hook wraps the `anti-save-predicates` sibling task
  installs). Coordinate as the task body's Cycle 1 updates stanza
  describes.

## Files to modify

- `config/workspaces/workspaces-mode.org` (NEW) — defines `workspaces-mode`, the idle timer, and `workspaces-mode-idle-frequency` defcustom. Auto-tangles to `workspaces-mode.el`.
- `config/workspaces/workspaces.org` (modify) — `require` the new module so it's loaded with the package.
- `config/workspaces/test/workspaces-mode-spec.el` (NEW) — three scenarios from the spec delta.

## Implementation steps

1. **Create `workspaces-mode.org`** with the standard literate-emacs headers:

   ```org
   #+title: Workspaces — Idle save mode
   #+author: Jeff Farr
   #+property: header-args:emacs-lisp :tangle workspaces-mode.el
   #+auto_tangle: y
   ```

   Module-level intro: this is the opt-in background-save mode that gives crash safety on top of the explicit save + tab-switch autosave that come for free.

2. **Defcustom** (in `workspaces-mode.org`):

   ```elisp
   (defcustom workspaces-mode-idle-frequency 60
     "Seconds of idle time before workspaces-mode captures :working-state.
   Set to nil to disable the idle-save trigger while keeping the mode on
   (rare; the explicit save and tab-switch autosave remain active)."
     :type '(choice (number :tag "Seconds") (const :tag "Disabled" nil))
     :group 'workspaces)
   ```

3. **Idle timer state**:

   ```elisp
   (defvar workspaces-mode--idle-timer nil
     "Active idle timer for `workspaces-mode'.")
   ```

4. **The mode**:

   ```elisp
   (define-minor-mode workspaces-mode
     "Global minor mode for periodic background save of workspace working state.
   When enabled, runs an idle timer (interval `workspaces-mode-idle-frequency')
   that captures the current workspace's :working-state. Off by default."
     :global t
     :group 'workspaces
     (when workspaces-mode--idle-timer
       (cancel-timer workspaces-mode--idle-timer)
       (setq workspaces-mode--idle-timer nil))
     (when (and workspaces-mode workspaces-mode-idle-frequency)
       (setq workspaces-mode--idle-timer
             (run-with-idle-timer workspaces-mode-idle-frequency
                                  t
                                  #'workspaces-mode--idle-tick))))
   ```

   Note the `t` second argument: repeat after every idle interval, not just once per idle period.

5. **Idle callback**:

   ```elisp
   (defun workspaces-mode--idle-tick ()
     "Idle-timer body for `workspaces-mode'.
   Captures the current workspace's :working-state, gated on the
   anti-save predicates."
     (when (workspace--current-name)
       (workspace--autosave-current-layout :working-state)))
   ```

   The guard on `workspace-anti-save-predicates` is already inside `workspace--autosave-current-layout` (from the `anti-save-predicates` task). No need to re-check here.

6. **Wire into the loader.** `config/workspaces/workspaces.org` (the entry-point module) `require`s `workspace-mode`:

   ```elisp
   (require 'workspaces-mode)
   ```

   The mode is *not* enabled by the package; the user does that explicitly in init.org if they want it.

7. **Document opt-in.** Add a section to `config/workspaces/docs/README.org` titled "Idle save (opt-in)" explaining:
   - The mode is off by default; enable with `(workspaces-mode 1)` in init.org or `M-x workspaces-mode`.
   - The default interval is 60s; override via `workspaces-mode-idle-frequency`.
   - Anti-save predicates apply (no save during debug or minibuffer activity).
   - The idle save never touches `:saved-state`; the explicit `C-x w S` is still the only way to update the explicit baseline.

8. **Specs (`workspaces-mode-spec.el`)**:
   - **Timer registration**: `cl-letf` mock `run-with-idle-timer` to capture its args; enable the mode; assert it was called with frequency 60 and `workspaces-mode--idle-tick`.
   - **Disable cleanup**: enable, then disable; mock `cancel-timer` and assert it was called with the stored timer.
   - **Idle tick captures working-state**: stub `workspace--current-name` to return `"alpha"`; stub `workspace--autosave-current-layout` to record its arg; invoke `workspaces-mode--idle-tick`; assert `:working-state` was the recorded arg.
   - **Idle tick is no-op off-workspace**: stub `workspace--current-name` to return nil; invoke; assert `workspace--autosave-current-layout` was NOT called.
   - **Idle tick respects anti-save predicates**: enable the mode; stub `workspace--current-name` non-nil; stub the predicate list to all return non-nil; invoke the tick; assert no mutation. (This exercises the existing guard inside `workspace--autosave-current-layout`, not new logic; included as a regression.)

## Design rationale

See `design.md` §D6. Catalog pattern 8.

Why `:global t`: workspaces are a session-level concept, not per-frame or per-buffer. The timer is a single global.

Why 60s default instead of activities.el's 5s: catalog Q4 + design D6. Tab-switch autosave already covers most intra-session state; idle save is purely the crash-safety net. 60s of work is a tolerable loss window; 5s of disk churn is not.

Why opt-in: the v1 MVP user explicitly chose explicit-only persistence. A new background timer is a behavior change the user should consciously enable.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/workspaces-mode.org
./bin/tangle-org.sh config/workspaces/workspaces.org
./bin/run-tests.sh -d config/workspaces
```

Specific assertions:

- `ls config/workspaces/workspaces-mode.el` exists.
- `grep -n "define-minor-mode workspaces-mode" config/workspaces/workspaces-mode.el` shows the mode definition.
- `grep -n "run-with-idle-timer" config/workspaces/workspaces-mode.el` shows the timer registration.
- `grep -n "require 'workspaces-mode" config/workspaces/workspaces.el` shows the loader wire-up.
- `grep -n "workspaces-mode" init.el` returns nothing (the user enables it themselves).

Manual smoke (optional):

```
1. M-x workspaces-mode 1
2. C-x w n alpha
3. C-x 3   (split)
4. Wait 60s untouched.
5. M-: (workspace--layout-working-state
         (workspace--group-recent-layout
          (workspace--find-group (gethash "alpha" workspace--registry) "home")))
6. Expect a non-nil window-state form.
```

## Context

- `runtime/straight/repos/activities.el/activities.el:499-525` — `activities-mode-idle-frequency` and `activities-mode--killing-emacs`.
- `openspec/changes/refine-workspaces-two-state-layout/design.md` §D6.
- `openspec/changes/refine-workspaces-two-state-layout/notes/activities-patterns-catalog.md` pattern 8, Q4.
- **Depends on**: `two-state-layout`. The idle save writes `:working-state`, which only exists after that task lands.
- Independent of `anti-save-predicates` in terms of *implementation* but pairs naturally with it: the idle-save scenario "respects anti-save predicates" is just a regression test, not new logic.

## Cycle 1 updates (2026-05-24)

This task's blocking dependency `two-state-layout` landed in cycle 1
(merge `8a9cb29`). Two adjustments:

- **`workspace--autosave-current-layout` is REQUIRED-SLOT** (cycle 1
  inline-fix `b70a5b4`). The idle-timer callback MUST pass
  `:working-state` explicitly: `(workspace--autosave-current-layout
  :working-state)`. There is no default; an omitted slot raises
  `wrong-number-of-arguments`.

- **Sibling task `anti-save-predicates` adds the predicate-consultation
  wrap** at the autosave entry points. The idle-timer callback in
  `workspaces-mode.org` will be one of those wrap points. Coordinate
  ordering: if `anti-save-predicates` lands first, write the callback
  as `(unless (run-hook-with-args-until-success
  'workspace-anti-save-predicates) (workspace--autosave-current-layout
  :working-state))`. If this task lands first, write the plain
  `(workspace--autosave-current-layout :working-state)` call and let
  the sibling task wrap it in its own diff.

> Cycle 1: required-slot signature pinned by `arch-cycle-20260524-
> 200631-on-touch-two-state-layout-1`. See
> `.orchestrator/cycles/cycle-20260524-200631/reconciliations/
> invariant-autosave-never-writes-saved-state.md`.
