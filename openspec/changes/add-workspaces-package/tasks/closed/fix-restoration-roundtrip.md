---
name: fix-restoration-roundtrip
description: Restoration of a saved workspace did not restore its window configuration. Two independent bugs (tab-bar dropping custom params; frameset wrong primitive for per-tab) were diagnosed and fixed in commit 83e7c1e. Buttercup specs cover the round-trip; manual GUI smoke deferred per user judgment 2026-05-24.
change: add-workspaces-package
status: done
relations:
  - "discovered-from:persistence"
  - "discovered-from:layout-commands"
---

## What the user saw

After implementing tasks 1–7 (everything except the cutover) and trying the
package by hand:

1. Created a workspace `test` and arranged two windows in it.
2. Invoked `workspace-switch` and typed `test` — got "match needed" (the
   minibuffer rejected the input as not in the candidate list).
3. Quit Emacs, relaunched. The `test` tab reappeared in the tab bar
   (so the *registry* was restored) but selecting it showed no buffers
   from the saved layout — the saved frameset was effectively lost.

State file inspection: `state/workspaces/personal/workspaces.eld` exists,
~2.8 KB, contains a `:version 1` plist with the `test` workspace, its
`home` layout-group, and a non-trivial `:frameset` blob. So the *write*
side is working.

## Bug A — tab parameter dropped by tab-bar (DIAGNOSED, fix attempted, not yet verified)

### Root cause

Both `tab-bar-select-tab` and `tab-bar--current-tab-make` (Emacs 30,
`/Applications/Emacs.app/Contents/Resources/lisp/tab-bar.el.gz` lines
1352–1424) rebuild the current-tab alist from scratch when switching
tabs. The reconstruction copies only these keys from the source tab:

- `name`
- `explicit-name`
- optionally `group`

Any custom parameter (we had been using `workspace-name`) is *dropped
on every tab switch*. Our advices read `workspace--current-name`, which
became `nil` immediately after any switch, so:

- `workspace-switch`'s completing-read candidate list (built from
  `(seq-filter #'workspace--tab-for (workspace--registered-names))`)
  became empty — hence "match needed".
- The lazy-activation advice (`workspace--persistence-after-tab-switch`)
  saw `workspace--current-name = nil` and never called
  `workspace--activate-pending-workspace`.

### Attempted fix (commit not yet made)

Pivoted the discriminator from a custom tab parameter to the tab's
`name` slot, which tab-bar *does* preserve. A tab is "owned by
workspaces" iff its `name` is a key in `workspace--registry`. Changes
landed in this branch:

- `config/workspaces/tabs.org` — rewrote the *Tab identity helpers*
  section. `workspace--tab-workspace-name` now looks up
  `(cdr (assq 'name tab))` and returns it only when the registry
  contains that key. `workspace--ensure-current-tab-tagged` and
  `workspace--set-tab-workspace-name` were removed; they served only
  to attach the now-doomed `workspace-name` parameter.
- `workspace-new` now puthashes the registry entry *before* invoking
  the home-builder, so `workspace--current-name` resolves correctly
  from inside the builder.
- `config/workspaces/persistence.org` — `workspace--restore-tabs`
  no longer attempts to set the `workspace-name` parameter.
- Tests updated to use `workspace--tab-name` instead of asserting on
  the dropped parameter. A regression spec was added to `tabs-spec.el`
  ("survives a tab-bar-select-tab round-trip") that creates two
  workspaces and bounces between them, expecting
  `workspace--current-name` to remain correct after each switch.
- All 58 Buttercup specs pass.

### Not yet verified

The user reported "still the same behavior" after the fix was applied
in-session. It is unclear whether they reloaded the module / restarted
Emacs after the change. The next session should:

1. Restart Emacs (clean slate; persistence file already contains
   workspace `test`).
2. Confirm the `test` tab is recreated by restore.
3. Run `M-: (workspace--current-name)` while on that tab — expect
   `"test"`. If it returns `nil`, bug A is *not* actually fixed, and
   the next step is to add a `message`-trace to
   `workspace--persistence-after-tab-switch` and `workspace--tab-workspace-name`
   to find out which side is returning nil.
4. Try `C-x w s` (`workspace-switch`); the completion list should
   include `test`.

## Bug B — frameset-restore is the wrong primitive for per-tab restore (DIAGNOSED, NOT YET FIXED)

This is the more likely explanation for "tab is back but no buffers
open" if bug A is in fact addressed.

### Why frameset is wrong here

`config/workspaces/layouts.org` (the *Frame ↔ frameset serialization*
section) uses:

- `frameset-save (list (selected-frame))` to capture
- `frameset-restore frameset :reuse-frames t :cleanup-frames t` to restore

`frameset.el` is designed to round-trip *entire Emacs sessions*
(`desktop.el` is its primary client). On restore it identifies frames
by `frameset--id` and either reuses, creates, or deletes whole frames.
It is not a per-tab window-configuration primitive.

What tab-bar itself uses for per-tab window state is
`window-state-get` / `window-state-put` (visible in `tab-bar.el`'s
`tab-bar--tab`, line 1379: `(ws . ,(window-state-get ...))`). That
operates on the selected frame's root window without frame-level
side effects — exactly what we want.

Consequence with frameset: when `workspace--activate-pending-workspace`
runs, `frameset-restore` doesn't find a frame with the saved
`frameset--id` (it never will, since we restarted Emacs and the id
is per-session), so its decision is "create a new frame from scratch
and/or delete the existing one." In a tab-bar context that translates
to "do nothing useful for this tab" — the current tab's window
configuration is unchanged, the user sees no buffers from the saved
layout.

### Proposed fix

Switch the per-layout serializer from frameset to window-state:

```elisp
(defun workspace--capture-window-config ()
  "Return a serializable window-state of the selected frame's root window."
  (window-state-get (frame-root-window) 'writable))

(defun workspace--restore-window-config (state)
  "Apply window-state STATE to the selected frame's root window."
  (window-state-put state (frame-root-window) 'safe))
```

Then:

- `workspace--layout-make` stores the result of `window-state-get`
  in the `:frameset` slot (rename to `:window-state` for clarity;
  bump `:version` to 2 only if we care about back-compat with the
  existing test state file — for MVP the user accepted "delete the
  state file" as an acceptable migration).
- `workspace--activate-pending-workspace` and `workspace-switch-layout`
  call `window-state-put` instead of `frameset-restore`.
- `workspace--sanitize-frameset` becomes a sanitizer for the
  *window-state* form (still needed in principle, but window-state
  is much closer to `read`-able by construction; it may not need
  the `persp--*` filtering).

### Why this also closes a smaller paper-cut

The `workspace--sanitize-frameset` work we did to strip
`perspective.el`'s `persp--hash` from frame parameters was only
needed because we were saving *frame parameters* alongside window
state. Switching to `window-state-get` drops frame parameters from
the serialization entirely — the perspective coexistence becomes
a non-issue here.

## Plan for the next session

1. **First verify bug A's fix actually works.** Restart Emacs.
   - If `M-:` `(workspace--current-name)` returns the workspace name
     after a tab switch, bug A is fixed. Proceed to step 2.
   - If it returns nil, bug A is not actually fixed; debug
     `workspace--tab-workspace-name` and `workspace--current-name`
     in the running env (likely a registry/registry-key mismatch we
     have not yet noticed).
2. **Fix bug B by switching to window-state-get/put.** Files:
   - `config/workspaces/layouts.org` — replace `workspace--capture-frameset`
     and `workspace--restore-frameset`. Consider renaming the data
     slot from `:frameset` to `:window-state`. Update
     `workspace--layout-frameset` to match. Remove
     `workspace--sanitize-frameset` (and its helpers) if no longer
     needed; otherwise repurpose it for window-state.
   - `config/workspaces/persistence.org` — `workspace--activate-pending-workspace`
     uses the renamed accessor.
   - `config/workspaces/test/layouts-spec.el`,
     `config/workspaces/test/persistence-spec.el` — update assertions
     to match the new slot name and new restore primitive.
3. **Delete the existing state file** (`state/workspaces/personal/workspaces.eld`)
   before testing, since it contains the now-incompatible frameset
   form. Re-create workspaces by hand and verify restart roundtrip.
4. **Add a manual smoke spec** to the docs/README: a step-by-step
   "save, restart, verify" recipe the user can follow once to gain
   confidence before authorizing the cutover task.
5. **Then proceed to the cutover task** (still gated on user
   judgment).

## Verification (for this task)

Manual: with the user, run through the smoke flow:

```
1. M-: (delete-file (workspace--state-file))   ; clean slate
2. C-x w n alpha                                ; create workspace alpha
3. C-x 3                                        ; split the window
4. C-x C-f ~/some/file RET                      ; open a file in the right pane
5. C-x w n beta                                 ; create workspace beta
6. (do something visibly different in beta)
7. C-x C-c                                      ; quit Emacs cleanly
8. (relaunch Emacs)
9. (assert) tab bar shows both `alpha` and `beta`
10. Click `alpha`. Window configuration should be restored:
    split with the file from step 4 visible.
11. Click `beta`. Beta's distinct configuration should be restored.
```

Both layout restorations must succeed before this task is done.

## Context

- `tab-bar.el` lines 1352–1424 (the `tab-bar--tab` /
  `tab-bar--current-tab-make` reconstruction that drops custom params).
- `frameset.el` — note the frame-id matching logic in
  `frameset-restore`; the `:reuse-frames` arg and what it actually
  does is the relevant subtlety.
- design.md §D4 ("Persist via `frameset.el` for window configs") —
  this decision is being revised here. Update the design narrative
  when this task is done, or note the revision in this task's
  closing commit.
- `config/workspaces/layouts.org` (the `Frame ↔ frameset
  serialization` section is the change site)
- `config/workspaces/persistence.org` (`workspace--activate-pending-workspace`
  is the second change site)
- State file currently on disk:
  `state/workspaces/personal/workspaces.eld` — contains a workspace
  named `test` saved with the old frameset format; delete before
  testing the fix.
