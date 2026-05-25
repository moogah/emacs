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

## Observations

- The `r` binding under `C-x w` was NOT free at task start despite design.md §D9 listing it as such — bookmark-reincarnation's base commit (in scope of bookmark-reincarnation's task or earlier) had bound `C-x w r` to `workspace-remove-buffer`. To honour design.md's `C-x w r → workspace-revert` mapping I relocated `workspace-remove-buffer` to `C-x w b` (mnemonic: "buffer"). The keybinding table in `workspaces.org` notes this; no other module references the old binding.
- The pre-existing `save-restore-spec.el` "file buffer round-trip" test required a one-line reorder: with the v2 tab-switch advice installed, the test's `(kill-buffer)` → `(tab-bar-select-tab 1)` sequence captured the post-kill `*scratch*` frame into `:working-state`, which then "won" on restore via the new working-over-saved precedence. Reordering to `(tab-bar-select-tab 1)` → `(close-tab)` → `(kill-buffer)` lets the advice capture the file-bearing frame instead. This is faithful to v2 semantics — `:working-state` is the latest snapshot of the live frame, which on switch-away is still file-bearing. The test as-was implicitly assumed a v1 single-state model.
- `workspace--autosave-current-layout` retains a default `SLOT = :saved-state` for back-compat at the source level (no in-tree callers rely on it; every in-tree call site now passes its slot explicitly). The default is the conservative choice because an accidental omission writes the explicit slot, which is at worst a redundant write rather than a stealth corruption of an existing `:saved-state` baseline (which v1 was prone to).
- `workspace-switch-layout` previously called `workspace--restore-frameset` against `workspace--layout-frameset` of the recent layout in the destination group. v2 routes through `workspace--layout-effective-state`, so the destination's `:working-state` (if any) wins over `:saved-state` on switch-in. This is consistent with the design's restore precedence and with the spec's "restart restores working-state, not saved-state, when both present" scenario.

## Discoveries

- discovery_id: disc-two-state-layout-1
  class: deviation
  description: |
    The scaffolding for `register/vocabulary/workspace-state-slot`
    declares a centralised `workspace--slot-for-trigger` pcase that
    maps the closed set of trigger symbols (`'tab-switch`, `'idle`,
    `'kill-emacs`, `'explicit-save`, `'save-layout`,
    `'new-home-stamp`, `'switch-layout`) to the two slot keywords.
    My implementation does NOT define this function. Instead, each
    autosave call site passes the slot keyword directly:
    `(workspace--autosave-current-layout :working-state)` in the
    tab-switch advice, the kill-emacs flush, and
    `workspace-switch-layout`; `(workspace--autosave-current-layout
    :saved-state)` in `workspace-save`. Routing is by direct caller
    specification (no trigger → slot indirection).
  affected_register_entry: register/vocabulary/workspace-state-slot
  recommendation: |
    Reconcile the entry as `divergent` (or `reconciled` with the
    canonical_mapping_function field replaced by an explicit
    enumeration of call-site → slot pairs). Justification: with only
    two slots and ~7 call sites, a trigger-symbol enum + dispatch
    function adds indirection without preventing the failure mode the
    entry guards against. Caller-level routing makes "which slot does
    this site write to" a one-line literal at the site itself; typos
    are syntactic keyword typos (`:save-state` instead of
    `:saved-state`) which are caught by the existing slot-routing
    tests and by the closed-set validator
    `workspace--state-slot-p` (which I could provide as a
    defensive guard at the slot-route boundary if the reviewer wants
    structural enforcement). The closed-set discipline is preserved
    by the two-keyword grammar of the SLOT argument itself.
- discovery_id: disc-two-state-layout-2
  class: scope-question
  description: |
    The task body §3 says "`workspace-save-layout` and
    `workspace-switch-layout` write `:working-state` for the outgoing
    slot (matching the autosave model)" — but design.md §D4's table
    is explicit that `workspace-save-layout NAME` writes `:saved-state`
    of the named layout-group (it's an explicit save variant), and
    only `workspace-switch-layout` writes `:working-state` of the
    outgoing group. The two statements appear to contradict.
    I implemented per design.md §D4: `workspace-save-layout`
    constructs a fresh layout via `workspace--layout-make` (which
    populates `:saved-state` and leaves `:working-state` nil — i.e.
    the named layout's saved-state is the new capture);
    `workspace-switch-layout` calls
    `workspace--autosave-current-layout :working-state` to snapshot
    the outgoing layout into its `:working-state` slot.
  affected_register_entry: register/vocabulary/workspace-state-slot
  recommendation: |
    Rewrite the task body §3 to align with design.md §D4, OR amend
    design.md §D4 if the intent was that `workspace-save-layout` writes
    `:working-state` for symmetry with the autosave path. The
    register entry's `consumer_mapping` field aligns with design.md
    (workspace-save-layout → :saved-state), so the register is the
    source of truth I followed. If the design intent shifts, both
    the layouts.org implementation and the layouts-spec.el coverage
    need updating; flag at integrate.
- discovery_id: disc-two-state-layout-3
  class: invariant-gap
  description: |
    The scaffolding for
    `register/invariant/explicit-save-clears-working-state` includes a
    scenario "workspace-switch-layout's save-on-switch clears
    :working-state on the destination". The scenario is hedged
    ("revise the scaffold and note the design choice in `##
    Discoveries`"). design.md §D4's table does NOT call for clearing
    `:working-state` on the destination of a switch — only on
    explicit `workspace-save`. I implemented per design: switch-layout
    captures the OUTGOING into `:working-state` and restores
    effective-state of the destination (which prefers an existing
    `:working-state` if present). Clearing the destination would
    contradict the "restart restores working-state when present"
    promise. I did not write a test asserting destination-clear.
  affected_register_entry: register/invariant/explicit-save-clears-working-state
  recommendation: |
    Reconcile the invariant entry's enforcement scope to
    `{workspace-save, workspace-save-layout, workspace-new home stamp}`
    only — workspace-switch-layout is NOT an explicit-save variant;
    it's a navigation that incidentally captures into the outgoing
    slot. Drop the "destination clear" scenario from the scaffolding
    or move it to a separate "switch-layout-destination-precedence"
    invariant (which would assert the opposite — destination keeps
    its `:working-state` and effective-state is what restores).
- discovery_id: disc-two-state-layout-4
  class: shape-fragmentation
  description: |
    The scaffolding for `register/boundary/autosave-guard-pipeline`
    declares a 4-stage pipeline with distinct functions
    (`workspace--pipeline-anti-save-check`,
    `workspace--pipeline-autosave-capture`,
    `workspace--pipeline-slot-route`, `workspace--pipeline-flush`).
    My implementation has the boundary present *conceptually* but
    folded: stage 2 (capture) and stage 3 (slot-route) live inside
    `workspace--autosave-current-layout` as a single function body;
    stage 4 (flush) is provided by the existing
    `workspace--persistence-after-autosave` advice (debounced) and by
    explicit `workspace--flush-state` in `workspace-save` /
    `workspace--kill-emacs-flush`; stage 1 (anti-save-check) does not
    exist yet (it's the sibling task `anti-save-predicates`'s scope).
  affected_register_entry: register/boundary/autosave-guard-pipeline
  recommendation: |
    Reconcile the boundary entry to reflect the merged-stage
    implementation: stages 2+3 are a single function
    (`workspace--autosave-current-layout`), stage 4 is the existing
    `workspace--persistence-after-autosave` + `workspace--flush-state`
    pair. Stage 1 remains speculated and is the sibling task's
    deliverable. The cross-stage invariants
    (`autosave-never-writes-saved-state`,
    `explicit-save-clears-working-state`,
    `explicit-save-bypasses-anti-save`) still hold structurally; only
    the function-decomposition is different from speculation.
