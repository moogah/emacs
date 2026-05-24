## Why

The MVP `workspaces` package (in-flight change `add-workspaces-package`)
intentionally removed autosave-on-workspace-switch because the v1
persistence schema holds exactly **one** window-state per layout-group.
With autosave on, switching tabs (or `tab-bar-close-tab`, which calls
`tab-bar-select-tab` internally) would capture the *current* — often
transient or post-kill — frame state and overwrite the user's explicit
`workspace-save` snapshot. The fix shipped was to make all persistence
deliberate via `workspace-save` (`C-x w S`).

That fix is correct but the resulting daily workflow is unfriendly:
every time the user switches tabs they must remember to save, or any
intra-session arrangement on the outgoing workspace is lost on restart.
The design (`add-workspaces-package/design.md` §D7) names this as a
known gap that must close before beta and points at the activities.el
two-state pattern as the model — every activity has both a `default`
(explicit) and `last` (auto-captured) state slot
(`runtime/straight/repos/activities.el/activities.el` lines 73–80,
596–611).

This change does the schema split and re-enables autosave-on-switch
without re-creating the clobber bug.

## What Changes

- **Persistence schema bumps to `:version 2`**:
  - Each layout grows two slots: `:saved-state` and `:working-state`.
    Both hold `window-state-get`-form blobs; either may be nil.
  - The legacy v1 `:frameset` slot is migrated to `:saved-state` on
    read. The file is rewritten in v2 form on the next save.
  - Schema migration is read-time only; no separate migration command
    and no schema version downgrade path.
- **`workspace-save` writes only to `:saved-state`** (unchanged
  behavior from the user's perspective).
- **Autosave-on-context-switch is restored**, writing exclusively to
  `:working-state`:
  - On workspace switch (advice on `tab-bar-select-tab` /
    `tab-bar-switch-to-tab`), the outgoing workspace's recent layout's
    `:working-state` is updated.
  - On `workspace-switch-layout`, the outgoing layout's
    `:working-state` is updated (today this writes to `:frameset`).
  - On `kill-emacs-hook`, the current workspace's `:working-state`
    is captured before the registry is flushed.
  - Autosave never touches `:saved-state`.
- **Restore precedence**: `workspace--apply-saved-layout` and
  `workspace-switch-layout` prefer `:working-state` when present; fall
  back to `:saved-state`. This makes restart restore the user's live
  arrangement, not their last explicit save.
- **NEW**: `workspace-revert` (`C-x w r`) command — resets the current
  layout's `:working-state` to nil so the next restore uses
  `:saved-state`. The user's "undo my drift" affordance.
- **NEW** (optional, low cost): `workspace-save` clears
  `:working-state` after writing `:saved-state`, so the explicit save
  becomes the new clean baseline.
- The `:buffer-files` slot is unchanged by this change. The bufferlo
  sync remains gated to `workspace-save` (per `add-workspaces-package`
  design §D3); autosave does *not* mutate `:buffer-files`.
- The v1 single-`:frameset` shape is removed from in-memory data
  structures after migration. The reader accepts v1 files for one
  release as a transitional courtesy; v2 is the only written form.

## Capabilities

### New Capabilities

_(none — this is a refinement of an existing capability)_

### Modified Capabilities

- `workspaces`: the persistence and restore requirements are revised
  to add the two-state slot model, restore autosave-on-context-switch,
  and add `workspace-revert`. Specifically the requirements impacted:
  - *Auto-save layout on context switch* — the "MVP gap" scenario
    (workspace switch does NOT auto-save) is **replaced** by the
    inverse: workspace switch DOES auto-save, into `:working-state`.
  - *Per-machine persistence and restoration* — schema version,
    layout shape, restore precedence, and the trigger list all
    change.
  - *Explicit save command* — `workspace-save`'s contract grows the
    `:working-state` clear.
  - A **new requirement** is added: *Working-state revert* (with the
    `workspace-revert` command).

  **Note on ordering**: the `workspaces` spec does not yet live at
  `openspec/specs/workspaces.md` — it currently exists only in
  `openspec/changes/add-workspaces-package/specs/workspaces/spec.md`
  and lands at the canonical path when `add-workspaces-package` is
  archived (which is gated on `cutover-remove-legacy`). The delta
  spec in this change is written against the add-workspaces-package
  spec as if it were already at the canonical path; the implementation
  order is documented in design.md.

## Impact

- **Modified**: `config/workspaces/data-model.org` — layout shape grows
  two slots; accessors and constructors updated.
- **Modified**: `config/workspaces/layouts.org` —
  `workspace--autosave-current-layout` writes `:working-state`;
  `workspace--restore-layout` (new internal helper) implements the
  prefer-working-then-saved selection used by both
  `workspace-switch-layout` and `workspace--apply-saved-layout`.
- **Modified**: `config/workspaces/persistence.org` — schema version
  constant bumps to 2; read-time migration on `workspace--deserialize-state`;
  `workspace-save` clears `:working-state` after capture;
  re-introduces a (clobber-safe) advice on `tab-bar-select-tab` that
  captures the outgoing workspace's `:working-state` before the switch
  completes; `workspace--kill-emacs-flush` captures `:working-state`
  before flushing.
- **NEW command + binding**: `workspace-revert` on `C-x w r`.
- **Modified**: `config/workspaces/test/` — new specs for the two-state
  selection, migration, autosave triggers, and revert. Existing
  layout/persistence specs revised to assert against the new slots.
- **Modified**: `add-workspaces-package/design.md` §D7 — once this
  change archives, mark the deferred two-state work as resolved and
  point at this change's design.
- **Modified**: `add-workspaces-package/specs/workspaces/spec.md` (or
  its archived form at `openspec/specs/workspaces.md` if this change
  lands post-archive) — revise the *Auto-save*, *Per-machine
  persistence*, and *Explicit save* requirements; add the new
  *Working-state revert* requirement.
- **No new dependencies**.
- **State-file compatibility**: v1 files read successfully (migrated
  on first save). v2 files are NOT readable by the MVP code on the
  `add-workspaces-package` branch — users who roll back the
  refinement after writing v2 must delete the state file. This is
  acceptable per the MVP-era user agreement.
- **Cutover ordering** (`cutover-remove-legacy` in
  `add-workspaces-package`): this change does NOT block cutover, but
  it should land *before* cutover so the autosave behavior is in
  place when activities.el is removed from the dependency tree
  (otherwise the user loses both the activities autosave habit and
  has no workspaces equivalent).
