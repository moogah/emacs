## Why

On Emacs startup the package recreates a tab for **every** workspace in
the persistence file (`workspace--restore` → `workspace--restore-tabs`).
The original design treated workspaces as ephemeral, so a tab-per-
workspace restore was reasonable. In practice workspaces accumulate and
are long-lived, so every launch reopens a growing wall of tabs the user
must immediately close.

Worse, those startup-created tabs never resume their layout. Layout
application for restored tabs is *lazy* — deferred to the first tab-switch
into each one, via a `:restore-pending` flag and a `tab-bar-switch` `:after`
advice. That path is unreliable in practice (QA confirmed startup tabs stay
bare), while the two **explicit** paths — `workspace-restore` and
`workspace-switch-layout` — apply layout synchronously and work correctly.

The fix is to stop materializing tabs at startup. If startup only hydrates
the registry and every tab is born from an explicit `workspace-restore`,
the tab-explosion disappears *and* the broken lazy-restore path disappears
with it (there are no startup tabs left to fail). The registry remains the
authoritative, disk-decoupled source of truth — which is deliberate: it
lets a workspace be dropped from the registry while its directory stays on
disk, i.e. **implicit archive**.

## What Changes

- **Startup hydrates the registry only.** `workspace--restore` reads the
  persistence file into `workspace--registry` (unchanged) but no longer
  calls `workspace--restore-tabs`. No tabs are created at startup.
- **Remove `workspace--restore-tabs`** (the tab-per-workspace creation loop).
- **Retire the lazy-restore machinery entirely** (decision: remove, not
  leave dormant):
  - Drop `:restore-pending` tagging in `workspace--deserialize-state`
    (and its serializer-side filtering, now moot).
  - Delete `workspace--activate-pending-workspace` and
    `workspace--persistence-after-tab-switch`'s lazy-activation responsibility.
  - Remove the `tab-bar-switch-to-tab` / `tab-bar-select-tab` `:after`
    advice that drove lazy activation. (The `:before` autosave advice that
    snapshots the outgoing workspace's `:working-state` is unrelated and
    **stays**.)
- **`workspace-restore` gains a home-builder fallback.** When the chosen
  workspace has no saved layout (`:layout-groups` is nil — e.g. workspaces
  recovered with only their on-disk directory), `workspace-restore` runs
  the `workspace-home-builder` (opens `<home>/home.org`) instead of
  leaving a bare tab. This matches `workspace-new`'s behavior and makes
  layout-less restores useful.
- Workspaces remain reachable after startup because `workspace-restore`
  already completes over the **full registry** (including saved-but-not-
  materialized workspaces), and `workspace-switch` continues to cover live
  tabs only. No new discovery mechanism is needed.

Not a breaking change to the persistence file format (still v3); it is a
behavioral change to startup. Users who relied on "all my tabs come back
on launch" will instead restore the ones they want via `C-x w o`
(`workspace-restore`).

## Capabilities

### New Capabilities

_None._ This change modifies startup/restore behavior of an existing
capability; it introduces no new capability.

### Modified Capabilities

- `workspaces`: The *Per-machine persistence and restoration* requirement
  changes — startup SHALL hydrate the registry **without** recreating
  tabs, and the lazy per-workspace layout-application requirement is
  removed. The *Explicit restore command* requirement gains a
  home-builder fallback for workspaces with no saved layout. The
  "Restart restores working-state…" scenario is reframed around an
  explicit `workspace-restore` (there is no startup tab to select).

## Impact

- **Code:** `config/workspaces/persistence.org` (tangles to
  `persistence.el`) — `workspace--restore`, removal of
  `workspace--restore-tabs`, removal of `workspace--activate-pending-
  workspace` and the lazy-activation `:after` advice, `:restore-pending`
  handling in `workspace--deserialize-state` /
  `workspace--persistence-serialize-workspace`, and the `workspace-restore`
  home-builder fallback.
- **Tests:** `config/workspaces/test/persistence-spec.el` (startup-restore
  scenarios), plus any spec asserting `:restore-pending` or lazy activation
  (e.g. broken-home / save-restore specs). The "Workspaces survive restart"
  and working-state-precedence scenarios are rewritten to assert
  registry hydration + explicit `workspace-restore`.
- **Spec:** delta under `openspec/changes/workspaces-startup-no-auto-tab-
  restore/specs/workspaces/spec.md`.
- **No change** to the persistence file schema, the autosave/flush
  triggers, or the gptel session/worktree integrations.
