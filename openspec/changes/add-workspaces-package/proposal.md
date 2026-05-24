## Why

`activities.el` only supports a single "default" and "last" window
configuration per activity — it cannot hold an arbitrary number of
named layouts that the user navigates between as their focus shifts.
`perspective` provides buffer-list scoping that we want (per-workspace
isolation, filtered `consult-buffer`) but lacks the workflow shell,
tab visibility, and per-workspace persistence we already get from
activities. The existing `config/activities/` extension is an early
prototype not worth evolving further. This change introduces a
purpose-built `workspaces` package that combines the strengths of
both, and replaces them.

## What Changes

- **NEW**: `workspaces` package (`config/workspaces/`) providing:
  - Tab-per-workspace UI built on `tab-bar-mode`
  - Per-workspace named *layouts* (window configurations), unlimited
    count, with a recent-layout pointer
  - Per-workspace `home` layout stamped from a configurable builder
  - Per-workspace buffer membership scoping (via `bufferlo`)
  - `workspace-remove-buffer` command (preferred for "I'm done with
    this here"); `kill-buffer` semantics left intact (Story A)
  - Explicit `workspace-save` (C-x w S) and `workspace-restore`
    (C-x w o) commands. Auto-save on workspace context switch is
    **deferred for MVP** pending a two-state (saved/working) data-model
    revision — see design.md §D7. Auto-save on intra-workspace
    layout switch (`workspace-switch-layout`) is retained.
  - Per-machine persistence (matches the existing `jf/machine-role`
    convention used by activities)
  - Data model forward-compatible with timestamped layout revisions
    grouped under a named *layout-group*, and per-layout-group git
    observations (deferred; not surfaced in MVP UI)
- **BREAKING**: Remove `config/activities/` (activities-extensions)
  after the workspaces package is stable. Hard cutover — no migration
  adapter.
- **BREAKING**: Remove `perspective` from the active configuration.
- Add `bufferlo` dependency (via straight.el).
- Remove `perspective` dependency.
- `config/gptel/sessions/activities-integration.org` retires alongside
  activities-extensions.

## Capabilities

### New Capabilities

- `workspaces`: Named tab-based workspaces with per-workspace named
  layouts, configurable home, recent-layout pointer, per-workspace
  buffer membership, and per-machine persistence.

### Modified Capabilities

- `activities-extensions`: Deprecate and remove. All behaviors are
  either superseded by `workspaces` or dropped.

## Impact

- **New**: `config/workspaces/` (literate, multi-file)
- **New**: `state/workspaces/<machine-role>/` persistence directory
- **Modified**: `init.org` — register `workspaces` in
  `jf/enabled-modules`; remove `activities/activities` entry after
  cutover
- **Modified**: `config/core/window-management.org` — remove
  `perspective` and `activities` `use-package` blocks (`winner-mode`
  and `ace-window` remain unchanged)
- **Removed (post-cutover)**: `config/activities/` (entire subtree)
- **Removed (post-cutover)**: `config/gptel/sessions/activities-integration.org`
  and any loader reference from `config/gptel/gptel.org`
- **Dependencies**: add `bufferlo` via `straight.el`; remove `perspective`
- **Specs**: new `openspec/specs/workspaces.md`; deprecate
  `openspec/specs/activities-extensions.md`
- **Cutover sequence**: workspaces package lands and is exercised
  while activities and perspective remain loaded; once stable, a
  single cutover commit removes the legacy modules, dependencies,
  and integration glue.
