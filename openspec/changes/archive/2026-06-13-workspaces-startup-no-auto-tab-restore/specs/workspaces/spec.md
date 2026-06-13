## MODIFIED Requirements

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine
path keyed by `jf/machine-role`. The persistence file SHALL use **schema
version 3**:

- Each workspace plist SHALL carry a required `:home` slot holding the
  absolute filesystem path of its home directory. Loading a workspace
  whose serialized form lacks `:home` is a data error (treated like a
  bad-version file: notice + skip).
- Each layout SHALL carry two window-state slots:
  - `:saved-state` — written only by explicit `workspace-save` (or its
    variants `workspace-save-layout`, `workspace-switch-layout`,
    `workspace-new`'s home stamp). Authoritative across restarts.
  - `:working-state` — written only by autosaves (workspace context
    switch, idle timer, kill-emacs). May be nil.
- Each layout SHALL carry an `:etc` alist slot for forward-compatible
  extension data. Replaces the legacy single-purpose `:git-state`
  placeholder.
- Each leaf in a layout's window-state SHALL carry a `workspace-buffer`
  entry in its `parameters`, encoding a bookmark record, filename, buffer
  name, and narrowed/indirect flags (see *Buffer reincarnation across
  restart*).

The package SHALL NOT support v1 or v2 persistence files. The package is
pre-alpha; users with older files on disk are expected to delete the
state file before running the v3 code. The reader SHALL emit a
non-fatal `*Messages*` notice for files whose `:version` is not 3, and
proceed as if no persistence file exists.

On Emacs startup the package SHALL hydrate the in-memory registry from
the persistence file but SHALL NOT create any tabs. Restored workspaces
are *saved-but-not-materialized*: they are reachable via
`workspace-restore` (which completes over the full registry) but no tab
exists for any of them until the user explicitly restores one. Workspaces
whose `:home` no longer exists on disk SHALL be loaded into the registry
in a *broken* state per Requirement: Broken home directory tolerated on
restore — they are still listed but cannot be activated. The package
SHALL NOT perform any lazy per-workspace layout application on tab
selection (no `:restore-pending` flag, no tab-switch activation hook);
layout is applied only by the explicit restore / switch-layout commands
(see Requirement: Explicit restore command), which preserve the
`:working-state`-over-`:saved-state` precedence.

Persistence to disk SHALL be triggered by all of:
- Explicit `workspace-save` (synchronous flush; writes `:saved-state`).
- Workspace context switch (debounced flush; writes `:working-state` of the outgoing workspace).
- Intra-workspace layout switch (debounced flush; writes `:working-state` of the outgoing layout).
- `workspaces-mode` idle timer (debounced flush; writes `:working-state` of the current workspace).
- `kill-emacs-hook` (synchronous flush; captures `:working-state` of the current workspace once before writing).

When the persistence file is missing or unreadable the package SHALL start with no workspaces and SHALL NOT raise an error visible to the user beyond an `*Messages*` notice.

#### Scenario: V2 persistence file is rejected with a notice
- **WHEN** a workspaces persistence file exists with `:version 2`
- **AND** Emacs starts up
- **THEN** an `*Messages*` notice is emitted naming the file and the
  version mismatch
- **AND** no workspaces are restored from the file
- **AND** the user can invoke `workspace-new` normally to start fresh

#### Scenario: Workspace lacking :home in persistence is skipped
- **WHEN** the persistence file is `:version 3` but contains one
  workspace plist without a `:home` slot
- **AND** Emacs starts up
- **THEN** an `*Messages*` notice names the malformed entry
- **AND** that entry is skipped (not added to the registry)
- **AND** other well-formed entries in the same file ARE restored

#### Scenario: Startup hydrates the registry without creating tabs
- **WHEN** the persistence file is `:version 3` and contains workspaces
  `alpha` and `writing`, both with existing `:home` directories
- **AND** Emacs starts up
- **THEN** both `alpha` and `writing` are present in the in-memory
  registry
- **AND** no tab is created for either workspace at startup (the tab-bar
  is unchanged by restore)
- **AND** each is offered as a candidate by `workspace-restore`

#### Scenario: Restart restores working-state, not saved-state, when both present
- **WHEN** workspace `code` has `:home ~/emacs-workspaces/code/` (which
  exists), `:saved-state` S (last explicit save) and `:working-state` W
  (latest autosave)
- **AND** Emacs is restarted, so `code` is hydrated into the registry
  with no tab created
- **AND** the user invokes `workspace-restore` and chooses `code`
- **THEN** a new tab named `code` is created and selected
- **AND** the window configuration applied on `code`'s frame is W, not S
- **AND** the buffers in W are reincarnated via the bookmark chain
  (Requirement: Buffer reincarnation across restart)

### Requirement: Explicit restore command

The package SHALL provide `workspace-restore` as the user-facing command to materialize a saved workspace into a tab. Interactively it SHALL `completing-read` over every workspace name currently in the in-memory registry (which includes saved-but-not-materialized workspaces loaded from disk at startup). Because startup no longer creates tabs (see Requirement: Per-machine persistence and restoration), `workspace-restore` is the primary way a persisted workspace re-enters a live session.

For the chosen workspace:
- If a live tab for that workspace already exists, `workspace-restore` SHALL switch to it (equivalent to `workspace-switch` for that case).
- Otherwise it SHALL create a new tab and name it after the workspace, then:
  - **When the workspace has a saved layout**, pre-load `:buffer-files` via `find-file-noselect` and `window-state-put` the saved layout onto the current frame's root window.
  - **When the workspace has NO saved layout** (its `:layout-groups` is nil — e.g. a workspace recovered from its on-disk directory alone, never explicitly saved), it SHALL instead invoke `workspace-home-builder` so the tab opens `<home>/home.org`, matching `workspace-new`'s behavior, rather than leaving a bare tab. No `window-state-put` is attempted in this case.

The `workspace-restore` binding is `C-x w o`. It complements (does not replace) `workspace-switch`, which is scoped to live tabs only.

#### Scenario: Restore an unmaterialized workspace
- **WHEN** workspace `code` is in the registry but has no live tab (e.g. it was loaded from disk at startup and never restored, or its tab was closed mid-session)
- **AND** workspace `code` has a saved layout
- **AND** the user invokes `workspace-restore` and chooses `code`
- **THEN** a new tab named `code` is created and selected
- **AND** the files in `:buffer-files` are loaded as buffers before the window-state is applied
- **AND** the saved window-state is applied to the new tab's window tree
- **AND** the displayed buffers match the saved configuration

#### Scenario: Restore a workspace with no saved layout opens home.org
- **WHEN** workspace `alpha` is in the registry with `:home ~/emacs-workspaces/alpha/` (which exists) and no saved layout (`:layout-groups` is nil)
- **AND** the user invokes `workspace-restore` and chooses `alpha`
- **THEN** a new tab named `alpha` is created and selected
- **AND** `workspace-home-builder` runs, opening `~/emacs-workspaces/alpha/home.org`
- **AND** no `window-state-put` is attempted (there is no saved layout to apply)

#### Scenario: Restore a live workspace
- **WHEN** workspace `notes` already has a live tab in the current Emacs session
- **AND** the user invokes `workspace-restore` and chooses `notes`
- **THEN** the existing `notes` tab is selected
- **AND** no new tab is created
- **AND** the existing in-session window state is preserved (no re-application of the saved layout)

#### Scenario: Restore errors on unknown name
- **WHEN** the user passes a workspace name that is not in the registry
- **THEN** a `user-error` is signalled
- **AND** no tab is created
