# Workspaces

## Purpose

Provides a purpose-built window/workspace management system layered
directly on `tab-bar-mode`. A *workspace* is a uniquely-named
user-facing context surfaced as a single tab; each workspace owns an
arbitrary number of named *layouts* (window configurations), a
*home* layout produced by a configurable builder, a *recent-layout
pointer*, a per-workspace *buffer membership* set scoped via
`bufferlo`, and a per-machine persistence file keyed by
`jf/machine-role`.

This capability supersedes the legacy combination of `activities.el`
+ `activities-extensions` (workflow shell + tabs + persistence but
only one default+last window-state per activity) and `perspective.el`
(buffer scoping but no tab-bar, no named layouts, no persistence in
this configuration). Both legacy modules were removed in the
`add-workspaces-package` cutover.

Implementation: `config/workspaces/` (literate, multi-file:
`data-model`, `tabs`, `layouts`, `buffer-membership`, `persistence`,
`workspaces`). User-facing prefix: `C-x w`.

The on-disk persistence schema is `:version 1` (one window-state per
layout-group; explicit `workspace-save` only). Two follow-up
refinements are in flight: a `:version 2` schema bump that splits
layouts into `:saved-state` and `:working-state` slots and restores
autosave-on-context-switch (see
`openspec/changes/refine-workspaces-two-state-layout/`), and a
bookmark-based buffer reincarnation port from activities.el that
preserves point, narrowing, major-mode state, and non-file buffers
across restart.

## Requirements

### Requirement: Workspace identity and tab visibility

The `workspaces` package SHALL model each *workspace* as a uniquely-named user-facing context that surfaces as a single tab in `tab-bar-mode`. Switching to a workspace's tab SHALL activate that workspace, including its buffer membership and most recent layout.

Workspaces SHALL be the sole concept driving the tab bar: every visible tab corresponds to exactly one workspace, and every workspace has exactly one tab while it is loaded.

#### Scenario: Creating a workspace creates a tab
- **WHEN** the user invokes `workspace-new` with name `"notes"`
- **THEN** a new tab named `notes` appears in the tab bar
- **AND** the tab is selected
- **AND** the workspace's `home` layout is active (Requirement: Per-workspace home layout)

#### Scenario: Switching tabs switches workspaces
- **WHEN** two workspaces `notes` and `code` exist as tabs and `notes` is selected
- **AND** the user selects the `code` tab via any tab-bar command
- **THEN** the active workspace becomes `code`
- **AND** the buffer membership visible to completion commands reflects `code` (Requirement: Workspace-scoped buffer membership)

#### Scenario: Workspace names are unique
- **WHEN** the user invokes `workspace-new` with a name matching an existing workspace
- **THEN** the existing workspace's tab is selected
- **AND** no new workspace is created

---

### Requirement: Per-workspace named layouts

Each workspace SHALL hold zero or more *layouts*. A layout is a named, restorable window configuration scoped to its parent workspace. The number of layouts per workspace is not bounded by the package.

The package SHALL expose commands to:
- Save the current window configuration as a layout under a user-supplied name.
- Switch to a named layout within the current workspace.
- List the current workspace's layouts.
- Delete a named layout.

Switching to a layout SHALL restore its window configuration into the current frame.

#### Scenario: Saving and switching layouts within a workspace
- **WHEN** the current workspace `code` has layouts `home` and `magit`
- **AND** the user invokes `workspace-switch-layout` and chooses `magit`
- **THEN** the window configuration saved with `magit` is restored
- **AND** the workspace's recent-layout pointer becomes `magit` (Requirement: Recent-layout pointer)

#### Scenario: Layout names are scoped to their workspace
- **WHEN** workspace `code` has a layout named `tests`
- **AND** workspace `notes` also has a layout named `tests`
- **THEN** switching to `tests` in either workspace restores that workspace's own `tests` layout
- **AND** the two layouts are independent

#### Scenario: Deleting a layout
- **WHEN** the current workspace has layouts `home` and `magit`
- **AND** the user invokes `workspace-delete-layout` and chooses `magit`
- **THEN** `magit` is removed from the workspace's layout list
- **AND** the recent-layout pointer is reassigned to the next most recently used remaining layout (or `home` if none)

---

### Requirement: Per-workspace home layout

Each workspace SHALL have a *home* layout. When a workspace is first created the home layout SHALL be constructed by a user-configurable builder function (defcustom `workspace-home-builder`) whose default produces a sensible single-window starting state.

The `home` layout name SHALL be reserved: it cannot be deleted by `workspace-delete-layout`, and saving the current configuration as `home` overwrites the existing home (it does not invoke the builder).

The home builder SHALL run in the context of the freshly-created workspace, so any buffers it opens become members of that workspace (Requirement: Workspace-scoped buffer membership).

#### Scenario: New workspace uses the home builder
- **WHEN** `workspace-home-builder` is configured to open `~/notes/index.org` in a single window
- **AND** the user invokes `workspace-new` with name `"writing"`
- **THEN** the new workspace's `home` layout shows `~/notes/index.org` in a single window
- **AND** `~/notes/index.org` is a member of the `writing` workspace

#### Scenario: Home layout cannot be deleted
- **WHEN** the user invokes `workspace-delete-layout` on `home`
- **THEN** the deletion is rejected with a user-visible message
- **AND** the `home` layout is unchanged

#### Scenario: Re-saving home overwrites without invoking the builder
- **WHEN** the user is on the `home` layout of workspace `writing` and edits its window configuration
- **AND** the user invokes `workspace-save-layout` with name `home`
- **THEN** the workspace's `home` layout is updated to the current window configuration
- **AND** the builder function is not invoked

---

### Requirement: Recent-layout pointer

Each workspace SHALL track the name of the most recently activated layout within it. A command (e.g. `workspace-switch-to-recent-layout`) SHALL jump to that layout.

The recent-layout pointer SHALL update whenever a layout becomes active, including when restored from persistence (Requirement: Per-machine persistence and restoration).

#### Scenario: Recent-layout updates on switch
- **WHEN** workspace `code` has layouts `home`, `magit`, `tests`
- **AND** the user switches to `home`, then `magit`, then `tests`
- **THEN** the workspace's recent-layout pointer is `tests`
- **AND** invoking `workspace-switch-to-recent-layout` from elsewhere navigates to `tests`

#### Scenario: Recent-layout survives workspace switching
- **WHEN** workspace `code` was most recently on layout `magit` and the user switches to workspace `notes`
- **AND** the user later switches back to workspace `code`
- **THEN** the layout active in `code` is `magit`

---

### Requirement: Auto-save layout on context switch (MVP: partial; full coverage deferred to v2 schema)

The package SHALL auto-save the outgoing layout's window configuration on intra-workspace layout switch, capturing it into the layout-group slot before restoring the destination layout. The package SHALL NOT auto-save on workspace context switch (tab change) under the v1 schema; the rationale and the deferred follow-up are described below.

**MVP scope (as implemented):** Auto-save on intra-workspace **layout** switch is in scope and behaves as specified below. Auto-save on **workspace** switch (changing tabs) is **deferred** until the layout schema supports separate `:saved-state` and `:working-state` slots (see design.md §D7). The v1 schema holds exactly one window-state per layout-group, so a tab-switch autosave would overwrite the user's explicit `workspace-save` snapshot with whatever transient state happened to be in the frame at switch time. This was the source of an MVP-blocking restore bug (see closed task `fix-restoration-roundtrip`).

Until the two-state schema lands, persistence-across-workspace-switch is the user's deliberate call via `workspace-save` (Requirement: Explicit save command).

`workspace-save-layout` remains available for naming a new layout or for explicit snapshotting (orthogonal to the workspace-vs-layout distinction).

#### Scenario: Switching layouts within a workspace snapshots the outgoing layout
- **WHEN** the user is on layout `home` in workspace `code`
- **AND** the user rearranges windows on `home`
- **AND** the user invokes `workspace-switch-layout` to `magit`
- **THEN** the rearranged configuration is persisted to `code`'s `home` layout

#### Scenario: Switching workspaces does NOT auto-save (MVP gap)
- **WHEN** the user has workspace `code` selected and rearranges its windows
- **AND** the user switches to workspace `notes` via `workspace-switch` or by clicking the `notes` tab
- **THEN** the rearranged configuration is NOT persisted to `code`'s recent layout
- **AND** returning to `code` shows whatever in-session state `tab-bar` preserved (which may differ from the persisted layout)
- **AND** to capture the rearrangement to disk the user must invoke `workspace-save` while on `code`

> **Note:** This scenario documents the current MVP gap, not the steady-state design. The intended behavior is for `code`'s working state to be auto-captured so that restart restores it; that requires the two-state schema and is tracked as a follow-up before beta.

---

### Requirement: Workspace-scoped buffer membership

Each workspace SHALL maintain a membership set of buffers. Completion-based buffer commands (`switch-to-buffer`, `consult-buffer`, and any other command that surfaces the user-facing buffer list) SHALL be filtered to the current workspace's membership.

A buffer SHALL become a member of the current workspace when it is displayed in any window of that workspace. There SHALL be no built-in exclusion list — `*Messages*`, `*scratch*`, magit buffers, and other non-file buffers are eligible for workspace membership on the same terms as file buffers.

A buffer MAY be a member of multiple workspaces simultaneously. Membership is a per-workspace relation, not a buffer-exclusive assignment.

#### Scenario: Displayed buffers become members
- **WHEN** the user is in workspace `code` and visits `~/project/foo.el`
- **THEN** `foo.el` is a member of workspace `code`
- **AND** `foo.el` appears in completion lists when `code` is active

#### Scenario: Non-file buffers can be members
- **WHEN** the user is in workspace `code` and displays `*Messages*` in a window
- **THEN** `*Messages*` is a member of workspace `code`
- **AND** `*Messages*` appears in completion lists when `code` is active

#### Scenario: Membership is per-workspace
- **WHEN** the user visits `~/shared/util.el` in workspace `code`
- **AND** the user later visits the same file in workspace `notes`
- **THEN** the single underlying buffer is a member of both workspaces
- **AND** completion lists in either workspace include it

#### Scenario: Buffers from other workspaces are filtered out
- **WHEN** the user is in workspace `notes` and `foo.el` is a member only of workspace `code`
- **THEN** `foo.el` does not appear in completion lists while `notes` is active
- **AND** `foo.el` remains a live buffer

---

### Requirement: workspace-remove-buffer command

The package SHALL provide a `workspace-remove-buffer` command that removes a buffer from the current workspace's membership without invoking `kill-buffer`. Removed buffers SHALL remain live and SHALL retain membership in any other workspaces that hold them.

This command SHALL be the preferred user-facing means of saying "I am done with this buffer in this workspace."

#### Scenario: Remove from current workspace only
- **WHEN** `foo.el` is a member of both workspace `code` and workspace `notes`
- **AND** the user is in `code` and invokes `workspace-remove-buffer` on `foo.el`
- **THEN** `foo.el` is no longer a member of `code`
- **AND** `foo.el` remains a member of `notes`
- **AND** the underlying buffer is live (not killed)

#### Scenario: Remove the only membership
- **WHEN** `foo.el` is a member of only workspace `code`
- **AND** the user invokes `workspace-remove-buffer` on `foo.el`
- **THEN** `foo.el` is no longer a member of any workspace
- **AND** the underlying buffer is live (not killed)

---

### Requirement: kill-buffer remains globally destructive (live membership only)

The `workspaces` package SHALL NOT shadow, advise, or rebind `kill-buffer`. Invoking `kill-buffer` SHALL continue to kill the buffer for all of Emacs.

This preserves the universal Emacs convention that `C-x k` ends a buffer's life. (Story A in the design.)

**Membership semantics (revised):** there are two distinct senses of "membership":

1. **Live in-session membership** — the per-tab bufferlo list that filters `consult-buffer` and friends. When a buffer is killed, it is gone from this list across all workspaces by virtue of no longer existing.
2. **Persisted file list (`:buffer-files`)** — the saved set of file paths the workspace should reopen on restore. This is mutated only by `workspace-save` (additive sync from bufferlo at save time) and `workspace-remove-buffer` (explicit removal). It is **not** mutated by `kill-buffer`.

The split exists because the user may kill a buffer for transient reasons (free memory, clear an error, etc.) without intending to drop the file from the workspace's saved file list. The earlier implementation that wiped `:buffer-files` on every `kill-buffer-hook` made the close-tab-then-restore flow lose buffers; the hook has been removed (see design.md §D3).

#### Scenario: kill-buffer ends the buffer's life
- **WHEN** `foo.el` is a member of workspaces `code` and `notes`
- **AND** the user invokes `kill-buffer` on `foo.el` from any workspace
- **THEN** the buffer is killed
- **AND** `foo.el` is no longer in either workspace's *live* membership (because no buffer object exists)

#### Scenario: kill-buffer alone does NOT alter the saved file list
- **WHEN** `foo.el` is in `code`'s saved `:buffer-files`
- **AND** the user kills the `foo.el` buffer (no `workspace-save` follows)
- **THEN** `code`'s saved `:buffer-files` still contains `foo.el`'s path
- **AND** a subsequent `workspace-restore code` re-opens `foo.el` from disk

#### Scenario: workspace-save replaces the saved file list from the current bufferlo set
- **WHEN** the user kills `foo.el`
- **AND** invokes `workspace-save` on `code`
- **THEN** `:buffer-files` is replaced with bufferlo's current per-tab file list, which no longer includes `foo.el`
- **AND** a subsequent `workspace-restore code` does not re-open `foo.el`

> **Note:** The replace-on-save semantics mean that the explicit save is also the explicit "clean up the file list" gesture. A user who wants to keep a file in `:buffer-files` despite having closed its buffer should re-open it before invoking `workspace-save`. A finer-grained model (embed buffer info in the saved window-state, à la activities.el's per-leaf `activities-buffer` struct) is recommended as a follow-up enhancement.

---

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine path keyed by `jf/machine-role` (matching the existing convention used by `activities`). Persistence SHALL include:

- The set of workspaces and their names
- The recent-layout pointer for each workspace
- The named layouts for each workspace (window configurations as `window-state-get` forms, stored in the legacy-named `:frameset` slot; restorable across restarts)
- The buffer file list (`:buffer-files`) for each workspace — by absolute file path; non-file buffers cannot be persisted

Persistence to disk SHALL be triggered:
- Explicitly via `workspace-save` (captures current frame + syncs `:buffer-files` + writes synchronously)
- Implicitly via `workspace-save-layout`, `workspace-switch-layout`, and `workspace-new`'s home stamp (each calls `workspace--autosave-current-layout` which schedules a debounced write via the internal `workspace-save-state` flush primitive)
- On Emacs shutdown via `kill-emacs-hook` (flushes the in-memory registry without re-capturing)

Auto-save on **workspace** context switch is intentionally NOT a trigger in MVP — see Requirement: Auto-save layout on context switch for the rationale and the deferred two-state-model follow-up.

On Emacs startup the package SHALL restore the persisted workspaces by recreating their tabs. Per-workspace layout application (window-state-put on the saved configuration, plus file-buffer pre-load) is lazy: it happens on the first tab-switch into each restored workspace.

When the persistence file is missing or unreadable the package SHALL start with no workspaces and SHALL NOT raise an error visible to the user beyond an `*Messages*` notice.

#### Scenario: Persistence directory is per-machine
- **WHEN** `jf/machine-role` is `"personal-mac"`
- **THEN** the persistence file lives under `state/workspaces/personal-mac/` relative to `jf/emacs-dir`

#### Scenario: Workspaces survive restart
- **WHEN** the user has workspaces `code` and `notes`, with `code`'s `:buffer-files` containing `~/p/foo.el`
- **AND** Emacs is restarted
- **THEN** both workspaces are restored as tabs
- **AND** on first selection of the `code` tab, `~/p/foo.el` is `find-file-noselect`'d before `window-state-put` is applied
- **AND** the saved window-state's references to `foo.el` resolve to the live buffer (rather than falling back to `*scratch*`)

#### Scenario: Missing persistence file is non-fatal
- **WHEN** no persistence file exists for the current machine role
- **AND** Emacs starts up with the `workspaces` package loaded
- **THEN** no workspaces are created and no error is signalled
- **AND** the user can invoke `workspace-new` normally

---

### Requirement: Explicit save command

The package SHALL provide `workspace-save` as the user-facing explicit save command. Invoking `workspace-save` SHALL:

1. Snapshot the current frame into the current workspace's recent layout-group (`workspace--autosave-current-layout`).
2. Sync `:buffer-files` from bufferlo's current per-tab file list (replace, not merge).
3. Flush the registry to disk synchronously (no debounce).

`workspace-save` SHALL error when not on a workspace-managed tab.

The `workspace-save` binding is `C-x w S`. (The internal `workspace-save-state` flush primitive is retained for use by other code paths but is no longer bound by default — its name historically suggested a capturing save but its body only flushes the in-memory registry.)

#### Scenario: workspace-save captures, syncs, and flushes
- **WHEN** the user is on workspace `code` with a multi-window layout open
- **AND** `code`'s `:buffer-files` is `("~/p/foo.el")` and bufferlo's current list also has `~/p/bar.el`
- **AND** the user invokes `workspace-save`
- **THEN** `code`'s recent layout reflects the current multi-window configuration
- **AND** `code`'s `:buffer-files` is `("~/p/foo.el" "~/p/bar.el")` (in bufferlo's order)
- **AND** the state file on disk contains the new content immediately (no debounce wait)

#### Scenario: workspace-save errors off-workspace
- **WHEN** the current tab is not registered as a workspace
- **AND** the user invokes `workspace-save`
- **THEN** a `user-error` is signalled
- **AND** no disk write occurs

---

### Requirement: Explicit restore command

The package SHALL provide `workspace-restore` as the user-facing command to materialize a saved workspace into a tab. Interactively it SHALL `completing-read` over every workspace name currently in the in-memory registry (which includes saved-but-not-materialized workspaces loaded from disk at startup).

For the chosen workspace:
- If a live tab for that workspace already exists, `workspace-restore` SHALL switch to it (equivalent to `workspace-switch` for that case).
- Otherwise it SHALL create a new tab, name it after the workspace, pre-load `:buffer-files` via `find-file-noselect`, and then `window-state-put` the saved layout onto the current frame's root window.

The `workspace-restore` binding is `C-x w o`. It complements (does not replace) `workspace-switch`, which is scoped to live tabs only.

#### Scenario: Restore an unmaterialized workspace
- **WHEN** workspace `code` is in the registry but has no live tab (e.g. its tab was closed mid-session, or it was loaded from disk and never selected)
- **AND** the user invokes `workspace-restore` and chooses `code`
- **THEN** a new tab named `code` is created and selected
- **AND** the files in `:buffer-files` are loaded as buffers before the window-state is applied
- **AND** the saved window-state is applied to the new tab's window tree
- **AND** the displayed buffers match the saved configuration

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
