## ADDED Requirements

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

### Requirement: Auto-save layout on context switch

The active layout's window configuration SHALL be auto-saved to its layout slot whenever the user switches workspaces or switches layouts within a workspace. No explicit save command SHALL be required for routine work.

`workspace-save-layout` SHALL remain available for naming a new layout or for explicit snapshotting.

#### Scenario: Switching workspaces snapshots the outgoing layout
- **WHEN** the current workspace `code` has the `magit` layout active with a particular window configuration
- **AND** the user rearranges windows in `magit`
- **AND** the user switches to workspace `notes`
- **THEN** the rearranged window configuration is persisted to `code`'s `magit` layout
- **AND** returning to `code` later restores the rearranged configuration

#### Scenario: Switching layouts within a workspace snapshots the outgoing layout
- **WHEN** the user is on layout `home` in workspace `code`
- **AND** the user rearranges windows on `home`
- **AND** the user invokes `workspace-switch-layout` to `magit`
- **THEN** the rearranged configuration is persisted to `code`'s `home` layout

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

### Requirement: kill-buffer remains globally destructive

The `workspaces` package SHALL NOT shadow, advise, or rebind `kill-buffer`. Invoking `kill-buffer` SHALL continue to kill the buffer for all of Emacs, removing it from every workspace's membership as a side effect of buffer death.

This preserves the universal Emacs convention that `C-x k` ends a buffer's life. (Story A in the design.)

#### Scenario: kill-buffer kills globally
- **WHEN** `foo.el` is a member of workspaces `code` and `notes`
- **AND** the user invokes `kill-buffer` on `foo.el` from any workspace
- **THEN** the buffer is killed
- **AND** `foo.el` is no longer a member of `code` or `notes`

---

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine path keyed by `jf/machine-role` (matching the existing convention used by `activities`). Persistence SHALL include:

- The set of workspaces and their names
- The recent-layout pointer for each workspace
- The named layouts for each workspace (window configurations, restorable across restarts)
- The buffer membership for each workspace (by file path; non-file buffers that cannot be restored are omitted)

Persistence SHALL be triggered:
- On every layout context switch (auto-save; Requirement: Auto-save layout on context switch)
- On Emacs shutdown via `kill-emacs-hook`
- Explicitly via `workspace-save-state`

On Emacs startup the package SHALL restore the persisted workspaces and re-establish each workspace's most recent layout (without forcing the user to a particular tab).

When the persistence file is missing or unreadable the package SHALL start with no workspaces and SHALL NOT raise an error visible to the user beyond an `*Messages*` notice.

#### Scenario: Persistence directory is per-machine
- **WHEN** `jf/machine-role` is `"personal-mac"`
- **THEN** the persistence file lives under `state/workspaces/personal-mac/` relative to `jf/emacs-dir`

#### Scenario: Workspaces survive restart
- **WHEN** the user has workspaces `code` and `notes` with multiple layouts each
- **AND** Emacs is restarted
- **THEN** both workspaces are restored as tabs
- **AND** each workspace's most recent layout is the active layout for that workspace
- **AND** file buffers from each workspace's membership are reopened as members on first activation

#### Scenario: Missing persistence file is non-fatal
- **WHEN** no persistence file exists for the current machine role
- **AND** Emacs starts up with the `workspaces` package loaded
- **THEN** no workspaces are created and no error is signalled
- **AND** the user can invoke `workspace-new` normally
