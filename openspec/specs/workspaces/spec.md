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
`workspaces`, `workspaces-mode`). User-facing prefix: `C-x w`.

The on-disk persistence schema is `:version 2`. Each layout holds a
`:saved-state` slot (written only by explicit `workspace-save`) and
a `:working-state` slot (written by autosaves). Buffer reincarnation
across restart uses bookmark records embedded in window-state leaves
so point position, narrowing, and non-file buffers (magit, eshell,
*Messages*) survive the round-trip. Background idle save is
available as an opt-in `workspaces-mode`. The prior-art analysis
that informed these choices is preserved in the archived
`refine-workspaces-two-state-layout` change directory under
`notes/activities-patterns-catalog.md`.

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

### Requirement: Auto-save layout on context switch

The package SHALL auto-save the outgoing layout's window configuration on intra-workspace layout switch, capturing it into the layout-group slot before restoring the destination layout.

The package SHALL ALSO auto-save the outgoing workspace's window configuration on workspace context switch (tab change), capturing it into the recent layout's `:working-state` slot. The explicit-save slot (`:saved-state`) is never written by autosave, so the user's `workspace-save` snapshot is never clobbered by transient or post-kill frame state.

The package SHALL also auto-save periodically via an idle timer when `workspaces-mode` is enabled (see *Idle save mode*), and once on Emacs shutdown via `kill-emacs-hook`.

The MVP-gap scenario (workspace switch does not auto-save) is **removed** by this revision; the requirement is now the inverse.

#### Scenario: Switching workspaces auto-saves the outgoing workspace's working state
- **WHEN** the user is on workspace `code` and rearranges its windows
- **AND** the user switches to workspace `notes` via `workspace-switch` or by clicking the `notes` tab
- **THEN** `code`'s recent layout's `:working-state` is updated to reflect the rearrangement
- **AND** `code`'s recent layout's `:saved-state` is unchanged
- **AND** returning to `code` (and on next Emacs startup) restores the rearrangement, because `:working-state` is preferred over `:saved-state` when present

#### Scenario: Autosave does not clobber an explicit save
- **WHEN** the user has invoked `workspace-save` on workspace `code` producing a `:saved-state` snapshot S
- **AND** the user later rearranges windows and switches to workspace `notes`
- **THEN** `code`'s `:saved-state` is still S
- **AND** `code`'s `:working-state` reflects the rearrangement
- **AND** `workspace-revert` (Requirement: Working-state revert) restores S on `code`

#### Scenario: Autosave is suppressed during debug or minibuffer activity
- **WHEN** a `*Backtrace*` window is visible on the outgoing workspace's frame, or a minibuffer is active
- **AND** the user switches to a different workspace
- **THEN** no autosave occurs (the predicates in `workspace-anti-save-predicates` short-circuit it)
- **AND** the workspace's `:working-state` is unchanged

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

Workspaces and their layouts SHALL persist to disk under a per-machine path keyed by `jf/machine-role`. The persistence file SHALL use **schema version 2**:

- Each layout SHALL carry two window-state slots:
  - `:saved-state` — written only by explicit `workspace-save` (or its variants `workspace-save-layout`, `workspace-switch-layout`, `workspace-new`'s home stamp). Authoritative across restarts.
  - `:working-state` — written only by autosaves (workspace context switch, idle timer, kill-emacs). May be nil.
- Each layout SHALL carry an `:etc` alist slot for forward-compatible extension data. Replaces the legacy single-purpose `:git-state` placeholder.
- Each leaf in a layout's window-state SHALL carry a `workspace-buffer` entry in its `parameters`, encoding a bookmark record, filename, buffer name, and narrowed/indirect flags (see *Buffer reincarnation across restart*).

The package SHALL NOT support v1 persistence files. The package is pre-alpha; users with v1 files on disk are expected to delete the state file before running the v2 code. The reader SHALL emit a non-fatal `*Messages*` notice for files whose `:version` is not 2, and proceed as if no persistence file exists.

On Emacs startup the package SHALL restore the persisted workspaces by recreating their tabs. Per-workspace layout application is lazy: it happens on the first tab-switch into each restored workspace. The lazy application SHALL prefer `:working-state` when present, falling back to `:saved-state`.

Persistence to disk SHALL be triggered by all of:
- Explicit `workspace-save` (synchronous flush; writes `:saved-state`).
- Workspace context switch (debounced flush; writes `:working-state` of the outgoing workspace).
- Intra-workspace layout switch (debounced flush; writes `:working-state` of the outgoing layout).
- `workspaces-mode` idle timer (debounced flush; writes `:working-state` of the current workspace).
- `kill-emacs-hook` (synchronous flush; captures `:working-state` of the current workspace once before writing).

When the persistence file is missing or unreadable the package SHALL start with no workspaces and SHALL NOT raise an error visible to the user beyond an `*Messages*` notice.

#### Scenario: V1 persistence file is rejected with a notice
- **WHEN** a workspaces persistence file exists with `:version 1`
- **AND** Emacs starts up
- **THEN** an `*Messages*` notice is emitted naming the file and the version mismatch
- **AND** no workspaces are restored from the file
- **AND** the user can invoke `workspace-new` normally to start fresh

#### Scenario: Restart restores working-state, not saved-state, when both present
- **WHEN** workspace `code` has `:saved-state` S (last explicit save) and `:working-state` W (latest autosave)
- **AND** Emacs is restarted and the user selects the `code` tab
- **THEN** the window configuration restored on `code`'s frame is W, not S
- **AND** the buffers in W are reincarnated via the bookmark chain (Requirement: Buffer reincarnation across restart)

---

### Requirement: Explicit save command

The package SHALL provide `workspace-save` as the user-facing explicit save command. Invoking `workspace-save` SHALL:

1. Snapshot the current frame into the current workspace's recent layout-group's `:saved-state` slot.
2. Clear the recent layout-group's `:working-state` slot to nil — the explicit save becomes the new clean baseline.
3. Sync `:buffer-files` from bufferlo's current per-tab file list (replace, not merge).
4. Flush the registry to disk synchronously (no debounce).

`workspace-save` SHALL error when not on a workspace-managed tab. The `workspace-save` binding is `C-x w S`.

#### Scenario: workspace-save clears any working-state drift
- **WHEN** the user has been rearranging windows on workspace `code` and an autosave has populated `:working-state`
- **AND** the user invokes `workspace-save`
- **THEN** `code`'s recent layout's `:saved-state` reflects the current frame
- **AND** `code`'s recent layout's `:working-state` is nil
- **AND** the next restart restores the explicit save (since `:working-state` is empty, the restore precedence falls through to `:saved-state`)

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

---

### Requirement: Buffer reincarnation across restart

Each window-state leaf written by `workspace-save` (or by any autosave path) SHALL carry a `workspace-buffer` entry in its window `parameters` slot. The entry encodes enough information to reconstruct the buffer's visible state across a restart, even when the buffer is non-file (`*Messages*`, `magit-status`, `eshell`, narrowed indirect buffers, etc).

A `workspace-buffer` SHALL contain at least:
- A bookmark record produced by `bookmark-make-record` (the standard Emacs bookmark API).
- The buffer's filename (for file-backed buffers), or nil.
- The buffer's name string.
- Flags indicating whether the buffer is narrowed and/or an indirect buffer.

On restore, the package SHALL walk the saved window-state and replace each leaf's buffer reference with a freshly reincarnated buffer. The reincarnation SHALL attempt, in order:

1. **Bookmark restore** — `bookmark-handle-bookmark` on the saved record. This restores point position, narrowing region, major-mode-specific state, and works for non-file buffers via their registered bookmark handlers.
2. **Filename fallback** — `find-file-noselect` on the saved filename, if the bookmark restore errored or returned no buffer.
3. **Name fallback** — `get-buffer` on the saved name, if neither of the above succeeded.
4. **Error buffer** — a visible, named error buffer describing what failed, when all three above fail. The window slot is never left empty.

The package SHALL trap read-time errors per leaf so that one unreadable bookmark (e.g. the `help-mode` natively-compiled-subr `bug#56643`) does not abort the entire restore. Each failed leaf falls through to the next step in the chain.

The package SHALL defer the `window-state-put` call via `run-at-time nil nil ...` so the restore does not race against `bookmark--jump-via`'s buffer-display call.

#### Scenario: Point position is preserved across restart
- **WHEN** the user has `~/p/foo.el` open with point at line 234
- **AND** the user invokes `workspace-save` and quits Emacs
- **AND** Emacs is restarted and the user selects the workspace tab
- **THEN** `~/p/foo.el` is open in the restored window
- **AND** point is at line 234 (not line 1)

#### Scenario: Narrowed buffer comes back narrowed
- **WHEN** the user has `~/p/big.el` narrowed to a region in workspace `code`
- **AND** the user invokes `workspace-save` and restarts Emacs
- **AND** the user selects the `code` tab
- **THEN** `~/p/big.el` is displayed
- **AND** the narrowing is restored to the same region
- **AND** `widen` returns the buffer to its full extent (so the narrowing is the bookmark's, not a buffer truncation)

#### Scenario: Magit status buffer is reincarnated, not replaced
- **WHEN** the user has a `magit-status` window open on `~/repo/` in workspace `code`
- **AND** the user invokes `workspace-save` and restarts Emacs
- **AND** the user selects the `code` tab
- **THEN** the window shows a live `magit-status` buffer for `~/repo/` (via magit's registered bookmark handler)
- **AND** the window is NOT showing `*scratch*`

#### Scenario: One unreadable bookmark does not abort the restore
- **WHEN** a saved layout has three windows, one of which holds a `*Help*` buffer whose bookmark contains a natively-compiled subr (`bug#56643`)
- **AND** the user restores the layout
- **THEN** the other two windows restore successfully via the bookmark chain
- **AND** the third window holds either the file-based fallback (if the help buffer was visiting a file) or a clearly named error buffer
- **AND** no exception escapes to the user

---

### Requirement: Working-state revert

The package SHALL provide `workspace-revert` as the user-facing command to discard accumulated autosave drift on the current workspace and return to its last explicit-save baseline.

Invoking `workspace-revert` SHALL:
1. Clear the current workspace's recent layout-group's `:working-state` slot to nil.
2. Apply the recent layout-group's `:saved-state` via the standard restore path (including buffer reincarnation per Requirement: Buffer reincarnation across restart).
3. Flush the registry to disk synchronously.

`workspace-revert` SHALL error when not on a workspace-managed tab. The `workspace-revert` binding is `C-x w r`.

#### Scenario: Revert restores the last explicit save
- **WHEN** the user is on workspace `code` with `:saved-state` S and `:working-state` W ≠ S
- **AND** the user invokes `workspace-revert`
- **THEN** the displayed window configuration matches S
- **AND** `code`'s `:working-state` is nil
- **AND** the file on disk reflects the cleared `:working-state`

#### Scenario: Revert errors off-workspace
- **WHEN** the current tab is not registered as a workspace
- **AND** the user invokes `workspace-revert`
- **THEN** a `user-error` is signalled
- **AND** no disk write occurs

#### Scenario: Revert with no working-state is a no-op
- **WHEN** the user is on workspace `code` whose `:working-state` is already nil
- **AND** the user invokes `workspace-revert`
- **THEN** `:saved-state` is applied (resetting any in-memory drift not captured by autosave)
- **AND** no error is signalled
- **AND** the file on disk is unchanged (or is a no-op rewrite of the same content)

---

### Requirement: Anti-save predicates

The package SHALL provide `workspace-anti-save-predicates`, a customizable list of nullary predicate functions consulted before any autosave. If any predicate returns non-nil, the autosave SHALL be skipped (the autosave is silent; the predicates do not signal user-visible errors).

The default predicate list SHALL include:
- `active-minibuffer-window` (built-in) — skip while a minibuffer is active.
- `workspace--backtrace-visible-p` — skip when a `*Backtrace*` window is visible on the current frame.

Explicit `workspace-save` SHALL NOT consult the predicate list; the user's deliberate save is never blocked.

The predicates SHALL run in the context of the outgoing workspace (so they can read its frame's window list, current minibuffer state, etc).

#### Scenario: Minibuffer activity blocks autosave
- **WHEN** the user has invoked `M-x ` and the minibuffer is open
- **AND** an autosave-trigger fires (idle timer, tab switch, etc.)
- **THEN** the autosave is skipped
- **AND** the workspace's `:working-state` is unchanged

#### Scenario: Backtrace visibility blocks autosave
- **WHEN** a `*Backtrace*` window is visible on the current frame (e.g. mid debug session)
- **AND** the user switches workspaces
- **THEN** the autosave is skipped
- **AND** the workspace's `:working-state` is unchanged
- **AND** the explicit `:saved-state` (presumably from before the debug session) is undisturbed

#### Scenario: Explicit save bypasses the predicates
- **WHEN** the predicate list contains a predicate that always returns non-nil
- **AND** the user invokes `workspace-save`
- **THEN** the save proceeds (writes `:saved-state`, clears `:working-state`, flushes)
- **AND** the predicates are not consulted

---

### Requirement: Idle save mode

The package SHALL provide `workspaces-mode`, a global minor mode that, when enabled, runs an idle timer that captures the current workspace's `:working-state` periodically. The interval SHALL be configurable via `workspaces-mode-idle-frequency` (default 60 seconds).

`workspaces-mode` SHALL be off by default; the user opts in via `(workspaces-mode 1)` or `M-x workspaces-mode`.

The idle-save trigger SHALL share the same code path as the workspace-context-switch autosave, including:
- Respecting `workspace-anti-save-predicates` (Requirement: Anti-save predicates).
- Writing only to `:working-state` (never `:saved-state`).
- Coalescing burst-fire via the existing debounce in `workspace-save-state`.

`workspaces-mode` SHALL clean up its idle timer when disabled.

#### Scenario: Idle save captures working state after the configured interval
- **WHEN** `workspaces-mode` is enabled and `workspaces-mode-idle-frequency` is 60
- **AND** the user is on workspace `code` and rearranges windows
- **AND** Emacs is idle for ≥ 60 seconds
- **THEN** `code`'s `:working-state` reflects the rearrangement
- **AND** `:saved-state` is unchanged
- **AND** the persistence file on disk reflects the autosave (after the standard debounce delay)

#### Scenario: Disabling workspaces-mode stops the idle timer
- **WHEN** `workspaces-mode` is enabled and an idle timer is registered
- **AND** the user invokes `(workspaces-mode -1)` or `M-x workspaces-mode` to toggle off
- **THEN** the idle timer is cancelled
- **AND** subsequent idle periods do not trigger autosaves
- **AND** explicit `workspace-save` continues to work normally

#### Scenario: Idle save respects anti-save predicates
- **WHEN** `workspaces-mode` is enabled and a `*Backtrace*` window is visible
- **AND** Emacs goes idle past `workspaces-mode-idle-frequency`
- **THEN** the idle save is skipped (predicate short-circuit)
- **AND** the workspace's `:working-state` is unchanged
