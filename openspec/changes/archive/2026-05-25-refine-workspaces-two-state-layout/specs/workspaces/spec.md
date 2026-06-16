## MODIFIED Requirements

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

## ADDED Requirements

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
