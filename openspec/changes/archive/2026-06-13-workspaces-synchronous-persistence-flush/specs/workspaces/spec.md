## MODIFIED Requirements

### Requirement: Auto-save layout on context switch

The package SHALL auto-save the outgoing layout's window configuration on intra-workspace layout switch, capturing it into the layout-group slot before restoring the destination layout.

The package SHALL ALSO auto-save the outgoing workspace's window configuration on workspace context switch (tab change), capturing it into the recent layout's `:working-state` slot. The explicit-save slot (`:saved-state`) is never written by autosave, so the user's `workspace-save` snapshot is never clobbered by transient or post-kill frame state.

Both auto-saves SHALL flush to disk **synchronously** — immediately, with no
debounce delay. The working-state *capture* on context switch and layout switch
is unchanged from the prior revision; only the flush *timing* changes from a
debounced idle-timer write to a synchronous write. Context switches and layout
switches are discrete, low-frequency user actions, so there is no burst to
coalesce; a synchronous flush guarantees the outgoing state is on disk the
moment the user leaves it.

The package SHALL also auto-save periodically via an idle timer when `workspaces-mode` is enabled (see *Idle save mode*), and once on Emacs shutdown via `kill-emacs-hook`.

#### Scenario: Switching workspaces auto-saves the outgoing workspace's working state
- **WHEN** the user is on workspace `code` and rearranges its windows
- **AND** the user switches to workspace `notes` via `workspace-switch` or by clicking the `notes` tab
- **THEN** `code`'s recent layout's `:working-state` is updated to reflect the rearrangement
- **AND** `code`'s recent layout's `:saved-state` is unchanged
- **AND** returning to `code` (and on next Emacs startup) restores the rearrangement, because `:working-state` is preferred over `:saved-state` when present

#### Scenario: Context-switch autosave is flushed synchronously
- **WHEN** the user rearranges windows on workspace `code` and then switches to workspace `notes`
- **THEN** the persistence file on disk reflects `code`'s updated `:working-state` immediately after the switch returns
- **AND** no idle delay or debounce timer is required for the write to land

#### Scenario: Layout-switch autosave is flushed synchronously
- **WHEN** the current workspace `code` is on layout `home`, the user rearranges windows, and then invokes `workspace-switch-layout` choosing `magit`
- **THEN** `home`'s `:working-state` is captured and the persistence file on disk reflects it immediately after the command returns
- **AND** no idle delay or debounce timer is required for the write to land

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

Persistence to disk SHALL be triggered by all of, and every trigger SHALL
flush **synchronously** (immediately, with no debounce delay):
- Explicit `workspace-save` (synchronous flush; writes `:saved-state`).
- `workspace-save-layout` and `workspace-new`'s home stamp (synchronous
  flush; writes `:saved-state`).
- Workspace context switch (synchronous flush; writes `:working-state` of the outgoing workspace).
- Intra-workspace layout switch (synchronous flush; writes `:working-state` of the outgoing layout).
- `workspaces-mode` idle timer (synchronous flush; writes `:working-state` of the current workspace).
- `kill-emacs-hook` (synchronous flush; captures `:working-state` of the current workspace once before writing).

The package SHALL NOT debounce any of these flushes. Every trigger above is
a discrete, low-frequency, quiescent event with no burst to coalesce, so a
debounced (idle-timer) write provides no benefit and only delays the on-disk
write. (A debounce is warranted only for a high-frequency trigger such as
autosave on window-configuration change, which is not part of this package;
should such a trigger be added, it would re-introduce a debounce scoped to
itself.)

In addition to the schema, hydration, and flush-trigger guarantees above,
persistence SHALL be **readable by construction** and **corruption-safe**:

**Readable-by-construction.** The serialized persistence form SHALL NOT
contain any live Emacs object that `read` cannot reconstruct (a buffer,
marker, overlay, frame, window, process, or non-symbol function object).

- Window-state captured for a layout SHALL pass every persistent window
  parameter whose value embeds such an object through a
  *serialize/deserialize translator* that converts it to a readable form
  and back. In particular, the `window-preserved-size` parameter's buffer
  object SHALL be serialized as the buffer's name and restored via
  buffer-name lookup, so a preserved size survives restart without
  writing an unreadable object.
- The buffer-reincarnation record captured per window leaf (its
  `bookmark-make-record` result) SHALL be scrubbed of any unreadable
  value before persistence.
- Before committing a write, the package SHALL verify the serialized form
  round-trips through `read`. If it does not, the package SHALL abort the
  write, SHALL leave the previously persisted file intact, and SHALL emit
  a warning — it SHALL NOT write an unreadable file.

**Corruption-safe I/O.** Writing the persistence file SHALL be atomic
with respect to the live file: a write SHALL NOT truncate or partially
overwrite the existing file if it is interrupted (e.g. serialize to a
sibling temp file and rename over the target).

A persistence file that exists but cannot be read (parse/`read` failure)
is **present-but-unreadable**, which the package SHALL distinguish from
**absent**:

- On a present-but-unreadable file, the package SHALL preserve the file by
  renaming it to a timestamped sibling (`workspaces.eld.corrupt-<ts>`),
  SHALL emit a warning naming the backup path, and SHALL NOT overwrite the
  original path with the (empty) in-memory registry.
- After a present-but-unreadable load, the package SHALL suppress all
  persistence writes for the session (every flush trigger no-ops with a
  one-time warning) — so an autosave, idle flush, or `kill-emacs` flush
  cannot destroy data by reserializing an empty or partial registry over
  the path.
- On an absent file, the package SHALL start fresh and persistence SHALL
  proceed normally (writes permitted), without raising an error visible to
  the user beyond an `*Messages*` notice.

The flush triggers listed above are unchanged except that each is gated by
this suppression rule when the prior load was present-but-unreadable.

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

#### Scenario: A deliberate command flushes synchronously
- **WHEN** the user invokes `workspace-new`, `workspace-save-layout`, or
  `workspace-switch-layout`
- **THEN** the persistence file on disk reflects the resulting registry
  state immediately after the command returns
- **AND** the write does not wait on an idle timer or debounce delay

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

#### Scenario: A live Emacs object is never written to disk
- **WHEN** a workspace layout is captured whose window tree carries a
  `window-preserved-size` parameter referencing a buffer (live or killed)
- **AND** the layout is serialized for persistence
- **THEN** the serialized form contains no `#<…>` token and is readable
  by `read`
- **AND** the `window-preserved-size` value is encoded by buffer name

#### Scenario: Preserved window size round-trips across restart
- **WHEN** a workspace with a preserved-size window is saved and the
  persistence file is reloaded in a fresh session
- **THEN** the persisted form deserializes without error
- **AND** the preserved-size parameter is rehydrated against the live
  (reincarnated) buffer by name

#### Scenario: An unreadable persistence file is preserved, not overwritten
- **WHEN** the persistence file exists but contains an unreadable object
  (e.g. `#<killed buffer>`) so `read` fails
- **AND** Emacs starts up
- **THEN** the file is renamed to `workspaces.eld.corrupt-<timestamp>`
  and a warning names that path
- **AND** the in-memory registry is empty (no workspaces hydrated)
- **AND** the original path is NOT overwritten with `(:version 3
  :workspaces nil)`

#### Scenario: Autosave is suppressed after a failed load
- **WHEN** a startup load was present-but-unreadable (persistence blocked
  for the session)
- **AND** an autosave, idle flush, or `kill-emacs` flush subsequently fires
- **THEN** no write to the persistence path occurs (the flush no-ops with
  a one-time warning)

#### Scenario: A write that would be unreadable is aborted
- **WHEN** a persistence write is attempted whose serialized form fails the
  pre-write `read` round-trip (an unknown unreadable value slipped past the
  translators)
- **THEN** the write is aborted and a warning is emitted
- **AND** the previously persisted file is left intact (not truncated or
  replaced)

#### Scenario: An absent file starts fresh and saves normally
- **WHEN** no persistence file exists
- **AND** Emacs starts up and the user creates and saves a workspace
- **THEN** the registry starts empty, persistence is not blocked, and the
  save writes the file normally

---

### Requirement: Idle save mode

The package SHALL provide `workspaces-mode`, a global minor mode that, when enabled, runs an idle timer that captures the current workspace's `:working-state` periodically. The interval SHALL be configurable via `workspaces-mode-idle-frequency` (default 60 seconds).

`workspaces-mode` SHALL be off by default; the user opts in via `(workspaces-mode 1)` or `M-x workspaces-mode`.

The idle-save trigger SHALL share the same capture code path as the workspace-context-switch autosave, including:
- Respecting `workspace-anti-save-predicates` (Requirement: Anti-save predicates).
- Writing only to `:working-state` (never `:saved-state`).
- Flushing to disk **synchronously** when it captures. The idle tick fires at
  most once per idle period, so there is no burst to coalesce; a synchronous
  flush also avoids the `run-with-idle-timer` already-idle trap, in which a
  debounced (short idle-delay) write armed from within an already-idle period
  is deferred to a future idle period — which would silently defeat the idle
  save's crash-safety purpose.

`workspaces-mode` SHALL clean up its idle timer when disabled.

#### Scenario: Idle save captures working state after the configured interval
- **WHEN** `workspaces-mode` is enabled and `workspaces-mode-idle-frequency` is 60
- **AND** the user is on workspace `code` and rearranges windows
- **AND** Emacs is idle for ≥ 60 seconds
- **THEN** `code`'s `:working-state` reflects the rearrangement
- **AND** `:saved-state` is unchanged
- **AND** the persistence file on disk reflects the autosave synchronously when the idle tick fires (no further debounce delay)

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

---

## REMOVED Requirements

<!-- No spec-level requirement is removed. The debounce machinery
(`workspace-save-state`, `workspace--save-timer`, `workspace-save-idle-delay`,
`workspace--persistence-after-autosave`, and the blanket advice on
`workspace--autosave-current-layout`) is an implementation detail; its removal
is captured in the MODIFIED flush-trigger semantics above and in design.md. The
`workspace-save-idle-delay` defcustom is removed as a user-facing customization
(BREAKING for anyone who set it), but it does not correspond to its own spec
requirement. -->
