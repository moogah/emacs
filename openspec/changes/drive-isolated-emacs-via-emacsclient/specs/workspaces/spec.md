## ADDED Requirements

### Requirement: Public name-addressed workspace API

The package SHALL provide public (non-internal) functions, usable by programs
without reference to the selected frame or current tab, to:

- list the names of all registered workspaces;
- get a workspace by name;
- list a named workspace's layouts, including each layout's metadata (see
  Requirement: Layout metadata), optionally filtered by origin and/or source;
- find the workspace that owns a directory: the registered workspace whose `:home`
  is the longest prefix of the directory's absolute path, or none;
- report whether a named layout of a named workspace is **on screen** — that is,
  the layout is the workspace's recent layout and the workspace has an open tab.

These functions SHALL NOT change any frame, tab, window, buffer or persisted state.
Other modules SHALL use them instead of reading the internal registry directly.

#### Scenario: Listing layouts of a workspace that has no tab
- **WHEN** workspace `notes` is registered (restored from disk) but has no open tab
- **AND** a program lists `notes`' layouts by name
- **THEN** it receives `notes`' layout names and metadata
- **AND** no tab is created and the selected frame is unchanged

#### Scenario: Workspace found from a nested directory
- **WHEN** workspace `emacs-mcp` has home `~/emacs-workspaces/emacs-mcp/`
- **AND** a program asks which workspace owns
  `~/emacs-workspaces/emacs-mcp/emacs/openspec`
- **THEN** the answer is `emacs-mcp`
- **AND** a directory under no workspace home yields none

#### Scenario: Listing agent layouts for one source
- **WHEN** workspace `code` has layouts `home` (origin `user`), `ask/a1` (origin
  `agent`, source `a1`) and `ask/a2` (origin `agent`, source `a2`)
- **AND** a program lists `code`'s layouts filtered by origin `agent` and source `a1`
- **THEN** only `ask/a1` is returned

#### Scenario: On-screen check
- **WHEN** workspace `code` has an open tab and its recent layout is `home`
- **THEN** `home` of `code` is reported on screen
- **AND** any other layout of `code`, and every layout of a workspace with no open
  tab, is reported not on screen

### Requirement: Programmatic layout addition

The package SHALL provide a public function that adds a layout to a named
workspace, given the layout's name, its saved state (a captured window-state or a
recipe) and optional metadata. Adding a layout SHALL:

1. store the given state as the layout's `:saved-state` with `:working-state` nil;
2. leave the workspace's recent-layout pointer unchanged;
3. not create, select or modify any frame, tab, window or buffer;
4. flush the registry to disk synchronously.

It SHALL work for any registered workspace, whether or not that workspace has an
open tab, and SHALL be subject to Requirement: Protection of user layouts from
programmatic writes.

#### Scenario: Adding a layout does not disturb the user
- **WHEN** the user is working on workspace `code`
- **AND** a program adds layout `demo` to workspace `code`
- **THEN** `code` has a layout `demo` whose `:working-state` is nil
- **AND** `code`'s recent-layout pointer, the selected frame, the current tab and
  the window configuration are all unchanged
- **AND** the persistence file on disk contains `demo`

#### Scenario: Adding a layout to a workspace without a tab
- **WHEN** workspace `notes` has no open tab
- **AND** a program adds layout `review` to `notes`
- **THEN** `notes` has layout `review` and still has no open tab

#### Scenario: User drift on an added layout is revertible
- **WHEN** a program has added layout `demo` to workspace `code`
- **AND** the user switches to `demo`, rearranges windows, and an autosave captures
  the rearrangement
- **AND** the user invokes `workspace-revert`
- **THEN** the layout is shown as originally added and its `:working-state` is nil

### Requirement: Programmatic layout removal

The package SHALL provide a public function that removes a named layout from a named
workspace on an agent's behalf. It SHALL refuse, with an error naming the reason and
without changing any state, when the layout has origin `user`, when it is on screen,
or when it is the `home` layout. Otherwise it SHALL remove the layout, leave the
recent-layout pointer unchanged unless it named the removed layout (in which case
the pointer is reassigned as `workspace-delete-layout` does), touch no frame, tab,
window or buffer, and flush the registry to disk synchronously.

#### Scenario: Agent layout removed when its artifact closes
- **WHEN** workspace `code` has layout `ask/a1` with origin `agent`, not on screen
- **AND** a program removes `ask/a1` from `code`
- **THEN** `code` no longer has `ask/a1`
- **AND** the selected frame, current tab and window configuration are unchanged

#### Scenario: User layout cannot be removed programmatically
- **WHEN** workspace `code` has layout `review` with origin `user`
- **AND** a program attempts to remove `review`
- **THEN** the removal is refused with an error stating that `review` belongs to the
  user, and `review` is unchanged

#### Scenario: On-screen layout is not removed
- **WHEN** layout `ask/a1` of `code` is on screen
- **AND** a program attempts to remove it
- **THEN** the removal is refused with an error stating that `ask/a1` is on screen

### Requirement: Layout metadata

Each layout SHALL carry metadata in its `:etc` slot:

- `:origin` — `user` or `agent`. A layout without an origin SHALL be treated as
  `user`. Layouts created by the user's commands SHALL have origin `user`; layouts
  added programmatically on an agent's behalf SHALL have origin `agent`.
- `:description` — optional free text.
- `:source` — optional reference to what produced the layout (e.g. an OpenSpec
  change or orchestration cycle).

Any explicit save that writes a layout's `:saved-state` (`workspace-save`,
`workspace-save-layout`) SHALL set that layout's origin to `user`.

#### Scenario: Pre-existing layouts count as the user's
- **WHEN** a layout loaded from a version-3 file has no metadata
- **THEN** its origin is reported as `user`

#### Scenario: Explicit save takes ownership
- **WHEN** layout `demo` of workspace `code` has origin `agent`
- **AND** the user, with `demo` as the recent layout, invokes `workspace-save`
- **THEN** `demo`'s origin is `user`
- **AND** its `:saved-state` is a captured window-state of the current frame

### Requirement: Protection of user layouts from programmatic writes

Programmatic layout addition SHALL refuse, with an error naming the conflict and
without changing any state, when:

- a layout of the same name exists in that workspace with origin `user`; or
- a layout of the same name exists with origin `agent` and is on screen (see
  Requirement: Public name-addressed workspace API).

Otherwise, when a layout of the same name exists with origin `agent`, it SHALL be
replaced, discarding any `:working-state` it had.

#### Scenario: User layout is never overwritten
- **WHEN** workspace `code` has a layout `review` with origin `user`
- **AND** a program attempts to add layout `review` to `code`
- **THEN** the addition is refused with an error stating that `review` belongs to
  the user
- **AND** `review`'s saved state, working state and metadata are unchanged

#### Scenario: On-screen agent layout is not replaced
- **WHEN** workspace `code` has an open tab whose recent layout is `demo`, origin
  `agent`
- **AND** a program attempts to add layout `demo` to `code`
- **THEN** the addition is refused with an error stating that `demo` is on screen

#### Scenario: Off-screen agent layout is replaced
- **WHEN** workspace `code` has layout `demo`, origin `agent`, not on screen, with a
  non-nil `:working-state`
- **AND** a program adds layout `demo` to `code` with new state S
- **THEN** `demo`'s `:saved-state` is S and its `:working-state` is nil

### Requirement: Recipe layouts

The package SHALL accept a **recipe** as a layout's `:saved-state`: a declarative
description of the layout rather than a captured window-state. A recipe SHALL at minimum describe a
split direction (`columns` or `rows`) and an ordered list of panes, where each pane
either names a file by absolute path (optionally with a line number) or is itself a
split with its own direction and panes, to any depth.

When a layout whose effective state is a recipe is applied (by
`workspace-switch-layout`, `workspace-restore` or `workspace-revert`), the package
SHALL build the described windows in the selected frame through the same restore
path used for captured states: each pane shows its file, with point at the given
line when one is given, and the panes of each split share that split's space evenly
along its direction.
A pane whose file no longer exists SHALL be shown as an explanatory placeholder
without preventing the other panes from being built.

A recipe is never written to `:working-state`; autosaves capture the frame as
usual. An explicit save over a recipe layout SHALL replace the recipe with a
captured window-state.

#### Scenario: Switching to a recipe layout builds it
- **WHEN** workspace `code` has layout `demo` whose saved state is a recipe with
  split `columns` and panes `a.org` at line 40 and `design.md`
- **AND** the user invokes `workspace-switch-layout` and chooses `demo`
- **THEN** the frame shows two side-by-side windows, the left visiting `a.org` with
  point on line 40 and the right visiting `design.md`

#### Scenario: Nested recipe builds a main pane beside a stack
- **WHEN** a recipe has split `columns` with panes `ask.org` and a nested split
  `rows` of `scaffold.org` (line 157) and `design.md`
- **AND** the layout is applied
- **THEN** the frame shows `ask.org` on the left, and on the right `scaffold.org`
  above `design.md`, with point on line 157 of `scaffold.org`

#### Scenario: Revert rebuilds from the recipe
- **WHEN** the user has rearranged recipe layout `demo` and an autosave has captured
  the rearrangement
- **AND** the user invokes `workspace-revert`
- **THEN** the recipe is applied again against the current contents of its files
- **AND** `demo`'s `:working-state` is nil and its `:saved-state` is still the recipe

#### Scenario: Missing file does not break the layout
- **WHEN** a recipe names a file that has since been deleted
- **AND** the layout is applied
- **THEN** that pane shows a placeholder naming the missing file
- **AND** the other panes are built normally

### Requirement: Agent tool for adding layouts

When gptel is loaded, the package SHALL register two gptel tools.

`workspace_add_layout` takes a workspace — given either by name or by a directory
the workspace owns — a layout name, a split direction, a list of panes (each
`{file, line?}` or a nested `{split, panes}`), and optional description and source.
It SHALL:

- resolve the workspace (by name, or via the owning-directory lookup) and validate
  that every file exists (relative paths resolved against the workspace home) and
  every line number is positive, returning an error that names the problem
  otherwise;
- add the layout as a recipe with origin `agent` and the given description and
  source, via programmatic layout addition;
- open no files and create no frames, tabs or windows;
- return refusals from Requirement: Protection of user layouts from programmatic
  writes as errors naming the conflict.

`workspace_remove_layout` takes a workspace (by name or directory) and a layout name
and removes the layout via programmatic layout removal, returning its refusals as
errors naming the reason.

#### Scenario: Agent adds a demonstration layout
- **WHEN** an agent calls `workspace_add_layout` for workspace `code`, layout
  `demo`, split `columns`, panes `config/a.org` (line 40) and `design.md`
- **THEN** `code` has a layout `demo` with origin `agent` whose saved state is the
  corresponding recipe with absolute paths
- **AND** no buffer visiting either file was created by the call

#### Scenario: Agent addresses the workspace by directory
- **WHEN** an agent calls `workspace_add_layout` with directory
  `~/emacs-workspaces/emacs-mcp/emacs` instead of a workspace name
- **THEN** the layout is added to workspace `emacs-mcp`, whose home owns that
  directory

#### Scenario: Agent removes its ask layout
- **WHEN** an agent calls `workspace_remove_layout` for an agent-origin layout that
  is not on screen
- **THEN** the layout is removed and the tool reports success

#### Scenario: Invalid file is reported
- **WHEN** an agent calls `workspace_add_layout` with a pane naming a file that does
  not exist
- **THEN** the tool returns an error naming that file
- **AND** no layout is added

## MODIFIED Requirements

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine
path keyed by `jf/machine-role`. The persistence file SHALL be written
with **schema version 4**; the reader SHALL accept versions 3 and 4
(a version-3 file is a valid version-4 file that contains no recipe
layouts and no layout metadata):

- Each workspace plist SHALL carry a required `:home` slot holding the
  absolute filesystem path of its home directory. Loading a workspace
  whose serialized form lacks `:home` is a data error (treated like a
  bad-version file: notice + skip).
- Each layout SHALL carry two state slots:
  - `:saved-state` — a captured window-state or a recipe (see
    Requirement: Recipe layouts). Written only by explicit `workspace-save`
    (or its variants `workspace-save-layout`, `workspace-switch-layout`,
    `workspace-new`'s home stamp) and by programmatic layout addition
    (see Requirement: Programmatic layout addition). Authoritative across
    restarts.
  - `:working-state` — always a captured window-state; written only by
    autosaves (workspace context switch, idle timer, kill-emacs). May be
    nil.
- Each layout SHALL carry an `:etc` alist slot for forward-compatible
  extension data, including layout metadata (see Requirement: Layout
  metadata). Replaces the legacy single-purpose `:git-state`
  placeholder.
- Each leaf in a captured window-state SHALL carry a `workspace-buffer`
  entry in its `parameters`, encoding a bookmark record, filename, buffer
  name, and narrowed/indirect flags (see *Buffer reincarnation across
  restart*).

The package SHALL NOT support v1 or v2 persistence files. The package is
pre-alpha; users with older files on disk are expected to delete the
state file before running the current code. The reader SHALL emit a
non-fatal `*Messages*` notice for files whose `:version` is neither 3
nor 4, and proceed as if no persistence file exists. A version-3 file
SHALL be loaded without loss and rewritten as version 4 on the next
flush.

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
- Programmatic layout addition (synchronous flush; writes `:saved-state`
  of the added layout).
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

#### Scenario: Version-3 file loads and is upgraded on next flush
- **WHEN** the persistence file is `:version 3` and contains workspace
  `alpha` with captured layouts
- **AND** Emacs starts up and a flush trigger then fires
- **THEN** `alpha` and its layouts are restored unchanged
- **AND** the file on disk is rewritten with `:version 4`

#### Scenario: Recipe and metadata round-trip across restart
- **WHEN** workspace `alpha` has a layout whose `:saved-state` is a recipe
  and whose metadata records origin `agent` and a description
- **AND** Emacs is restarted
- **THEN** the layout's `:saved-state` is the same recipe and its metadata
  is unchanged
