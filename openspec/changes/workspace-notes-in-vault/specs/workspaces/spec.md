# workspaces capability — delta for workspace-notes-in-vault

The home directory becomes purely operational: no `home.org`, no
`sessions/`. Identity gains an opaque `:node-id` slot (an org ID supplied
by an integration); display name and home layout become inversion points
so the package never reads a note itself.

## ADDED Requirements

### Requirement: Workspace node identity

Each workspace record SHALL carry a `:node-id` slot holding an opaque
string or nil. The package SHALL treat the value as an identity token it
neither interprets nor resolves: it is set through a public setter
(`workspace-set-node-id`), exposed through a public getter, included in
the anchor payload, and persisted. A nil `:node-id` is valid (the
workspace is not anchored to any external identity). `workspace-new` SHALL
accept an optional node ID argument so a caller can register a workspace
against a pre-existing identity.

#### Scenario: Node ID round-trips through persistence
- **WHEN** an integration sets workspace `myproj`'s `:node-id` to `"ABC"`
  and the persistence file is flushed and reloaded
- **THEN** the restored `myproj` record's `:node-id` is `"ABC"`

#### Scenario: Absent node ID loads as nil
- **WHEN** a persisted workspace plist lacks `:node-id`
- **THEN** the workspace is restored normally with `:node-id` nil and no
  notice is emitted

### Requirement: Display-name inversion point

The package SHALL expose `workspace-display-name-function`, a variable
holding a function of one argument (the workspace record) returning a
string. Its default SHALL return the registry name. `workspace--display-name`
SHALL call it and fall back to the registry name if it returns nil or
signals. The package SHALL NOT read any file to compute a display name.

#### Scenario: Default display name is the registry name
- **WHEN** `workspace-display-name-function` is at its default
- **THEN** the tab-bar label for workspace `myproj` is `myproj`

#### Scenario: Installed function drives the tab label
- **WHEN** `workspace-display-name-function` is set to a function
  returning `"My Cool Project"` for `myproj`
- **THEN** the tab-bar label reads `My Cool Project` and the registry name
  is unchanged

#### Scenario: Failing function falls back
- **WHEN** the installed function signals an error
- **THEN** the tab-bar label is the registry name and no error reaches the
  user

## MODIFIED Requirements

### Requirement: Required home directory and identity coupling

Every workspace SHALL have a `:home` slot holding an absolute filesystem
path. `:home` is required — the package SHALL NOT support floating
workspaces. Workspace creation paths and persistence loading paths SHALL
NOT produce a workspace with `:home` unset.

The workspace's *registry name* SHALL default to `(file-name-nondirectory
(directory-file-name :home))` — the basename of `:home`. The registry
name is the key used by `workspace--registry`, the `tab-bar` tab name,
and `completing-read` prompts. The registry name is a local key; the
optional `:node-id` (Requirement: Workspace node identity) is the
workspace's durable identity, and an integration MAY use it to recognise
the same workspace across machines.

The workspace's *display name* SHALL default to the registry name and MAY
be overridden by the installed `workspace-display-name-function`
(Requirement: Display-name inversion point). The display name SHALL drive
the **tab-bar label**; the registry name SHALL be used wherever the
workspace is used as a key (registry lookup, persistence file,
programmatic API) and wherever the workspace is keyed in a
`completing-read` prompt. The package SHALL NOT read `<home>/home.org` or
any other file to obtain a display name.

Directory rename — and therefore registry-name rename — SHALL be an
out-of-band user action: move the directory, restart Emacs (the workspace
loads in a broken state because its persisted `:home` no longer exists),
then `workspace-re-anchor` the broken entry to the new location, which
renames the registry entry to the new basename and preserves `:node-id`.

#### Scenario: New workspace has :home set
- **WHEN** the user invokes `workspace-new` with name `"myproj"`
- **THEN** the resulting workspace's `:home` slot is set to an absolute
  path whose basename is `myproj`
- **AND** the workspace is recognized by `workspace--current-name` as
  `myproj`

#### Scenario: Display-name function overrides the label but not the registry name
- **WHEN** workspace `myproj` exists and the installed display-name
  function returns `My Cool Project` for it
- **THEN** the workspace's display name is `My Cool Project`
- **AND** the workspace's registry name is still `myproj`
- **AND** `completing-read` prompts that key by registry name list `myproj`
- **AND** the tab-bar label displays `My Cool Project`

#### Scenario: Re-anchor after an out-of-band move keeps the node ID
- **WHEN** workspace `myproj` with `:node-id "ABC"` is moved on disk to
  `~/emacs-workspaces/renamed/` and Emacs restarts
- **THEN** the workspace loads in a broken state naming the missing path
- **AND** `workspace-re-anchor` to the new directory renames the registry
  entry to `renamed` and its `:node-id` is still `"ABC"`

### Requirement: Per-workspace home layout

Each workspace SHALL have a *home* layout. When a workspace is first
created the home layout SHALL be constructed by a user-configurable
builder function (defcustom `workspace-home-builder`) whose **default
SHALL switch to `*scratch*` in a single window**. The home builder runs
in the context of the freshly-created workspace, so any buffer it opens
becomes a member of that workspace (Requirement: Workspace-scoped buffer
membership). The package SHALL pass the workspace record (including
`:home` and `:node-id`) to the builder so an integration can open content
of its choosing; the package itself SHALL NOT open any file from `:home`.

The `home` layout name SHALL be reserved: it cannot be deleted by
`workspace-delete-layout`, and saving the current configuration as `home`
overwrites the existing home (it does not invoke the builder).

#### Scenario: Default home builder opens scratch
- **WHEN** `workspace-home-builder` is left at its default value
- **AND** the user invokes `workspace-new` with name `"writing"`
- **THEN** the new workspace's `home` layout shows `*scratch*` in a single
  window
- **AND** no file under `~/emacs-workspaces/writing/` is created or visited

#### Scenario: Custom home builder receives the workspace record
- **WHEN** `workspace-home-builder` is set to a custom function `F`
- **AND** the user invokes `workspace-new` with name `"writing"`
- **THEN** `F` is invoked in the context of the new workspace with the
  record, and can read `:home` and `:node-id`

#### Scenario: Home layout cannot be deleted
- **WHEN** the user invokes `workspace-delete-layout` on `home`
- **THEN** the deletion is rejected with a user-visible message
- **AND** the `home` layout is unchanged

#### Scenario: Re-saving home overwrites without invoking the builder
- **WHEN** the user is on the `home` layout of workspace `writing` and
  edits its window configuration
- **AND** the user invokes `workspace-save-layout` with name `home`
- **THEN** the workspace's `home` layout is updated to the current window
  configuration
- **AND** the builder function is not invoked

### Requirement: workspace-new default scaffolding

The package SHALL scaffold a fresh workspace directory at
`(expand-file-name NAME workspaces-default-parent-directory)` when
`workspace-new NAME` is invoked without a prefix argument. The default
value of `workspaces-default-parent-directory` SHALL be
`~/emacs-workspaces/`.

The scaffold pipeline SHALL, in order:

1. Compute the home path; signal `user-error` if a directory already
   exists at that path.
2. `make-directory HOME t`.
3. `git init` in `HOME` via subprocess.
4. Insert the workspace into the registry with `:home` set to `HOME` and
   `:node-id` set to the optional node ID argument (else nil), create and
   select its tab, and run `workspace-home-builder`.
5. Run integration creation-time dispatch with `:context` `fresh`. This
   step is additive: its outcome SHALL NOT affect whether the workspace
   was created.

The package SHALL NOT write any file into `HOME`: no `home.org`, no
`sessions/`, no initial commit (there is nothing to commit). The
repository is left empty for the user and integrations.

The pipeline SHALL stop and signal a user-visible error if step 2 or 3
fails. On failure the package SHALL NOT register the workspace and SHALL
leave any partially-scaffolded directory in place. A failure in step 5
SHALL NOT signal a pipeline error and SHALL NOT unregister the workspace.

#### Scenario: Default-path workspace is an empty repository
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg and `~/emacs-workspaces/myproj/` does not exist
- **THEN** `~/emacs-workspaces/myproj/` exists and contains only `.git/`
- **AND** the repository has no commits
- **AND** the tab `myproj` is selected
- **AND** the workspace's `:home` is `~/emacs-workspaces/myproj/`

#### Scenario: Creation dispatches integrations with fresh context
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** after the workspace is registered, every registered
  integration's `:on-create` handler runs once with `:context` `fresh`

#### Scenario: Default-path collision is rejected
- **WHEN** `~/emacs-workspaces/myproj/` already exists
- **AND** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** the command signals a `user-error`, the directory is unchanged,
  no tab is created, and the message directs the user to the prefix-arg
  anchor flow

#### Scenario: Mid-pipeline failure leaves no registry entry
- **WHEN** `git init` fails during scaffolding of `"myproj"`
- **THEN** the command signals a user-visible error, the partially-created
  directory is NOT removed, the registry contains no entry for `myproj`,
  no tab is created, and no `:on-create` handler is run

### Requirement: Anchoring an existing directory via prefix arg

When `workspace-new` is invoked with a prefix argument, the package SHALL
prompt the user for an existing directory via `read-directory-name` and
SHALL anchor a workspace to that directory:

1. **Already a git repository** → register only; no git operations occur.
   The registry name is the directory's basename. Dispatch context is
   `anchored`.
2. **Not a git repository** → run `git init` in the directory, then
   register. Dispatch context is `fresh`.

The package SHALL NOT create, modify, rename, or delete any file in the
target directory other than `.git/` in case 2. There is no `home.org`
discriminator: whether the directory was previously a workspace is
irrelevant to anchoring, and an optional node ID argument lets the caller
re-associate a known identity.

After the workspace is registered, the package SHALL run integration
creation-time dispatch with the context above. The package SHALL signal
`user-error` if a workspace is already registered for that directory.

#### Scenario: Anchor an existing project repo
- **WHEN** `~/code/myproj/` is a git repo and the user invokes
  `workspace-new` with a prefix arg and chooses it
- **THEN** no files in `~/code/myproj/` are created or modified, no git
  command is invoked, a workspace is registered with `:home ~/code/myproj/`
  and registry name `myproj`, the tab is selected, and dispatch runs with
  `:context` `anchored`

#### Scenario: Anchor a non-repo directory initialises a repository
- **WHEN** `~/work/notes/` exists without `.git/` and the user anchors it
- **THEN** `git init` runs in `~/work/notes/`, nothing else is written,
  and dispatch runs with `:context` `fresh`

#### Scenario: Anchoring rejects a directory already registered
- **WHEN** a workspace is already registered with `:home ~/code/myproj/`
  and the user anchors `~/code/myproj/` again
- **THEN** the command signals `user-error`, the registry is unchanged, no
  tab is created, and no `:on-create` handler runs

### Requirement: Birth-time integration offer

`workspace-new` SHALL offer the user, after a created workspace's
`:on-create` dispatch completes, the opportunity to invoke registered
integration `:menu` command(s) for both the `fresh` and `anchored` contexts. Because
the package writes nothing into the home, an adopted repository is not
modified by the offer itself; only commands the user chooses to run
write anything. For the offered command(s), `workspace-new` SHALL invoke
registered integration `:menu` command(s) against the just-created
workspace, in registration order, building the same anchor payload a menu
invocation would. The offer SHALL let the user run zero or more such
commands and SHALL let the user invoke the same command more than once.
Declining the offer SHALL leave a valid workspace. `workspace-new` SHALL
name no specific integration.

#### Scenario: Birth offers registered menu commands
- **WHEN** the user creates a fresh workspace and a git integration is
  registered with an add-worktree `:menu` command
- **THEN** after creation the user is offered the chance to run the
  add-worktree command against the new workspace
- **AND** accepting it adds a worktree under the workspace home

#### Scenario: Several worktrees added at birth
- **WHEN** the user creates a fresh workspace and, at the birth offer,
  runs the add-worktree command twice for two different repos
- **THEN** the new workspace contains two worktrees when birth completes

#### Scenario: Declining the offer leaves a valid empty workspace
- **WHEN** the user creates a fresh workspace and declines the birth offer
- **THEN** the workspace is registered, has a live tab, and is usable
- **AND** no integration menu command was run

#### Scenario: Anchored repositories are offered the birth step
- **WHEN** the user anchors an existing repository (`anchored` context)
- **THEN** the birth-time offer is presented and declining it leaves the
  repository byte-identical

### Requirement: workspace-delete is unregister-only by default

The package SHALL provide `workspace-delete NAME` as the user-facing
command to remove a workspace from the registry. By default
`workspace-delete` SHALL:

1. Remove the entry from `workspace--registry`.
2. Close the workspace's tab if one is live.
3. Flush the persistence file.
4. Leave `:home` and all its contents on disk untouched.

`workspace-delete` SHALL NOT delete, move, or modify the home directory
and SHALL NOT touch anything outside it (an integration's external
identity, such as a note, is not the package's to remove). The user
retains the option to re-anchor the directory later via `workspace-new`
with a prefix arg, optionally supplying the former `:node-id`.

The `workspace-delete` binding SHALL be `C-x w D`.

#### Scenario: Delete removes from registry without filesystem changes
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
  containing committed history and the user invokes `workspace-delete`
- **THEN** `myproj` is no longer in the registry, its tab is closed, and
  `~/emacs-workspaces/myproj/` and all its contents still exist on disk

#### Scenario: Deleted workspace can be re-anchored with its identity
- **WHEN** the user has deleted `myproj` whose `:node-id` was `"ABC"` and
  anchors `~/emacs-workspaces/myproj/` again supplying `"ABC"`
- **THEN** the workspace is re-registered with `:node-id "ABC"` and no
  scaffolding side effects

### Requirement: workspace-purge as the destructive deletion command

The package SHALL provide `workspace-purge NAME` as the destructive
counterpart to `workspace-delete`. `workspace-purge` SHALL:

1. Confirm with the user via `yes-or-no-p`, displaying the absolute path
   to be deleted.
2. On confirmation, dispatch each registered integration's `:on-purge`
   handler with the anchor payload (Requirement: Purge-time teardown
   dispatch). This step is additive and error-guarded.
3. Perform `workspace-delete`'s unregister steps.
4. Recursively delete `:home` from the filesystem.

`:on-purge` dispatch SHALL occur before filesystem deletion so
integrations can clean up resources anchored under `:home`. Purge SHALL
NOT touch anything outside `:home`; whether an integration retires its
external identity (e.g. marks a project note done) is the integration's
decision, made in its `:on-purge` handler.

`workspace-purge` SHALL refuse to operate when `:home` is not a
descendant of `workspaces-default-parent-directory` unless the user passes
a prefix argument.

#### Scenario: Purge deletes the home directory after confirmation
- **WHEN** the user purges `myproj` and confirms
- **THEN** registered `:on-purge` handlers run while the directory still
  exists, then `~/emacs-workspaces/myproj/` is removed, `myproj` leaves
  the registry, and its tab closes

#### Scenario: Purge can be cancelled
- **WHEN** the user answers `no` at the confirmation prompt
- **THEN** no `:on-purge` handler runs, no deletion occurs, and `myproj`
  remains registered with its tab

#### Scenario: Purge refuses anchored external project without confirmation
- **WHEN** `myproj` has `:home ~/code/myproj/` and the user purges without
  a prefix arg
- **THEN** the command signals a `user-error`, no prompt is shown, no
  handler runs, and no filesystem change occurs

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine
path keyed by `jf/machine-role`. The persistence file SHALL use **schema
version 3**:

- Each workspace plist SHALL carry a required `:home` slot holding the
  absolute filesystem path of its home directory. Loading a workspace
  whose serialized form lacks `:home` is a data error (notice + skip).
- Each workspace plist MAY carry a `:node-id` slot holding a string; when
  absent it loads as nil (Requirement: Workspace node identity).
- Each layout SHALL carry two window-state slots, `:saved-state` (written
  only by explicit saves; authoritative across restarts) and
  `:working-state` (written only by autosaves; may be nil).
- Each layout SHALL carry an `:etc` alist slot for forward-compatible
  extension data.
- Each leaf in a layout's window-state SHALL carry a `workspace-buffer`
  entry in its `parameters` (see *Buffer reincarnation across restart*).

The package SHALL NOT support v1 or v2 persistence files; the reader
SHALL emit a non-fatal notice for other versions and proceed as if no
file exists.

On Emacs startup the package SHALL hydrate the in-memory registry from
the persistence file but SHALL NOT create any tabs. Restored workspaces
are saved-but-not-materialized, reachable via `workspace-restore`.
Workspaces whose `:home` no longer exists SHALL load in a *broken* state.
Layout is applied only by explicit restore / switch-layout commands,
preserving `:working-state`-over-`:saved-state` precedence.

Persistence SHALL be triggered by, and flush synchronously on: explicit
`workspace-save`; `workspace-save-layout` and `workspace-new`'s home
stamp; workspace context switch; intra-workspace layout switch; the
`workspaces-mode` idle timer; `kill-emacs-hook`; and `workspace-set-node-id`.
The package SHALL NOT debounce any flush.

Persistence SHALL be **readable by construction** (no live Emacs object
in the serialized form; window parameters embedding such objects pass
through serialize/deserialize translators; the reincarnation record is
scrubbed; a pre-write `read` round-trip aborts an unreadable write) and
**corruption-safe** (atomic write via temp file + rename; a
present-but-unreadable file is preserved as `workspaces.eld.corrupt-<ts>`
with a warning and all writes are suppressed for the session; an absent
file starts fresh with writes permitted).

#### Scenario: V2 persistence file is rejected with a notice
- **WHEN** a persistence file exists with `:version 2` and Emacs starts
- **THEN** a notice names the file and mismatch, no workspaces are
  restored, and `workspace-new` works normally

#### Scenario: Workspace lacking :home in persistence is skipped
- **WHEN** a `:version 3` file contains one workspace plist without `:home`
- **THEN** a notice names the entry, it is skipped, and other entries are
  restored

#### Scenario: Node ID is persisted and restored
- **WHEN** `workspace-set-node-id` sets `myproj`'s `:node-id` to `"ABC"`
- **THEN** the persistence file on disk reflects it immediately
- **AND** after restart the hydrated `myproj` record has `:node-id "ABC"`

#### Scenario: Startup hydrates the registry without creating tabs
- **WHEN** a `:version 3` file contains `alpha` and `writing` with existing
  homes and Emacs starts
- **THEN** both are in the registry, no tab is created, and each is
  offered by `workspace-restore`

#### Scenario: A deliberate command flushes synchronously
- **WHEN** the user invokes `workspace-new`, `workspace-save-layout`, or
  `workspace-switch-layout`
- **THEN** the persistence file reflects the resulting state immediately
  after the command returns

#### Scenario: Restart restores working-state, not saved-state, when both present
- **WHEN** `code` has `:saved-state` S and `:working-state` W and the user
  restores it after a restart
- **THEN** the applied window configuration is W and its buffers are
  reincarnated via the bookmark chain

#### Scenario: A live Emacs object is never written to disk
- **WHEN** a layout carries a `window-preserved-size` parameter referencing
  a buffer and is serialized
- **THEN** the serialized form contains no `#<…>` token and encodes the
  value by buffer name

#### Scenario: An unreadable persistence file is preserved, not overwritten
- **WHEN** the persistence file cannot be `read` at startup
- **THEN** it is renamed to `workspaces.eld.corrupt-<timestamp>`, a warning
  names that path, the registry is empty, and the original path is not
  overwritten

#### Scenario: Autosave is suppressed after a failed load
- **WHEN** a startup load was present-but-unreadable and a flush trigger
  fires
- **THEN** no write occurs and a one-time warning is emitted

#### Scenario: A write that would be unreadable is aborted
- **WHEN** a serialized form fails the pre-write `read` round-trip
- **THEN** the write is aborted with a warning and the previous file is
  intact

## REMOVED Requirements

### Requirement: home.org is user-authored after creation

**Reason**: The package no longer writes or reads `home.org`; the
workspace's identity note lives in the vault and is owned by the
integration layer (`workspace-graph-integration`).

**Migration**: The integration layer's migration command carries an
existing `home.org` title and body into the project note and removes the
file from `:home`.

### Requirement: Filesystem-authoritative session inventory

**Reason**: Sessions no longer live under `:home`. A workspace's sessions
are the session notes with a `part-of` edge to its project note, queried
through the graph index by the integration layer. This requirement was
never implemented.

**Migration**: `find-in-workspace` and the project note's connected-notes
block replace any per-directory listing.

### Requirement: Workspace-aware gptel session creation

**Reason**: Violates package independence (gptel sessions consulted
workspaces). Sessions always target gptel's single sessions root; the
association is an edge written by the integration layer from a gptel
hook.

**Migration**: Remove `workspace-sessions-dir` and the gptel consult; the
`-global` command is deleted because every creation is now "global".
Existing workspace-local sessions are moved by the migration command.
