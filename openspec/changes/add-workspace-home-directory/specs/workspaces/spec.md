## ADDED Requirements

### Requirement: Required home directory and identity coupling

Every workspace SHALL have a `:home` slot holding an absolute filesystem
path. `:home` is required — the package SHALL NOT support floating
workspaces. Workspace creation paths and persistence loading paths SHALL
NOT produce a workspace with `:home` unset.

The workspace's *registry name* SHALL equal `(file-name-nondirectory
(directory-file-name :home))` — that is, the basename of `:home`. The
registry name is the key used by `workspace--registry`, the
`tab-bar` tab name, and `completing-read` prompts.

The workspace's *display name* SHALL default to the registry name and MAY
be overridden by a `#+TITLE:` keyword in `<home>/home.org`. The display
name SHALL be used wherever the workspace is presented to the user as
prose (mode-line label, tab display label, status messages); the registry
name SHALL be used wherever the workspace is used as a key (registry
lookup, persistence file, programmatic API).

Editing `#+TITLE:` in `home.org` SHALL never change the registry name and
SHALL never invalidate the persistence file. Directory rename — and
therefore registry-name rename — SHALL be an out-of-band user action
(e.g., `mv` the directory and restart Emacs); the package does not provide
a `workspace-rename` command in this change.

#### Scenario: New workspace has :home set
- **WHEN** the user invokes `workspace-new` with name `"myproj"`
- **THEN** the resulting workspace's `:home` slot is set to an absolute
  path whose basename is `myproj`
- **AND** the workspace is recognized by `workspace--current-name` as
  `myproj`

#### Scenario: #+TITLE: overrides display name but not registry name
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
- **AND** `home.org` contains `#+TITLE: My Cool Project`
- **THEN** the workspace's display name is `My Cool Project`
- **AND** the workspace's registry name is still `myproj`
- **AND** `completing-read` prompts that key by registry name list `myproj`
- **AND** the tab-bar label displays `My Cool Project`

#### Scenario: Registry name follows directory basename, not #+TITLE:
- **WHEN** the user moves a workspace directory from
  `~/emacs-workspaces/myproj/` to `~/emacs-workspaces/renamed/`
  out-of-band and restarts Emacs
- **THEN** the workspace re-appears in the registry under the name `renamed`
- **AND** any `#+TITLE:` in `home.org` continues to drive the display name

---

### Requirement: workspace-new default scaffolding

The package SHALL scaffold a fresh workspace directory at `(expand-file-name NAME workspaces-default-parent-directory)` when `workspace-new NAME` is invoked without a prefix argument. The default value of `workspaces-default-parent-directory` SHALL be `~/emacs-workspaces/`.

The scaffold pipeline SHALL, in order:

1. Compute the home path; signal `user-error` if a directory already
   exists at that path (the default path is reserved; collisions force the
   user to either pick a different name or use the prefix-arg anchor flow).
2. `make-directory HOME t`.
3. `git init` in `HOME` via subprocess.
4. Write `<HOME>/home.org` containing the skeleton template (at minimum:
   `#+TITLE: NAME` keyword, a `* Description` heading, and a `* Notes`
   heading; the package MAY include additional decorative headings but
   SHALL NOT include a `* Sessions` heading that the package will later
   want to overwrite).
5. `make-directory <HOME>/sessions/`.
6. Create `<HOME>/sessions/<ISO-date>-initial.org` containing an empty
   gptel session skeleton suitable for opening in `gptel`.
7. `git add .` followed by `git commit -m "Initial workspace"` via
   subprocess.
8. Insert the workspace into the registry with `:home` set to `HOME`,
   create and select its tab, and run `workspace-home-builder`.

The pipeline SHALL stop and signal a user-visible error if any of steps
2–7 fail. On failure the package SHALL NOT register the workspace and
SHALL leave any partially-scaffolded directory in place for the user to
inspect (cleanup is not automatic — silent `rm -rf` on errors is too risky).

#### Scenario: Default-path workspace is fully scaffolded and committed
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **AND** `~/emacs-workspaces/myproj/` does not exist
- **THEN** `~/emacs-workspaces/myproj/` exists and contains a `.git/`
  directory
- **AND** `~/emacs-workspaces/myproj/home.org` exists and contains
  `#+TITLE: myproj`
- **AND** `~/emacs-workspaces/myproj/sessions/` exists and contains one
  file matching `*-initial.org`
- **AND** the git log shows exactly one commit whose message is `Initial
  workspace`
- **AND** the tab `myproj` is selected
- **AND** the workspace's `:home` is `~/emacs-workspaces/myproj/`

#### Scenario: Default-path collision is rejected
- **WHEN** `~/emacs-workspaces/myproj/` already exists (from any source —
  earlier session, manual mkdir, prior failed scaffold)
- **AND** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** the command signals a `user-error`
- **AND** the existing directory contents are unchanged
- **AND** no tab is created
- **AND** the user is directed (in the error message) to retry with a
  prefix arg to anchor the existing directory

#### Scenario: Mid-pipeline failure leaves no registry entry
- **WHEN** `git init` fails (e.g., `git` not on PATH) during scaffolding
  of `"myproj"`
- **THEN** the command signals a user-visible error
- **AND** the partially-created directory is NOT removed
- **AND** the registry contains no entry for `myproj`
- **AND** no tab is created

---

### Requirement: Anchoring an existing directory via prefix arg

When `workspace-new` is invoked with a prefix argument, the package SHALL
prompt the user for an existing directory via `read-directory-name` and
SHALL anchor a workspace to that directory. The behavior SHALL depend on
the chosen directory's state:

1. **Already a git repository AND contains `home.org`** → register only;
   no scaffolding occurs; no git operations occur. The workspace's
   registry name is the directory's basename.
2. **Already a git repository AND lacks `home.org`** → write
   `<HOME>/home.org`, create `<HOME>/sessions/` if absent, create one
   `<HOME>/sessions/<ISO-date>-initial.org`. The package SHALL NOT run
   any git commands — the user owns this repository and decides when to
   stage/commit.
3. **Not a git repository** → run the full scaffold pipeline from
   `Requirement: workspace-new default scaffolding` starting at step 3
   (the directory already exists; skip `make-directory`).

The package SHALL NOT modify, rename, or delete any file in the target
directory that it did not itself create as part of scaffolding.

The package SHALL signal `user-error` if a workspace is already
registered for that directory (no double-registration).

#### Scenario: Anchor an existing project repo with home.org
- **WHEN** `~/code/myproj/` is a git repo containing `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/`
- **THEN** no files in `~/code/myproj/` are created or modified
- **AND** no git command is invoked
- **AND** a workspace is registered with `:home ~/code/myproj/` and
  registry name `myproj`
- **AND** the tab `myproj` is selected

#### Scenario: Anchor an existing repo without home.org — no auto-commit
- **WHEN** `~/code/myproj/` is a git repo with no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/`
- **THEN** `~/code/myproj/home.org` is created with the skeleton template
- **AND** `~/code/myproj/sessions/<ISO-date>-initial.org` is created
- **AND** NO git command is run (the new files appear as untracked /
  unstaged to the user's existing repo)
- **AND** a workspace is registered with `:home ~/code/myproj/`

#### Scenario: Anchor a non-repo directory triggers full scaffold including git init
- **WHEN** `~/work/notes/` exists but has no `.git/` and no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/work/notes/`
- **THEN** `git init` runs in `~/work/notes/`
- **AND** `home.org`, `sessions/`, and `sessions/<ISO-date>-initial.org`
  are created
- **AND** an initial commit is made

#### Scenario: Anchoring rejects a directory already registered
- **WHEN** a workspace is already registered with `:home ~/code/myproj/`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/` again
- **THEN** the command signals `user-error`
- **AND** the existing registry entry is unchanged
- **AND** no new tab is created

---

### Requirement: home.org is user-authored after creation

The package SHALL write `<HOME>/home.org` exactly once, during scaffolding.
After creation the package SHALL NOT modify `home.org` from any code path
— not on session create/delete, not on workspace activation, not on
shutdown, not via any "refresh" command in this change.

The package MAY read `home.org` at any time. Reads SHALL be live
(re-parse the file on every call); no caching layer is provided in this
change.

The package SHALL read `#+TITLE:` from `home.org` to obtain the display
name (Requirement: Required home directory and identity coupling). Any
further metadata schema for `home.org` is deferred to a later change; the
package SHALL tolerate arbitrary user content in `home.org`.

#### Scenario: User edits to home.org are never overwritten
- **WHEN** the user edits `<home>/home.org` to add custom headings,
  re-order content, or change `#+TITLE:`
- **AND** the user creates a new gptel session, switches workspaces,
  triggers an idle autosave, or quits Emacs
- **THEN** `<home>/home.org` on disk is byte-identical to the user's
  edited version

#### Scenario: home.org changes are picked up on next read
- **WHEN** the user edits `#+TITLE: New Display Name` in `<home>/home.org`
  and saves
- **AND** the package next queries the workspace's display name
- **THEN** the returned display name is `New Display Name`
- **AND** no cache invalidation step was required

---

### Requirement: Filesystem-authoritative session inventory

The list of gptel sessions belonging to a workspace SHALL be derived
exclusively from the contents of `<HOME>/sessions/`. The package SHALL
NOT consult any heading or list in `home.org` to determine which sessions
exist.

The package SHALL NOT auto-render, auto-sync, or auto-update a `*
Sessions` (or any other) heading in `home.org`. If the user chooses to
maintain a session-list heading in `home.org`, that content is purely
user-authored.

#### Scenario: Sessions directory is the source of truth
- **WHEN** `<home>/sessions/` contains files `a.org`, `b.org`, `c.org`
- **AND** `<home>/home.org` contains a `* Sessions` heading listing only
  `a.org` (the user has been hand-curating)
- **THEN** any package query for "the workspace's sessions" returns
  `a.org`, `b.org`, `c.org`
- **AND** `<home>/home.org` is not rewritten

#### Scenario: Removing a session file removes it from the inventory
- **WHEN** the user deletes `<home>/sessions/b.org` from the filesystem
- **AND** the package next queries the workspace's sessions
- **THEN** the returned list does not include `b.org`
- **AND** no error is signalled

---

### Requirement: Workspace-aware gptel session creation

New gptel sessions SHALL be written to `<HOME>/sessions/` of the active workspace by default, rather than to the gptel module's global session directory, when a workspace is the active context at session-creation time.

The integration SHALL be a *soft* dependency: the gptel sessions module
SHALL guard the workspaces consult with a `(featurep 'workspaces)` check
(or equivalent) so that the gptel sessions module continues to work in
configurations where workspaces is not loaded.

The package SHALL provide an escape hatch — a SEPARATE
`-global`-suffixed command (e.g. `jf/gptel-persistent-session-global`)
alongside the workspace-aware session-creation command (e.g.
`jf/gptel-persistent-session`) — that forces the global default
location even when a workspace is active. The escape hatch is a
distinct command rather than a prefix-arg overload because the
existing workspace-aware command already consumes
`current-prefix-arg` for an orthogonal concern (preset selection);
overloading the prefix slot would break that pre-existing
affordance. Both commands surface in `M-x` completion, making the
escape hatch discoverable.

#### Scenario: New session is filed under the active workspace
- **WHEN** the user is on workspace `myproj` (`:home
  ~/emacs-workspaces/myproj/`)
- **AND** the user invokes the workspace-aware session-creation
  command (e.g. `M-x jf/gptel-persistent-session`)
- **THEN** the resulting session file is created under
  `~/emacs-workspaces/myproj/sessions/`
- **AND** the global gptel session directory is not written to

#### Scenario: -global command forces global save
- **WHEN** the user is on workspace `myproj`
- **AND** the user invokes the `-global`-suffixed session-creation
  command (e.g. `M-x jf/gptel-persistent-session-global`)
- **THEN** the resulting session file is created in the global gptel
  session directory
- **AND** `~/emacs-workspaces/myproj/sessions/` is not written to

#### Scenario: gptel works without workspaces loaded
- **WHEN** the `workspaces` feature is not loaded
- **AND** the user invokes the gptel session-creation command
- **THEN** the session is created in the global gptel session directory
- **AND** no error is signalled about a missing `workspaces` feature

#### Scenario: Off-workspace creation uses global default
- **WHEN** the `workspaces` feature is loaded but the current tab is not
  registered as a workspace
- **AND** the user invokes the gptel session-creation command
- **THEN** the session is created in the global gptel session directory

---

### Requirement: workspace-delete is unregister-only by default

The package SHALL provide `workspace-delete NAME` as the user-facing
command to remove a workspace from the registry. By default
`workspace-delete` SHALL:

1. Remove the entry from `workspace--registry`.
2. Close the workspace's tab if one is live.
3. Flush the persistence file.
4. Leave `:home` and all its contents (including `.git/`, `home.org`,
   `sessions/`) on disk untouched.

`workspace-delete` SHALL NOT delete, move, or modify the home directory.
The user retains the option to re-anchor the directory later via
`workspace-new` with a prefix arg.

The `workspace-delete` binding SHALL be `C-x w D` (uppercase D, to
distinguish from buffer-removal commands).

#### Scenario: Delete removes from registry without filesystem changes
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
  containing committed history
- **AND** the user invokes `workspace-delete` on `myproj`
- **THEN** `myproj` is no longer in the registry
- **AND** the `myproj` tab is closed
- **AND** `~/emacs-workspaces/myproj/` and all its contents still exist on disk
- **AND** the git history is intact

#### Scenario: Deleted workspace can be re-anchored
- **WHEN** the user has invoked `workspace-delete` on `myproj`
- **AND** `~/emacs-workspaces/myproj/` is still present on disk with its
  `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/emacs-workspaces/myproj/`
- **THEN** the workspace is re-registered with no scaffolding side effects
- **AND** the `myproj` tab is recreated

---

### Requirement: workspace-purge as the destructive deletion command

The package SHALL provide `workspace-purge NAME` as the destructive
counterpart to `workspace-delete`. `workspace-purge` SHALL:

1. Confirm with the user via `yes-or-no-p`, displaying the absolute path
   to be deleted.
2. On confirmation, perform `workspace-delete`'s unregister steps.
3. Recursively delete `:home` from the filesystem.

`workspace-purge` SHALL refuse to operate when `:home` is not a
descendant of `workspaces-default-parent-directory` unless the user passes
a prefix argument acknowledging the unusual location. This guards against
accidentally purging an anchored existing project (e.g., `~/code/myproj/`).

#### Scenario: Purge deletes the home directory after confirmation
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
- **AND** the user invokes `workspace-purge` on `myproj`
- **AND** the user confirms `yes` at the prompt
- **THEN** `~/emacs-workspaces/myproj/` and all its contents are removed
  from the filesystem
- **AND** `myproj` is no longer in the registry
- **AND** the `myproj` tab is closed

#### Scenario: Purge can be cancelled
- **WHEN** the user invokes `workspace-purge` on `myproj`
- **AND** the user answers `no` at the confirmation prompt
- **THEN** no filesystem deletion occurs
- **AND** `myproj` remains in the registry with its tab

#### Scenario: Purge refuses anchored external project without confirmation
- **WHEN** workspace `myproj` exists with `:home ~/code/myproj/`
  (anchored, outside the default parent directory)
- **AND** the user invokes `workspace-purge` on `myproj` without a prefix arg
- **THEN** the command signals a `user-error` explaining the safeguard
- **AND** no `yes-or-no-p` prompt is shown
- **AND** no filesystem changes occur
- **AND** the user is directed to use a prefix arg to override

---

### Requirement: Broken home directory tolerated on restore

When the persistence file is loaded at startup, the package SHALL tolerate
workspaces whose `:home` no longer exists on disk. For each such broken
workspace the package SHALL:

1. Emit a non-fatal `*Messages*` notice naming the workspace and the
   missing path.
2. Leave the workspace in the registry in a *broken* state — present in
   `workspace--registry` but flagged such that activation is refused.
3. NOT recreate the directory.
4. NOT remove the workspace from the registry automatically.

The package SHALL provide `workspace-re-anchor NAME PATH` (or an
equivalent re-anchoring flow accessible via interactive `workspace-purge`
or `workspace-restore`) that lets the user point a broken workspace at a
new home path. Until re-anchored or purged, attempting to switch to or
restore a broken workspace SHALL signal `user-error` with a message
explaining the broken state.

#### Scenario: Missing home directory is logged, not auto-recreated
- **WHEN** the persistence file contains workspace `myproj` with
  `:home ~/emacs-workspaces/myproj/`
- **AND** `~/emacs-workspaces/myproj/` has been deleted out-of-band
- **AND** Emacs starts up
- **THEN** a `*Messages*` notice names `myproj` and the missing path
- **AND** `myproj` appears in the registry as broken
- **AND** `~/emacs-workspaces/myproj/` is NOT recreated

#### Scenario: Switching to a broken workspace is refused
- **WHEN** workspace `myproj` is in the broken state
- **AND** the user invokes `workspace-switch` or `workspace-restore` and
  chooses `myproj`
- **THEN** a `user-error` is signalled naming the missing path
- **AND** the user is directed to re-anchor or purge
- **AND** no tab is created

#### Scenario: Purging a broken workspace removes only the registry entry
- **WHEN** workspace `myproj` is broken (`:home` does not exist)
- **AND** the user invokes `workspace-purge` on `myproj`
- **THEN** there is no filesystem deletion to perform (path is already
  absent)
- **AND** the registry entry is removed
- **AND** the persistence file is flushed

---

## MODIFIED Requirements

### Requirement: Per-workspace home layout

Each workspace SHALL have a *home* layout. When a workspace is first
created the home layout SHALL be constructed by a user-configurable
builder function (defcustom `workspace-home-builder`) whose **default
SHALL `find-file` `<HOME>/home.org` in a single window**, replacing the
previous "switch to `*scratch*`" default. The home builder runs in the
context of the freshly-created workspace, so the buffer it opens becomes
a member of that workspace (Requirement: Workspace-scoped buffer
membership).

The `home` layout name SHALL be reserved: it cannot be deleted by
`workspace-delete-layout`, and saving the current configuration as `home`
overwrites the existing home (it does not invoke the builder).

The layout name `home` and the directory concept "home" are intentionally
co-named: by default, the home *layout* opens content from the home
*directory*. Users who replace `workspace-home-builder` SHALL be free to
ignore `<HOME>/home.org` entirely; the package SHALL pass the
workspace's `:home` to the builder (or make it readily available via the
in-progress workspace context) so custom builders can use it.

#### Scenario: New workspace uses the default home builder to open home.org
- **WHEN** `workspace-home-builder` is left at its default value
- **AND** the user invokes `workspace-new` with name `"writing"` (no
  prefix arg)
- **THEN** scaffolding creates `~/emacs-workspaces/writing/home.org`
- **AND** the new workspace's `home` layout shows that `home.org` in a
  single window
- **AND** the `home.org` buffer is a member of the `writing` workspace

#### Scenario: Custom home builder receives workspace context
- **WHEN** `workspace-home-builder` is set to a custom function `F`
- **AND** the user invokes `workspace-new` with name `"writing"`
- **THEN** `F` is invoked in the context of the new workspace and can
  read the workspace's `:home` to do its own scaffolding (e.g., open
  `<home>/README.org` instead)

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

On Emacs startup the package SHALL restore the persisted workspaces by
recreating their tabs. Workspaces whose `:home` no longer exists on disk
SHALL be loaded into the registry in a *broken* state per Requirement:
Broken home directory tolerated on restore — they are still listed but
cannot be activated. Per-workspace layout application is lazy: it happens
on the first tab-switch into each restored workspace. The lazy
application SHALL prefer `:working-state` when present, falling back to
`:saved-state`.

Persistence to disk SHALL be triggered by all of:
- Explicit `workspace-save` (synchronous flush; writes `:saved-state`).
- Workspace context switch (debounced flush; writes `:working-state` of
  the outgoing workspace).
- Intra-workspace layout switch (debounced flush; writes
  `:working-state` of the outgoing layout).
- `workspaces-mode` idle timer (debounced flush; writes `:working-state`
  of the current workspace).
- `kill-emacs-hook` (synchronous flush; captures `:working-state` of the
  current workspace once before writing).

When the persistence file is missing or unreadable the package SHALL
start with no workspaces and SHALL NOT raise an error visible to the
user beyond an `*Messages*` notice.

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

#### Scenario: Restart restores working-state, not saved-state, when both present
- **WHEN** workspace `code` has `:home ~/emacs-workspaces/code/` (which
  exists), `:saved-state` S (last explicit save) and `:working-state` W
  (latest autosave)
- **AND** Emacs is restarted and the user selects the `code` tab
- **THEN** the window configuration restored on `code`'s frame is W, not S
- **AND** the buffers in W are reincarnated via the bookmark chain
  (Requirement: Buffer reincarnation across restart)
