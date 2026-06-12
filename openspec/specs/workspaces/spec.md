# Workspaces

## Purpose

Provides a purpose-built window/workspace management system layered
directly on `tab-bar-mode`. A *workspace* is a uniquely-named
user-facing context surfaced as a single tab, anchored to a
filesystem `:home` directory; each workspace owns an arbitrary
number of named *layouts* (window configurations), a *home* layout
produced by a configurable builder, a *recent-layout pointer*, a
per-workspace *buffer membership* set scoped via `bufferlo`, and a
per-machine persistence file keyed by `jf/machine-role`.

This capability supersedes the legacy combination of `activities.el`
+ `activities-extensions` (workflow shell + tabs + persistence but
only one default+last window-state per activity) and `perspective.el`
(buffer scoping but no tab-bar, no named layouts, no persistence in
this configuration). Both legacy modules were removed in the
`add-workspaces-package` cutover.

Implementation: `config/workspaces/` (literate, multi-file:
`data-model`, `tabs`, `layouts`, `buffer-membership`, `persistence`,
`workspaces`, `workspaces-mode`). User-facing prefix: `C-x w`.

The on-disk persistence schema is `:version 3`. Each workspace carries a
required `:home` slot; its registry name is the basename of `:home`, and
its display name may be overridden by a `#+TITLE:` keyword in
`<home>/home.org`. Each layout holds a
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
be overridden by a `#+TITLE:` keyword in `<home>/home.org`, resolved by
`workspace--display-name` (a live read of `home.org`, no caching). In
this change the display name SHALL drive the **tab-bar label**; other
prose surfaces (mode-line label, status messages) MAY adopt
`workspace--display-name` in a later change but continue to show the
registry name today. The registry name SHALL be used wherever the
workspace is used as a key (registry lookup, persistence file,
programmatic API) and wherever the workspace is keyed in a
`completing-read` prompt.

Editing `#+TITLE:` in `home.org` SHALL never change the registry name and
SHALL never invalidate the persistence file. Directory rename — and
therefore registry-name rename — SHALL be an out-of-band user action:
move the directory, restart Emacs (the workspace loads in a broken state
because its persisted `:home` no longer exists — the package performs no
automatic directory rediscovery, which is explicitly deferred), then
`workspace-re-anchor` the broken entry to the new location, which renames
the registry entry to the new basename. The package provides neither a
`workspace-rename` command nor directory discovery beyond the prefix-arg
anchor flow in this change.

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

#### Scenario: Registry name follows the new basename after an out-of-band move + re-anchor
- **WHEN** the user moves a workspace directory from
  `~/emacs-workspaces/myproj/` to `~/emacs-workspaces/renamed/`
  out-of-band and restarts Emacs
- **THEN** the workspace loads in a broken state (its persisted `:home`
  no longer exists) and a `*Messages*` notice names the missing path
  (there is no automatic rediscovery of the moved directory)
- **AND** invoking `workspace-re-anchor` for it and choosing
  `~/emacs-workspaces/renamed/` renames the registry entry to `renamed`
  (the registry name follows the new basename)
- **AND** any `#+TITLE:` in `home.org` continues to drive the display name

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

### Requirement: Workspace transient menu

The package SHALL provide a transient menu that is the primary entry point for
both creating and operating on workspaces. The menu SHALL detect the current
context from the selected tab and present a context-appropriate set of
actions:

- When the current tab is **not a workspace**: entry actions only — create a
  new workspace, switch workspaces, restore persisted workspaces.
- When the current tab is a **healthy workspace**: the entry actions, plus
  operational actions (layout switch/save/recent; explicit save; working-state
  revert; delete; purge; re-anchor), plus the registry-driven *Integrations*
  group (Requirement: Integrations populate the workspaces transient, in the
  `workspace-integrations` capability).
- When the current tab is a **broken workspace**: the entry actions, plus
  recovery actions only (re-anchor, purge, delete). The operational and
  Integrations groups SHALL NOT appear, because a broken workspace has no
  resolvable home and cannot be operated on or integrated into.

Invoking the menu's create action SHALL use the same creation path as
`workspace-new` (including its prefix-arg anchor flow), so the menu and the
command share one implementation.

#### Scenario: Off-workspace menu shows only entry actions
- **WHEN** the current tab is not a registered workspace
- **AND** the user opens the workspaces transient
- **THEN** the menu offers create, switch, and restore
- **AND** it offers no operational, recovery, or Integrations actions

#### Scenario: Healthy-workspace menu shows operations and integrations
- **WHEN** the current tab is a healthy workspace
- **AND** the user opens the workspaces transient
- **THEN** the menu offers the entry actions, the operational actions, and the
  Integrations group

#### Scenario: Broken-workspace menu shows recovery only
- **WHEN** the current tab is a broken workspace
- **AND** the user opens the workspaces transient
- **THEN** the menu offers the entry actions and recovery actions (re-anchor,
  purge, delete)
- **AND** it offers no operational actions and no Integrations group

#### Scenario: Creating from the menu matches workspace-new
- **WHEN** the user invokes the menu's create action and supplies name
  `"notes"`
- **THEN** the workspace is created exactly as `workspace-new` would create it
  (Requirement: workspace-new default scaffolding)
- **AND** the tab `notes` is selected

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
5. `make-directory <HOME>/sessions/` (created empty; the package SHALL NOT
   write any session file here).
6. `git add .` followed by `git commit -m "Initial workspace"` via
   subprocess.
7. Insert the workspace into the registry with `:home` set to `HOME`,
   create and select its tab, and run `workspace-home-builder`.
8. Run integration creation-time dispatch with `:context` `fresh` (see the
   `workspace-integrations` capability). This step is additive: its outcome
   SHALL NOT affect whether the workspace was created, and any files an
   integration creates here are NOT part of the `Initial workspace` commit.

The pipeline SHALL stop and signal a user-visible error if any of steps
2–6 fail. On failure the package SHALL NOT register the workspace and
SHALL leave any partially-scaffolded directory in place for the user to
inspect (cleanup is not automatic — silent `rm -rf` on errors is too risky).
A failure in step 8 (integration dispatch) SHALL NOT signal a pipeline error
and SHALL NOT unregister the already-created workspace.

#### Scenario: Default-path workspace is fully scaffolded and committed
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **AND** `~/emacs-workspaces/myproj/` does not exist
- **THEN** `~/emacs-workspaces/myproj/` exists and contains a `.git/`
  directory
- **AND** `~/emacs-workspaces/myproj/home.org` exists and contains
  `#+TITLE: myproj`
- **AND** `~/emacs-workspaces/myproj/sessions/` exists
- **AND** no file matching `*-initial.org` is created by the package itself
- **AND** the git log shows exactly one commit whose message is `Initial
  workspace`
- **AND** the tab `myproj` is selected
- **AND** the workspace's `:home` is `~/emacs-workspaces/myproj/`

#### Scenario: Creation dispatches integrations with fresh context
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** after the workspace is registered, every registered integration's
  `:on-create` handler runs once with `:context` `fresh`

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
- **AND** no integration `:on-create` handler is run

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
   `<HOME>/home.org` and create `<HOME>/sessions/` if absent. The package
   SHALL NOT create any session file and SHALL NOT run any git commands —
   the user owns this repository and decides when to stage/commit.
3. **Not a git repository** → run the full scaffold pipeline from
   `Requirement: workspace-new default scaffolding` starting at step 3
   (the directory already exists; skip `make-directory`).

After the workspace is registered, the package SHALL run integration
creation-time dispatch (see the `workspace-integrations` capability) with
`:context` `anchored-existing` for case 1, `anchored-scaffolded` for case 2,
and `fresh` for case 3. Integration dispatch is additive and its outcome SHALL
NOT affect whether the workspace was anchored.

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
- **AND** integration dispatch runs with `:context` `anchored-existing`

#### Scenario: Anchor an existing repo without home.org — no auto-commit
- **WHEN** `~/code/myproj/` is a git repo with no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/`
- **THEN** `~/code/myproj/home.org` is created with the skeleton template
- **AND** `~/code/myproj/sessions/` is created (empty)
- **AND** no `*-initial.org` file is created by the package
- **AND** NO git command is run (the new files appear as untracked /
  unstaged to the user's existing repo)
- **AND** a workspace is registered with `:home ~/code/myproj/`
- **AND** integration dispatch runs with `:context` `anchored-scaffolded`

#### Scenario: Anchor a non-repo directory triggers full scaffold including git init
- **WHEN** `~/work/notes/` exists but has no `.git/` and no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/work/notes/`
- **THEN** `git init` runs in `~/work/notes/`
- **AND** `home.org` and `sessions/` are created
- **AND** an initial commit is made
- **AND** integration dispatch runs with `:context` `fresh`

#### Scenario: Anchoring rejects a directory already registered
- **WHEN** a workspace is already registered with `:home ~/code/myproj/`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/` again
- **THEN** the command signals `user-error`
- **AND** the existing registry entry is unchanged
- **AND** no new tab is created
- **AND** no integration `:on-create` handler is run

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

#### Scenario: Restart restores working-state, not saved-state, when both present
- **WHEN** workspace `code` has `:home ~/emacs-workspaces/code/` (which
  exists), `:saved-state` S (last explicit save) and `:working-state` W
  (latest autosave)
- **AND** Emacs is restarted and the user selects the `code` tab
- **THEN** the window configuration restored on `code`'s frame is W, not S
- **AND** the buffers in W are reincarnated via the bookmark chain
  (Requirement: Buffer reincarnation across restart)

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
