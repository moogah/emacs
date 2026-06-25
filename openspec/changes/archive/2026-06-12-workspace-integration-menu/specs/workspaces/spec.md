## ADDED Requirements

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

## MODIFIED Requirements

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
