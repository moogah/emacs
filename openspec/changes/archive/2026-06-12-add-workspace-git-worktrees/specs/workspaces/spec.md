## MODIFIED Requirements

### Requirement: workspace-purge as the destructive deletion command

The package SHALL provide `workspace-purge NAME` as the destructive
counterpart to `workspace-delete`. `workspace-purge` SHALL:

1. Confirm with the user via `yes-or-no-p`, displaying the absolute path
   to be deleted.
2. On confirmation, dispatch each registered integration's `:on-purge`
   handler with the anchor payload for the workspace (see the
   `workspace-integrations` capability, Requirement: Purge-time teardown
   dispatch). This step is additive and error-guarded: its outcome SHALL NOT
   affect whether the workspace is purged, and a failing handler SHALL be
   surfaced visibly but SHALL NOT abort the purge.
3. Perform `workspace-delete`'s unregister steps.
4. Recursively delete `:home` from the filesystem.

`:on-purge` dispatch (step 2) SHALL occur before the filesystem deletion
(step 4) so integrations can clean up resources anchored under `:home` (e.g.
git worktrees) while the directory still exists.

`workspace-purge` SHALL refuse to operate when `:home` is not a
descendant of `workspaces-default-parent-directory` unless the user passes
a prefix argument acknowledging the unusual location. This guards against
accidentally purging an anchored existing project (e.g., `~/code/myproj/`).

#### Scenario: Purge deletes the home directory after confirmation
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
- **AND** the user invokes `workspace-purge` on `myproj`
- **AND** the user confirms `yes` at the prompt
- **THEN** registered `:on-purge` handlers run while the directory still exists
- **AND** `~/emacs-workspaces/myproj/` and all its contents are then removed
  from the filesystem
- **AND** `myproj` is no longer in the registry
- **AND** the `myproj` tab is closed

#### Scenario: Purge can be cancelled
- **WHEN** the user invokes `workspace-purge` on `myproj`
- **AND** the user answers `no` at the confirmation prompt
- **THEN** no `:on-purge` handler runs
- **AND** no filesystem deletion occurs
- **AND** `myproj` remains in the registry with its tab

#### Scenario: Purge refuses anchored external project without confirmation
- **WHEN** workspace `myproj` exists with `:home ~/code/myproj/`
  (anchored, outside the default parent directory)
- **AND** the user invokes `workspace-purge` on `myproj` without a prefix arg
- **THEN** the command signals a `user-error` explaining the safeguard
- **AND** no `yes-or-no-p` prompt is shown
- **AND** no `:on-purge` handler runs
- **AND** no filesystem changes occur
- **AND** the user is directed to use a prefix arg to override

## ADDED Requirements

### Requirement: Birth-time integration offer

After a created workspace's `:on-create` dispatch completes, `workspace-new` SHALL offer the user the opportunity to invoke registered integration `:menu`
command(s) — but only for the `fresh` and `anchored-scaffolded` contexts, never
`anchored-existing` (an adopted workspace the user already owns SHALL NOT be
modified). For the offered command(s), `workspace-new` SHALL invoke
registered integration `:menu` command(s) against the just-created workspace,
in registration order, building the same anchor payload a menu invocation
would. The offer SHALL let the user run zero or more such commands and SHALL
let the user invoke the same command more than once (so several git worktrees,
each off a different repo, MAY be added at birth). Declining the offer SHALL
leave a valid workspace. This offer reuses the existing interactive `:menu`
surface; `workspace-new` SHALL name no specific integration, preserving the
publish/attach directionality boundary.

#### Scenario: Birth offers registered menu commands
- **WHEN** the user creates a fresh workspace and a git integration is
  registered with an add-worktree `:menu` command
- **THEN** after creation the user is offered the chance to run the add-worktree
  command against the new workspace
- **AND** accepting it adds a worktree under the workspace home

#### Scenario: Several worktrees added at birth
- **WHEN** the user creates a fresh workspace and, at the birth offer, runs the
  add-worktree command twice for two different repos
- **THEN** the new workspace contains two worktrees when birth completes

#### Scenario: Declining the offer leaves a valid empty workspace
- **WHEN** the user creates a fresh workspace and declines the birth offer
- **THEN** the workspace is registered, has a live tab, and is usable
- **AND** no integration menu command was run

#### Scenario: Adopted workspaces are not offered the birth step
- **WHEN** the user anchors a directory that is already a workspace
  (`anchored-existing` context)
- **THEN** no birth-time integration offer is presented
