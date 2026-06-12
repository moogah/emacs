---
name: add-worktree-command
description: Interactive :menu command — create a worktree off main inside the workspace home
change: add-workspace-git-worktrees
status: blocked
relations:
  - blocked-by:git-worktrees-module-scaffold
---

## Files to modify
- config/workspaces/git-worktrees.org (modify) — add the command, and tangled .el
- config/workspaces/test/git-worktrees-spec.el (modify)

## Implementation steps
1. Implement `jf/workspace--add-worktree` taking the anchor PAYLOAD (the
   `:menu` calling convention — operate solely on the payload, never on the
   current tab). Steps:
   a. `repo` ← `jf/workspace--worktree-read-repo` (from the scaffold task).
   b. `main` ← main branch of `repo`: bind `default-directory` to `repo` and
      call `(magit-main-branch)`.
   c. `branch` ← `read-string` defaulting to `(plist-get payload :name)` (the
      workspace name = the feature branch shared across repos). Sanitize the
      default into a valid git ref (replace spaces/illegal chars) and show the
      sanitized value as the editable default.
   d. `dir` ← `(expand-file-name (file-name-nondirectory (directory-file-name repo)) (plist-get payload :home))`
      — child of `:home` named for the source repo's basename.
   e. If `dir` already exists, return `(cons 'failed (format "worktree dir already exists: %s" dir))` — never overwrite.
   f. Create the worktree on a NEW branch off main: bind `default-directory` to
      `repo` and call `(magit-worktree-branch dir branch main)`.
   g. Return `'ok` on success; wrap the magit call in `condition-case` and
      return `(cons 'failed (error-message-string err))` on any git error.
2. Confirm the `:menu` registration in the scaffold task references this symbol
   (`(cons "w" #'jf/workspace--add-worktree)`).
3. Tangle + validate: `./bin/tangle-org.sh config/workspaces/git-worktrees.org`.
4. Extend `git-worktrees-spec.el` (behavioral, mock at the magit boundary):
   spy `magit-main-branch`, `magit-worktree-branch`, and the repo-read/`read-string`
   prompts. Assert: a worktree is created at `<home>/<repo-basename>/` on a new
   branch off the detected main with the default branch = workspace name;
   invoking twice for two repos yields two child worktrees; a pre-existing
   child dir returns `failed` and makes no magit call; a magit error returns
   `failed`.

## Design rationale
A worktree is a CHILD of `:home`, so creation is a post-anchor effect that fits
the interactive `:menu` surface (it prompts; `:on-create` must not). Using
magit's `magit-worktree-branch`/`magit-main-branch` reuses tested plumbing and
avoids re-implementing main-branch detection. The branch defaults to the
workspace name so one feature spans N repos on one branch — the core ergonomic.
Binding `default-directory` per call is mandatory because magit functions act on
the current repo.

## Design pattern
Model the command shape and the `ok`/`(failed . reason)` return on
config/gptel/sessions/workspace-integration.org's `jf/gptel--workspace-add-session`
(the `:menu` handler that prompts then files an artifact and returns `ok`).

## Verification
- `./bin/tangle-org.sh config/workspaces/git-worktrees.org` validates
- `./bin/run-tests.sh -d config/workspaces` — add-worktree specs pass
- Acceptance (spec scenarios): add a worktree off main on a workspace-named
  branch; multiple repos in one workspace; child collision refused.

## Context
design.md § Decisions 'D1' and 'D6 — No persisted git state; everything derived'
specs/workspace-git-worktrees/spec.md (Requirement: Worktree creation off main)
