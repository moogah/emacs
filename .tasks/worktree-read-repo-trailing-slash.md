---
name: worktree-read-repo-trailing-slash
description: jf/workspace--worktree-read-repo returns a typed source path without a trailing slash, so magit-main-branch and magit-worktree-branch run in the PARENT directory
status: ready
task_class: bug
source: openspec/changes/workspace-portable-bookmarks (cycle-1790453714)
relations:
  - discovered-from:discovery-memo-dead-helpers-and-picker-scope
  - related:worktree-read-repo-empty-input
discovered_from: discovery-memo-dead-helpers-and-picker-scope
discovered_by: implementor
discovered_class: invariant-gap
reconciled_into: null
---

> Externalised by the orchestrator, cycle-1790453714 execute (2026-09-27). Pre-existing in core `git-worktrees`, outside the
> portable-bookmarks change's outcome, so it is not an in-change task.

## The bug

`jf/workspace--worktree-read-repo` (`config/workspaces/git-worktrees.org`) returns `(expand-file-name choice)` for a free-form
answer, and the `read-directory-name` floor's result the same way — neither is passed through `file-name-as-directory`.
`jf/workspace--add-worktree` then binds it as `default-directory` for `magit-main-branch` and `magit-worktree-branch`. Emacs reads a
`default-directory` without a trailing slash as a FILE name, so git runs in the parent directory: the wrong repository, or none.
Completion candidates are slash-ended, so only typed paths hit it. The `:source-repo` arm (added in cycle-1790453714) already
slash-ends its directory; the prompt arm does not.

## Fix
Wrap both return values in `file-name-as-directory`; one spec per arm (typed path without slash; `read-directory-name` floor) asserting
the `default-directory` seen by a stubbed `magit-main-branch`. Tangle `git-worktrees.org`; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green.
