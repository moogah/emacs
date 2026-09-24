---
name: workspace-anchor-linked-worktree-scaffolds-into-repo
description: "Anchoring a workspace at an existing LINKED worktree falls into the full-scaffold path and runs git init / git add . / commit inside the user's checkout, on their branch."
source: discovery-refinements on-touch architect (cycle-1789935943, workspace-portable-bookmarks)
status: ready
task_class: bug
discovered_from: discovery-refinements
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
relations:
  - discovered-from:discovery-refinements
---

## Problem

`workspace--new-anchor-existing` treats a directory as an existing repository
only when its `.git` is a DIRECTORY. A linked worktree's `.git` is a FILE
(`gitdir: ...`), so anchoring a workspace at a linked worktree falls into the
full-scaffold case: it runs `git init`, then `git add .`, then a commit, inside
the user's existing checkout and on the user's branch.

The architect confirmed the `git init` step in a throwaway probe: git printed
"Reinitialized existing Git repository", and an untracked file showed as `??`.
It deliberately did NOT run the commit step. Severity is therefore unconfirmed
past `git init`, but the code path indicates an unwanted commit of every
untracked file on the user's branch.

This is workspaces CORE, outside workspace-portable-bookmarks. It matters to
that change because `discovery-refinements` part B (anchored homes count as
checkouts) is exactly the path a user would take to anchor at a worktree.

## Suggested fix

Detect "already a git checkout" with `git rev-parse --is-inside-work-tree` (or
`.git` existing as a file OR a directory), not with `file-directory-p` on `.git`.
Add a spec that anchors at a real linked worktree and asserts HEAD and the index
are unchanged.
