---
name: portable-bookmarks-ownership-refinements-after-use
description: "Two ownership refinements deliberately deferred until the user has lived with portable bookmarks and the git-worktree picker for a while: ownership by tracked content instead of location, and anchored repos in the worktree picker."
source: workspace-portable-bookmarks cycle-1789935943 (asks 12 and 14)
status: backlog
task_class: feature
discovered_from: [canonical-predicate-consolidation, worktree-candidates-git-first]
discovered_by: reviewer
discovered_class: spec-signal
reconciled_into: null
relations:
  - discovered-from:canonical-predicate-consolidation
  - discovered-from:worktree-candidates-git-first
---

USER 2026-09-26: "Let's punt on this decision, I want to use this a while before
we decide on polish like this." Both items FAIL CLOSED today: they can only
hide a checkout, never offer a wrong one.

## 1. Ownership by tracked content, not location (ask 12)

A workspace home currently owns any main clone it sits INSIDE. That hides the
clone (as the resolver fallback AND as a worktree-picker source) for every
workspace when the home is:
- a gitignored clone nested inside it, e.g. `runtime/straight/repos/<pkg>`
  inside every Emacs checkout (anchoring a workspace there to hack on a package
  makes `~/emacs` non-canonical everywhere, silently);
- a subdirectory of a nested same-repo worktree;
- untracked under a dotfiles `$HOME` repo.

Proposal (reviewer): a home owns a containing clone only when the clone TRACKS
content at the home's path (`git -C <checkout> ls-files -z -- <relpath>`, one
subprocess). That keeps the decided case (a workspace at `~/emacs/config` owns
`~/emacs`) and would replace the nested-same-repo-worktree exemption from DD-13.

## 2. Anchored repos in the worktree picker (ask 14)

A workspace anchored at an existing repo has that repo as its home, so the repo
classifies `workspace-owned` and the picker never offers it, even to OTHER
workspaces that want a worktree of it. The resolver's reason for hiding owned
checkouts (never show another workspace's in-progress tree) does not apply to
choosing a worktree SOURCE. The purge hazard is already covered, because
`workspace-purge` refuses an anchored external home without C-u. Options: a
picker-specific exception for anchored homes, or keep one rule and type the path.
