---
name: canonical-checkout-dotgit-named-gitdir
description: --canonical-checkout returns a directory that merely holds a git dir named `.git` (separate-git-dir or symlinked .git) instead of the main worktree
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
relations:
  - blocks:resolver-outcomes
discovered_from: wave3-review-fixes
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
---

## Problem

When a repository's real git directory is named `.git` but lives OUTSIDE its
work tree, `workspace-bookmark--canonical-checkout` returns the directory that
CONTAINS that git dir rather than the main worktree. This happens with
`git init --separate-git-dir=<elsewhere>/.git`, or with a main worktree whose
`.git` is a symlink to such a directory. `--canonical-for-identity` then offers
a directory holding only `.git`, even when the real main worktree is
`jf/emacs-dir` itself.

The cause is in `--canonical-checkout` (portable-bookmarks.el ~519-529): the
`--git-common-dir` parent, the first porcelain entry, and the `--show-toplevel`
check all agree on the wrong directory, because git run inside the parent of a
bare-looking `.git` dir treats that parent as a work tree. The logic predates
wave3-review-fixes; the reviewer of that fix re-found it while re-attacking
the safety rule.

It usually degrades to a missing file, but it is the "not the repository's
main worktree" outcome the canonical-checkout safety rule forbids, and
`resolver-outcomes` is about to consume this function.

## Implementation steps

1. Reproduce both layouts in a fixture repo (separate git dir named `.git`,
   and a symlinked `.git`), and pin the current wrong answer in a failing spec.
2. Make `--canonical-checkout` return a directory only if git, run THERE,
   reports that directory as its top level AND that directory's git dir IS the
   repository's common dir (compare with `workspace-bookmark--same-file-p`),
   AND the directory contains something besides the git dir. Choose the
   smallest rule that rejects both layouts. Prefer nil to a guess.
3. Keep every existing discovery spec green. Show that each new spec fails
   with the fix reverted.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green.
- Full-suite set comparison: NO-REGRESSION.

## Context
- `.orchestrator/cycles/cycle-1789935943/reviews/wave3-review-fixes.md` (advisory 1)
