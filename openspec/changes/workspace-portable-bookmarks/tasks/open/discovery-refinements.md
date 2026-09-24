---
name: discovery-refinements
description: Discovery fixes before the resolver consumes it — .git-named external git dir, anchored workspace homes, sibling-worktree ambiguity, directory-only symlink ownership
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
discovered_from: [wave3-review-fixes, checkout-discovery]
discovered_by: [reviewer, user]
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

## Scope (user decisions 2026-09-24 folded in)

This task began as the `.git`-named git dir bug (A, below, unchanged). The user's
decisions on asks 7 and 8 add three small changes to the SAME `* Checkout
discovery` section, so they ship together:

- **B. Anchored workspaces (ask 7a, agreed).** A workspace created by
  anchoring to an existing repository has that repository AS its `:home`, not
  as a child of it. `--home-checkouts` must include the home itself when the
  home is a checkout, so such a workspace can resolve bookmarks into its own
  repository. Add a behavioral spec using the real anchor constructor
  (`workspace--new-anchor-existing` or whatever data-model path it uses;
  verify in source).
- **C. Sibling worktrees of one repository in one home (ask 7b).** Stop
  silently returning the alphabetically-first. Add
  `workspace-bookmark--workspace-checkouts (identity workspace-name)` returning
  EVERY matching checkout. `--workspace-checkout` returns the match only when it
  is unique, and nil when there are none OR several. The resolver
  (`resolver-outcomes`) decides how ambiguity maps onto the outcome vocabulary;
  do NOT add an outcome here. Spec both counts. (The user's longer-term
  direction, a checkout-path hint in the stamp, is a separate backlog item. Do
  not change the stamp.)
- **D. Symlink ownership, directory links only (ask 8).** Narrow the
  wave3-review-fixes widening. A symlinked child of a home confers ownership
  only when the link is to a DIRECTORY, and then only on the checkout whose
  root IS or CONTAINS the link target. File links confer nothing. A directory
  link to a directory ABOVE a checkout (e.g. `ws/A/src -> ~/src`) confers
  nothing. Keep the existing symlinked-child-to-checkout-root specs green.
  Add specs showing that a single-file link and a link above a repository no
  longer make that repository workspace-owned.

## Implementation steps (A: the .git-named git dir)

1. Reproduce both layouts in a fixture repo (separate git dir named `.git`,
   and a symlinked `.git`), and pin the current wrong answer in a failing spec.
2. Make `--canonical-checkout` return a directory only if git, run THERE,
   reports that directory as its top level AND that directory's git dir IS the
   repository's common dir (compare with `workspace-bookmark--same-file-p`),
   AND the directory contains something besides the git dir. Choose the
   smallest rule that rejects both layouts. Prefer nil to a guess.
3. Implement B, C and D above, each with its own specs.
4. Keep every existing discovery spec green. Show that each new spec fails
   with the fix reverted.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green.
- Full-suite set comparison: NO-REGRESSION.

## Context
- `.orchestrator/cycles/cycle-1789935943/reviews/wave3-review-fixes.md` (advisory 1, spec-signal 1)
- `.orchestrator/cycles/cycle-1789935943/reviews/checkout-discovery.md` (spec-signal: :home-children model)
