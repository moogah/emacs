---
name: canonical-predicate-consolidation
description: One shared "real canonical checkout" predicate and one public in-view entry point for both portable-bookmarks and git-worktrees; close the OS-artifact husk gap
change: workspace-portable-bookmarks
status: blocked
task_class: refactor
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
  - register/boundary/workspace-git-worktrees-consumer
relations:
  - blocked-by:worktree-candidates-git-first
  - blocks:resolver-outcomes
discovered_from: [discovery-refinements, worktree-candidates-git-first]
discovered_by: [architect, reviewer]
discovered_class: duplication
reconciled_into: null
---

## Why

Three independent findings converge on one shape problem:

1. **OS-artifact husk (architect F1 on discovery-refinements, re-found independently by
   its author-blind reviewer).** `--canonical-checkout`'s fourth check ("a
   tracked path exists under the candidate") is fooled when the repository TRACKS
   an OS artifact such as `.DS_Store` that Finder or a sync client also writes into
   the directory holding only a `.git`-named external git dir. Reproduced:
   `--canonical-for-identity` returns that directory. The org prose claims the
   check defeats exactly these artifacts.
2. **Second route to the husk (architect F2 on discovery-refinements).**
   git-worktrees filters `--checkouts-of` through `--classify` alone. That never runs
   `--canonical-checkout`'s checks, so it offers the husk as a worktree source and
   omits the real main worktree.
3. **Undeclared cross-module coupling (architect F2 on worktree-candidates-git-first).**
   git-worktrees makes fboundp-guarded calls to three `--`-private
   portable-bookmarks functions. Renaming one silently empties the git-derived
   source.

## Implementation steps

1. Add ONE predicate that answers "is DIR a real canonical checkout", composing
   `--canonical-checkout`'s checks with `--classify` = `canonical`. Every "offer as
   canonical" path in BOTH modules goes through it.
2. Harden the tracked-path check against OS and sync artifacts. Either ignore a short,
   documented list of artifact names (`.DS_Store`, `Thumbs.db`, `desktop.ini`,
   `Icon\r`, `.localized`, ...), or require a meaningful fraction of tracked
   top-level entries to exist. Pick one, justify it, and fix the org prose. Spec: a repo
   that tracks `.DS_Store`, with a husk that holds `.git` + `.DS_Store`, returns nil.
3. Add a PUBLIC `workspace-bookmark-canonical-checkouts-in-view` (no `--`) that
   returns the canonical checkouts in view, in order, de-duplicated by truename.
   git-worktrees calls only that, behind its existing call-time `fboundp` guard
   and `condition-case`. Remove the three private calls.
4. Guard (reviewer note): a child of an anchored home whose `.git` is an EMPTY
   directory is not a checkout root. It already fails closed; add the one-line
   guard and a spec.
5. Keep every existing spec in both suites green. Show that each new spec fails with its
   change reverted.

## Verification
- Tangle both `.org` files; no `.el` drift.
- `./bin/run-tests.sh -d config/workspaces` all green; full-suite NO-REGRESSION.
- `git grep -n "workspace-bookmark--" config/workspaces/git-worktrees.el` →
  no calls, only prose if any.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-discovery-refinements.md` (F1, F2)
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-worktree-candidates-git-first.md` (F2)
- `.orchestrator/cycles/cycle-1789935943/reviews/discovery-refinements.md` (advisory; note on empty .git)
