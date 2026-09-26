---
name: ownership-and-picker-polish
description: Small follow-ups from the wave-4 reviews — remote magit entries never touch the network, and prose drift
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: false
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/workspace-git-worktrees-consumer
relations:
discovered_from: [worktree-candidates-git-first, canonical-predicate-consolidation]
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
---

## Decided (implement)

1. **Remote magit entries must never touch the network** (worktree-candidates review,
   advisory). The git-repo filter probes a TRAMP entry that magit returns BEFORE
   classification. A failed connection raises `file-error` out of
   `jf/workspace--worktree-repo-candidates`, so the "Source git repo:" prompt and
   its `read-directory-name` floor never appear. Drop remote entries in
   `jf/workspace--worktree-dedupe-dirs` (they could never classify as canonical,
   because discovery is local-only). Correct the prose "building the list touches
   no network" (git-worktrees.org:112-118). Spec: a stubbed `magit-list-repos`
   returning `/ssh:nohost.invalid:/a/` still reaches the prompt.
2. **Prose drift** (consolidation review): portable-bookmarks.org ~1822 still says
   classification "runs no subprocess". Correct it.

## Deferred by the user (NOT in scope)

Ownership by tracked content (ask 12) and anchored repos in the picker (ask 14)
were deferred on 2026-09-26 until the user has used the feature. See
`.tasks/portable-bookmarks-ownership-refinements-after-use.md`. Do not implement
them here.

## Verification
- Tangle both .org files; no .el drift.
- `./bin/run-tests.sh -d config/workspaces` all green; full-suite NO-REGRESSION.
