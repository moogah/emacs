---
name: discovery-scope-hardening
description: Error barrier around git-worktrees' call into the public discovery scope; keep the add-worktree re-resolve out of the discovery memo; refresh two stale docstrings
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/boundary/workspace-git-worktrees-consumer
  - register/boundary/git-derived-checkout-discovery
relations:
  - discovered-from:arch-on-touch-1790453714-47
  - discovered-from:arch-on-touch-1790453714-48
  - discovered-from:arch-on-touch-1790453714-50
  - blocked-by:workspaces-file-equal-p-timestamp-race
discovered_from: [arch-on-touch-1790453714-47, arch-on-touch-1790453714-48, arch-on-touch-1790453714-50]
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-27T21:00:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute from the on-touch audit of
> `discovery-memo-dead-helpers-and-picker-scope` (merge 4dec801c), `findings/on-touch-dead-helpers.md`. Advisory; runs after
> `workspaces-file-equal-p-timestamp-race` merges (both edit portable-bookmarks.org and git-worktrees.org).

## Steps
1. **(47)** `git-worktrees.org`: the call to `workspace-bookmark-call-with-discovery-memo` (git-worktrees.el:93-103 at 4dec801c) has
   no error barrier, so a present-but-signalling scope aborts the "w" command instead of warning and falling back to the typed-path
   prompt that git-worktrees.org:133 promises for every portable-bookmarks call. Give it the same barrier as the other soft calls.
   Spec: scope stubbed to signal → the command warns and reaches the prompt.
2. **(48)** `portable-bookmarks.org`: state the precondition in `workspace-bookmark-call-with-discovery-memo`'s docstring (answers are
   equal only while nothing mutates the repositories inside the scope), and make `--add-worktree-and-replan` re-resolve OUTSIDE any
   enclosing discovery memo after the worktree is added. Spec: add-worktree inside an enclosing scope still finds the new worktree.
3. **(50)** Docstrings: git-worktrees.el:62-63 ("the only portable-bookmarks symbol") and the context list in
   `workspace--integration-payload`'s docstring (integrations.org) — name `bookmark-jump`.
4. Tangle each `.org`; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION via compare-baseline.py.
- Each new spec fails with its production change reverted.
