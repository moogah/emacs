---
name: discovery-memo-dead-helpers-and-picker-scope
description: Remove or re-home the three discovery helpers left without a production caller by the memo work, and let the worktree picker build its candidates in one discovery-memo scope
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
  - register/boundary/workspace-git-worktrees-consumer
relations:
  - discovered-from:arch-on-touch-1790453714-23
  - discovered-from:arch-on-touch-1790453714-25
  - discovered-from:arch-on-touch-1790453714-26
  - discovered-from:disc-missing-repo-choice-2
  - blocked-by:discovery-identity-memo-and-cost-contract
discovered_from: [arch-on-touch-1790453714-23, arch-on-touch-1790453714-25, arch-on-touch-1790453714-26]
discovered_by: architect
discovered_class: dead-branch
created_at: 2026-09-27T11:30:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute from the on-touch audit of
> `discovery-identity-memo-and-cost-contract` (merge 6736b02a). Wave 4, after that task's fix pass lands.

## Summary

The per-call discovery memo left `workspace-bookmark--checkouts-of`, `--identity` and `--repo-root` with no production caller
(second instance of the class end-of-cycle finding 2 named last cycle: helpers kept alive by tests only). Separately, the
git-worktrees repo picker builds its candidate list in TWO memo scopes because the stable public entry
`workspace-bookmark-canonical-checkouts-in-view` is called twice (once for the view, once for magit's list), so a repository
present in both pays its three git calls twice. This task closes both without changing any outcome.

## Cited register entries

- `register/boundary/git-derived-checkout-discovery` (reconciled, load-bearing): its `functions` list must end up naming only
  live functions; its cost_contract (re-stated at integrate to the measured floor) should hold with the picker in one scope.
  Pressure-test: is `--checkouts-of` worth keeping as the memo-aware home for `--main-worktree-of`, or is deleting it and
  inlining cleaner? Decide from the call graph, report in `## Discoveries`.
- `register/boundary/workspace-git-worktrees-consumer` (confirmed, load-bearing): pins 2/3 (sources git-derived + magit; one
  admission predicate; DD-10 soft `fboundp` call). A one-scope public entry is a change to the stable surface both modules
  share — keep the old entry point working (or alias it) and add the scope-wrapping form; say which in `## Discoveries`.

## Implementation steps

1. Route `--main-worktree-of` through a memo-aware `--checkouts-of`, or delete `--checkouts-of`; delete `--identity` and
   `--repo-root` if nothing but tests calls them (move any still-valuable spec onto the live path). `git grep` in `config/`
   excluding tests must show each surviving helper with a production caller.
2. Give the picker one discovery-memo scope: either a public `workspace-bookmark-with-discovery-memo` macro/function that
   git-worktrees.el wraps around its whole candidate build (guarded by `fboundp` per DD-10), or a single public entry taking
   both candidate sources. Re-pin the picker budget (currently 3 per repo + 1 per extra checkout, with a repo in both sources
   paying twice) so a repo present in both view and magit list pays once.
3. In the picker budget spec header, name the `magit-toplevel` post-filter spawn as knowingly uncounted (spy is at
   `workspace-bookmark--git`).
4. (added 2026-09-27 from missing-repo-choice discovery disc-missing-repo-choice-2, class interface-drift) `jf/workspace--add-worktree`
   ignores the anchor payload's `:source-repo` and re-prompts through `--worktree-read-repo` (a discovery re-run with the free-form
   floor). Make it honour `:source-repo` when present (skip the prompt; ~3 lines in `git-worktrees.org`), keep the prompt when absent,
   and add a spec. This is what lets the bookmark chooser's "add worktree here" target the canonical checkout it was handed.
   Reconcile `register/boundary/workspace-git-worktrees-consumer` / `register/shape/workspace-integration-anchor-payload` at integrate.
5. Tangle `portable-bookmarks.org` and `git-worktrees.org`; validate.

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set (compare-baseline.py NO-REGRESSION).
- `git grep -n 'workspace-bookmark--checkouts-of\|workspace-bookmark--identity\b\|workspace-bookmark--repo-root' -- 'config/**/*.el' ':!config/**/test/**'`
  shows only definitions WITH a production caller (or nothing for deleted ones).
- Picker budget spec passes with the re-pinned number and its bypass companion pins the higher one.

## Out-of-scope
Any change to classification or resolution outcomes. Replacing the worktree-list signal (the design change that would reach a
2-spawn floor) — that is a user decision at integrate.

## Context
- `.orchestrator/cycles/cycle-1790453714/findings/on-touch-identity-memo.md` findings 23, 25, 26
- DD-10 (soft fboundp call), DD-12/13 (picker sources)
