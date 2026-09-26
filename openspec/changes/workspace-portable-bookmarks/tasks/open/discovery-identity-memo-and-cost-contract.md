---
name: discovery-identity-memo-and-cost-contract
description: Route discovery through the identity memo, memoise the canonical test per common dir within a call, and pin a spawn budget for the resolve and picker paths
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-2
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
relations:
  - discovered-from:arch-cycle-1789935943-eoc-06
  - relates-to:jump-seam
discovered_from: arch-cycle-1789935943-eoc-06
discovered_by: architect
discovered_class: invariant-gap
created_at: 2026-09-26T14:00:00-04:00
---

## Summary

The stamp path is memoised (`--memoised-identity`, `.el:239-251`) but the discovery path is
not: `--workspace-checkouts` (`.el:711-714`) and `--canonical-checkouts-for-identity`
(`.el:796-797`) run `--repo-root` + identity per candidate. Measured on the audit fixture:
`missing-repo` = 10 spawns / ~160 ms, the picker = 21 spawns / ~600 ms with three magit
entries, about four spawns per entry. DD-15 (`magit-repository-directories` = `~/src`, ~51
repos) made the picker cost live: ~200 spawns before the "Source git repo:" prompt.

## Cited register entries

- `register/boundary/git-derived-checkout-discovery` (reconciled this cycle; the audit drafted
  a `cost_contract`): the memo is validated by the git config mtime, so the resolver inherits
  the stamp path's invalidation rule. Pressure-test that rule under a worktree whose config
  is the common dir's.

## Implementation steps

1. `--checkout-position` already returns ROOT and GIT-COMMON-DIR from one `rev-parse`; have
   both plural discovery functions take identity through `--memoised-identity` so a warm memo
   costs one spawn per candidate.
2. In `--canonical-checkouts-among`, memoise `--canonical-checkout` per common dir within one
   call, so N linked worktrees of one repository pay once.
3. Specs: extend the spawn-budget pattern of `stamp-spec.el:539-687` to `--resolve` (warm
   memo: `resolved` ≤ 2 spawns, `missing-repo` ≤ 1 + in-view count) and to
   `jf/workspace--worktree-repo-candidates` (≤ 2 spawns per magit entry on a warm memo).
   Each must fail with the memo bypassed.
4. Tangle and validate.

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.

## Out-of-scope
Async or cached discovery across calls beyond the existing memo. Changing outcomes.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Finding 6
- DD-7 (memoise repo-root → identity), DD-15
