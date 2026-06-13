---
name: verify-change
description: Full workspaces test run, confirm every delta-spec scenario maps to a passing test, snapshot
change: add-workspace-git-worktrees
status: blocked
relations:
  - blocked-by:purge-teardown-dispatch
  - blocked-by:birth-transient-offer
  - blocked-by:add-worktree-command
  - blocked-by:on-purge-teardown-handler
---

## Files to modify
- config/workspaces/test-report.txt (modify) — snapshot output
- (no source changes expected; fix regressions in the relevant module if any)

## Implementation steps
1. Run the full workspaces suite: `./bin/run-tests.sh -d config/workspaces`.
   All Buttercup specs must pass (existing + new).
2. Walk each `#### Scenario` across the three delta specs and confirm a
   corresponding passing `it` exists:
   - specs/workspace-git-worktrees/spec.md — git non-mandatory / absent-magit /
     repo-source merge+filter+floor / worktree-off-main / multiple repos /
     collision / teardown removal / merged-vs-unmerged branch / dirty-no-abort /
     derived-no-persistence.
   - specs/workspace-integrations/spec.md — on-purge-only registration / none-of-
     three-surfaces error / purge dispatch order / skipped-on-delete / throwing
     handler non-fatal.
   - specs/workspaces/spec.md — purge dispatch before deletion / cancel / refuse
     external / birth offer for fresh+anchored-scaffolded / decline / adopted
     skipped.
   Note any scenario lacking a test and add it to the owning module's spec file.
3. Confirm directionality lint still holds: workspaces core
   (data-model/integrations/workspaces/transient) names no git or magit symbol;
   only git-worktrees.org references magit.
4. Confirm `git-worktrees.el` removal safety: with the module not loaded,
   workspaces core still tangles/loads and the registry simply has no git entry.
5. Snapshot: `./bin/run-tests.sh -d config/workspaces --snapshot` (or
   `make test-report DIR=config/workspaces`) and commit the updated
   test-report.txt with the change.

## Design rationale
The change touches three capabilities and a new module; a single final pass
guarantees the delta-spec scenarios are actually covered and that the
directionality/droppability invariants the design rests on still hold end to
end.

## Verification
- `./bin/run-tests.sh -d config/workspaces` — green
- Every delta-spec scenario maps to a passing test
- Directionality + droppability invariants confirmed
- test-report.txt snapshot updated

## Context
design.md § 'D7 — Testing approach' and § Risks / Trade-offs
specs/** (all three delta specs)
