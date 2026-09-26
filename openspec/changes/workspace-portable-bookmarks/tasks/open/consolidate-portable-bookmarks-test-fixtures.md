---
name: consolidate-portable-bookmarks-test-fixtures
description: One fixtures-owned registry scope and one commit-a-file helper for the portable-bookmarks and git-worktrees specs; fold the five per-file git wrappers into one
change: workspace-portable-bookmarks
status: ready
task_class: test
on_critical_path: false
cycle: cycle-2
cites_register_entries:
  - register/boundary/portable-bookmarks-consumer
relations:
  - discovered-from:arch-cycle-1789935943-eoc-04
  - relates-to:.tasks/test-suite-shared-global-state-isolation
  - relates-to:.tasks/investigate-cross-suite-pollution-scope-tests
discovered_from: arch-cycle-1789935943-eoc-04
discovered_by: architect
discovered_class: duplication
created_at: 2026-09-26T14:00:00-04:00
---

## Summary

Three tasks each wrote the same test plumbing: swap `workspace--registry` for a fresh table
around the fixtures (discovery-spec.el:60-73, resolver-spec.el:63-84, git-worktrees-spec.el:285-314)
and "write a file, git add, git commit" (discovery-spec.el:380-385, resolver-spec.el:161-172,
stamp-spec.el:41-59). The shared `portable-bookmarks-fixtures.el` exists to receive them. Test
hygiene is in scope for this change by user decision (scope amendment 2026-09-20).

## Cited register entries

- `register/boundary/portable-bookmarks-consumer` (confirmed this cycle): its `test_corpus`
  names the spec files; keep the list accurate after the move.

## Implementation steps

1. Add to `config/workspaces/test/portable-bookmarks-fixtures.el`:
   `workspace-bookmark-fixtures-commit-file (repo relpath &optional contents)` and a macro
   `workspace-bookmark-fixtures-with-empty-registry` (save the registry, install a fresh
   table, run fixtures setup; restore on exit, including non-local exit).
2. Route discovery-, resolver-, stamp- and git-worktrees-spec through them; delete the local
   copies. The resolver helper's re-setting of user.email / user.name / gpgsign duplicates
   `fixtures-make-repo` (fixtures.el:151-153); drop it.
3. Fold the five per-file git wrappers (fixtures.el:115-128, birth-offer-spec.el:55,
   integration-dispatch-spec.el:58, scaffold-spec.el:45, workspace-new-anchor-spec.el:52) into
   one `config/workspaces/test/` helper, if the four pre-existing `call-process` forms can
   share it without changing their assertions; otherwise leave them and say why in
   `## Observations`.
4. All specs green, in isolation AND in the full run (the shared-registry hash is what the
   `.tasks/test-suite-shared-global-state-isolation` item is about; do not regress it).

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.
- `grep -c "workspace--registry" config/workspaces/test/*-spec.el` shows the swap in one place.

## Out-of-scope
Promoting the two `.tasks/` test-hygiene items (a PM/user decision, ask in the cycle-1 digest).
Any production code.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Finding 4
- meta-discovery M7; process note on the `rm -rf $TMPDIR/wsbm-*` incident
