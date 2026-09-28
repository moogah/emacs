---
name: test-stand-in-consolidation
description: Move per-spec stand-ins (fake TRAMP handlers, registered-names stand-ins, the bookmark-jump harness) into the shared fixtures module
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/invariant/spec-registry-isolation-via-fixtures
relations:
  - discovered-from:eoc-1790453714-03
  - discovered-from:meta-discovery/fixture-regrowth
discovered_from: [eoc-1790453714-03, meta-discovery/fixture-regrowth]
discovered_by: architect
discovered_class: duplication
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-1790453714-03.

## Steps
1. One fake-remote file-name handler in `test/portable-bookmarks-fixtures.el` (or a core test-helpers file if core specs need it
   without loading the bookmarks module — decide and say why), parameterised for "name ops pass through" vs "only file-remote-p";
   replace the five copies.
2. One `workspace--registered-names` stand-in; replace the six copies.
3. One bookmark-jump harness (incl. the dired case of `--visits-p`, which the off-workspace copy lost); replace the three copies.
4. Add a counting validator (a spec that greps the test dir) so stand-ins cannot regrow.

## Verification
- Suite green; spec count unchanged except the validator; each replaced spec still fails with its production change reverted (sample 3).
