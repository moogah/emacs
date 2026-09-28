---
name: outcome-action-mapping
description: One outcome-to-action mapping for the jump seam, chooser and off-workspace flow; the seam declines (not falls through) for stamped records; drop the unreachable fallback payload builder
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/invariant/no-silent-wrong-file-open
relations:
  - discovered-from:eoc-1790453714-04
  - discovered-from:eoc-1790453714-05
discovered_from: [eoc-1790453714-04, eoc-1790453714-05]
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-04 / eoc-05.

## Steps
1. (05) The outcome → action mapping is spelled in five places; the seam's error barrier falls through to the stored path while the
   chooser's and off-workspace barriers decline, so "a catch-all must signal, never open" holds per site but not end to end. Introduce ONE
   mapping function; for a STAMPED record the seam's barrier declines instead of calling ORIG-FN with the original argument (unstamped
   records keep stock behaviour). Spec: an error inside the seam's decision for a stamped record opens nothing.
2. (04) Delete the unreachable hand-built fallback in `--add-worktree-payload` (the offer gate and core's constructor live together) and
   the spec that keeps it alive by unbinding the constructor.
3. Tangle; commit `.org` + `.el`.

## Verification
- Suite green; full NO-REGRESSION; new spec fails with the seam change reverted.
