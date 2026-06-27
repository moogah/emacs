---
name: set-vulpea-db-path-per-d8
description: Set vulpea-db-location to runtime/state/vulpea/notes.db per design D8 so the org-graph DB is isolated from org-roam, instead of the default runtime/vulpea.db.
change: org-graph-spike
status: ready
relations:
  - discovered-from:registry-discovery
discovered_by: architect
discovered_class: interface-drift
---

## Why
End-of-cycle architect finding `arch-cycle-1782551613-eoc-1`. Design D8: "Vulpea
DB at `runtime/state/vulpea/notes.db`, distinct from any org-roam path … No
shared state" — the resilience decision so wiping/rebuilding the org-graph DB
never affects org-roam. The implementation never sets `vulpea-db-location`, so
vulpea uses its default (`runtime/vulpea.db` was observed when specs ran). The
isolation guarantee is unmet.

## Files to modify
- `config/org-graph/org-graph.org` (loader) OR `config/org-graph/discovery.org`
  (configure-sync) — set `vulpea-db-location`.

## Implementation steps
1. Set `vulpea-db-location` to `(expand-file-name "state/vulpea/notes.db" <runtime-dir>)`
   per D8. Resolve `<runtime-dir>` from the existing runtime path convention
   (see how other modules locate `runtime/state/`). Create the directory if
   vulpea does not.
2. Decide the home: the loader (alongside the vulpea use-package) is the
   natural place for a path defcustom/setq; configure-sync is acceptable if it
   must run before first sync. Document the choice.
3. Add a spec (or load-time assertion) that `vulpea-db-location` resolves under
   `runtime/state/`.

## Verification
- `./bin/run-tests.sh -d config/org-graph` green.
- Manual: after a sync, the DB file exists under `runtime/state/vulpea/` and NOT
  at `runtime/vulpea.db`. `runtime/state/` is already gitignored (no stray DB in
  git status).

## Context
Finding `.orchestrator/cycles/cycle-1782551613/findings/arch-cycle-1782551613-eoc-1.md`;
design.md D8; config/org-graph/org-graph.org; config/org-graph/discovery.org.
