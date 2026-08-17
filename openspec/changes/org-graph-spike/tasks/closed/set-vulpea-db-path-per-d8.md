---
name: set-vulpea-db-path-per-d8
description: Set vulpea-db-location to runtime/state/vulpea/notes.db per design D8 so the org-graph DB is isolated from org-roam, instead of the default runtime/vulpea.db.
change: org-graph-spike
status: done
relations:
  - discovered-from:registry-discovery
cites_register_entries:
  - register/invariant/vulpea-db-isolation
discovered_by: architect
discovered_class: interface-drift
---

> **Cycle 1782561220 (plan):** cites the new speculative invariant
> `register/invariant/vulpea-db-isolation`. This task IS that invariant's
> enforcement mechanism — setting `vulpea-db-location` + a load-time
> assertion/spec confirms `speculated → confirmed` at integrate. The path
> home is the loader (`org-graph.org`); the loader is touched by two other
> batch tasks this cycle — keep your edit **append-only / localized** to the
> vulpea `use-package`/setq, do not reorder existing loads. Stage files
> explicitly (no `git add -A`).

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

## Observations

- **Runtime-dir convention resolved to `user-emacs-directory`.** `early-init.el`
  sets `user-emacs-directory` to the worktree's `runtime/` directory (priority
  2: "use local `runtime/` if it exists"; in batch tests `EMACS_USER_DIRECTORY`
  = `RUNTIME_DIR`). So `(expand-file-name "state/vulpea/notes.db"
  user-emacs-directory)` = `runtime/state/vulpea/notes.db` and is automatically
  worktree-isolated. Note: the only other `state/` consumer
  (`config/core/defaults.org` bookmarks) bases off `jf/emacs-dir` (repo root,
  git-tracked `state/`), which is the WRONG base here — D8 wants the gitignored
  `runtime/state/`, so `user-emacs-directory` is the correct anchor.
- **Home = the loader, for a stronger reason than the plan's "before first
  sync".** `discovery.el`'s load-time `org-id-locations` seed
  (`org-graph/seed-org-id-locations`) calls `vulpea-db-query` AT MODULE LOAD,
  which opens the DB. discovery.el is loaded from the loader's Submodules
  section, i.e. AFTER the vulpea `use-package`. Setting `vulpea-db-location` in
  `configure-sync` would therefore be too late — the seed would already have
  opened the DB at the default path. The loader's vulpea `:config` runs eagerly
  (no `:defer`) before any submodule loads, so it is the only correct home.
- **Edit kept minimal/localized** to the vulpea `use-package` block + its
  immediately-preceding prose paragraph (the two other loader-touching batch
  tasks edit different sections). No existing loads reordered.
- **Manual check passed:** after the spec run, `runtime/state/vulpea/notes.db`
  exists (~110 KB, real DB) and `runtime/vulpea.db` does NOT. `runtime/state/`
  is gitignored — `git status` shows only the intended `.org`/`.el`/spec
  changes, no stray DB.
- **Spec is end-to-end, not a duplicated computation.** `db-location-spec.el`
  requires the actual loader (`org-graph.el`) so the assertion exercises the
  real `use-package :config`. vulpea is already loaded in the batch process via
  `helpers-spec.el`, and `init.el` is loaded by the Makefile batch invocation,
  so `use-package`/straight/`user-emacs-directory` are all live — the require
  is cheap and hits no network.

## Discoveries

```yaml
affected_register_entry: register/invariant/vulpea-db-isolation
recommendation: speculated → confirmed
held_as_speculated: true
as_built_path: runtime/state/vulpea/notes.db   # = (expand-file-name "state/vulpea/notes.db" user-emacs-directory)
default_path_avoided: runtime/vulpea.db         # confirmed absent after a real run
enforcement:
  - kind: spec
    location: config/org-graph/test/db-location-spec.el
    asserts: vulpea-db-location is under runtime/state/, basename notes.db, parent dir vulpea/, and != runtime/vulpea.db; parent dir is created
  - kind: load-time-side-effect
    location: config/org-graph/org-graph.org (vulpea use-package :config)
    asserts: make-directory of the DB parent at loader load
refinements:
  - The plan said home it in the loader "unless it must run before first sync,
    in which case configure-sync". The truth is stronger: it must run before
    discovery.el's LOAD-TIME org-id seed (which queries the DB), not merely
    before configure-sync. configure-sync would have been too late. The loader
    is the only correct home — recorded in the loader prose and Observations.
  - status_note in the register entry can drop the parenthetical "(or
    configure-sync, if it must precede first sync)" — configure-sync is not a
    viable home given the load-time seed.
notes: |
  Speculation held exactly: vulpea-db-location now resolves to
  runtime/state/vulpea/notes.db, isolated from org-roam and vulpea's default.
  Recommend flipping speculated → confirmed at integrate; the entry is now
  load-bearing (enforced by db-location-spec.el + the loader make-directory).
```
