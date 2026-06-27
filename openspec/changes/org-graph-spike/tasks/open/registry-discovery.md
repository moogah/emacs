---
name: registry-discovery
description: Make vulpea the single index fed explicit roots from the workspace registry plus the roam vault, and seed org-id-locations from the vulpea DB at startup so id-links resolve immediately.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:install-packages
---

## Files to modify
- `config/org-graph/discovery.el` ← via `config/org-graph/org-graph.org`
  (Discovery section)

## Implementation steps
1. `org-graph/index-roots` — compute the explicit root list: `~/org/roam/`
   (`org-graph-roam-root`) plus, when `org-graph-watch-workspace-homes`, each
   active workspace `:home` directory. Derive homes from the workspaces
   registry (it enumerates homes; see `workspace-sessions-dir`,
   `workspace--home`, and the registry accessors in
   `config/workspaces/data-model.el`). Guard the workspace lookups with
   `(when (featurep 'workspaces) ...)` — workspaces is a soft dependency.
2. `org-graph/configure-sync` — set `vulpea-db-sync-directories` to
   `(org-graph/index-roots)` and enable `vulpea-db-autosync-mode`. Trigger an
   initial scan with `vulpea-db-sync-full-scan` (async). Make this idempotent
   and interactive so it can be re-run after roots change.
3. `org-graph/seed-org-id-locations` — at module load (after vulpea is
   available), seed Emacs's global `org-id-locations` from the vulpea DB:
   iterate `(vulpea-db-query)` results and `org-id-add-location` each note's
   id → path. This closes the one gap vulpea leaves: it registers ids lazily
   (per file touch) and has no bulk-seed, so on a fresh session a previously
   indexed note may not be link-resolvable until its file is re-touched.
   Keep it cheap; run once on load.
4. Do NOT walk `~/work/` blindly. The registry gives a bounded set of homes
   the user actually created — that is the whole point of RE-2 (bounded watch
   load, no inotify/fsevents blow-up).
5. Note: dynamically adding a new workspace home after autosync is enabled
   does NOT auto-install a watcher — that is handled by the
   `workspace-integration` task's `:on-create` handler, not here.

## Design rationale
RE-1/RE-2: workspaces now supplies project-co-located structure, so org-graph
consumes the registry instead of re-deriving discovery. vulpea already feeds
`org-id-locations` via `org-id-add-location` (`vulpea-db-extract.el:1015`),
which was org-node's only unique value — so vulpea alone suffices, plus the
startup seed for fresh-session immediacy. Explicit registry roots also retire
the original watch-load risk.

## Design pattern
Soft-dependency guarding mirrors `config/gptel/sessions/workspace-integration.org`
(`with-eval-after-load 'workspaces`). Interactive idempotent command style
like the workspaces module's re-anchor/refresh commands.

## Verification
- `./bin/run-tests.sh -d config/org-graph` — a discovery spec (mock the
  registry accessors and `vulpea-db-query`/`org-id-add-location` via
  `org-graph-test/with-stubbed-vulpea`) asserts: roots include roam-root and
  each mocked workspace home; seed calls `org-id-add-location` once per DB
  note.
- Manual: after `M-x org-graph/configure-sync`, `vulpea-doctor` shows the
  expected directories; clicking an `id:` link into a roam note from an
  unrelated buffer resolves on a fresh session.

## Context
design.md § Re-evaluation (RE-1, RE-2 — vulpea-only + the two shims);
config/workspaces/data-model.el (registry accessors).
</content>
