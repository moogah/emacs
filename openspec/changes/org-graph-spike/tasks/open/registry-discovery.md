---
name: registry-discovery
description: Make vulpea the single index fed explicit roots from the workspace registry plus the roam vault, and seed org-id-locations from the vulpea DB at startup so id-links resolve immediately.
change: org-graph-spike
status: ready
relations:
  - blocked-by:install-packages
cites_register_entries:
  - register/invariant/bounded-discovery-roots
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

## Orchestrator brief addenda (cycle-1782551613)

From the foundation Architect audit; cited register entry in `interfaces.org`.

- **Follow design RE-1/RE-2, NOT architecture.md.** Finding
  `arch-cycle-1782551613-01`: architecture.md describes an
  `org-graph-watched-roots` defcustom with default `'("~/org/roam/" "~/work/")`
  and a `~/work` recursive eager-scan — both are wrong and superseded. The
  implemented knobs are `org-graph-roam-root` + `org-graph-watch-workspace-homes`
  (already in the loader). There is no `~/work` walk.

- **`register/invariant/bounded-discovery-roots` is the load-bearing contract
  and your acceptance gate.** The discovery spec MUST include the negative
  assertion: no `directory-files-recursively` (or equivalent wholesale walk)
  over `~/work`. Roots = roam-root + mocked workspace homes only; the seed
  calls `org-id-add-location` once per DB note.

- **Helper coverage gap.** `org-id-add-location` is NOT in the
  `org-graph-test/with-stubbed-vulpea` stub map. The discovery spec will need
  its own `cl-letf` for it (and for the workspaces registry accessors).

## Observations

- Implemented as a NEW submodule `config/org-graph/discovery.org`
  (tangles `discovery.el`); `org-graph.org` was deliberately NOT edited
  (load-wiring deferred per the structural rule). New spec:
  `config/org-graph/test/discovery-spec.el` (buttercup).
- Three public functions land: `org-graph/index-roots`,
  `org-graph/configure-sync`, `org-graph/seed-org-id-locations`, plus a
  private `org-graph--active-workspace-homes` helper.
- Workspaces registry shape confirmed: the in-memory source of truth is
  `workspace--registry` (hash, NAME→plist, defined in
  `config/workspaces/tabs.el`); enumerate with
  `workspace--registered-names`, resolve home via `workspace--home`,
  sessions via `workspace--sessions-dir`. `index-roots` appends each
  home AND its `sessions/` subdir (both canonicalised, trailing slash).
- Soft-dep guard is `(featurep 'workspaces)` — `workspaces.el` provides
  `'workspaces`; mirrors `gptel/sessions/filesystem.el` and the
  `with-eval-after-load 'workspaces` pattern in `workspace-integration`.
- vulpea 2.4 API verified against `runtime/straight/build/vulpea`:
  `vulpea-db-sync-directories` (defcustom), `vulpea-db-autosync-mode`
  (global minor mode), `vulpea-db-sync-full-scan` (async, optional arg),
  `vulpea-db-query` (optional predicate → notes), `vulpea-note-id`,
  `vulpea-note-path`, `org-id-add-location`.
- Load-time seed is wrapped in `ignore-errors` so an unbuilt/missing DB
  at load time never aborts module loading; the unit-tested behaviour is
  exercised directly against `org-graph/seed-org-id-locations`.
- Tests: `./bin/run-tests.sh -d config/org-graph` → Ran 36 specs, 0
  failed (22 baseline + new discovery specs). Negative invariant asserted
  via buttercup spies on `directory-files-recursively` AND
  `directory-files` (`:not :to-have-been-called`).

## Discoveries

- class: register-confirmation
  affected_register_entry: register/invariant/bounded-discovery-roots
  detail: |
    The speculated invariant held exactly as written and is now enforced
    by the discovery spec. `org-graph/index-roots` returns ONLY
    `org-graph-roam-root` plus registry-derived workspace homes (+ their
    sessions/ subdirs); there is no fallback that walks a wider tree. The
    spec includes the negative assertion (no `directory-files-recursively`
    / `directory-files` during root computation), the per-DB-note seed
    assertion (`org-id-add-location` once per note), the watch-flag-off
    case, and the workspaces-absent degradation case.
    RECOMMENDATION: promote speculated -> confirmed.
  push_back: none — entry was accurate, including the "(and sessions/)"
    detail, which is implemented.

- class: helper-gap-confirmed
  affected_register_entry: register/invariant/bounded-discovery-roots
  detail: |
    Confirmed the addenda's helper gap: `org-graph-test/with-stubbed-vulpea`
    covers `vulpea-db-query` (used for the seed) but NOT
    `org-id-add-location` nor the workspaces registry accessors. The
    discovery spec installs its own scoped `cl-letf` stubs for
    `org-id-add-location`, `workspace--registered-names`,
    `workspace--home`, `workspace--sessions-dir`, and dynamically binds a
    fixture `workspace--registry` + the `workspaces` feature (via
    `let`-bound `features`). No change to the shared helper was made
    (keeps the helper's vulpea-only boundary clean); if future tasks need
    the same workspace stubs, consider promoting
    `org-graph-test--with-workspaces` from this spec into helpers-spec.

- class: stale-artifact
  affected_register_entry: n/a
  detail: |
    architecture.md's `org-graph-watched-roots` defcustom (default
    `'("~/org/roam/" "~/work/")`) and `~/work` eager recursive scan are
    stale (already flagged by arch-cycle-1782551613-01). Implementation
    follows design RE-1/RE-2: knobs are `org-graph-roam-root` +
    `org-graph-watch-workspace-homes`, no `~/work` walk. No new finding;
    just confirming the audit was correct.
