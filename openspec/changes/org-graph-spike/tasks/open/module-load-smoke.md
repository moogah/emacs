---
name: module-load-smoke
description: Integration spec asserting the module loads cleanly, defcustoms and schemas register, gptel tools and the workspace integration are registered, and org-roam remains intact.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:registry-discovery
  - blocked-by:auto-id-scaffold
  - blocked-by:note-type-schemas
  - blocked-by:finders-and-filters
  - blocked-by:typed-edge-query
  - blocked-by:coordinator-lock
  - blocked-by:gptel-tools
  - blocked-by:workspace-integration
---

## Files to modify
- `config/org-graph/test/module-load-spec.el` (new)

## Implementation steps
1. Load the tangled `org-graph.el` (and its submodules) in the test and
   assert no error.
2. Assert defcustoms exist with documented defaults: `org-graph-roam-root`,
   `org-graph-relation-types`, `org-graph-note-types`,
   `org-graph-watch-workspace-homes`.
3. Assert the note-type schemas are registered (`vulpea-schema-get` /
   `vulpea-schema-list` returns the five org-graph schemas).
4. Assert the typed-edge extractor is registered with vulpea (the
   `typed_edges` schema is present in the registered extractor set).
5. Assert the three gptel tools (`org-graph-query`, `org-graph-typed-edges`,
   `org-graph-write-node`) are present in the gptel tool registry, and
   `org-graph/agent-tools` returns them.
6. Assert the workspace integration is registered (stub/inspect
   `workspace-register-integration`).
7. Assert org-roam coexistence: org-roam variables/functions remain bound and
   unchanged (e.g. `org-roam-directory` still bound, `org-roam-db-sync` still
   `fboundp`). The vulpea DB path is distinct from any org-roam path.
8. This is a behavioral integration spec — mock at the boundary
   (vulpea/gptel/workspaces registration entry points) where needed; do not
   require a live SQLite DB or fswatch.

## Design rationale
A single smoke spec is the gate before wiring into init (D7/RE-5). It proves
the module's registrations all landed and that org-roam is untouched (the
coexistence constraint, D8) — the spike's rollback story depends on org-roam
being unaffected.

## Design pattern
Mirror existing integration specs under `config/gptel/scope/test/integration/`.
Assert presence/binding, not deep behavior (that is the per-module specs).

## Verification
- `./bin/run-tests.sh -d config/org-graph` — the full org-graph suite,
  including this spec, is green.
- `make test-report DIR=config/org-graph` — snapshot the result.
- org-roam assertions pass (nothing about org-roam changed).

## Context
design.md § Decisions D7, D8; design.md § Re-evaluation (RE-5);
architecture.md § Testing Approach (Scenario Mapping — Coexistence,
Module load).

## Cycle 1782551613 updates (cycle-1782551613)
> Still blocked (blockers `finders-and-filters`, `typed-edge-query`, `gptel-tools`, `workspace-integration` remain open), but the foundation has shifted — read before implementing.

- **Submodules exist but are NOT load-wired yet.** This cycle added
  `config/org-graph/{schemas,discovery}.el` and extended `extractor.el`, each as
  standalone submodules with their own `provide`. By deliberate decision the
  `org-graph.org` Submodules section is still empty and the placeholder sections
  are unfilled — **wiring the submodule loads into `org-graph.org` (and the load
  order) is part of THIS task / `wire-into-init`.** So this smoke spec must
  assert that, after the loader runs, `org-graph-schemas`/`-discovery`/extractor
  registration have actually been invoked (registration is exposed as functions:
  `org-graph-extractor-register`, `org-graph-schemas-register`,
  `org-graph/configure-sync`, `org-graph/seed-org-id-locations`).
- **Registration is function-exposed, not load-time** (avoids require-time DB
  open). The smoke test should drive the loader path that calls them.
- **D8 DB-path follow-up pending** (`set-vulpea-db-path-per-d8`): once it lands,
  assert the vulpea DB resolves under `runtime/state/vulpea/`, not the default
  `runtime/vulpea.db`.
- **Typed-graph granularity is an open user decision** (`scope-extractor-edges-per-note`,
  register `parser-extractor-db` = divergent) — keep the smoke spec model-agnostic.

## Cycle 1782561220 updates (cycle-1782561220)
Most blockers now done — remaining: `gptel-tools`, `workspace-integration`.
- **`finders-and-filters` + `typed-edge-query` landed** as new `finders.org` /
  `query.org` submodules. Step 5 above should also assert the finder commands and
  `org-graph-query/outgoing`/`-incoming`/`-connected` are defined after load.
- **D8 satisfied — supersede the "DB-path follow-up pending" note above.**
  `set-vulpea-db-path-per-d8` landed; assert `vulpea-db-location` resolves under
  `runtime/state/vulpea/` and `!= runtime/vulpea.db`
  (`register/invariant/vulpea-db-isolation`, CONFIRMED). `db-location-spec.el`
  already exists — this smoke spec asserts it at the loader level.
- **Supersede the "keep the smoke spec model-agnostic" note above.**
  `parser-extractor-db` is RECONCILED note-granular (no longer divergent); the
  granularity model is decided. No model-agnosticism needed.
- **Loader load-order is the live risk (architect eoc-1/eoc-2).** schemas →
  finders was inline-fixed this cycle (`3fb895f2`); the FULL ordered submodule
  sequence is `wire-into-init`'s job. This smoke spec is the gate proving the
  loader loads cleanly standalone with all submodules wired and org-roam intact —
  share the canonical order with `wire-into-init`.
