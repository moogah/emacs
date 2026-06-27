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
</content>
