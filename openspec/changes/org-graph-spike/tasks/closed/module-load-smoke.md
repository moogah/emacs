---
name: module-load-smoke
description: Integration spec asserting the module loads cleanly, defcustoms and schemas register, gptel tools and the workspace integration are registered, and org-roam remains intact.
change: org-graph-spike
status: done
relations:
  - blocked-by:registry-discovery
  - blocked-by:auto-id-scaffold
  - blocked-by:note-type-schemas
  - blocked-by:finders-and-filters
  - blocked-by:typed-edge-query
  - blocked-by:coordinator-lock
  - blocked-by:gptel-tools
  - blocked-by:workspace-integration
cites_register_entries:
  - register/invariant/org-graph-loader-ordered-sequence
  - register/boundary/workspace-integration-registry
  - register/boundary/org-graph-agent-tools
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

## Cycle 1782564058 updates (cycle-1782564058)
> Still blocked — sole remaining blocker is `workspace-integration` (now `ready`,
> expected next cycle). `gptel-tools` landed this cycle (`135139b4`).

- **Step 5 is now concrete.** `tools.el` is implemented and load-wired in
  `org-graph.org`'s gptel-tools section. Assert the **three snake_case gptel
  `:name`s** `org_graph_query` / `org_graph_typed_edges` / `org_graph_write_node`
  are present in the gptel tool registry, and that `org-graph/agent-tools`
  returns the constructed tool objects. Note registration is gated on
  `(fboundp 'gptel-make-tool)` — drive the loader path that calls
  `org-graph-tools-register`, or assert presence only when gptel is loaded
  (mirror how `db-location-spec` loads `org-graph.el` without gptel on
  `load-path`).
- `register/boundary/org-graph-agent-tools` is **RECONCILED** (see the
  reconciliation note) — the surface shape is now firm; assert against it.
- Loader-order risk is unchanged: the full ordered submodule sequence
  (now including `tools` after `query` + `coordinator`) is `wire-into-init`'s
  job; this smoke spec is the gate proving the consolidated loader loads cleanly
  standalone with all submodules wired and org-roam intact.

## Cycle 1782566912 updates (cycle-1782566912)
> **Unblocked → status flipped blocked → ready.** The last blocker
> `workspace-integration` landed this cycle (merge `6c5fa7ce`); every other blocker
> was already closed. This is the next ready task.

Absorb before implementing:
- **Now carries `cites_register_entries`** (it had none): it PROBES
  `register/invariant/org-graph-loader-ordered-sequence` (SPECULATED this cycle — the
  canonical submodule order `schemas → extractor → coordinator → query → finders →
  tools → discovery`, with `workspace-integration` after `tools`; basename≠feature
  makes order load-bearing). This smoke spec is the cold-standalone-load gate that
  proves every registration fires and org-roam is intact — i.e. it is the primary
  enforcement site that should drive that speculated invariant toward reconciliation.
  Also cites `register/boundary/workspace-integration-registry` (step 6) and
  `register/boundary/org-graph-agent-tools` (step 5).
- **Step 6 (workspace integration registered) is now concretely backed.** The
  org-graph integration is implemented (`config/org-graph/workspace-integration.el`,
  feature `org-graph-workspace-integration`): it registers via
  `(with-eval-after-load 'workspaces (workspace-register-integration 'org-graph :label … :on-create … :menu (cons "G" …)))`
  and populates the `workspace-assistant` `:tools` slot via
  `with-eval-after-load 'gptel-preset-workspace-assistant`. Assert the integration is
  registered (stub/inspect `workspace-register-integration`), and that the
  `:on-create`/`:menu` handlers are present. Note: registration + slot population are
  **`with-eval-after-load`-gated**, so in a bare test process where workspaces/the
  preset never load, the registration form does not fire — drive the loader path or
  trigger the `after-load` to assert it (mirror how `db-location-spec` loads
  `org-graph.el` without gptel).
- **Step 5 unchanged**: assert the three snake_case gptel `:name`s (`org_graph_query`
  / `org_graph_typed_edges` / `org_graph_write_node`) and that `org-graph/agent-tools`
  returns the tool objects (nil-tolerant when gptel absent).
- **Caveat for the gate run (process note, not spec content):** the worktree runtime
  for `workspace-integration` was missing `vulpea` (its `init-worktree-runtime.sh`
  predates vulpea landing in `main`); the org-graph suite needs vulpea on `load-path`.
  Ensure a fresh `init-worktree-runtime.sh` (post-vulpea) or copy vulpea into the
  worktree runtime before running `-d config/org-graph`.

## Observations

- **The loader (`org-graph.el`) is scattered and omits two submodules.** The
  current loader loads, in this order: `query` -> `tools` ->
  `workspace-integration` -> `schemas` -> `finders` (coordinator pulled in
  transitively by tools). It does NOT load `extractor.el` or `discovery.el`.
  So a bare `(require 'org-graph)` does NOT define `org-graph-extractor-register`
  nor `org-graph/configure-sync` / `org-graph/seed-org-id-locations`, and never
  registers the typed-edge extractor. The smoke spec therefore loads every
  submodule by path in the canonical dependency order
  (`schemas -> extractor -> coordinator -> query -> finders -> tools ->
  discovery`, workspace-integration after tools) and drives the
  function-exposed registration entry points explicitly. This asserts the END
  STATE the downstream `wire-into-init` task must make the consolidated loader
  reach. See Discoveries (invariant-gap) for the register reconciliation.
- **Loader order also differs from canonical even for the submodules it DOES
  load:** it loads `query`/`tools`/`workspace-integration` BEFORE `schemas`,
  whereas the canonical order puts `schemas` (and `extractor`) first. This works
  today only because the schemas->finders edge is the sole hard load-time
  dependency the loader currently honors; `wire-into-init` owns the full
  reorder.
- **Registration is function-exposed by design (not load-time) to avoid a
  require-time DB open.** `org-graph-extractor-register` -> `vulpea-db-register-extractor`
  -> `vulpea-db--apply-plugin-schema` -> `(vulpea-db)` opens SQLite; and
  `discovery.el` runs `org-graph/seed-org-id-locations` (a `vulpea-db-query`) at
  load. The spec stubs the vulpea DB boundary (`vulpea-db`/`emacsql`/`vulpea-db-query`
  via `cl-letf`) so no live SQLite DB or fswatch is touched. This confirms the
  "registration is function-exposed, not load-time" cycle note: extractor/tools
  registration are safe to call, but the discovery seed at load DOES reach for
  the DB (error-guarded) -- a consolidated loader that simply `require`s
  discovery will attempt a DB query at load time. Flagged for `wire-into-init`.
- **Tool registration is gptel-gated.** `tools.el` only registers when
  `(fboundp 'gptel-make-tool)`. The spec loads gptel (adds the compat+gptel
  straight build dirs, mirroring `tools-spec`) so the real registration path
  fires and the three tools land in `gptel--known-tools` (asserted via
  `gptel-get-tool`). The per-module `tools-spec` already covers the nil-tolerant
  gptel-absent case, so this spec drives the present case.

## Discoveries
- discovery_id: disc-module-load-smoke-1
  class: invariant-gap
  description: |
    `register/invariant/org-graph-loader-ordered-sequence` is currently
    DIVERGENT, not reconciled. The invariant states `org-graph.org`/`.el`
    load-wires EVERY submodule via `jf/load-module` BY PATH in one
    consolidated dependency-correct sequence
    (`schemas -> extractor -> coordinator -> query -> finders -> tools ->
    discovery`, workspace-integration after tools). In reality the current
    loader (1) OMITS `extractor.el` and `discovery.el` entirely, and (2)
    loads the submodules it does load in a non-canonical order (query/tools/
    workspace-integration before schemas). Consequence: a cold standalone
    `(require 'org-graph)` does NOT fire all registrations -- the typed-edge
    extractor is never registered and the discovery functions are undefined.
    The smoke spec proves this by having to load `extractor.el`/`discovery.el`
    by path itself (the loader did not) to make every registration land.
  affected_register_entry: register/invariant/org-graph-loader-ordered-sequence
  recommendation: |
    Move the invariant speculated -> divergent until the downstream
    `wire-into-init` task consolidates the loader. `wire-into-init` MUST: add
    `extractor.el` and `discovery.el` to the loader; reorder to the canonical
    `schemas -> extractor -> coordinator -> query -> finders -> tools ->
    discovery` (workspace-integration after tools); and decide whether the
    discovery seed should run at load (it reaches the DB) or be deferred to a
    post-init hook. Once consolidated, this smoke spec can be simplified to
    `(require 'org-graph)` alone (dropping the explicit per-submodule
    `require`s) and the invariant can move divergent -> reconciled. Until then
    the spec documents the END STATE the loader must reach.
- discovery_id: disc-module-load-smoke-2
  class: spec-signal
  description: |
    Step 5 of the task and the cycle update reference "the gptel tool
    registry"; concretely that is gptel's `gptel--known-tools` alist, queried
    via `gptel-get-tool` (which signals if absent), with the org-graph tools
    filed under category "org-graph". `org-graph/agent-tools` is the separate
    reusable accessor returning the constructed `gptel-tool` objects. Both
    surfaces are asserted and both depend on `org-graph-tools-register` having
    run under a loaded gptel. This is consistent with
    `register/boundary/org-graph-agent-tools` (RECONCILED); no drift found
    there.
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    No reconciliation needed -- entry matches implementation. Recorded only so
    integrate has the concrete registry symbol (`gptel--known-tools` /
    `gptel-get-tool`) the smoke gate uses.
