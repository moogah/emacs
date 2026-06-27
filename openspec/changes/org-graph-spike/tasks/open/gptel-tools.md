---
name: gptel-tools
description: Register graph query and coordinator-mediated write-node gptel tools so AI agents can read the typed graph and write notes safely.
change: org-graph-spike
status: ready
relations:
  - blocked-by:typed-edge-query
  - blocked-by:coordinator-lock
cites_register_entries:
  - register/boundary/org-graph-agent-tools
  - register/boundary/typed-edge-query-api
  - register/invariant/coordinator-lock-contract
  - register/invariant/typed-edge-extraction-scope
---

## Files to modify
- `config/org-graph/tools.el` ← via `config/org-graph/org-graph.org`
  (gptel tools section)
- `config/org-graph/test/tools-spec.el` (new)

## Implementation steps
1. Register gptel tools via `gptel-make-tool`, namespaced `org-graph-*`:
   - `org-graph-query` — structured note query (wraps `vulpea-db-query` /
     finders); returns plists of matching notes (id, title, tags, path).
   - `org-graph-typed-edges` — wraps `org-graph-query/{outgoing,incoming,
     connected}`; returns resolved edges (from/rel/to with target titles).
   - `org-graph-write-node` — creates/updates a note; the write MUST route
     through `org-graph-coordinator/with-file-lock`. Stamp an
     `:agent-draft:` filetag on agent-created notes. Returns the new note id
     and path.
2. Keep the tools as a registration LIST/function the workspace-integration
   task can hand to the `workspace-assistant` preset `:tools` slot — do not
   hard-wire them only into the global gptel tool registry. Expose
   `org-graph/agent-tools` returning the tool objects/names.
3. Tool descriptions must state the boundary: typed-edge extraction runs only
   on `~/org/roam/` notes (D2/RE-4), so writing a project-local note will not
   produce typed edges. This keeps agent prompts honest.
4. Write `tools-spec.el`: assert the three tools are constructed; the
   write-node path invokes the coordinator (stub `with-file-lock` and assert
   it wrapped the write); the write stamps `:agent-draft:`.

## Design rationale
RE-5: the agent-facing surface plugs into workspaces. Exposing the tools as a
reusable list lets the workspace-integration task fill the
`workspace-assistant` preset's deliberately-empty `:tools` slot, so the
per-workspace agent (already directory-scoped via `GPTEL_WORK_ROOT`) can query
the graph. Routing every write through the coordinator (D5) is the
corruption-safety guarantee.

## Design pattern
`gptel-make-tool` per existing tools in `config/gptel/tools/`. Read tools
return plists; the write tool returns id + path. Mirror the metadata/argument
conventions of the filesystem tools and their contract tests in
`config/gptel/tools/test/`.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — tools spec passes
  (construction, coordinator-wrapped write, draft stamp).
- Manual smoke: the three tools appear in the gptel tool registry; a write
  through `org-graph-write-node` creates a note with `:agent-draft:` and does
  not corrupt under a simulated double call.

## Context
design.md § Re-evaluation (RE-5); design.md § Decisions D5;
architecture.md § Interfaces (gptel tool surface);
config/gptel/presets/workspace-assistant/preset.org (empty :tools slot).
</content>

## Cycle 1782561220 updates (cycle-1782561220)
> Unblocked: both blockers (`typed-edge-query`, `coordinator-lock`) are done. Status flipped blocked → ready.

Absorb before implementing:
- **`register/boundary/typed-edge-query-api` (CONFIRMED this cycle):** the read
  surface (`org-graph-query/outgoing` / `-incoming` / `-connected`) now exists in
  `config/org-graph/query.el`. It returns edge plists `(:from :rel :to :note)`,
  `:rel` a SYMBOL, no auto-symmetry, `:note` the resolved far-end note. Build the
  graph-read gptel tools on these functions; don't re-query the table directly.
- **Side tables need raw emacsql, NOT `vulpea-db-query`.** `vulpea-db-query` reads
  only vulpea's `notes` table; `typed_edges` is read via `(emacsql (vulpea-db) …)`.
  If a tool must read org-graph side tables, use the raw connection.
- **Coordinator-mediated writes:** `org-graph-coordinator/with-file-lock` is
  unconditional (always locks the canonicalised path) per
  `register/invariant/coordinator-lock-contract` — wrap write-node tools in it.
- **Loader placement:** a new tools submodule follows the one-`.org`-per-`.el`
  convention; it must load AFTER `query` and `coordinator`. The full ordered
  submodule sequence is `wire-into-init`'s responsibility (see its stanza) — note
  the dependency there rather than re-deriving load order here.
