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

## Observations

- **Worktree runtime was missing vulpea.** This worktree's
  `runtime/straight/{build,repos}/vulpea` were absent (gptel was present),
  so the org-graph suite could not load and the baseline could not be
  reproduced. I copied `vulpea` build+repo from the parent worktree
  (`/Users/jefffarr/emacs-org-graph-spike/runtime/straight/...`) — matching
  what `init-worktree-runtime.sh` does. `runtime/` is gitignored, so this
  is not part of the diff; flagging it because future tasks in fresh
  worktrees may hit the same gap until vulpea lands in `jf/enabled-modules`
  (the `wire-into-init` task).

- **`with-file-lock` is a macro, so the spec verifies the lock behaviorally
  rather than stubbing it.** The task said "stub `with-file-lock` and assert
  it wrapped the write." A `defmacro` cannot be `cl-letf`-rebound at call
  time. Instead the stubbed `vulpea-create` reads the *live* coordinator
  lock table from inside the write and asserts (a) this path's lock is held
  and (b) exactly one lock is held during the write, then asserts the lock
  is released after `write-node` returns. This exercises the real macro +
  real coordinator end-to-end — strictly stronger than stubbing the wrapper.

- **gptel tool registration is gated on `(fboundp 'gptel-make-tool)`, not a
  hard `(require 'gptel)`.** A hard require would break `db-location-spec`,
  which loads `org-graph.el` (now loading `tools.el`) in a process that does
  not put gptel on `load-path`. The core tool *functions* are always defined;
  only the gptel-object construction is gated. In production load order
  (wire-into-init) gptel is loaded first, so registration runs. The spec
  adds the `compat`+`gptel` straight build dirs to `load-path` (mirroring how
  `helpers-spec` adds vulpea) and requires gptel before requiring tools.

- **`tools.el` self-requires `query`/`coordinator` by path.** The loader's
  Coordinator section is still a placeholder (owned by `wire-into-init`
  ordering), so I did not wire coordinator loading into `org-graph.org`.
  `tools.el` requires both deps by feature-with-file-fallback, which is a
  no-op once a feature is provided — robust under any load order without
  re-deriving the loader sequence.

- **Read-tool gptel `:function` wrappers stringify via `format "%S"`.** gptel
  tool results are LLM-facing text; the wrappers return a readable sexp of
  the structured plist. The core `org-graph-tools/{query,typed-edges,
  write-node}` functions return the plists the specs assert against — the
  tested contract is the core functions, not the string formatting.

- **`org-graph-tools/query` has no result cap.** With neither filter it
  returns every indexed note via `vulpea-db-query` (which loads the whole
  notes table). Fine at spike scale; a `limit` arg is an easy follow-up if
  the vault grows.

## Discoveries

- discovery_id: disc-gptel-tools-1
  class: interface-drift
  description: |
    The SPECULATED entry register/boundary/org-graph-agent-tools named the
    three tools `org-graph-query`, `org-graph-typed-edges`,
    `org-graph-write-node` (kebab-case). I registered the gptel tool
    `:name`s as snake_case (`org_graph_query`, `org_graph_typed_edges`,
    `org_graph_write_node`) per gptel's documented convention
    ("recommended to be in Javascript style snake_case", `gptel-make-tool`
    docstring) and to match the existing filesystem tools (`create_file`,
    `read_file`). The elisp FUNCTION names remain kebab/lisp-style
    (`org-graph-tools/query` etc.). So the kebab names in the speculation
    map to the lisp functions, and the LLM-facing tool identifiers are
    snake_case.
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    When crystallising the entry, record two name spaces: gptel tool
    `:name`s are snake_case (`org_graph_query` / `org_graph_typed_edges` /
    `org_graph_write_node`); the backing functions are
    `org-graph-tools/query`, `org-graph-tools/typed-edges`,
    `org-graph-tools/write-node`. The reusable accessor is
    `org-graph/agent-tools`.

- discovery_id: disc-gptel-tools-2
  class: spec-signal
  description: |
    The speculation flagged uncertainty over whether `org-graph/agent-tools`
    should return tool NAMES or OBJECTS, and whether the write-node
    signature differs. Both resolved in favor of the speculated shape:
    `org-graph/agent-tools` returns the constructed `gptel-tool` OBJECTS
    (the workspace-assistant preset's `:tools` slot takes objects, and the
    objects are also registered globally by `gptel-make-tool` as a side
    effect). `org-graph-tools/write-node` is
    `(title &optional directory tags body)` and returns `(:id ID :path
    PATH)` — id + path as speculated. The accessor is a plain function (no
    args) returning the cached list; it is nil until
    `org-graph-tools-register` has run.
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    Confirm the entry as written for the object-vs-name and write-node
    questions: accessor returns objects; write-node is
    (title &optional directory tags body) -> (:id :path). Note that
    registration is gated on gptel being loaded, so `org-graph/agent-tools`
    returns nil in a gptel-less process (e.g. a bare unit test) until
    `org-graph-tools-register` is called.

- discovery_id: disc-gptel-tools-3
  class: responsibility-leakage
  description: |
    `org-graph-tools/write-node` computes the target file path itself
    (`<timestamp>-<slug>.org` under the directory) and passes it as
    `vulpea-create`'s absolute `file-name`, rather than letting
    `vulpea-create` derive the name from `vulpea-create-default-template`.
    This is required so the coordinator lock can be keyed on a known path
    BEFORE the write, and so the path can be returned. A side effect is
    that org-graph owns its own filename policy instead of deferring to
    vulpea's create template. For the spike this is the right call (the lock
    contract needs the path up front); noting it because a future
    convergence on vulpea's naming policy would touch this function.
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    When crystallising the entry, document that write-node owns filename
    generation (timestamped slug) so the coordinator can lock the path
    pre-write; it does not consult vulpea-create-default-template.
