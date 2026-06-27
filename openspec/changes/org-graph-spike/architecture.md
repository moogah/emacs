## Components

The spike introduces a single new module `config/org-graph/`, organized
into focused sub-modules. The top-level loader (`org-graph.org`) declares
the custom group + defcustoms, pins and configures vulpea, and loads the
sub-modules in dependency order. vulpea (v2.4+) is the **single** index /
discovery engine — there is no org-node / org-mem (RE-2).

- **`org-graph.org`** — Top-level loader. Declares the `org-graph` custom
  group and the module defcustoms (`org-graph-roam-root`,
  `org-graph-relation-types`, `org-graph-watch-workspace-homes`,
  `org-graph-note-types`), pins vulpea via straight (`:branch "v2.4.0"`)
  and — in the vulpea `use-package` `:config` — sets `vulpea-db-location`
  to the isolated worktree path `runtime/state/vulpea/notes.db` (D8 /
  `register/invariant/vulpea-db-isolation`). The DB path is fixed here,
  eagerly, before any sub-module opens the DB. The loader then loads
  sub-modules in dependency order (schemas before finders).

- **`discovery`** (`discovery.el`) — Registry-driven vulpea sync (RE-1 /
  RE-2). Builds an *explicit, bounded* root set — `org-graph-roam-root`
  plus, when `org-graph-watch-workspace-homes` is non-nil, each active
  workspace `:home` and its `sessions/` subdir read from the `workspaces`
  registry — and points `vulpea-db-sync-directories` at it. There is **no**
  recursive `~/work` walk and no `directory-files-recursively` eager-scan;
  the bounded-roots discipline is the load-bearing reason org-node was
  dropped (`register/invariant/bounded-discovery-roots`). `workspaces` is a
  *soft* dependency (guarded by `featurep`), so discovery degrades to the
  roam vault alone when it is absent. A load-time shim seeds the global
  `org-id-locations` from `(vulpea-db-query)` so `id:` links resolve on a
  fresh session (vulpea registers IDs lazily and offers no bulk seed).

- **`schemas`** (`schemas.el`) — Note-type taxonomy via `vulpea-schema`
  (RE-3). Defines one `vulpea-schema-define` per type in
  `org-graph-note-types` (`log debug topic reference project`). Each
  schema's `:predicate` selects notes by **filetag membership** (the
  filetag is the selector, encoded only in the predicate; schema *fields*
  read metadata via `vulpea-note-meta-get`, so a filetag is never itself a
  field). `agent-draft` is **not** a note type — it is a cross-cutting
  filetag the write tool stamps, with no schema. Also exposes validation
  wrappers (`org-graph/validate-note-type`, `org-graph/validate-all-of-type`).

- **`finders`** (`finders.el`) — Schema-aware per-type finder commands
  (RE-3). Each finder drives `vulpea-find`'s `:filter-fn` from the *same*
  note-type schema predicate the schemas module validates against (single
  source of truth for "what a topic note is"), rather than a hand-rolled
  filetag predicate over org-node candidates. `org-graph/find-agent-drafts`
  filters the cross-cutting `agent-draft` filetag directly, bypassing the
  type schemas.

- **`extractor`** (`extractor.el`) — Typed-edge extraction. Holds the
  **pure parser** (`org-graph-extractor/parse-typed-edges`, a pure function
  over an `org-element` AST, unit-tested with synthetic trees) plus the
  vulpea extractor wrapper and registration. The wrapper registers via
  `make-vulpea-extractor` with a `typed_edges` schema foreign-keyed to
  `notes(id)` `:on-delete :cascade`, at priority 50, and relies on the
  vulpea 2.4 parser-epoch for cache invalidation (RE-4). A scope gate
  restricts extraction to notes under `org-graph-roam-root`; extraction is
  attributed note-granularly to each note's own PROPERTIES drawer.

- **`query`** (`query.el`) — Read API for typed edges:
  `org-graph-query/outgoing`, `org-graph-query/incoming`,
  `org-graph-query/connected`. `typed_edges` is org-graph's own side table
  (not a vulpea-managed `notes` table), so queries route through raw
  emacsql on the shared `(vulpea-db)` connection via a single
  `org-graph-query--select` seam — **not** `vulpea-db-query`. Results are
  returned as edge plists (`:from :rel :to :note`), resolving the far-end
  `vulpea-note` per edge.

- **`coordinator`** (`coordinator.el`) — Per-file cooperative write lock
  for agent tools (D5). `org-graph-coordinator/with-file-lock (PATH BODY...)`
  serializes writes to the same canonicalised path and permits parallel
  writes to distinct paths, releasing the lock in `unwind-protect` on error
  or non-local exit. Busy-waits via `accept-process-output` up to
  `org-graph-coordinator-timeout` (a defcustom owned by this sub-module),
  then signals `org-graph-coordinator-lock-timeout`.

- **gptel tools + workspace integration** (planned; loader placeholders) —
  The loader carries placeholder sections for the gptel query/write tool
  surface and the `workspaces` integration (`:on-create` watch-add handler,
  menu entry, `workspace-assistant` `:tools` population, RE-5), to be
  filled by their own tasks. They are not yet implemented.

## Interfaces

**Public Lisp API (consumed by the user, by other modules, by tests):**

```
;; Discovery
(org-graph/index-roots)                          ;; -> bounded list of dirs to index
(org-graph/configure-sync)                       ;; point vulpea at roots, enable autosync, scan
(org-graph/seed-org-id-locations)                ;; seed org-id-locations from the vulpea DB

;; Finders (interactive)
(org-graph/find-topic)
(org-graph/find-debug)
(org-graph/find-log)
(org-graph/find-reference)
(org-graph/find-project)
(org-graph/find-any)
(org-graph/find-agent-drafts)

;; Schemas / validation
(org-graph/validate-note-type NOTE)              ;; in-memory, no DB
(org-graph/validate-all-of-type TYPE)            ;; hits the vulpea DB

;; Query
(org-graph-query/outgoing FROM-ID &optional REL-TYPE)
(org-graph-query/incoming TO-ID   &optional REL-TYPE)
(org-graph-query/connected NOTE-ID)

;; Coordinator
(org-graph-coordinator/with-file-lock PATH BODY...)

;; Pure parser (testable without vulpea)
(org-graph-extractor/parse-typed-edges ELEMENT-TREE NOTE-ID)
;; -> list of (FROM-ID REL-TYPE TO-ID) tuples
```

**Relation-type vocabulary** (`register/vocabulary/relation-types`,
confirmed): the closed initial set is the symbols `implements`,
`contradicts`, `supersedes`, `relates-to`, declared by the
`org-graph-relation-types` defcustom. Each maps to a PROPERTIES-drawer key
by upcasing and turning hyphens into underscores (`relates-to` ⇄
`:RELATES_TO:`, `implements` ⇄ `:IMPLEMENTS:`), via
`org-graph-extractor--rel-key` / `--key->rel` (the only allowed
translation sites). The relation symbol is stored **verbatim as a SYMBOL**
in the `typed_edges` `rel-type` column (emacsql `prin1`/`read` round-trips
symbols), and the query layer matches on the symbol.

**Vulpea integration:** `extractor` registers via `make-vulpea-extractor`
with a `typed_edges` schema, foreign-keyed to `notes(id)` with
`:on-delete :cascade`, at priority 50 (after vulpea's core extractors).
Registration applies the schema (opening the DB) and installs the
extractor; it is exposed as a function the loader calls once vulpea is
available, so a bare `require` does not touch the DB.

**Discovery integration:** vulpea is the single index. Discovery feeds it
explicit bounded roots from the `workspaces` registry plus
`org-graph-roam-root` and enables `vulpea-db-autosync-mode`. The global
`org-id-locations` cache is seeded from the vulpea DB at load. No
`org-node`, no `org-id-update-id-locations` sweep, no
`directory-files-recursively`.

**gptel tool surface (planned):** Tools will register through
`gptel-make-tool`, namespaced `org-graph-*`, and into the
`workspace-assistant` preset `:tools` slot (RE-5). Read tools return
plists; the write tool routes through the coordinator and stamps the
`agent-draft` filetag. Not yet implemented.

## Boundaries

**In scope for the spike:**
- The sub-modules above (discovery, schemas, finders, extractor, query,
  coordinator) plus the planned gptel-tools / workspace-integration.
- The PROPERTIES-drawer convention for typed edges (initial relation set:
  implements / contradicts / supersedes / relates-to).
- Note-type taxonomy via `vulpea-schema` (log / debug / topic / reference /
  project). `agent-draft` is a cross-cutting filetag, not a taxonomy type.
- Coexistence with org-roam in the same vault.
- Registry-driven vulpea discovery over `org-graph-roam-root` plus active
  workspace `:home` directories, so pre-existing notes are reachable.

**Out of scope for the spike (deferred to follow-up changes):**
- Migrating any content out of `~/org/roam/`.
- Retiring or modifying the existing org-roam configuration.
- A graph visualization UI (org-supertag's React-Flow board, vulpea-ui
  dashboards). Findings inform whether to add one.
- Bidirectional typed-edge inference (declaring `IMPLEMENTS` on one side
  and auto-creating `IMPLEMENTED_BY` on the other).
- Promotion workflow for `agent-draft` notes (review → demote tag).
- Capture templates for any of the new note types.
- Org-roam-to-org-graph sync of legacy backlinks.

## Testing Approach

### Test Framework

**Buttercup** (BDD-style, codebase preferred for new tests). Tests live in
`*-spec.el` files; ERT is not introduced for any new spike code.

### Test Organization

Flat layout — one `*-spec.el` per sub-module concern under
`config/org-graph/test/`:

```
config/org-graph/test/
├── helpers-spec.el            ; shared fixtures, AST builders, vulpea stubs
├── db-location-spec.el        ; vulpea DB isolation invariant (D8)
├── discovery-spec.el          ; index-roots, seed-org-id-locations, configure-sync
├── schemas-spec.el            ; note-type schema registration + validation
├── finders-spec.el            ; schema-aware finder filtering behavior
├── parse-typed-edges-spec.el  ; pure-parser unit tests
├── extractor-spec.el          ; scope gate, note-granular attribution, storage shape, registration
├── typed-edges-spec.el        ; query API (outgoing/incoming/connected), vulpea stubbed
└── coordinator-spec.el        ; write-coordinator lock semantics
```

### Naming Conventions

- File: `<concern>-spec.el` (matches existing codebase).
- Suite: `(describe "org-graph-<concern>" ...)`.
- Test: `(it "<expected behavior>" ...)`. Tests phrased as observable
  behavior, not implementation.

### Running Tests

```
./bin/run-tests.sh -d config/org-graph                        # All
./bin/run-tests.sh -d config/org-graph -f buttercup            # Explicit
make test-buttercup-directory DIR=config/org-graph             # Make
make test-report DIR=config/org-graph                          # Snapshot
```

Interactive: `C-c t` (existing transient menu).

### Test Patterns

**Mocking strategy: API-boundary mocks via `cl-letf`.** Tests do not spin
up a real vulpea SQLite DB. Functions on the vulpea API surface are stubbed
inside each spec:

```elisp
(cl-letf (((symbol-function 'org-graph-query--select)
           (lambda (column id &optional rel-type) <fixture rows>)))
  ...)
```

This matches the codebase's behavioral-test convention (function-scoped
mocks, no global state).

**Pure-parser tests** (`parse-typed-edges-spec.el`): construct synthetic
`org-element` trees in-memory via the test helper
`org-graph-test/build-tree`; assert the parser returns the expected
`(from-id rel-type to-id)` tuples. No file I/O, no vulpea, no org-mode
state.

**Extractor tests** (`extractor-spec.el`) exercise the scope gate
(in/out-of-`org-graph-roam-root`), note-granular attribution (a heading's
own drawer, never a descendant's), the symbol storage shape, and
`make-vulpea-extractor` registration — with `emacsql` / `vulpea-db`
stubbed.

**Finder tests** stub `vulpea-find` and `vulpea-schema-applies-p`; assert
the per-type filter admits the right notes and that `agent-draft` is
orthogonal to the type finders.

**Coordinator tests** are sequential. They verify:
- A function executed under `with-file-lock` runs to completion.
- A second `with-file-lock` on the same path queues until the first
  releases (by inspecting the in-process lock table).
- An error inside the body releases the lock.
- Locks on distinct paths are independent.
- A held lock past `org-graph-coordinator-timeout` signals
  `org-graph-coordinator-lock-timeout`.

**DB-isolation test** (`db-location-spec.el`) asserts `vulpea-db-location`
resolves under `runtime/state/vulpea/notes.db`, distinct from org-roam and
from vulpea's default (D8).

**Shared helpers** (`helpers-spec.el`):
- `org-graph-test/build-tree` — build an `org-element` AST with PROPERTIES
  drawers and filetags from a plist spec.
- `org-graph-test/with-stubbed-vulpea` — macro wrapping common `cl-letf`
  stubs for the vulpea API surface.
- `org-graph-test/note-fixture` / `org-graph-test/link-plist` — construct
  `vulpea-note`-shaped plists / link plists for query and finder tests.

### Scenario Mapping

This is **spike-grade** coverage: every requirement gets at least one test,
but not every scenario. Priorities:

| Spec Requirement                  | Test Coverage |
|-----------------------------------|---------------|
| Distributed Note Discovery        | `discovery-spec` covers `index-roots` (bounded roam + workspace homes, no wider walk), the `org-id-locations` seed, and `configure-sync`. The 5-second / external-change scenarios are validated manually during the spike. |
| Typed Semantic Edges              | Full coverage of the pure parser (`parse-typed-edges-spec`): single property, multi-valued, multiple property types, malformed input, empty drawer. Extractor (`extractor-spec`): scope gate, note-granular attribution, storage-as-symbol, registration. Query API (`typed-edges-spec`): outgoing, incoming, connected — each with stubbed `org-graph-query--select`. |
| Note-Type Taxonomy and Finders    | `schemas-spec` covers schema registration, predicate selection, and validation; `finders-spec` covers one filter test per finder plus the agent-draft orthogonality case. |
| Agent-Facing Graph Tools          | Coordinator tests below cover the concurrent-write path. gptel tool-registration and write-tool filetag stamping land with the (pending) gptel-tools task. |
| Coexistence with org-roam         | `db-location-spec` asserts DB isolation; org-roam UX continuity is a manual check during the spike. |
| Non-Blocking Synchronization      | **Not automatically tested.** Latency budget validated manually during spike usage; a benchmark spec is deferred to a permanent-module follow-up. |
| Coordinator (cross-cutting)       | `coordinator-spec`: lock acquired/released, queued call waits, error releases lock, distinct paths independent, timeout signals. |

Manually validated during spike (not in the test suite): file-watcher
latency, external-change detection, save-latency budget, real concurrent
agent-tool writes against the live coordinator.

## Dependencies

**New emacs packages (via straight.el):**
- `vulpea` v2.4+ (d12frosted/vulpea) — the single index / discovery engine
  and typed-edge index host. Pinned via `:branch "v2.4.0"` (release tag,
  detached HEAD at commit `0f55c96…`).
- `vulpea-journal` (d12frosted/vulpea-journal) — provisional daily-log slot
  (Open Question 5).

There is **no** `org-node` and **no** `org-mem`: vulpea already keeps
`org-id-locations` populated via `org-id-add-location`, so a second index
would be redundant (RE-2).

**Existing dependencies the module relies on:**
- `org-id` (built-in) — ID-based discovery; the discovery seed depends on
  it explicitly.
- `org-element` (built-in) — AST for the pure parser.
- `workspaces` — **soft** dependency. Discovery reads the workspace
  registry for `:home` roots, guarded by `(featurep 'workspaces)`, and
  degrades to the roam vault alone when absent.
- `gptel` — tool registration (pending gptel-tools task). Load order:
  org-graph after both `gptel` and `workspaces` (RE-5).
- `straight.el` — package management (existing infrastructure).

**System dependencies:**
- `fswatch` — preferred for vulpea change detection on macOS, but
  **optional**: vulpea 2.4 falls back to fd/find polling, so the spike does
  not require it.

## Constraints

- **Org-roam coexistence:** No org-roam variable, advice, or schema
  modification is permitted. The org-graph vulpea DB lives at
  `runtime/state/vulpea/notes.db` (D8 / `register/invariant/vulpea-db-isolation`),
  distinct from any org-roam path and from vulpea's default
  (`runtime/vulpea.db`). DB isolation is the spike's clean-rollback
  property: wiping/rebuilding the org-graph DB never touches org-roam's DB
  or the `org-id-locations` cache.
- **Bounded discovery roots:** Discovery indexes only `org-graph-roam-root`
  plus the active workspace `:home` directories the registry enumerates
  (`register/invariant/bounded-discovery-roots`). org-graph deliberately
  **never** walks `~/work` or any wider tree — that blind recursive
  scan/watch is the inotify/fsevents blow-up risk that dropping org-node
  avoids. The user tunes the root set via `org-graph-roam-root` and
  `org-graph-watch-workspace-homes` (default `t`).
- **Performance:** Save latency under watched roots SHOULD stay low in the
  foreground. The spike does not enforce this in CI; manual measurement
  during use is the gate. If save latency regresses, narrowing the watched
  workspace-home set is the first lever.
- **AI agent writes:** All gptel tool writes producing graph files MUST go
  through `org-graph-coordinator/with-file-lock`. Any tool that writes
  outside the coordinator is a bug.
- **Scope of the typed-edge index:** The extractor only runs on notes whose
  path is under `org-graph-roam-root` (default `~/org/roam/`, the durable
  concept vault). Notes under workspace `:home` / `sessions/` participate in
  discovery and navigation but contribute zero typed edges
  (`register/invariant/typed-edge-extraction-scope`, RE-4 / D2). This is a
  hard boundary for the spike.
- **Spike duration:** This change is scoped for ~1 week of evaluation. If
  the user keeps it, a follow-up `org-graph-v1` change captures production
  hardening (latency benchmarks, full scenario coverage, capture templates,
  promotion workflow, documentation).
