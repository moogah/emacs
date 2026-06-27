> [!WARNING]
> **PARTIALLY SUPERSEDED BY `design.md` "Re-evaluation (2026-06-27)" (RE-1..RE-6).**
> This document predates the workspaces + vulpea-2.4 re-evaluation and the
> implemented loader. Where it conflicts with `design.md` RE-1..RE-6 or
> `config/org-graph/org-graph.org`, **those win.** Architect finding
> `arch-cycle-1782551613-01` enumerates the false claims. In short:
>
> - **No org-node / org-mem.** Dropped (RE-2). vulpea is the single index.
>   Ignore every "org-node integration", "org-node's navigator", and
>   "org-node's discovery" reference below.
> - **No `~/work` walk / `directory-files-recursively` eager-scan.** Discovery
>   is registry-driven vulpea sync over `org-graph-roam-root` + workspace
>   `:home` directories, plus an `org-id-locations` DB seed (RE-1/RE-2).
> - **No `org-graph-watched-roots` / `org-graph-typed-graph-root` defcustom.**
>   The implemented knobs are `org-graph-roam-root` (single dir) and
>   `org-graph-watch-workspace-homes` (bool). There is no
>   `(org-graph/watched-roots)` function.
> - **Finders are schema-aware** (`vulpea-schema`), not filetag-predicate over
>   org-node candidates (RE-3).
> - **Test layout is flat**: `config/org-graph/test/{helpers,parse-typed-edges,coordinator}-spec.el`,
>   not the nested `extractor/ finders/ query/ coordinator/ integration/` tree shown below.
>
> STILL VALID (do not re-derive): the `make-vulpea-extractor` + `typed_edges`
> table + `notes(id)` FK `:on-delete :cascade` integration; the pure-parser
> tuple shape `(FROM-ID REL-TYPE TO-ID)`; the `with-file-lock` signature;
> Buttercup as the framework; the PROPERTIES-drawer typed-edge convention.
> A full section rewrite is parked as a `.tasks/` doc-hygiene follow-up.

## Components

The spike introduces a single new module `config/org-graph/`, organized
into focused sub-modules:

- **`org-graph.org`** — Top-level loader. Declares the module, loads
  packages via straight, sets defcustoms (root directories, taxonomy
  filetags, relation-type list), and dispatches to sub-modules.
- **`org-graph-discovery`** — Eager-discovery entry point that walks
  configured roots and registers ID-bearing files with
  `org-id-locations`. Wraps org-node's discovery; provides
  `org-graph/eager-discover` and an idempotent re-scan command.
- **`org-graph-extractor`** — Vulpea extractor plugin that parses
  typed-edge properties (`IMPLEMENTS`, `CONTRADICTS`, `SUPERSEDES`,
  `RELATES_TO`) into a `typed_edges` table. The parser is factored as
  a pure function over `org-element` AST so it can be unit-tested
  independently of vulpea's plugin runtime.
- **`org-graph-finders`** — Per-taxonomy finder commands
  (`org-graph/find-topic`, `find-debug`, `find-log`, `find-reference`,
  `find-project`, plus a catch-all `find-any`) backed by org-node's
  navigator with a filetag predicate. Plus `org-graph/find-agent-drafts`
  for review.
- **`org-graph-query`** — Query API for typed edges:
  `org-graph-query/outgoing`, `org-graph-query/incoming`,
  `org-graph-query/connected`. Built on `vulpea-db-query` against the
  `typed_edges` table.
- **`org-graph-coordinator`** — Per-file write lock for agent tools.
  Exposes `org-graph-coordinator/with-file-lock (path) BODY` that
  serializes writes to the same path and permits parallel writes to
  distinct paths. Releases the lock on error or non-local exit.
- **`org-graph-tools`** — gptel tool registrations that wrap
  `org-graph-query/*` and a `org-graph-tools/write-node` writer that
  routes through the coordinator and stamps the `:agent-draft:`
  filetag.

## Interfaces

**Public Lisp API (consumed by the user, by other modules, by tests):**

```
;; Discovery
(org-graph/eager-discover)                       ;; one-shot scan
(org-graph/watched-roots)                        ;; -> list of dirs

;; Finders (interactive)
(org-graph/find-topic)
(org-graph/find-debug)
(org-graph/find-log)
(org-graph/find-reference)
(org-graph/find-project)
(org-graph/find-any)
(org-graph/find-agent-drafts)

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

**Vulpea integration:** `org-graph-extractor` registers via
`make-vulpea-extractor` with a `typed_edges` schema, foreign-keyed to
`notes(id)` with `:on-delete :cascade`. The extractor is registered
during module load after vulpea's DB is initialized.

**org-node integration:** Finders use `org-node-find` with a custom
candidate filter. Eager discovery uses
`(directory-files-recursively ROOT "\\.org\\'")` followed by
`org-id-update-id-locations`.

**gptel tool surface:** Tools registered through the existing
`gptel-make-tool` mechanism, namespaced `org-graph-*`. Read tools
return plists; the write tool returns the new note's ID and path.

## Boundaries

**In scope for the spike:**
- The seven sub-modules above.
- The PROPERTIES-drawer convention for typed edges (initial relation
  set: implements / contradicts / supersedes / relates-to).
- Filetag taxonomy (log / debug / topic / reference / project /
  agent-draft).
- Coexistence with org-roam in the same vault.
- Eager-scan helper for `~/work` so pre-existing project notes are
  reachable on first launch.

**Out of scope for the spike (deferred to follow-up changes):**
- Migrating any content out of `~/org/roam/`.
- Retiring or modifying the existing org-roam configuration.
- A graph visualization UI (org-supertag's React-Flow board, vulpea-ui
  dashboards). Findings inform whether to add one.
- Bidirectional typed-edge inference (declaring `IMPLEMENTS` on one
  side and auto-creating `IMPLEMENTED_BY` on the other).
- Promotion workflow for `:agent-draft:` notes (review → demote tag).
- Capture templates for any of the new note types.
- Org-roam-to-org-graph sync of legacy backlinks.

## Testing Approach

### Test Framework

**Buttercup** (BDD-style, codebase preferred for new tests). Tests
live in `*-spec.el` files; ERT is not introduced for any new spike
code.

### Test Organization

```
config/org-graph/test/
├── helpers-spec.el                  ; shared fixtures, AST builders
├── extractor/
│   └── parse-typed-edges-spec.el    ; pure-parser unit tests
├── finders/
│   └── filetag-filter-spec.el       ; finder filtering behavior
├── query/
│   └── typed-edges-spec.el          ; query API behavior, vulpea mocked
├── coordinator/
│   └── lock-semantics-spec.el       ; write-coordinator lock tests
└── integration/
    └── module-load-spec.el          ; module loads cleanly, defcustoms set
```

### Naming Conventions

- File: `<concern>-spec.el` (matches existing codebase).
- Suite: `(describe "org-graph-<sub-module>" ...)`.
- Test: `(it "<expected behavior>" ...)`. Tests phrased as observable
  behavior, not implementation.

### Running Tests

```
./bin/run-tests.sh -d config/org-graph                        # All
./bin/run-tests.sh -d config/org-graph/test/extractor          # Subset
./bin/run-tests.sh -d config/org-graph -f buttercup            # Explicit
make test-buttercup-directory DIR=config/org-graph             # Make
make test-report DIR=config/org-graph                          # Snapshot
```

Interactive: `C-c t` (existing transient menu).

### Test Patterns

**Mocking strategy: API-boundary mocks via `cl-letf`.** Tests do not
spin up a real vulpea SQLite DB. Functions on the vulpea API surface
are stubbed inside each spec:

```elisp
(cl-letf (((symbol-function 'vulpea-db-query)
           (lambda (predicates) <fixture rows>)))
  ...)
```

This matches the codebase's behavioral-test convention (function-scoped
mocks, no global state).

**Pure-parser tests** (`extractor/parse-typed-edges-spec.el`): construct
synthetic `org-element` trees in-memory via the test helper
`org-graph-test/build-tree`; assert the parser returns the expected
`(from-id rel-type to-id)` tuples. No file I/O, no vulpea, no org-mode
state.

**Finder tests** mock `org-node-find` and the candidate-source
function; assert the candidate predicate filters the right set.

**Coordinator tests** are sequential. They verify:
- A function executed under `with-file-lock` runs to completion.
- A second `with-file-lock` on the same path queues until the first
  releases (simulated by inspecting the in-process lock table, not by
  spawning real timers).
- An error inside the body releases the lock.
- Locks on distinct paths are independent.

No real concurrent timer-driven writes — that's deferred to the
follow-up change if findings justify it.

**Shared helpers** (`helpers-spec.el`):
- `org-graph-test/build-tree` — build an `org-element` AST with
  PROPERTIES drawers and filetags from a plist spec.
- `org-graph-test/with-stubbed-vulpea` — macro wrapping common
  `cl-letf` stubs for `vulpea-db-query`, `vulpea-db-insert`,
  `vulpea-db-get-by-id`.
- `org-graph-test/note-fixture` — construct a `vulpea-note`-shaped
  plist for query tests.

### Scenario Mapping

This is **spike-grade** coverage: every requirement gets at least one
test, but not every scenario. Priorities:

| Spec Requirement                  | Test Coverage |
|-----------------------------------|---------------|
| Distributed Note Discovery        | One eager-scan test against a temp dir tree (real `org-id-update-id-locations`); auto-watch and reindex are smoke-tested only via module-load. The 5-second / external-change scenarios are validated manually during the spike. |
| Typed Semantic Edges              | Full coverage of the pure parser: single property, multi-valued, multiple property types, malformed input, empty drawer. Query API: outgoing, incoming, connected — each tested with stubbed `vulpea-db-query`. The "project-local excluded" scenario is tested by asserting the extractor is only registered for the typed-graph root. |
| Note-Type Taxonomy and Finders    | One test per finder asserting candidate-filter behavior; one negative test for an untagged note. |
| Agent-Facing Graph Tools          | Tool-registration smoke test (gptel registry contains the three tools); write-tool stamps `:agent-draft:`; concurrent-write scenario covered by coordinator tests below. |
| Coexistence with org-roam         | Module-load test asserts org-roam variables and functions remain bound; a manual check during the spike covers org-roam UX continuity. |
| Non-Blocking Synchronization      | **Not automatically tested.** Latency budget validated manually during spike usage; if the spike promotes to a permanent module the follow-up change adds a benchmark spec. |
| Coordinator (cross-cutting)       | Lock acquired/released, queued call waits, error releases lock, distinct paths independent. |

Manually validated during spike (not in the test suite): file-watcher
latency, external-change detection, save-latency budget, real concurrent
agent-tool writes against the live coordinator.

## Dependencies

**New emacs packages (via straight.el):**
- `org-node` (meedstrom/org-node) — distributed discovery + navigator.
- `vulpea` v2 (d12frosted/vulpea) — typed-edge index host.
- `vulpea-journal` (d12frosted/vulpea-journal) — daily-log slot.
- Likely transitive: `org-mem` (pulled by org-node).

**Existing dependencies the module relies on:**
- `org-id` (built-in) — ID-based discovery.
- `org-element` (built-in) — AST for the pure parser.
- `gptel` — tool registration. Module load order: org-graph after gptel.
- `straight.el` — package management (existing infrastructure).

**System dependencies:**
- `fswatch` — required by vulpea v2 for external-change detection.
  Already available on macOS via `brew install fswatch`. The module
  surfaces a startup check that warns if `fswatch` is missing.

## Constraints

- **Org-roam coexistence:** No org-roam variable, advice, or schema
  modification is permitted. The vulpea DB lives at
  `runtime/state/vulpea/notes.db`, distinct from any org-roam path.
- **Performance:** Save latency under watched roots SHALL stay under
  50ms in the foreground. The spike does not enforce this in CI; manual
  measurement during use is the gate. If save latency regresses, the
  watched-root list is the first thing to narrow.
- **Filesystem-watcher load:** Watching `~/work` recursively on a
  machine with many checked-out repos may exceed inotify/fsevents
  limits. The module exposes `org-graph-watched-roots` as a defcustom
  so the user can tune it; default is `'("~/org/roam/" "~/work/")` but
  the user may shrink to just `"~/org/roam/"` if watcher load is
  excessive.
- **AI agent writes:** All gptel tool writes producing graph files MUST
  go through `org-graph-coordinator/with-file-lock`. Any tool that
  writes outside the coordinator is a bug; the spike includes a
  module-load assertion that the registered write tool wraps the
  coordinator.
- **Scope of the typed-edge index:** The extractor only runs on notes
  whose path is under `org-graph-typed-graph-root` (default
  `~/org/roam/`). Project-local notes participate in discovery and
  navigation but not in typed-edge analysis. This is a hard boundary
  for the spike.
- **Spike duration:** This change is scoped for ~1 week of evaluation.
  If at the end the user wants to keep it, a follow-up `org-graph-v1`
  change captures the production hardening (latency benchmarks, full
  scenario coverage, capture templates, promotion workflow,
  documentation).
