## Open-Vocabulary Typed Edges (2026-07-17)

Hands-on use during the spike-eval window established that the **closed
relation set is the wrong shape**: a fixed list forces authors to overload
`relates-to` as a junk drawer (the exact worry raised in Open Question 4)
and blocks domain-specific precision (`benchmarks`, `deprecates`,
`falsifies`, `refines`) that only surfaces in real use. This section
pivots typed edges to an **open vocabulary with two authoring surfaces and
an optional in-vault registry**. **Where it conflicts with D3, D4, RE-4,
the "auto-derived symmetry" Non-Goal, or Open Question 4, this section
wins.** Those are preserved below unedited for provenance.

Because there are effectively **no real typed edges in the vault yet**,
this is a **clean break** — there is no backward-compatibility obligation
to the bare `:IMPLEMENTS:`-style keys or the closed `org-graph-relation-types`
list, and no data-migration task.

**User-confirmed direction:** folksonomy-with-optional-registry; support
*both* drawer properties and inline links; drop the closed set.

- **OV-1 — Open relation vocabulary (supersedes D3's closed set; resolves
  OQ4).** There is no allowlist. A relation type is any author-coined
  symbol and is extracted the moment it appears. `org-graph-relation-types`
  stops being a *gate* and is redefined as a non-authoritative **seed
  suggestion list for completion only**. Coining a new type requires zero
  ceremony (OV-2/OV-3); graduating one into a curated taxonomy is optional
  (OV-6).

- **OV-2 — `REL_` namespace is the edge discriminator (splits the two jobs
  D3's list conflated).** The old closed list did double duty: it was both
  *the vocabulary* and *the signal that a property is an edge at all*
  (`--key->rel` returned `nil` for non-members, and the parser dropped
  them). With an open vocabulary, membership can no longer be the
  discriminator. **A drawer property is a typed edge iff its key is
  `REL_<TYPE>`**; the relation is `<TYPE>` lowercased with `_`→`-`
  (`:REL_FALSIFIES:` → `falsifies`, `:REL_RELATES_TO:` → `relates-to`).
  Ordinary properties — including ones whose value happens to hold an `id:`
  link (`:SOURCE:`, `:CATEGORY:`) — are never edges. `--rel-key` /
  `--key->rel` become prefix add/strip, not list lookups. The delimiter is
  `_` (not the `/` sketched in exploration): it matches org property-key
  convention and the existing hyphen→underscore transform, and keeps the
  key a single completion token.

- **OV-3 — Typed inline links as a second authoring surface (support-both).**
  Register a custom `rel:` org link type with path syntax
  `rel:<type>:<target-id>`, e.g. `[[rel:falsifies:<uuid>][the earlier
  claim]]`. This lets an edge be asserted **in prose, where the claim is
  actually made**, carrying its surrounding context, without a drawer
  entry. `org-link-set-parameters` supplies the machinery for free:
  `:follow` (jump to the target via org-id), `:complete` (prompt for type
  then target-node, seeded from the vocabulary), `:face` (visually mark
  rel-links so they don't read as ordinary `id:` links), and `:export`.
  Both surfaces feed the **same** `(from-id rel-type to-id)` tuple stream
  and the same `typed_edges` table — the storage model does not change.

- **OV-4 — Inline-link edges attribute to the enclosing node.** A drawer
  edge's `from-id` is the drawer's own `:ID:`. An inline `rel:` link has no
  such anchor, so its `from-id` is the **nearest ancestor carrying an
  `:ID:`** (the enclosing heading, else the file-level node), resolved via
  `org-element-lineage` / property inheritance. This is the one genuinely
  new piece of parsing complexity and the bulk of the new test surface:
  link at file top, link under a subheading that has its own `:ID:`, link
  under a heading that has none (walks further up), link with no ID-bearing
  ancestor at all (dropped).

- **OV-5 — Two pure scanners, one stream (refines D4).** D4's
  pure-function boundary stands and generalizes. The extractor holds two
  pure functions over an `org-element` AST —
  `org-graph-extractor/parse-typed-edges` (drawer, `REL_`-prefix based) and
  `org-graph-extractor/parse-rel-links` (inline, enclosing-node resolved) —
  each emitting `(from-id rel-type to-id)` tuples. The vulpea extractor
  wrapper runs both and unions the rows. The `typed_edges` schema is
  unchanged because `rel-type` was already stored as an open symbol.

- **OV-6 — Optional in-vault registry of edge types (folksonomy →
  taxonomy).** A relation type works with **zero registration**. A note
  tagged `:edge-type:` MAY *graduate* a type by declaring its metadata: a
  human `LABEL`, `:INVERSE:` (a symbol), `:SYMMETRIC:` (boolean), and a
  free-text description. The registry is **ordinary vault data** — coined
  and edited at any time, git-versioned, discoverable via a dedicated
  finder, and indexed like any other note. It is loaded to (a) seed
  completion for both surfaces, (b) render inverse labels, and (c) mark
  symmetric types. **Unregistered types remain fully functional** and
  render as their raw symbol; the registry only enriches. Four seed
  registry notes (`implements` / `contradicts` / `supersedes` /
  `relates-to`, each with a sensible `:INVERSE:`) ship as starter data,
  replacing the hardcoded closed list.

- **OV-7 — Inverse/symmetry derived at query/display time, never
  materialized (narrows the "auto-derived symmetry" Non-Goal and RE-4).**
  The system still SHALL NOT write inverse rows — `typed_edges` stays
  **canonical and directional**, so there is no double-write divergence. A
  registered `:INVERSE:` lets the *display* layer render "X is falsified-by
  Y" from a single stored `falsifies` edge, and `:SYMMETRIC: t` lets a read
  query surface the relation in both directions. This is a read/render
  concern only; storage is untouched.

**Task impact:**
- `parse-typed-edges` / `vulpea-extractor-plugin` (both closed) get a
  **follow-up**: replace the allowlist with the `REL_` prefix, add the
  `parse-rel-links` scanner with enclosing-node resolution, and union both
  in the extractor wrapper.
- **New task — `rel:` link type:** register via `org-link-set-parameters`
  with `:follow` / `:complete` / `:face` / `:export`.
- **New task — edge-type registry:** an `:edge-type:` predicate + a loader
  that reads registry metadata, a `org-graph/find-edge-type` finder, and
  the four seed registry notes.
- `typed-edge-query` gets a **follow-up**: optional symmetric-aware reads
  and inverse-label resolution from the registry.
- **Docs:** the spike-eval runbook's typed-edge checks move to the new
  surfaces.
- **No migration task** — near-zero existing edges (user-confirmed).

**New open questions:**
- **OV-Q1 —** `rel:` link path shape: bare `rel:<type>:<id>` (chosen for
  the spike — simplest to parse and complete) vs. a form that lets the
  target carry a non-`id:` link kind. Revisit only if targets need
  file/http kinds.
- **OV-Q2 —** Should completion offer only registered + already-observed
  types, or also free-text coinage? Spike default: **both** — offer
  registered + observed as candidates, allow free text to coin a new type
  on the spot.
- **OV-Q3 —** Cheapest source for the "observed types" completion set:
  `SELECT DISTINCT rel-type FROM typed_edges` (cached) vs. scanning
  registry notes only. Tentative: the distinct query, cached per session.

---

## Re-evaluation (2026-06-27)

The branch sat cold from late April while ~500 commits landed on `main`.
Two of those threads change the load-bearing assumptions below; this
section records the deltas. **Where a decision here conflicts with the
original D1–D8, this section wins.** The original decisions are preserved
unedited for provenance.

**What changed on `main`:**

- The **`workspaces` package** now ships project-co-located structure as a
  first-class concept (`:home` git repo + scaffolded `home.org` +
  `sessions/`), a published integration registry
  (`workspace-register-integration`: `:on-create` / `:on-purge` / `:menu`),
  per-workspace directory-scoped gptel sessions (`GPTEL_WORK_ROOT` =
  `:home`), and a `workspace-assistant` preset with a deliberately **empty
  `:tools` slot**.
- **vulpea 2.4** added a schema system (`vulpea-schema-define` /
  `-validate`), a native link/graph query API (`vulpea-db-query-links` with
  link `:type`, `dead-links`, `orphan-notes`, `isolated-notes`,
  `backlink-counts`), a **parser epoch** invalidating the file cache when
  extractors change, automatic DB rebuild on schema bumps, and
  async-by-default sync + `vulpea-doctor`.

**Decision deltas (user-confirmed direction: re-center as a workspace
layer; use vulpea-schema for taxonomy):**

- **RE-1 — Re-center on the substrate (revises Context friction #2 and the
  co-location goal).** Project co-location is now delivered by `workspaces`.
  org-graph stops re-deriving "discover notes co-located with repos" and
  instead treats workspaces as substrate: index the workspace `:home`
  directories the registry already enumerates (plus their `sessions/`),
  and register through the integration registry. The co-location pillar
  shrinks from "headline goal" to "consume what workspaces provides."

- **RE-2 — Discovery engine: vulpea-only (RESOLVED 2026-06-27; supersedes
  D1's "layer org-node + vulpea" and the org-node dependency).** org-node
  is **dropped**. vulpea is the single index/discovery engine, fed
  *explicit, bounded roots* from the workspace registry (each active
  workspace `:home`, plus `~/org/roam/`) rather than a directory-less or
  blind-`~/work`-walk scan.

  Source investigation established that org-node's headline advantage —
  keeping the global `org-id-locations` populated so `id:` links resolve in
  arbitrary buffers — is **already covered by vulpea**: `vulpea-db-update-file`
  calls `org-id-add-location` for every indexed ID (`vulpea-db-extract.el:1015`),
  as do `vulpea-create` / `-insert` / rename. Running org-node alongside
  vulpea would mean two scanners, two save-hook re-index paths, and two
  caches of the same files — redundant cost, no benefit, since we *want*
  explicit registry roots, not directory-less discovery. Registry-driven
  roots also retire the original watch-load risk: we watch only the handful
  of workspace homes the user has actually created, never `~/work`
  wholesale.

  Two small shims close the only real gaps vulpea leaves:
  - **`org-id-locations` startup seed.** vulpea registers IDs *lazily* (per
    file touch this session); it has no bulk-seed from its DB. On a fresh
    Emacs an indexed note is link-resolvable only if org-id persisted it to
    `org-id-locations-file` or its file is re-touched. → org-graph adds a
    ~10-line startup function that seeds `org-id-locations` from
    `(vulpea-db-query)`.
  - **New-workspace watch registration.** Adding a directory after
    `vulpea-db-autosync-mode` is enabled does not auto-install a filenotify
    watcher for it. → org-graph's workspace `:on-create` integration handler
    (RE-5) appends the new `:home` to `vulpea-db-sync-directories` and runs
    `vulpea-db-sync-update-directory`.

  Consequence: `org-node` (and its transitive `org-mem`) leaves the package
  set entirely. `vulpea-journal` remains provisional (Open Question 5).

- **RE-2a — Auto-assign IDs to workspace `home.org` and session files
  (RESOLVED 2026-06-27).** vulpea only indexes notes carrying an `:ID:`
  (`vulpea-db-extract.el:475`); workspace `home.org` and `sessions/*.org`
  are scaffolded without IDs, so they would otherwise be invisible to the
  index. The workspace **scaffold** (`config/workspaces/scaffold.org`) and
  **session creation** (`jf/gptel--create-session-core`) are extended
  *additively* to `org-id-get-create` the file at birth, so workspace homes
  and sessions become indexed nodes and valid `id:` link targets. This is
  the one place org-graph reaches into the `workspaces` / gptel-sessions
  code; it is purely additive (a file that already has an ID is untouched)
  and is captured as its own task.

- **RE-3 — Taxonomy via `vulpea-schema`, not bare filetags (revises the
  taxonomy goal and architecture's `org-graph-finders`/filetag design).**
  Note types are `vulpea-schema-define` definitions with field expectations
  and validation; the filetag becomes one validated field. Finders become
  schema-aware queries. The hand-rolled filetag-predicate approach in
  architecture.md is superseded.

- **RE-4 — Typed-edge extractor builds on vulpea 2.4 infra (refines D3,
  D4).** The PROPERTIES-drawer convention and pure-function parser (D3, D4)
  stand — vulpea's native link `:type` is link-*kind*, not semantic
  relation-*kind*, so the semantic extractor is still net-new. But it now
  rides on the v2.4 plugin + parser-epoch + schema-rebuild machinery, so
  the extractor wrapper should register via the current plugin API and rely
  on parser-epoch for cache invalidation rather than manual resync.

- **RE-5 — Agent surface plugs into workspaces (revises D7 and the
  gptel-tools architecture).** Graph query/write tools register into the
  `workspace-assistant` preset's empty `:tools` slot and via
  `workspace-register-integration`, instead of standing alone. Load order
  becomes: after **both** `gptel` and `workspaces`. The write-coordinator's
  natural context is per-workspace writes already scoped by
  `GPTEL_WORK_ROOT`.

- **RE-6 — Risk retired (revises the "vulpea v2 is recent" risk).** vulpea
  2.4's parser epoch, schema-version auto-rebuild, async-default sync, and
  `vulpea-doctor` largely retire the "API still evolving, pin commits as a
  hard requirement" risk. Pinning is now ordinary hygiene, not a
  spike-survival mechanism.

**Task impact:**
- `install-packages` — drop `org-node`/`org-mem` (RE-2); vulpea (+ optional
  `vulpea-journal`) only.
- `implement-discovery` — rewrite as registry-driven vulpea sync over
  workspace homes + `~/org/roam/`, plus the `org-id-locations` startup seed
  (RE-1/RE-2).
- `parse-typed-edges` / `vulpea-extractor-plugin` — target the current
  `make-vulpea-extractor` plugin API + parser-epoch (RE-4).
- `finders-and-filters` — schema-aware via `vulpea-schema` (RE-3).
- `gptel-tools` / `wire-into-init` — register the `org-graph` workspace
  integration and populate the `workspace-assistant` `:tools` slot; load
  after `gptel` and `workspaces` (RE-5).
- **New tasks:** (a) note-type **schema definitions** via
  `vulpea-schema-define` (RE-3); (b) **workspace-integration registration**
  including the `:on-create` watch-add handler (RE-2/RE-5); (c) **auto-ID
  on scaffold/session-create** (RE-2a).

The tasks under `tasks/open/` should be regenerated/adjusted before
`/opsx-apply`.

---

## Context

Earlier conversation findings established three concrete friction points
with the current org-roam-only setup:

1. **Single-bucket UX biases Zettel.** `org-roam-find` flattens daily
   logs, debug sessions, topic deep-dives, and project notes into one
   namespace.
2. **Single-directory discovery.** `org-roam-directory` doesn't
   accommodate notes co-located with project work
   (`~/work/<jira>/notes.org`) without either painful `include-fn`
   configuration or dumping everything into the central tree.
3. **No async sync, no semantic links, no agent-friendly API.**
   Save-time blocking, manual `org-roam-db-sync` after external writes,
   untyped link soup, and a SQL-schema-as-API mean AI-agent-driven
   workflows fight the tool.

Research surfaced three packages that solve these in different shapes:

- **org-node** is "just org-id" — distributed discovery, no directory
  setting, ~100× faster sync than org-roam at 3000-node scale.
- **vulpea v2** (Nov 2025 / Jan 2026 release) is a library — extractor
  plugins, materialized views, async file watchers
  (filenotify + fswatch), stable struct API.
- **org-supertag** ships out-of-box typed relations and a graph UI but
  is younger and a heavier abstraction.

The user explicitly wants to keep org-supertag as a reference/inspiration
rather than adopt it directly during the spike. They're already on
`org-graph-spike` branch off `main`, in the `~/emacs/` development tier.
This is a spike: validate the layered approach, learn what's actually
load-bearing, then commit to a long-term shape in a follow-up.

## Goals / Non-Goals

**Goals:**

- Layer org-node (discovery + navigator) and vulpea (typed-edge index +
  agent surface) on top of the existing org-roam install without
  touching org-roam.
- Validate the PROPERTIES-drawer convention for typed semantic edges
  (`IMPLEMENTS / CONTRADICTS / SUPERSEDES / RELATES_TO`) at real-world
  scale.
- Make project-co-located notes (under `~/work/<ticket>/`) reachable in
  the navigator without per-directory configuration.
- Give AI agents a stable, agent-friendly graph surface: typed-edge
  reads, structured-query reads, coordinator-mediated writes that don't
  corrupt files under concurrency.
- Establish a note-type taxonomy via filetags so finders can be
  type-scoped instead of one-bucket.

**Non-Goals:**

- Migrating notes out of `~/org/roam/` or modifying any existing
  org-roam state.
- Production-grade test coverage. Spike-grade only — see architecture
  Testing Approach.
- A graph visualization UI. Org-supertag stays a reference; if findings
  warrant a graph view, it's a follow-up change.
- Auto-derived relation symmetry (e.g. `IMPLEMENTS` → auto
  `IMPLEMENTED_BY`). Edges are explicitly authored.
- Promotion workflow for `:agent-draft:` notes. The tag exists; the
  curated review flow is deferred.
- Performance benchmarking in CI. Manual measurement during use only.

## Decisions

### D1 — Layer org-node + vulpea, do not pick one

**Decision:** Use both. org-node owns discovery and the navigator;
vulpea owns the typed-edge index and the agent-facing query/write
surface.

**Why:** Each tool wins on a different axis. org-node's `org-id`-based
discovery is the only clean answer to project-co-located notes. Vulpea's
plugin extractor system and stable `vulpea-note` struct are the only
clean answer to typed edges + agent integration. They use the same
on-disk format and run independent databases — no conflict.

**Alternatives considered:**

- *Vulpea alone with `vulpea-db-sync-directories` covering `~/work`* —
  Possible, but adds inotify/fsevents load on every project repo and
  pulls in noise (every `.org` file under any repo). Discovery cost
  scales with project count; org-node's approach scales with
  ID-bearing-file count.
- *org-node alone, layer typed edges on top with custom SQL* — Forces
  us to build the extractor plumbing vulpea already provides and
  reinvent migration handling. The whole point of vulpea v2 is "don't
  do this".
- *org-supertag end-to-end* — Solves typed relations cleanly but at the
  cost of taking on a younger codebase as primary. The user's stated
  preference is to keep org-supertag as a reference for now.
- *Stick with org-roam* — Doesn't address any of the three friction
  points.

### D2 — Typed-graph extraction is scoped to `~/org/roam/`, not all watched roots

**Decision:** `vulpea-db-sync-directories` is set to `'("~/org/roam/")`
only. Project-local notes are visible to org-node (navigator,
backlinks) but do not participate in the typed-edge index.

**Why:** Typed edges are a curation discipline that belongs on durable
concept notes, not on transient project notes. Indexing project notes
would dilute the typed-graph and balloon the inotify/fsevents load.
Operational notes (debug sessions, daily logs, project notes) link
*into* typed concept notes, not the other way around.

**Alternatives considered:**

- *Extract typed edges from project notes too* — Operational drag
  outweighs benefit for the spike; revisit if findings show project
  notes regularly carry curated relations.
- *Make this a runtime toggle per note* — Premature complexity; the
  filesystem-location boundary is a clean default.

### D3 — PROPERTIES-drawer convention for typed edges, not a custom block

**Decision:** Authors declare relations as PROPERTIES-drawer entries:

```org
:PROPERTIES:
:ID:           <uuid>
:IMPLEMENTS:   [[id:abc]]
:RELATES_TO:   [[id:def]] [[id:ghi]]
:END:
```

**Why:** Properties are first-class in the org-element AST, multi-valued
parsing is straightforward, the surface is invisible to the rendered
note (low cosmetic cost), and the data lives next to `:ID:` — the same
place the rest of the graph metadata lives. Vulpea's `properties` slot
exposes them already.

**Alternatives considered:**

- *Custom `#+begin_relations: ... #+end_relations` block* — More
  expressive (could capture per-edge metadata, evidence quotes), but
  requires a custom parser and shows up as visible content. Defer to a
  follow-up if per-edge metadata becomes needed.
- *Inline link description hack `[[id:abc][implements:foo]]`* —
  Rejected. Data lives in display strings, queries can't filter on it
  reliably.
- *Org-supertag's relation-type registry* — Rejected for primary use
  per user preference; the property convention can be migrated to it
  later if we adopt it.

The relation-type set is configurable; the initial closed set
(`implements`, `contradicts`, `supersedes`, `relates-to`) is small on
purpose. Adding domain-specific types ("benchmarks", "deprecates") is
an `add-rel-type` follow-up, not part of the spike.

### D4 — Pure-function parser, separable from vulpea's plugin runtime

**Decision:** The typed-edge parser is a pure function:

```elisp
(org-graph-extractor/parse-typed-edges ELEMENT-TREE NOTE-ID)
;; -> ((from-id rel-type to-id) ...)
```

The vulpea extractor wrapper calls it, hands the tuples to vulpea's DB
write path. Tests exercise the pure function with synthetic
`org-element` trees; vulpea is mocked at its DB API.

**Why:** Matches the codebase's behavioral-test convention
(function-scoped mocks via `cl-letf`). Lets us iterate on parsing
behavior without DB setup. If vulpea's plugin runtime API evolves, only
the wrapper changes.

### D5 — Per-file write coordinator, dynamic-let lock table

**Decision:** A module-private hash table maps absolute file paths to
mutex-like state. `org-graph-coordinator/with-file-lock` is a macro
that:

1. Canonicalizes PATH via `expand-file-name` + `file-truename`.
2. If unlocked: marks locked, runs BODY, unlocks in `unwind-protect`.
3. If locked: enters a busy-wait via `accept-process-output` with a
   short timeout, polling the lock state. Times out at a configurable
   ceiling (default 5s) raising a structured error.

Distinct paths are independent. Errors release the lock.

> **Amended cycle-1782551613 (user disposition of ask-cycle-1782551613-2,
> architect finding arch-cycle-1782551613-03):** the `:scope 'graph-only`
> keyword and the "no-op for paths under no watched root" behaviour are
> **dropped**. `with-file-lock` is **unconditional** — it always locks the
> canonicalised path. The keyword was speculative with no caller; a
> non-graph bypass can be added when an actual caller
> (gptel-tools / workspace-integration) needs one. The as-built contract is
> captured in `interfaces.org` → `register/invariant/coordinator-lock-contract`.

**Why:** Emacs is single-threaded for elisp; "concurrency" here means
overlapping callbacks driven by gptel tool dispatch and timers, not
real threads. A cooperative lock with `accept-process-output` is the
idiomatic pattern (used elsewhere in the codebase for filesystem-scope
serialization). No third-party concurrency lib needed.

**Alternatives considered:**

- *OS-level `flock` via `lockfile-1.5`* — Heavier, harder to test,
  unnecessary for in-process serialization.
- *Queue all agent writes through a single async pipeline* — Cleaner
  for very high concurrency, but overengineered for current scale (a
  handful of agent calls per minute).

### D6 — Eager discovery as a manual command, not a startup hook

**Decision:** `org-graph/eager-discover` is interactive and idempotent.
Not auto-run on Emacs startup.

**Why:** First-time scan of `~/work` may walk thousands of repos; we
don't want that on every Emacs launch. After the one-shot, day-to-day
new files get picked up by visit-on-open (`org-id-locations` updates
via `org-id-find` and friends) and by vulpea's file watcher for the
typed-graph root.

**Open question:** Whether to run a *cheap* sync on startup (just
`org-id-locations` reload from cache, no walk) — almost certainly yes,
but verify the cache hit semantics. Captured below.

### D7 — Module load order: after gptel, before nothing

**Decision:** Add `"org-graph"` to `jf/enabled-modules` immediately
after `"gptel"`. No downstream module loads after it during the spike.

**Why:** The agent-tool registration in `org-graph-tools` needs gptel
loaded. Nothing else in the codebase consumes org-graph yet.

### D8 — Coexistence story: independent DBs, shared on-disk format

**Decision:** Vulpea DB at `runtime/state/vulpea/notes.db`. org-roam
keeps its existing DB path. org-id uses its existing locations cache.
No shared state.

**Why:** Resilience. Wiping or rebuilding any one of the three never
affects the others. If the spike is abandoned, deleting
`config/org-graph/` and `runtime/state/vulpea/` is the entire rollback.

## Risks / Trade-offs

- **inotify/fsevents limits when watching `~/work`.** → Default the
  vulpea watch to `~/org/roam/` only (D2). org-node's discovery is
  visit-driven, not watch-driven, so its load is bounded. Eager scan is
  one-shot. If the user expands typed-graph scope later, watch load
  becomes a real concern; document the `org-graph-watched-roots`
  defcustom prominently.

- **Two indices, double-write divergence.** Vulpea and org-node both
  derive metadata from the same files but maintain separate caches. A
  pathological state could exist where a note is in one but not the
  other. → Mitigation: org-node uses `org-id-locations` directly (no
  cache to diverge from filesystem); vulpea's async watcher reconverges
  within seconds; module exposes `org-graph/full-resync` as the
  break-glass. Manual check during spike: assert finder count and
  vulpea node count match for `~/org/roam/`.

- **Vulpea v2 is recent (Jan 2026).** Bugs and API changes likely. →
  Pin the straight recipe to a specific commit, not `master`. Treat
  `vulpea-db-query` as the only stable API surface. If a vulpea bug
  blocks the spike, the pure-parser design (D4) lets us swap the
  storage backend without losing the extraction logic.

- **fswatch as a runtime dep.** macOS-only consideration; the user's
  primary machines are darwin (per CLAUDE.md machine roles). → Module
  load checks for fswatch and warns if missing; the spike does not
  attempt Linux compatibility work.

- **Org-roam users may share the same files.** If the user moves a note
  out of `~/org/roam/` mid-spike, org-roam removes it from its index
  but vulpea may still hold typed edges referencing it. → Acceptable
  for a spike; full-resync drops orphaned edges via the cascade
  foreign-key (architecture). Document as a known sharp edge.

- **Agent-write coordinator scope creep.** Agents may want to write
  files outside `~/org/roam/` (e.g. project-local notes under
  `~/work/`). The coordinator is location-agnostic — that's fine —
  but the typed-edge extractor won't run on those writes (D2). →
  Document this in the gptel tool description so the agent prompts
  reflect the boundary.

- **Spike abandonment cost.** If we kill the spike, has it cost
  anything beyond time? → Almost nothing. New PROPERTIES on existing
  notes are inert without the extractor. Filetags are universally
  understood by org. Removing the module is `git checkout main` plus
  deleting `runtime/state/vulpea/`.

- **Test coverage is spike-grade.** A real bug in the file watcher or
  save-latency budget can ship undetected. → Acceptable. Manual usage
  is the gate; the follow-up production-grade change adds the missing
  coverage if the spike survives.

## Migration Plan

This is additive-only — no data migration, no rollback artifact beyond
removing the module.

**Deploy:**

1. Install the three packages via straight.
2. Tangle and load `config/org-graph/`. Module-load test passes.
3. Run `org-graph/eager-discover` once to populate `org-id-locations`
   for `~/org/roam/` and `~/work/`.
4. Trigger initial vulpea sync (`vulpea-db-sync`) — async, completes
   in background.
5. Spot-check: `org-graph/find-topic` lists topic-tagged notes;
   `org-graph-query/outgoing` returns rows for a note with an
   `:IMPLEMENTS:` property.
6. Add 3–5 typed edges to existing concept notes. Verify queries and
   incoming-edge resolution.
7. Register agent tools with gptel; smoke-test a write through the
   coordinator.

**Rollback (if abandoned):**

1. Remove `"org-graph"` from `jf/enabled-modules` in `init.org`.
2. Delete `config/org-graph/`.
3. Delete `runtime/state/vulpea/`.
4. Existing notes retain any `:IMPLEMENTS:`-style PROPERTIES; they're
   inert without the extractor. Removing them is optional cleanup.

## Open Questions

1. **Should `org-graph/eager-discover` run a cheap startup variant?**
   Reloading `org-id-locations` from its on-disk cache costs ~ms and
   gives us "just visited" semantics without a full walk. Tentative
   yes; verify during implementation that the cache reload doesn't
   trigger `directory-files-recursively`.

2. **What's the right default for `org-graph-watched-roots` on
   apploi-mac vs personal-mac?** The two machines have very different
   `~/work` shapes. May need machine-role-specific overrides in
   `config/local/<role>.el`. Defer to during-spike usage.

3. **Should `:agent-draft:` finder exclusion be a defcustom or
   hard-coded?** Tentative defcustom (`org-graph-exclude-drafts-from`
   defaulting to `'(topic reference)`); revisit after one week of use.

4. **Is `RELATES_TO` too broad?** It absorbs everything that doesn't
   fit the other three. May need to either (a) drop it and force
   precision, or (b) add domain-specific types as the user discovers
   patterns. Decide after two weeks of use, not pre-emptively.

5. **vulpea-journal: drop in or skip?** The user has an existing
   org-roam dailies setup. Adding `vulpea-journal` may fragment daily
   logging or complement it. Default for the spike: **install but
   don't bind keys**, evaluate alongside.
