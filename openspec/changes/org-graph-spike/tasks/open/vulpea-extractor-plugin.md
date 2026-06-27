---
name: vulpea-extractor-plugin
description: Wrap the pure parser as a vulpea extractor that registers a typed_edges table and stores edge tuples, scoped to the roam vault.
change: org-graph-spike
status: ready
relations:
  - blocked-by:parse-typed-edges
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/vocabulary/relation-types
  - register/invariant/typed-edge-extraction-scope
  - register/boundary/parser-extractor-db
---

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/org-graph.org`
  (Extractor section) — the vulpea-registration part
- `config/org-graph/test/extractor-spec.el` (new)

## Implementation steps
1. Register an extractor via `make-vulpea-extractor` + `vulpea-db-register-extractor`:
   ```elisp
   (vulpea-db-register-extractor
    (make-vulpea-extractor
     :name 'org-graph-typed-edges
     :version 1
     :priority 50
     :schema '((typed_edges
                [(from-id :not-null) (rel-type :not-null) (to-id :not-null)]
                (:foreign-key [from-id] :references notes [id] :on-delete :cascade)))
     :extract-fn #'org-graph-extractor/extract))
   ```
2. `org-graph-extractor/extract (ctx note-data)`: obtain the AST via
   `vulpea-parse-ctx-ast` from `ctx`, call
   `org-graph-extractor/parse-typed-edges` with the note's id, and insert the
   resulting tuples into the `typed_edges` table via `emacsql`. Return
   `note-data` (possibly unchanged) per the extractor contract.
3. SCOPE the extraction to the roam vault: only emit edges when the note's
   path is under `org-graph-roam-root`. Project/session notes participate in
   discovery and navigation but NOT in the typed-edge index (D2/RE-4). Decide
   the scope check inside `extract` (cheap path prefix test).
4. Parser-epoch discipline: document that changing the parser's output for
   the same input requires bumping `vulpea-db-parser-epoch` so vulpea clears
   the file cache and re-extracts. Add a comment near the extractor noting
   this.
5. Write `extractor-spec.el`: stub `vulpea-parse-ctx-ast` and the emacsql
   insert (via the helper), assert that for a roam-path note the extractor
   inserts the expected tuples, and for a non-roam-path note it inserts
   nothing.

## Design rationale
RE-4: the semantic typed-edge extractor is net-new but now rides vulpea 2.4's
mature plugin API (`make-vulpea-extractor`) and parser-epoch cache
invalidation instead of bleeding-edge internals. Scoping to roam keeps the
typed graph a curation discipline on durable concept notes (D2) and bounds
write/inotify load.

## Design pattern
Extractor struct + registration from vulpea's plugin guide; `:on-delete
:cascade` on the notes FK so deleting a note drops its edges. Priority 50
(after core extractors 0-9). Keep the wrapper thin — all parsing logic lives
in the pure function from `parse-typed-edges`.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — extractor spec passes
  (roam note → tuples inserted; non-roam note → none).
- Manual: add an `:IMPLEMENTS:` property to a roam note, trigger a sync, and
  confirm the `typed_edges` table has the row (queryable via the next task).
- Caveat to check empirically (per research): confirm a `parser-epoch` bump
  actually re-runs this extractor to repopulate `typed_edges`.

## Context
design.md § Decisions D2, D4; design.md § Re-evaluation (RE-4);
architecture.md § Interfaces (Vulpea integration).

## Orchestrator brief addenda (cycle-1782551613)

These come from the foundation Architect audit. Cited register entries are in
`interfaces.org`; read them — they are reference material to pressure-test,
not authority to defer to. Speculated entries carry explicit licence to push
back.

- **architecture.md is STALE except for the Vulpea-integration section you
  cite.** Finding `arch-cycle-1782551613-01`: only the `make-vulpea-extractor`
  + `typed_edges` + `notes(id)` FK `:on-delete :cascade` claim is still valid
  (and matches step 1). Ignore every org-node / filetag / discovery reference
  elsewhere in that doc.

- **Single-source the relation vocabulary (folds in blocking finding
  `arch-cycle-1782551613-02`).** The rel-type column you write MUST draw from
  `register/vocabulary/relation-types` — do NOT re-list the four symbols.
  Acceptance criterion (gate): add a buttercup spec that requires BOTH
  `extractor.el` and `org-graph.el` and asserts
  `(equal org-graph-extractor--default-relation-types org-graph-relation-types)`,
  so the loader defcustom and the extractor fallback defconst can never
  silently diverge. (D4's standalone-load fallback is legitimate; the guard,
  not deletion, is the fix.)

- **The scope gate is a load-bearing invariant (folds in finding
  `arch-cycle-1782551613-04`).** `register/invariant/typed-edge-extraction-scope`:
  step 3's roam-root check is the contract, not an optimisation. Step 5's
  exclusion test (roam-path → tuples, non-roam-path → none) is the acceptance
  gate for that invariant.

- **String-vs-symbol storage decision.** Decide up front whether the
  `typed_edges.rel-type` column stores the relation as a symbol or a string,
  and record it: `register/shape/typed-edge-tuple` says the in-memory tuple
  carries a **symbol**. The next task (typed-edge-query) will match on whatever
  you store, so make the storage/readback round-trip explicit and tested.

## Observations

- The vulpea extractor pipeline (`vulpea-db--insert-note-from-plist`,
  vulpea-db-extract.el:1038-1070) inserts the note row FIRST, then runs
  `vulpea-db--run-extractors`, so the `typed_edges` FK onto `notes(id)` is
  always satisfiable at insert time. The `:extract-fn` itself performs the
  side-table write (the contract's "return note-data" is only for plist
  enrichment); typed edges are a side table, so `extract` returns NOTE-DATA
  unchanged and writes via `(emacsql (vulpea-db) ...)`.
- emacsql serialises scalars with `prin1-to-string` and reads them back with
  `read` (emacsql-compiler.el:112-118), so a stored *symbol* round-trips to a
  symbol and a stored *string* to a string — they are distinct, recoverable
  values. Storing `rel-type` as the symbol is therefore sound and keeps the
  parser→extractor→DB→query contract on one representation.
- Auto-registration is NOT done at top-level (a bare `require` of extractor.el
  would call `vulpea-db-register-extractor`, which applies the schema and opens
  the DB — breaking standalone unit tests and violating the "no real DB" rule).
  Registration is exposed as `org-graph-extractor-register`, to be called by the
  loader (wire-into-init task). This is the correct home for load-wiring per the
  task's own structural rule.
- Per-note vs per-file scope: vulpea runs extractors for every indexed note
  (file-level + each heading). `extract` parses the whole-file AST keyed by the
  current note's id, so a heading-level note under the roam root would attribute
  every file-level drawer relation to the heading id. For the spike this is
  benign — concept notes are flat, file-level notes by the D2/D3 convention — but
  if heading-level concept notes ever carry relation drawers this needs a
  subtree-scoped parse. Flagged, not blocked (no follow-up task filed: out of
  the spike's stated convention).

## Discoveries

- discovery:
    class: register-confirmation
    affected_register_entry: register/invariant/typed-edge-extraction-scope
    status_recommendation: speculated -> confirmed
    detail: |
      The roam-root scope gate is implemented as a cheap path-prefix test in
      `org-graph-extractor--roam-note-p` and enforced inside
      `org-graph-extractor/extract`. The acceptance specs (roam-path -> tuples,
      non-roam-path -> none, roam subdirectory -> in scope, unbound root ->
      fails closed) all pass. The invariant's `enforcement_mechanism` note
      ("currently a gap, finding arch-cycle-1782551613-04") is now closed.

- discovery:
    class: register-confirmation
    affected_register_entry: register/boundary/parser-extractor-db
    status_recommendation: speculated -> confirmed
    detail: |
      Stages 2 (scope-gate) and 3 (db-insert) are now implemented and tested.
      Stage 3's note "rel-type stored as a symbol" is confirmed sound by
      emacsql's prin1/read round-trip. One refinement to the entry's `producers`
      semantics: `org-graph-extractor/extract` enters vulpea's pipeline only
      once the loader calls `org-graph-extractor-register` (registration is
      deliberately a function, not a load-time side effect) — the pipeline
      shape is unchanged, only the wiring point is named.

- discovery:
    class: register-confirmation
    affected_register_entry: register/shape/typed-edge-tuple
    status_recommendation: confirmed (consumer now real)
    detail: |
      The speculated consumer `org-graph-extractor/extract` now exists and reads
      the tuple positionally (nth 0/1/2 -> from-id/rel-type/to-id), writing them
      to the `typed_edges` columns in declaration order. The symbol-typed
      rel-type is preserved end to end (storage-shape spec asserts
      `(symbolp (aref row 1))`).

- discovery:
    class: vocabulary-guard
    affected_register_entry: register/vocabulary/relation-types
    status_recommendation: keep confirmed
    detail: |
      The equality-guard spec loads BOTH the extractor (fallback defconst) and
      the real loader (`org-graph.el` defcustom) and asserts
      `(equal org-graph-extractor--default-relation-types org-graph-relation-types)`.
      Finding arch-cycle-1782551613-02 is now enforced by test, not review
      discipline. The defconst is retained (legitimate D4 standalone-load
      fallback); the guard, not deletion, is the fix.
