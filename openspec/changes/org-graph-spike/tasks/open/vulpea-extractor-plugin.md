---
name: vulpea-extractor-plugin
description: Wrap the pure parser as a vulpea extractor that registers a typed_edges table and stores edge tuples, scoped to the roam vault.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:parse-typed-edges
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
</content>
