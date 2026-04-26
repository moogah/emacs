---
name: vulpea-extractor-plugin
description: Wrap the pure parser as a vulpea extractor plugin that registers the typed_edges schema and stores edge tuples for query.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:parse-typed-edges"
---

## Files to modify

- `config/org-graph/org-graph.org` (modify) — extend the `Extractor` subtree with the vulpea plugin wrapper.

## Implementation steps

1. Inside the `Extractor` subtree (after the pure parser), define the plugin via `make-vulpea-extractor`:

   ```elisp
   (defvar org-graph-extractor--plugin
     (make-vulpea-extractor
      :name 'org-graph-typed-edges
      :version 1
      :priority 50
      :schema '((typed_edges
                 [(from-id :not-null) (rel-type :not-null) (to-id :not-null)]
                 (:primary-key [from-id rel-type to-id])
                 (:foreign-key [from-id] :references notes [id]
                  :on-delete :cascade)))
      :extract #'org-graph-extractor--extract))
   ```

2. Implement the `:extract` callback:
   - Receives `(parse-ctx note-data)`.
   - **Bail out fast** if `(file-in-directory-p (vulpea-parse-ctx-path parse-ctx) org-graph-typed-graph-root)` is nil — only the typed-graph root participates in the index (design.md §D2, spec scenario "Project-local note is excluded").
   - Call `org-graph-extractor/parse-typed-edges` with the AST and the note's id.
   - Return `note-data` augmented with `:typed-edges <tuples>`. Vulpea handles inserting into `typed_edges` based on the schema declaration.

3. Register the extractor with vulpea at module-load time (only after vulpea is loaded). Use `with-eval-after-load 'vulpea` to gate registration.

4. The `:on-delete :cascade` on the foreign key is mandatory — when a note is deleted, its outgoing edges go with it. Per the vulpea plugin guide.

5. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

The plugin wrapper is intentionally thin — its only jobs are (a) declaring the schema vulpea should manage, and (b) gating extraction to the typed-graph root. All parsing logic lives in the pure function (previous task) so it remains testable without vulpea (design.md §D4). The root-gate enforces design §D2: project-local notes are visible to discovery and navigation but never participate in the typed-edge index.

The `with-eval-after-load 'vulpea` gate ensures the registration doesn't break module-load if vulpea hasn't initialized yet.

## Verification

- `./bin/tangle-org.sh config/org-graph/org-graph.org` — exits 0.
- `grep -n "make-vulpea-extractor" config/org-graph/org-graph.el` — exactly one match.
- `grep -n "on-delete :cascade" config/org-graph/org-graph.el` — at least one match.
- `grep -n "file-in-directory-p.*org-graph-typed-graph-root" config/org-graph/org-graph.el` — at least one match (the root-gate).
- Manual eval: in a fresh Emacs with the spike module loaded, `(member 'org-graph-typed-edges (mapcar #'vulpea-extractor-name (vulpea-db-extractors)))` returns non-nil.

## Context

- design.md §D2, §D4
- architecture.md §Components §org-graph-extractor
- specs/org-graph/spec.md §Typed Semantic Edges (especially the "project-local excluded" scenario)
