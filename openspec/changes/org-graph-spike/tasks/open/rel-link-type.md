---
name: rel-link-type
description: "Register the custom rel: org link type (path rel:<type>:<id>) with follow/complete/face/export via org-link-set-parameters (OV-3)."
change: org-graph-spike
status: ready
relations:
  - blocked-by:edge-type-registry
cites_register_entries:
  - register/boundary/rel-link-path-syntax
  - register/boundary/edge-type-registry-lookup
  - register/vocabulary/relation-types
---

> The inline authoring UX. AST-level extraction of the same links lives in
> `parse-rel-links`; this task owns only the interactive link runtime.
> `:complete` candidates come from the edge-type registry, hence the
> dependency. See design.md § Open-Vocabulary Typed Edges, OV-3.

## Files to modify
- `config/org-graph/rel-link.el` ← via `config/org-graph/rel-link.org` (new module)
- `config/org-graph/org-graph.el` ← via `config/org-graph/org-graph.org` (load new module)
- `config/org-graph/test/rel-link-spec.el` (new)

## Implementation steps
1. Register the link type via `org-link-set-parameters`, reading its name
   from `org-graph-edge-link-type` (a `defcustom`, default `"rel"`, declared
   in `org-graph.org` — the parallel knob to `org-graph-edge-drawer`,
   LD-1/OV-3). Path syntax `<link-type>:<type>:<target-id>` (default
   `rel:<type>:<target-id>`):
   - `:follow` — parse `<type>:<target-id>`, jump to the target via
     `org-id-goto` / `org-id-find`.
   - `:complete` — prompt for a relation type (candidates: registry types ∪
     observed types ∪ `org-graph-relation-types` seed; free text allowed to
     coin a new type, OV-Q2), then a target node (org-roam/vulpea node
     completion), returning `rel:<type>:<id>`.
   - `:face` — a distinct face so `rel:` links read differently from bare
     `id:` links.
   - `:export` — render the description (fall back to the target title).
2. Degrade gracefully when the registry is empty — completion still offers
   observed + seed types and free-text coinage.
3. `rel-link-spec.el`: assert the registered type exists; a
   `rel:falsifies:<id>` path round-trips through the follow parser to
   `(falsifies . <id>)`; `:complete` output is well-formed
   `rel:<type>:<id>`. Stub `org-id-find` / node completion.

## Design rationale
`org-link-set-parameters` gives follow/complete/face/export for free (OV-3),
so the inline surface is cheap once the registry supplies candidates.
Keeping parsing in `parse-rel-links` (not here) means extraction stays
testable without the link runtime.

## Verification
- `./bin/tangle-org.sh config/org-graph/rel-link.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — rel-link spec passes.
- Manual: `C-c C-l rel RET` in a roam note completes a type then a target and
  inserts a working `rel:` link that `org-open-at-point` follows.

## Context
design.md § Open-Vocabulary Typed Edges (OV-3, OV-Q2); spec.md § Typed
Semantic Edges (inline `rel:` links); architecture.md § Components (`rel-link`).

## Cycle updates (cycle-1786458912)

- **Knob home settled:** declare `org-graph-edge-link-type` (defcustom,
  default "rel") in the loader `org-graph.org`, parallel to
  `org-graph-edge-drawer` — pinned by
  register/boundary/rel-link-path-syntax (reconciled) and the
  parse-rel-links review; the extractor already reads it via the
  fail-closed `org-graph-extractor--edge-link-type` helper, so declaring
  the defcustom activates the (already shipped and spec-tested)
  `parse-rel-links` scanner.
- **Stage-0 precondition:** register the link type at MODULE LOAD (DB-free,
  before any reindex) — an unregistered type parses as `fuzzy` and is
  invisible to extraction. Registration is load-bearing for the extraction
  pipeline, not just authoring UX.
- **:complete candidates:** the registry lookup is live —
  `org-graph/edge-types` (READ-ONLY table) / `org-graph/edge-type`
  (fresh-copy plist). Observed types (OQ per OV-Q3) and
  `org-graph-relation-types` seed complete the candidate set; free-text
  coinage stays allowed (OV-Q2).
- **Normalization:** the type segment the runtime inserts should round-trip
  through `org-graph-extractor--normalize-rel` semantics (completion may
  offer `follows-up`; a user typing "follows up" coins the same symbol).
