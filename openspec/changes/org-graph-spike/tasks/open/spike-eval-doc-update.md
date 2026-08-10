---
name: spike-eval-doc-update
description: Update the spike-eval runbook's typed-edge checks to exercise the open vocabulary, both authoring surfaces, and the optional registry.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:extractor-union
  - blocked-by:rel-link-type
  - blocked-by:query-inverse-symmetric
---

> Docs follow implementation. The runbook currently checks the closed-set
> properties surface; retarget it to the shipped open-vocab links-drawer /
> dual-surface / registry behavior. See design.md § Open-Vocabulary Typed
> Edges and § Links-Drawer Edge Surface.

## Files to modify
- `config/org-graph/docs/spike-eval.org`

## Implementation steps
1. Replace closed-set property checks (`:IMPLEMENTS:` etc.) with edge-drawer
   items (`- implements :: [[id:...]]` in the `:EDGES:` drawer), and add
   checks that a **novel** type (e.g. `- falsifies ::`) extracts with no
   configuration and a **multi-word** tag (`- follows up ::`) normalizes to
   `follows-up`.
2. Add a check for the inline surface: author a `rel:<type>:<id>` link in a
   roam note body, confirm it follows via `org-open-at-point`, completes via
   `C-c C-l rel`, and produces a `typed_edges` row attributed to the enclosing
   node.
3. Add checks for the discriminator: an ordinary `:SOURCE: [[id:]]` property
   and a bare body `[[id:]]` link outside the drawer produce **no** edge; a
   drawer item's link follows with `org-open-at-point` and its edge appears
   in the org-roam backlinks buffer.
4. Add a registry check: create an `:edge-type:` note with an `:INVERSE:`,
   confirm the query layer renders the inverse label, and that an unregistered
   type still works (raw symbol).
5. Fold the answers to OV-Q1..OV-Q3 into *Findings* / *Decision prompt* so the
   eval window resolves them empirically.

## Design rationale
The runbook is the spike's empirical gate; it must exercise the surfaces that
actually shipped, and capture the new open questions (OV-Q1..OV-Q3) for
resolution during real use.

## Verification
- `./bin/tangle-org.sh config/org-graph/docs/spike-eval.org` validates (if
  tangled) or the doc renders cleanly.
- `grep -n 'EDGES\|rel:\|edge-type' config/org-graph/docs/spike-eval.org` shows
  the new checks present.

## Context
design.md § Open-Vocabulary Typed Edges (OV-1..OV-7, OV-Q1..OV-Q3) and
§ Links-Drawer Edge Surface (LD-1..LD-6, LD-Q1..LD-Q3);
spec.md § Typed Semantic Edges.
