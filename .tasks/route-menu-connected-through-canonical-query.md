---
name: route-menu-connected-through-canonical-query
description: org-graph/edges-connected-at-point composes outgoing+incoming instead of calling org-graph-query/connected; when connected gains read-time enrichment (query-inverse-symmetric), the menu view silently diverges. Give connected direction-attributed output (or a partition helper) and route the menu through it.
status: ready
source: openspec/changes/vulpea-human-commands
relations:
  - discovered-from:menu-module
  - blocked-by:query-inverse-symmetric (org-graph-spike; sequence with-or-after it)
discovered_by: architect
discovered_class: duplication
---

> Architect end-of-cycle finding arch-cycle-1786636086-eoc-1
> (cycle-1786636086). Advisory, not blocking: today the composition is a
> one-line union with behavior identical to `org-graph-query/connected`,
> and the bypass is deliberate — the renderer needs per-direction far-end
> attribution that a flat union cannot supply for a self-edge (documented
> in menu.org's docstring since inline fix this cycle). The risk is
> FUTURE divergence: org-graph-spike's open `query-inverse-symmetric`
> task speculates symmetric-surfacing / inverse-label enrichment landing
> *inside* `connected`; the menu view would silently miss it. No test
> relates the two code paths.

## Files to modify
- config/org-graph/query.org (tangles to query.el)
- config/org-graph/menu.org (tangles to menu.el)
- config/org-graph/test/menu-spec.el, typed-edges-spec.el (as needed)

## Fix sketch
Either have `org-graph-query/connected` return direction-attributed results
the renderer can section (edges already carry :from/:to; add a far-key or a
partition helper in query.org), or add the enrichment at the directional
query layer so both surfaces inherit it. Then make
`org-graph/edges-connected-at-point` consume the canonical path and drop the
mirror-this-manually docstring caveat. Add a test relating the menu view to
`connected` so future enrichment cannot diverge silently.

## Context
- interfaces.org `register/boundary/typed-edge-query-api` (consumers list
  carries the coupling note pointing here)
- .orchestrator/cycles/cycle-1786636086/findings/arch-cycle-1786636086-eoc-1.md
- openspec/changes/org-graph-spike/tasks/open/query-inverse-symmetric.md
