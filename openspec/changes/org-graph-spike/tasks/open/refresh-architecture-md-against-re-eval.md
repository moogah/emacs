---
name: refresh-architecture-md-against-re-eval
description: Rewrite the stale sections of architecture.md to match design.md RE-1..RE-6 and the implemented loader; remove the SUPERSEDED banner once done.
change: org-graph-spike
status: ready
relations:
  - discovered-from:vulpea-extractor-plugin
cites_register_entries:
  - register/vocabulary/relation-types
discovered_by: architect
discovered_class: interface-drift
---

## Files to modify
- `openspec/changes/org-graph-spike/architecture.md`

## Why
Foundation Architect finding `arch-cycle-1782551613-01` (blocking, routed to
user; risk neutralised by an inline SUPERSEDED banner). architecture.md still
describes the pre-re-eval design: org-node, filetag finders, `~/work`
recursive eager-scan, `org-graph-watched-roots` /
`org-graph-typed-graph-root` defcustoms, and a nested test layout. All are
superseded by design.md RE-1..RE-6 and the implemented loader
(`config/org-graph/org-graph.org`).

## Implementation steps
1. Rewrite the **Components**, **Interfaces**, **Dependencies**,
   **Constraints**, and **Testing Approach > Test Organization** sections to
   match the implemented loader:
   - vulpea-only; no org-node / org-mem.
   - Defcustoms: `org-graph-roam-root`, `org-graph-relation-types`,
     `org-graph-watch-workspace-homes`, `org-graph-note-types`,
     `org-graph-coordinator-timeout`. No `org-graph-watched-roots` /
     `org-graph-typed-graph-root`.
   - Discovery = registry-driven vulpea sync + `org-id-locations` DB seed.
   - Finders = schema-aware (`vulpea-schema`).
   - Flat test layout: `config/org-graph/test/*-spec.el`.
2. Preserve the still-valid claims (do NOT delete): `make-vulpea-extractor` +
   `typed_edges` + `notes(id)` FK `:on-delete :cascade`; pure-parser tuple
   shape; `with-file-lock` signature; Buttercup; PROPERTIES-drawer convention.
3. Remove the SUPERSEDED banner at the top once the body is consistent.

## Verification
- `grep -n "org-node\|watched-roots\|typed-graph-root\|directory-files-recursively" openspec/changes/org-graph-spike/architecture.md`
  returns nothing (or only the historically-accurate Re-evaluation framing).
- Cross-check defcustom names against `config/org-graph/org-graph.org`.

## Context
Finding `.orchestrator/cycles/cycle-1782551613/findings/arch-cycle-1782551613-01.md`;
design.md § Re-evaluation (RE-1..RE-6); config/org-graph/org-graph.org.
Not part of the cycle-1782551613 execute batch (deferred doc hygiene; do
before archiving the change).
