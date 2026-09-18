---
name: org-graph-rel-symbol-shim-consolidation
description: Make org-graph-extractor--normalize-rel accept symbols (or expose one shared coercion helper beside it) and delete the twin 3-line shims org-graph-authoring--rel-symbol and org-graph-tools--rel-symbol.
status: externalised
source: openspec/changes/workspace-notes-in-vault
cites_register_entries:
  - register/vocabulary/relation-types
relations:
  - discovered-from:arch-cycle-1789334430-ot-edge-writer-1
  - discovered-from:og-edge-writer-and-project-note
discovered_from: arch-cycle-1789334430-ot-edge-writer-1
discovered_by: architect
discovered_class: duplication
created_at: 2026-09-17T00:00:00Z
---

> Externalised by cycle-1789334430 integrate (change workspace-notes-in-vault). Advisory
> duplication (overlay default would be blocking; downgraded because both sites route through
> the single canonical normaliser and the duplicate is a nil/symbol/string coercion shim, not a
> second translation site). Cleanup, not part of the proposal's outcome.

## Finding
`org-graph-authoring--rel-symbol` (config/org-graph/authoring.org ~:134) duplicates the
pre-existing `org-graph-tools--rel-symbol` (config/org-graph/tools.org ~:159). Each module
tests its own shim; nothing asserts a single coercion helper.

## Steps
1. Extend `org-graph-extractor--normalize-rel` (config/org-graph/extractor.org) to accept a
   symbol as well as a string (or add `org-graph-extractor--coerce-rel` next to it).
2. Route authoring.org and tools.org through it; delete both shims; tangle all three.
3. Spec: one `it` in the extractor spec covering nil / symbol / string inputs.

## Verification
```
./bin/tangle-org.sh config/org-graph/extractor.org config/org-graph/authoring.org config/org-graph/tools.org
grep -n 'rel-symbol' config/org-graph/*.el   # expect none
./bin/run-tests.sh -d config/org-graph --report
```
