---
name: module-load-smoke
description: Integration spec that asserts the module loads cleanly, defcustoms have expected defaults, gptel tools are registered, and org-roam remains intact.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:implement-discovery"
  - "blocked-by:vulpea-extractor-plugin"
  - "blocked-by:finders-and-filters"
  - "blocked-by:typed-edge-query"
  - "blocked-by:coordinator-lock"
  - "blocked-by:gptel-tools"
---

## Files to modify

- `config/org-graph/test/integration/module-load-spec.el` (new) — Buttercup spec.

## Implementation steps

1. Write a single `describe "org-graph module load"` block with these `it` cases:

   - **module loads without error**: `(load (expand-file-name "config/org-graph/org-graph.el" jf/emacs-dir))` returns t and signals no error.

   - **defcustoms have expected defaults**: `org-graph-watched-roots`, `org-graph-typed-graph-root`, `org-graph-relation-types`, `org-graph-taxonomy-tags`, `org-graph-coordinator-timeout` are all bound and have the expected default shapes (lists of the right length, non-empty strings, etc.).

   - **vulpea extractor registered**: `(member 'org-graph-typed-edges (mapcar #'vulpea-extractor-name (vulpea-db-extractors)))` returns non-nil. Mock `vulpea-db-extractors` if necessary; the assertion is that the registration call was issued.

   - **gptel tools registered**: query the gptel tool registry for `org-graph-query-notes`, `org-graph-typed-edges`, `org-graph-write-node`; all three resolve.

   - **org-roam intact**: after loading the module, `(fboundp 'org-roam-node-find)`, `(fboundp 'org-roam-buffer-toggle)`, `(fboundp 'org-roam-capture)` all return t. `org-roam-directory` retains its pre-load value (capture before, compare after).

   - **fswatch precondition warning is non-fatal**: stub `executable-find` to return nil for "fswatch", reload the module, assert it loads anyway and a warning was emitted (capture `display-warning` calls).

2. The spec MAY use `cl-letf` to stub the more expensive bits (vulpea sync, real fswatch invocation) but should let module-load run for real where possible. The point is to catch breakages introduced by missing requires, mis-named functions, or broken `with-eval-after-load` gates.

3. Run: `./bin/run-tests.sh -d config/org-graph/test/integration`.

## Design rationale

This spec is the safety net for the spike. The unit specs cover individual functions but miss "module loads cleanly end-to-end" failures: missing `require`s, ordering bugs in `with-eval-after-load`, mistakes in `gptel-make-tool` registrations that fail silently in unit tests because the registry is mocked. The org-roam-intact check is the explicit guard against design.md §D8 violations — accidental advice or variable mutation would surface here.

Per architecture decision (Spike-grade coverage), this is the only end-to-end test in the suite. Latency budgets, real file-watcher behavior, and external-change detection are validated manually during use.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test/integration` — green.
- The spec file contains six `it` blocks (or close to it; merging closely-related assertions into a single `it` is fine, but the six concerns above must each be checked).

## Context

- architecture.md §Testing Approach §Scenario Mapping (the manually-validated row)
- design.md §D8
- specs/org-graph/spec.md §Coexistence with org-roam
