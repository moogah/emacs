---
name: wire-into-init
description: Add "org-graph" to jf/enabled-modules and verify the spike module loads cleanly during a real isolated Emacs boot.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:module-load-smoke"
---

## Files to modify

- `init.org` (modify) — add `"org-graph"` to `jf/enabled-modules`, immediately after `"gptel"`.

## Implementation steps

1. Locate the `jf/enabled-modules` definition in `init.org`. Add `"org-graph"` to the list, positioned right after `"gptel"`. The CLAUDE.md load-order requirement is "org-graph after gptel so the agent-tool registration can attach to the existing gptel tool registry" (design.md §D7).

2. Tangle: `./bin/tangle-org.sh init.org`.

3. Run a clean isolated boot: `./bin/emacs-isolated.sh -nw --batch --eval "(message \"boot-ok\")"`. Expected: prints `boot-ok`, exits 0, no errors or warnings about org-graph.

4. Run an interactive isolated boot: `./bin/emacs-isolated.sh -nw`. Verify (a) no errors in `*Messages*`; (b) `M-x org-graph/find-topic` is bound; (c) `M-x org-graph/eager-discover` is bound.

5. Run the smoke test against the real boot: `./bin/run-tests.sh -d config/org-graph` — all green.

## Design rationale

Wiring is a separate task because module-load test (previous task) runs against the loader file directly, but the real failure mode (load-ordering, init.org tangle correctness, jf/load-module wrapping) only surfaces during an actual init.org boot. Doing this as its own task keeps the failure attributable: if the smoke test passes but boot fails, the bug is in `init.org`, not in `org-graph` itself.

Per CLAUDE.md, modules in `jf/enabled-modules` are loaded by `jf/load-module` with error handling. If the module fails to load, init continues but with the failure logged — exactly the safety we want for an experimental spike.

## Verification

- `./bin/tangle-org.sh init.org` — exits 0.
- `grep -n '"org-graph"' init.el` — at least one match (after "gptel").
- `./bin/emacs-isolated.sh -nw --batch --eval "(message \"boot-ok\")"` — prints `boot-ok` cleanly, exits 0.
- `./bin/emacs-isolated.sh -nw --batch --eval "(progn (require 'org-graph) (message \"%s\" (functionp 'org-graph/find-topic)))"` — prints `t`.
- `./bin/run-tests.sh -d config/org-graph` — green.

## Context

- design.md §D7
- CLAUDE.md §Module System
