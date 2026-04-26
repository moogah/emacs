---
name: install-packages
description: Install org-node, vulpea v2, and vulpea-journal via straight.el with pinned commits so subsequent tasks have the libraries available.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:scaffold-module"
---

## Files to modify

- `config/org-graph/org-graph.org` (modify) — add a `Packages` subtree before submodule subtrees with three `use-package` blocks pinned via `:straight (... :commit "...")`.

## Implementation steps

1. In `config/org-graph/org-graph.org`, add a `Packages` subtree with use-package + straight recipes for:
   - `org-node` — `:straight (org-node :type git :host github :repo "meedstrom/org-node" :commit "...")`. Look up the current HEAD commit at implementation time.
   - `vulpea` — `:straight (vulpea :type git :host github :repo "d12frosted/vulpea" :commit "...")`. Pin to a v2 release tag if available; otherwise current master at implementation time.
   - `vulpea-journal` — install but `:defer t` and DO NOT bind keys (per design.md §Open Question 5).

2. Add an `fswatch` precondition check that warns (does not error) at module-load if `(executable-find "fswatch")` returns nil. Vulpea v2 needs it for external-change detection.

3. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

4. Smoke-test in an isolated Emacs that the three packages load without error: `./bin/emacs-isolated.sh -nw --batch --eval "(progn (require 'org-node) (require 'vulpea) (require 'vulpea-journal) (message \"OK\"))"`.

## Design rationale

Pinning to specific commits is a hard requirement for vulpea v2 — released January 2026, API still evolving (design.md §Risks). Without pinning, a `straight pull` could break the spike unpredictably. `vulpea-journal` is installed-but-deferred so it's available for evaluation without committing to a daily-log workflow change yet (design.md §Open Question 5).

## Verification

- `./bin/tangle-org.sh config/org-graph/org-graph.org` — exits 0.
- `grep -n ":commit" config/org-graph/org-graph.el | wc -l` — 3.
- `./bin/emacs-isolated.sh -nw --batch --eval "(progn (require 'org-node) (require 'vulpea) (message \"OK\"))"` — prints `OK` and exits 0.
- `grep -n "fswatch" config/org-graph/org-graph.el` — at least one match for the precondition check.

## Context

- design.md §Decisions §D1, §Risks
- design.md §Open Questions §5
