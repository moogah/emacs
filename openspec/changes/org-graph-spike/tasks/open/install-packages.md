---
name: install-packages
description: Install vulpea v2.4+ via straight.el with a pinned commit (org-node is NOT adopted) so subsequent tasks have the library available.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:scaffold-module
---

## Files to modify
- `config/org-graph/org-graph.org` (modify) — add the `use-package`/straight
  recipe(s) in the loader's package section.

## Implementation steps
1. Add a `use-package vulpea` straight recipe pinned to a specific commit (NOT
   `master`):
   ```elisp
   (use-package vulpea
     :straight (vulpea :type git :host github :repo "d12frosted/vulpea" :commit "<pin>")
     :config ...)  ;; sync-directories / autosync configured in registry-discovery task
   ```
   Pin to a v2.4.x release tag/commit. Record the chosen commit in a comment.
2. Do NOT add org-node, org-mem. RE-2 resolved discovery to vulpea-only;
   org-node's only unique value (global `org-id-locations` upkeep) is already
   covered by vulpea's `org-id-add-location` calls. A second index would be
   redundant double-work.
3. `vulpea-journal` is OPTIONAL and deferred (Open Question 5): if installed,
   use `:defer t` and bind no keys. Prefer to omit it entirely for the first
   pass and add later if the eval wants daily-log evaluation.
4. Verify the system has `fswatch` available (preferred vulpea change-detection
   backend on macOS): `brew list fswatch` — note in a comment that absence
   falls back to fd/find polling.
5. Smoke-test load in isolated batch Emacs:
   `./bin/emacs-isolated.sh -nw --batch --eval "(progn (require 'vulpea) (message \"OK %s\" (vulpea-version)))"`.

## Design rationale
Pinning is now ordinary hygiene rather than a spike-survival mechanism: vulpea
2.4 ships parser-epoch cache invalidation, schema-version auto-rebuild, and
async-default sync, which retired the original "API still evolving" risk
(RE-6). Vulpea-only avoids the two-scanner / two-cache cost of running
org-node alongside it (RE-2).

## Design pattern
Straight recipe style per CLAUDE.md § Packages and existing `use-package`
blocks in `config/gptel/`. Keep the `:config` minimal here — directory sync
and autosync wiring belong to `registry-discovery`.

## Verification
- The smoke-test eval prints `OK <version>` and exits 0.
- `vulpea-doctor` (M-x in an interactive isolated session) reports the version
  and a sane configuration.
- `grep -n "org-node\|org-mem" config/org-graph/org-graph.org` returns nothing
  (org-node intentionally absent).

## Context
design.md § Re-evaluation (RE-2 discovery engine, RE-6 risk retired);
design.md § Dependencies; CLAUDE.md § Packages (straight.el).
</content>
