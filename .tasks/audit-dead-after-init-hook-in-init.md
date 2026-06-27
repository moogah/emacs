---
name: audit-dead-after-init-hook-in-init
description: An (add-hook 'after-init-hook ...) in init.org is dead under this repo's "emacs -q --load init.el" launch — after-init-hook fires before the --load actions, so module/init-added after-init-hook handlers never run. Audit and fix.
source: openspec/changes/org-graph-spike (task wire-into-init, cycle-1782573574)
status: ready
discovered_from: wire-into-init
discovered_by: implementor
discovered_class: dead-branch
relations:
  - discovered-from:wire-into-init
---

## Problem

While wiring org-graph's deferred work to a post-init seam, the implementor
verified empirically that under this repo's launch model — every path
(`bin/emacs-isolated.sh`, the `Makefile`'s `EMACS_TEST_BATCH`) runs
`emacs -q --load early-init.el --load init.el` — `after-init-hook` fires (and
`after-init-time` is set) BEFORE the command-line `--load` actions. So any
`after-init-hook` handler ADDED from inside `init.el` or a module loaded by it
**never runs** here.

`init.org` (~line 249) contains an `(add-hook 'after-init-hook ...)` that is
therefore **dead** under these launch methods. `config/look-and-feel/themes.el`
already worked around this by using `emacs-startup-hook` instead; org-graph
(wire-into-init) followed the same pattern.

## Why this is externalised (not in the org-graph change)

It is a pre-existing init.org defect, unrelated to org-graph, with potential
blast radius across any module that assumed `after-init-hook` works. It belongs
to whoever maintains init.org, not to the org-graph spike.

## Files to investigate
- `init.org` (~line 249) — the dead `after-init-hook` add-hook (and its tangled
  `init.el`).
- `grep -rn "after-init-hook" init.org config/` — find every other handler added
  to `after-init-hook` from init-loaded code; each is suspect under this launch
  model.
- `config/look-and-feel/themes.el` — the existing `emacs-startup-hook` precedent.

## Implementation steps (sketch)
1. Confirm the launch-model claim: add a probe to `after-init-hook` and
   `emacs-startup-hook` from init.el and observe which fires under
   `emacs -q --load init.el`.
2. Enumerate all init-added `after-init-hook` handlers; determine which are dead.
3. Migrate the genuinely-post-init ones to `emacs-startup-hook` (or the correct
   seam), or document why a given handler is intentionally pre-`--load`.
4. Consider a lint/check so future `after-init-hook` adds from init-loaded code
   are flagged.

## Verification
- The migrated handler(s) demonstrably fire under the real launch
  (`bin/emacs-isolated.sh -nw --batch --eval ...` probe, or an interactive boot).
- No regression in existing init behavior.

## Context
Discovered in `wire-into-init` (`## Observations`, cycle-1782573574). The
org-graph deferral seam (`emacs-startup-hook`) is the correct precedent.
