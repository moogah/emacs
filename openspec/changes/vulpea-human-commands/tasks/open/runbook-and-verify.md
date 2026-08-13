---
name: runbook-and-verify
description: Update spike-eval runbook for changed boot/root expectations; full test run and fresh-boot verification
change: vulpea-human-commands
status: blocked
relations:
  - blocked-by:menu-module
---

## Files to modify
- config/org-graph/docs/spike-eval.org (modify)
- (verification only) all config/org-graph tests

## Implementation steps
1. Update `spike-eval.org` where this change invalidates recorded
   expectations — edit the checklist text (leave existing `[x]` history
   intact; annotate rather than rewrite where an item was already checked
   under old behavior):
   - *Boot model* section: there are now THREE deferred
     `emacs-startup-hook` ops — extractor registration, sync
     configuration (`org-graph--configure-sync-deferred`), org-id seed —
     and the reload caveat applies to all three.
   - *Live-session firing* section: add a check — from a fresh boot,
     `(bound-and-true-p vulpea-db-autosync-mode)` → t and
     `vulpea-db-sync-directories` equals `(org-graph/index-roots)` with no
     manual step; `*Warnings*` additionally clean of "sync configuration
     skipped".
   - *Discovery* section: `(org-graph/index-roots)` now returns `~/org/`
     (the vault root) instead of `~/org/roam/`; the "ongoing pickup"
     sub-check (a) expectation CHANGES for boot-time roots: a note added
     under a boot-time root during the session IS picked up automatically
     (autosync live from startup). The unchanged limitation: roots added
     mid-session (new workspaces) still get only the `:on-create`
     one-shot.
   - *Coexistence (D8)* section: annotate that the vulpea-human-commands
     change supersedes the side-by-side framing — org-roam interop is an
     explicit non-goal now; keep the "org-roam UX unchanged" check (we
     still must not break it) but drop the expectation that the two
     indices track each other.
   - Add a short *Human commands & menu* capability section: `SPC v`
     opens the menu; find-or-create and insert-link behave per the
     org-graph-note-commands spec (create lands in `~/org/` as
     `<timestamp>-<slug>.org`, immediately findable and id-resolvable);
     edge queries at point render `*org-graph-edges*`.
2. Run the full suite: `./bin/run-tests.sh -d config/org-graph` — zero
   failures across both frameworks.
3. Fresh-boot manual verification (GUI `Emacs.app` or
   `./bin/emacs-isolated.sh`, NOT `jf/reload-module`) of the new/changed
   runbook items above; record results in the runbook checkboxes and
   Findings.
4. Sanity-check the untouched boundaries survived: typed-edge extraction
   still gated to `~/org/roam/` (an `EDGES` drawer on a note at `~/org/`
   top level yields no `typed_edges` rows — expected per design D1/OQ-A),
   agent `org_graph_write_node` still targets the roam root.

## Design rationale
The runbook is the spike's evidence base for the end-of-window decision
prompt; leaving stale expectations (roam-only root, no boot autosync,
side-by-side D8 framing) would corrupt the findings this change was born
from. The deferred ops are only wiring-tested by the automated suite —
their firing against a real DB on a real launch is exactly what the
runbook verifies by hand, so the new third op inherits that obligation.
The D8 annotation records the strategic shift (vulpea replaces org-roam;
coexistence during the eval is transitional, not a goal) without erasing
the still-binding "don't break org-roam while it's installed" check.

## Design pattern
Follow the runbook's own conventions (`spike-eval.org` § How to use this
runbook): each item names the exact command/eval and the observable
result meaning "pass"; surprises go under Findings keyed by RE-/OQ- ids.

## Verification
- `./bin/run-tests.sh -d config/org-graph` — all pass.
- Runbook diff reviewed: no previously-recorded `[x]` evidence deleted,
  only annotated/extended.
- All new runbook items exercised on a real fresh boot and checked off
  (or surprises recorded under Findings).

## Context
design.md § 'Migration Plan' and Risks (deferred-op wiring-only testing)
proposal.md § Impact (spike-eval supersession note)
config/org-graph/docs/spike-eval.org (structure and conventions)
