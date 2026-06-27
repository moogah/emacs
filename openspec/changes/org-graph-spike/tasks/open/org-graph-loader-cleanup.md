---
name: org-graph-loader-cleanup
description: Low-priority cleanup of the consolidated org-graph loader — unify the two DB-free deferral wrappers and prune the now-dead skeleton placeholder sections in org-graph.org.
change: org-graph-spike
status: ready
discovered_from: wire-into-init
discovered_by: reviewer
discovered_class: duplication
relations:
  - discovered-from:wire-into-init
cites_register_entries:
  - register/invariant/org-graph-loader-ordered-sequence
---

> Not on the critical path; does NOT block `spike-eval-checklist` or the spike
> eval. Pure polish on the loader landed by `wire-into-init` (merge 50af89ae).
> Groups two non-blocking findings from that task's cycle (cycle-1782573574).

## Files to modify
- `config/org-graph/org-graph.org` (+ tangled `.el`)
- `config/org-graph/discovery.org` (+ tangled `.el`)

## Cleanup items

1. **Unify the two DB-free deferral wrappers** (review Finding 2, advisory
   design-drift). The post-init deferral idiom (resilient `condition-case` wrapper
   added to `emacs-startup-hook`) is implemented twice with asymmetric ownership:
   - `org-graph--register-extractor` lives in the loader `org-graph.el`
     (defers the typed-edge extractor registration),
   - the discovery org-id seed self-defers inside `discovery.el`
     (`org-graph--seed-org-id-locations-deferred`).
   Same intent, ~6-line duplicated idiom, two homes. Consider one shared helper
   (e.g. `org-graph--defer-to-startup FN` in the loader or a small util) so both
   deferrals use one ownership model. Keep each deferral's behavior identical;
   this is a structure-only change.

2. **Prune the dead skeleton placeholder sections** (implementor obs 4). After
   consolidation, the seven `;; implemented in <task>` no-op placeholder sections
   in `org-graph.org` (Discovery / Auto-ID scaffold / Note-type schemas / Finders
   / Typed-edge parser / Extractor / Coordinator) are pure documentation — the
   real loads live in the single "* Submodules" section. They create
   "implemented elsewhere" ambiguity. Prune or collapse them into a short pointer
   to the "* Submodules" section.

## Verification
- `./bin/tangle-org.sh config/org-graph/org-graph.org` and
  `./bin/tangle-org.sh config/org-graph/discovery.org` succeed (parens valid).
- `./bin/run-tests.sh -d config/org-graph` — suite stays green (135 specs); the
  cold-load guard still passes (the unified deferral must keep
  `org-graph--register-extractor` wired onto `emacs-startup-hook`, or the guard's
  wiring assertion must be updated to the new helper name in the same task).
- The loader invariant `register/invariant/org-graph-loader-ordered-sequence`
  (reconciled) remains satisfied — no change to load order or by-path loading.

## Context
Review findings + observations from `wire-into-init`, cycle-1782573574
(`.orchestrator/cycles/cycle-1782573574/reviews/wire-into-init.md`;
`## Observations` in the closed task `tasks/closed/wire-into-init.md`).
