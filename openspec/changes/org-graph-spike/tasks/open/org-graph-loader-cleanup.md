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

## Observations

- Deviated from the task's suggested helper shape (`org-graph--defer-to-startup FN`
  as a helper that itself does the `add-hook`): that shape puts anonymous closures
  on `emacs-startup-hook`, which (a) breaks the cold-load guard's by-name
  membership assertion, (b) makes `add-hook` non-idempotent across module reloads
  (each reload adds a fresh non-`equal` closure, so the deferred op would run
  twice), and (c) defeats interactive `remove-hook`. Unified instead as: the
  loader owns BOTH deferrals, each remains a *named* wrapper on the hook, and the
  duplicated ~6-line resilience idiom collapses into one shared body,
  `org-graph--run-deferred-op (fn what)`. Behavior identical: same named hook
  members, same warning texts, and the seed is add-hooked before the extractor so
  the hook run order (extractor, then seed) matches the pre-refactor state
  (`add-hook` prepends).
- Symmetric ownership was resolved *toward the loader* (the seed's wrapper +
  `add-hook` moved from `discovery.el` into `org-graph.el`'s "Deferred DB-touching
  registrations" section) rather than toward per-module self-deferral, because
  `extractor.org` is owned by a parallel task this batch and the loader already
  narrates the DB-free-load contract. `discovery.el` keeps only the pure seed
  function plus a prose pointer to the loader-owned deferral.
- Extended the cold-load guard's existing wiring `it`-block
  (`config/org-graph/test/module-load-spec.el`) to also assert
  `org-graph--seed-org-id-locations-deferred` is wired onto `emacs-startup-hook`.
  The seed's wiring was previously untested in either home; asserting it inside
  the existing block strengthens the guard while keeping the spec count at 135.
- Fixed a stale line in `org-graph/seed-org-id-locations`'s docstring ("intended
  to run once at module load"), which contradicted the deferral contract and
  would have become actively misleading once the Post-init seed section's
  corrective prose moved out of `discovery.org`.
- The pruned `auto-id-scaffold` placeholder was the last in-module signpost to
  work that landed entirely *outside* org-graph (`config/workspaces/scaffold.org`
  and `config/gptel/sessions/commands.org` stamp IDs at file-creation time). The
  new "Submodule map" pointer section records that, so provenance is not lost.
- Latent doc note: `module-load-spec.el`'s commentary already said both
  DB-touching registrations were "deferred by the loader" *before* this change,
  when the seed in fact self-deferred inside `discovery.el` — the guard's prose
  anticipated the unified ownership model; it is now literally accurate.

## Discoveries

- discovery_id: disc-org-graph-loader-cleanup-1
  class: interface-drift
  description: |
    register/invariant/org-graph-loader-ordered-sequence (reconciled,
    load-bearing) is PRESERVED exactly: one contiguous by-path ordered
    "* Submodules" block (grep audit still shows the single 8-line
    sequence), every DB-free registration fires at require, and both
    DB-touching ops still fire on emacs-startup-hook with unchanged
    resilience semantics. However, the entry's narrative now understates
    the code in two small ways: (1) the status_note describes the seed as
    self-deferring "inside discovery.el", while ownership is now
    consolidated in the loader (both named wrappers delegate to the shared
    org-graph--run-deferred-op and are add-hooked from org-graph.el);
    (2) the enforcement_mechanism says the guard verifies "the extractor
    deferral is wired onto emacs-startup-hook", while the guard now
    verifies BOTH wirings (extractor + seed) by name.
  affected_register_entry: register/invariant/org-graph-loader-ordered-sequence
  recommendation: |
    Append a status_note addendum (no status change needed): deferral
    ownership consolidated in the loader via the shared helper
    org-graph--run-deferred-op (org-graph-loader-cleanup); named wrappers
    org-graph--register-extractor and
    org-graph--seed-org-id-locations-deferred remain the hook members; the
    cold-load guard now asserts both wirings. Optionally update the
    enforcement_mechanism location text to say "both DB-touching deferrals"
    instead of "the extractor deferral".
