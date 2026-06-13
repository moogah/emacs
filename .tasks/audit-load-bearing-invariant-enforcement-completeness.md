---
name: audit-load-bearing-invariant-enforcement-completeness
description: Walk every load_bearing:true invariant entry in interfaces.org and validate that the entry's enforcement_mechanism section names mechanisms that actually realise the invariant's statement
source: openspec/changes/add-workspace-home-directory (cycle-20260526-171719 end-of-cycle audit)
status: externalised
relations:
  - discovered-from:arch-cycle-20260526-171719-02
---

<!-- Provenance fields (orchestrator schema, externalisation contract):
     discovered_from: arch-cycle-20260526-171719-02
     discovered_by: architect (end-of-cycle audit)
     discovered_class: invariant-gap
     source: openspec/changes/add-workspace-home-directory
     finding_file: .orchestrator/cycles/cycle-20260526-171719/findings/arch-cycle-20260526-171719-02.md -->

## Why this task is externalised, not in-change

The cycle that surfaced this (`add-workspace-home-directory`,
cycle-3, `cycle-20260526-171719`) raised the pattern via two
concrete asks against two workspaces-subsystem invariants
(home-required-no-floating-workspaces; home-org-user-authored-after-creation).
But the *audit* the architect recommends walks **every load-bearing
invariant in `interfaces.org`** — gptel/scope, gptel/sessions,
gptel/tools, refine-workspaces, core/* — not just the workspaces
ones. Filing this in-change would force the proposal to acknowledge
a register-wide audit as a sub-outcome of "add workspace home
directory", which it isn't.

Externalised per `~/.claude/skills/opsx-orchestrate/externalisation.md`'s
heuristic: "if filing the task in-change would force the user to
update `proposal.md` to acknowledge a new sub-outcome, externalise."

## The pattern (consolidated from the cycle-3 architect finding)

Two cycle-3 spec-signals — both routed to the user as
`ask-cycle-20260526-171719-1` and `-2` — share the same shape:

1. `register/invariant/home-org-user-authored-after-creation`
   (load_bearing: true) names a structural lint as enforcement. The
   lint passes trivially because the proximity heuristic mismatches
   the actual writer idiom.
2. `register/invariant/home-required-no-floating-workspaces`
   (load_bearing: true) names `workspace--make` (constructor) and
   the persistence deserializer as enforcement. The constructor
   does not actually guard against relative paths; only the
   deserializer does.

In both cases the entry's `enforcement_mechanism` field's prose
says one thing; the code says another. The cycle's reviewer caught
each independently; the architect's role is to name the **class**
and propose a consolidated audit.

## Scope

Walk **every** entry under the `* Invariants` section of
`interfaces.org` whose `load_bearing` field is `true`. For each:

1. Read the `statement` field.
2. Read the `enforcement_mechanism` field's `location` sub-field.
3. For each location named, verify that the mechanism actually
   structurally catches a violation of the statement.

The audit's output: a markdown table at
`<repo>/.tasks/audit-results-load-bearing-invariant-enforcement.md`
listing, per invariant: enforcement claim, validated/not-validated,
gap (if any), recommended fix.

## Concrete starting set (cycle-20260526-171719 evidence)

From the cycle's reconciliation notes — partial sample:

| Entry | Claimed | Reality | Gap |
|-------|---------|---------|-----|
| home-required-no-floating-workspaces | constructor + deserializer | only deserializer enforces absolute-path; constructor accepts any string | Add `cl-check-type` or runtime check at `workspace--make` |
| home-org-user-authored-after-creation | structural lint | lint passes trivially (proximity heuristic mismatch) | Fix heuristic (see disposition-ask-writer-lint-heuristic) |

A complete audit will find more.

## Files to modify (per audit outcome)

This task is **the audit itself**, not the fixes. Each gap surfaces
its own follow-up task (in-change or externalised per the existing
rules). The audit's deliverable is the gap table; the fixes are
downstream.

## Verification

The audit is complete when:

1. Every `load_bearing: true` invariant in `interfaces.org` has a
   row in the audit-results table.
2. Each row's "gap" cell is empty (no gap) or names a concrete
   structural deficiency.
3. Each named gap has a follow-up task filed (in-change or
   `.tasks/` per externalisation rules).
4. The architect's role-contract is updated to add "enumerate
   enforcement-mechanism witnesses" to the forward-mode protocol
   for new invariant-tier entries — so the next entry doesn't
   reproduce the pattern.

## Notes for the implementor

This is an architect-tier audit. The work is read-only against
code + cross-referencing against `interfaces.org`. The fixes are
not in scope; they spawn from the audit.

Estimate: 1-2 cycles depending on the number of load-bearing
invariants in the register at audit time.

## Cited register entries

All `load_bearing: true` invariant-tier entries in
`/Users/jefffarr/emacs-add-workspaces-package/interfaces.org`.

## Context

- Architect finding: `.orchestrator/cycles/cycle-20260526-171719/findings/arch-cycle-20260526-171719-02.md`
- Originating asks (workspaces evidence):
  - `ask-cycle-20260526-171719-1` (writer-lint heuristic)
  - `ask-cycle-20260526-171719-2` (*scratch* fallback / constructor admits relative paths)
- Reconciliation notes (workspaces evidence):
  - `.orchestrator/cycles/cycle-20260526-171719/reconciliations/invariant-home-required-no-floating-workspaces.md`
  - `.orchestrator/cycles/cycle-20260526-171719/reconciliations/invariant-home-org-user-authored-after-creation.md`
