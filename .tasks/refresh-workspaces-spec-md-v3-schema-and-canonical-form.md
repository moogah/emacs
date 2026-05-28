---
name: refresh-workspaces-spec-md-v3-schema-and-canonical-form
description: openspec/specs/workspaces/spec.md still says :version 2 in four places and is silent on canonical_form for :home / :name / :buffer-files even though register/shape/workspace-plist-v3 now pins all three; resync spec.md to v3 schema and add canonical_form descriptions
source: add-workspace-home-directory (cycle-5)
status: ready
relations:
  - discovered-from:update-docs-readme
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: update-docs-readme (cycle 5) + canonicalize-workspace-home-path-form (cycle 5)
     discovered_by: reviewer (update-docs-readme F2) + architect (end-of-cycle-1) — broadened
     discovered_class: interface-drift
     finding_refs:
       - reviewer-update-docs-readme-F2
       - arch-cycle-20260528-180230-end-of-cycle-1
     source: add-workspace-home-directory -->

## Why this task exists

`openspec/specs/workspaces/spec.md` carries two stale claims:

1. **Schema version lag** (reviewer F2 on update-docs-readme):
   Four locations (lines 25, 262, 270, 284 at cycle-5 close)
   still describe the persistence schema as `:version 2`.
   The cycle-2 task `persistence-schema-v3` shipped v3 and
   updated the persistence module and the new README's
   `Schema v3` section, but the spec.md sweep was missed
   at that change's archive. The update-docs-readme
   implementor stayed in scope per their brief (spec edit
   limited to the Purpose paragraph), so this drift
   re-surfaced visibly in cycle-5 without being closed.

2. **Canonical_form silence** (architect end-of-cycle-1):
   Cycle-5's canonicalize-workspace-home-path-form task
   extended `register/shape/workspace-plist-v3` with
   `canonical_form` sub-fields per string-typed slot
   (`:name`, `:home`, `:buffer-files`). The spec.md
   persistence sections do not mention canonical_form at
   all — even after the v3 version-number resync, the
   spec will remain silent on the trailing-slash /
   expand-file-name / basename pins unless the resync
   explicitly adds them.

The two findings name the same artefact (spec.md). The
end-of-cycle architect broadened the reviewer F2's scope to
include canonical_form. One task; broadened scope.

This is externalised to `.tasks/` rather than filed in the
active change because the drift's root cause is the cycle-2
`persistence-schema-v3` task's incomplete archive sweep, not
this change's scope.

## Files to modify

- `openspec/specs/workspaces/spec.md` — lines 25, 262, 270,
  284 (the four `:version 2` mentions at cycle-5 close;
  re-grep at task-start time in case of drift).
- Same file — persistence sections (around lines 260-290):
  add `canonical_form` per string-typed slot description,
  citing `register/shape/workspace-plist-v3`.

## Implementation steps

1. Read `openspec/specs/workspaces/spec.md` end-to-end to
   internalise its structure and voice.
2. Re-grep `:version 2` to confirm the four hit locations
   haven't drifted: `grep -n ':version 2' openspec/specs/workspaces/spec.md`
3. Read `register/shape/workspace-plist-v3` in `interfaces.org`
   (around line 1035) post-cycle-5 — confirm the
   `canonical_form` sub-fields are at the form you want to
   document (especially if the parallel in-change task
   `reconcile-home-canonical-form-deserialiser-promise`
   landed first and softened the entry's phrasing).
4. Update the four `:version 2` mentions to `:version 3`.
5. Add a paragraph or sub-section in the persistence area
   describing canonical_form per slot. Cite the register
   entry by id; do not duplicate the entry's full text in
   the spec.
6. Sanity grep: `grep -in "canonical\|version" openspec/specs/workspaces/spec.md`
7. The README (post-cycle-5) is already correct on schema v3
   and canonical form; cross-check the spec's new wording
   against the README to avoid contradiction.

## Verification

- `./bin/run-tests.sh -d config/workspaces` → still green
  (no test impact).
- `grep -c ':version 3' openspec/specs/workspaces/spec.md`
  → 4 (matches prior `:version 2` count).
- `grep -c ':version 2' openspec/specs/workspaces/spec.md`
  → 0.
- `grep -in 'canonical' openspec/specs/workspaces/spec.md`
  → at least one mention per string-typed slot covered.

## Design rationale

The spec is for changes; the README is for users; the register
is for contracts. The spec.md drift is a process-pattern
externality (cycle-2's archive sweep should have caught it).
The end-of-cycle architect's broadening is the more important
half: even with the version number fixed, the canonical_form
silence would have remained an interface-drift instance
between two co-equal documents.

This task is externalised because:
- The drift originated outside cycle-5's change.
- The spec.md update is doc-only and does not gate the
  active change's archive.
- The .tasks/ register makes the cross-cutting nature
  explicit (audit-load-bearing-invariant-enforcement-
  completeness is the architect-tier complement).

## Context

- Reviewer finding (origin): `.orchestrator/cycles/cycle-20260528-180230/reviews/update-docs-readme.md` § Finding 2
- Architect end-of-cycle finding (broadening):
  `.orchestrator/cycles/cycle-20260528-180230/findings/arch-cycle-20260528-180230-end-of-cycle.md` § Finding
- Related cycle-3 architect externalised task:
  `.tasks/audit-load-bearing-invariant-enforcement-completeness.md`
  (the invariant-completeness audit complements this
  shape-spec-completeness audit)
- Source change: `openspec/changes/add-workspace-home-directory/`
  (cycle 5)
