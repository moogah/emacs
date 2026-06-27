---
name: reconcile-home-canonical-form-deserialiser-promise
description: Soften register/shape/workspace-plist-v3's :home canonical_form prose to describe verbatim-preservation at the persistence boundary, OR wire the home-not-canonical validator clause into the deserialiser; converged advisory finding from on-touch architect + reviewer F2 of canonicalize-workspace-home-path-form
change: add-workspace-home-directory
status: done
relations:
  - discovered-from:canonicalize-workspace-home-path-form
---

## Resolution (2026-06-06, manual review)

Applied **Option A** (architect-preferred). Confirmed empirically
against `config/workspaces/persistence.el:94-114`: the deserialiser
rejects only missing/non-string and non-absolute `:home` with a
`*Messages*` notice, tags missing-directory `:home` broken (also with
a notice), and otherwise preserves `:home` verbatim — a non-canonical-
but-absolute hand-edited path loads silently with no rewrite. The
register entry previously over-promised that such a path would
"surface in `*Messages*`".

Edited `register/shape/workspace-plist-v3` in `interfaces.org` (all
hunks within the single entry):
- `:home` `canonical_form` sub-field — dropped the false `*Messages*`
  promise; reframed canonical_form as an IN-SESSION guarantee
  (`workspace--make` / `workspace--set-home`) and described the
  loader's deliberate verbatim-preservation exception.
- deserialiser producer note — rewritten to state accurately what is
  rejected vs. preserved verbatim.
- `validator` block comment — added a SCOPE note: the
  `home-not-canonical` clause is documentation-only (no production
  caller) and is NOT run by the deserialiser. The clause itself is
  retained as the in-session shape contract.
- `status: reconciled-recommended → reconciled`; prepended a
  `status_note` documenting the disposition.

No code change (verbatim-preservation is intentional; Option B would
collide with `broken-tag-runtime-only` and surprise users with
hand-edited files). The same canonical_form-silence shape at
`spec.md` is already externalised to
`.tasks/refresh-workspaces-spec-md-v3-schema-and-canonical-form`
(verified present).

Verified:
- `./bin/run-tests.sh -d config/workspaces` → 231 passed (register
  edit; count unchanged from the prior batch, which had removed the
  writer-lint spec — the task's "232" predated that removal).
- `git diff interfaces.org` touches only the `workspace-plist-v3`
  entry (status_note, `:home` description, producers note, validator).
- No remaining promise of a `*Messages*` notice for a non-canonical
  persisted `:home`.

<!-- Provenance fields (orchestrator schema):
     discovered_from: canonicalize-workspace-home-path-form (cycle 5)
     discovered_by: architect (on-touch) + reviewer (F2 spec-signal) — converged
     discovered_class: interface-drift
     finding_refs:
       - arch-cycle-20260528-180230-on-touch-canonicalize
       - reviewer-canonicalize-workspace-home-path-form-F2
     reconciled_into: register/shape/workspace-plist-v3 (load_bearing: true) -->

## Why this task exists

Cycle-5's `canonicalize-workspace-home-path-form` added a
`canonical_form` sub-field to every string-typed slot of
`register/shape/workspace-plist-v3` and pinned a
`home-not-canonical` clause in the entry's `validator` block.
The on-touch architect (`arch-cycle-20260528-180230-on-touch-
canonicalize`, advisory interface-drift) and the reviewer
(`canonicalize-workspace-home-path-form` F2, spec-signal)
**independently arrived at the same root cause**: the entry's
prose and the `producers` note **promise** that non-canonical
persisted `:home` values will "surface in `*Messages*`", but
the persistence deserialiser at
`config/workspaces/persistence.el:94-114` only emits notices
for *missing* and *not-absolute* `:home`. The canonical-form
check is documented-only YAML with no production caller.

A pre-cycle-5 hand-edited persistence file with non-canonical
`:home` loads silently. The entry over-promises a surfacing
channel the deserialiser does not provide.

This is a single converged finding (cycle-5 dual-channel-
convergence pattern); one follow-up task, not two.

## Decision pivot — two viable resolutions

### Option A (preferred by architect): Soften the entry's phrasing

Verbatim-preservation at the deserialiser is intentional —
it avoids silently rewriting user persistence files. The
entry should describe the actual behaviour rather than
aspire to a future check.

Concretely:
- Edit `register/shape/workspace-plist-v3` `:home` description:
  drop "surface in `*Messages*`" promise; replace with a
  sentence describing canonicalisation at the attachment
  points (`workspace--make`, `workspace--set-home`) and
  verbatim-preservation at the deserialiser.
- Edit the `producers` note: distinguish the three in-session
  producers (canonical by construction) from the
  deserialiser (preserves whatever the persistence file
  carried).
- Drop the `home-not-canonical` clause from the `validator`
  block, OR convert it to a soft-check that the spec/scaffold
  layer rather than runtime ever uses.

Cost: ~30 lines of YAML/prose edit in `interfaces.org`. No
code change.

### Option B (conservative): Wire the validator into the deserialiser

Make the entry's promise true: have the deserialiser emit a
`*Messages*` notice when a persisted `:home` is non-canonical
(and either rewrite to canonical in the in-memory plist or
leave as-is).

Concretely:
- Modify the deserialiser to check
  `(string= home (file-name-as-directory (expand-file-name home)))`
  before puthash; emit a notice if the check fails.
- Decide whether to coerce to canonical at load (mutates the
  in-memory plist relative to the persisted file — surprises
  on next save) or leave-as-is and flag (the entry's
  `canonical_form` invariant is then violated in-memory by
  load, which contradicts the entry's strengthened pin).

Cost: ~10 lines of elisp + a new buttercup spec.
Risk: changing the persistence-load behaviour mid-pre-alpha
may surprise users with edited persistence files; the
mutate-on-load variant has its own canonical_form-invariant
contradiction.

## Architect recommendation

**Option A.** The verbatim-preservation choice is intentional,
the entry should describe behaviour rather than aspire to a
future check, and the change cost is contained to the register.

## Files to modify (Option A)

- `interfaces.org` — `register/shape/workspace-plist-v3`:
  - `:home` `canonical_form` sub-field description.
  - `producers` note (three-producer enumeration; deserialiser
    annotated as verbatim-preserving).
  - `validator` block — soften or drop the `home-not-canonical`
    clause.

## Implementation steps

1. Read `register/shape/workspace-plist-v3` in `interfaces.org`
   (around line 1035, but read the whole entry — the diff in
   cycle-5 merge `6afd70c` extended it substantially).
2. Read the converged findings:
   - `.orchestrator/cycles/cycle-20260528-180230/findings/arch-cycle-20260528-180230-on-touch-canonicalize.md`
   - `.orchestrator/cycles/cycle-20260528-180230/reviews/canonicalize-workspace-home-path-form.md` § Finding 2
3. Apply Option A edits to the entry. Cross-check against the
   actual deserialiser code at
   `config/workspaces/persistence.el:94-114` so the new prose
   is empirically true.
4. No code changes; no test changes.

## Verification

- `./bin/run-tests.sh -d config/workspaces` → 232 specs, 0
  failed (test count unchanged; this is a register edit).
- `grep -n "Messages" interfaces.org` near the
  `workspace-plist-v3` entry → no remaining promise of a
  notice for non-canonical persisted `:home`.
- Diff sanity: `git diff interfaces.org` should touch only
  the `workspace-plist-v3` entry's `:home` description,
  `producers` note, and `validator` block.

## Design rationale

The convergence of the on-touch architect's narrow-scope
audit and the reviewer's diff-against-design-drift direction
is itself a process signal: when both channels independently
name the same root cause, the routing rule "one follow-up
task, not N" is the right default. This task is the first
application of that rule.

Option A is preferred because changing the deserialiser's
load behaviour (Option B) interacts with the
`broken-tag-runtime-only` invariant (load-time mutations
surface in subtle ways), and the verbatim-preservation
choice is a documented intent.

## Context

- Convergent finding (architect): `arch-cycle-20260528-180230-on-touch-canonicalize`
- Convergent finding (reviewer): `.orchestrator/cycles/cycle-20260528-180230/reviews/canonicalize-workspace-home-path-form.md` § Finding 2
- End-of-cycle architect: `arch-cycle-20260528-180230-end-of-cycle-1`
  notes that **the same shape recurs at spec.md** (silent on
  canonical_form). The spec.md scope is externalised to
  `.tasks/refresh-workspaces-spec-md-v3-schema-and-canonical-form`;
  this task is in-change because it finishes a load_bearing
  entry's cleanup before the change archives.
- Cited register entry: `register/shape/workspace-plist-v3`
  (`interfaces.org:1035`, `load_bearing: true`)
