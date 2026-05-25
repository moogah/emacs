---
name: disposition-ask-persistence-schema-version-constant-name
description: User disposition for design.md/task-body naming drift — `workspace--persistence-schema-version` vs actual constant `workspace--state-version`
change: add-workspace-home-directory
status: blocked
relations: []
discovered_by: reviewer
discovered_class: spec-signal
discovered_from: ask-cycle-20260525-213500-1
blocker_note: |
  Waiting on user disposition of ask-cycle-20260525-213500-1.
  Provenance: cycle-2 reviewer of persistence-schema-v3 (clean
  review except this single spec-signal finding). See body for
  options and recommended disposition.
---

## Why this task exists

Cycle-2 reviewer of `persistence-schema-v3` (clean review except
this one finding) flagged a documentation/code naming drift. The
reviewer's evidence:

- `openspec/changes/add-workspace-home-directory/design.md:177` and
  `:360` refer to the persistence schema-version constant as
  `workspace--persistence-schema-version`.
- The actual constant in `config/workspaces/persistence.el` is
  named `workspace--state-version` (kept from the v1→v2 cutover).
- The task body for `persistence-schema-v3` (now closed at commit
  `8ac1073`) also referred to the design.md name as an alias
  (`disc-persistence-schema-v3-d1`).

This is exactly analogous to cycle-1's
`workspace--home-broken-p` → `workspace--broken-p` resolution
(closed at commit `36ae299` with disposition `apply-now`: doc-only
rename in design.md).

## User options

| Option | Action | Cost | Risk |
|---|---|---|---|
| **A. doc-only rename in design.md + task body** (recommended) | Update the 4 design.md/task references to use the actual constant name `workspace--state-version`. Add a one-line note that the constant retains its v2-era name and is generic across schema versions. | Tiny (4 line edits). | Minimal — design.md is reference material, not load-bearing for runtime. |
| **B. code rename of the constant** | Rename `workspace--state-version` → `workspace--persistence-schema-version` everywhere (persistence.el, all spec files, byte-equivalence test fixtures). | High (cross-module rename; cascade audit again). | The naming churn ships breakage if any caller is missed. |
| **C. defer with register marked divergent** | Annotate `register/shape/workspace-plist-v3` and the schema invariant with a `divergent` note that design.md and code disagree on the name. Leave both names "valid". | Low. | The drift stays open; future readers must learn the alias. |

## Recommended disposition

**A (doc-only rename).** Matches the cycle-1 precedent for
`workspace--home-broken-p`. The constant's existing name is
generic and accurate; the design.md prose was speculative naming
that has been superseded by the implementation.

## What this task ships

Per the user's disposition:

- **If A**: edit `design.md:177`, `design.md:360`, and the two
  task-body lines (now archived in
  `tasks/closed/persistence-schema-v3.md:18` and `:263`) — replace
  `workspace--persistence-schema-version` with
  `workspace--state-version`. Commit with reference to this task and
  the user disposition. Update `register/shape/workspace-plist-v3`'s
  `status_note` only if user disposition records the alias for
  future readers.
- **If B**: schedule a new task `rename-state-version-constant` in
  cycle 3+ with a full cascade audit (analogous to
  persistence-schema-v3's cascade self-audit).
- **If C**: write a `divergence-note` in
  `register/shape/workspace-plist-v3` and the parent invariant,
  then close this task.

## Verification

Per the chosen path:
- **A**: `grep -n 'workspace--persistence-schema-version' design.md openspec/changes/add-workspace-home-directory/` returns zero matches after the edit.
- **B**: `grep -rn 'workspace--state-version' config/` returns zero matches; full test suite green.
- **C**: register's `status_note` carries the divergence; no code touched.

## Context

- Cycle-2 reviewer findings file:
  `.orchestrator/cycles/cycle-20260525-213500/reviews/persistence-schema-v3.md`
- Analogous cycle-1 ask resolution: commit `36ae299`.
- This ask carried into cycle-2 integrate as
  `ask-cycle-20260525-213500-1`; resolution will land in the
  cycle-2 handshake artifact's `asks_for_user_resolved` array (and
  the corresponding `task_refinements` entry will mark this task
  done).
