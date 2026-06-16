---
name: disposition-ask-prefix-arg-vs-separate-command
description: User disposition for gptel-sessions force-global UX divergence — specs/workspaces/spec.md promises prefix-arg, impl ships separate command `jf/gptel-persistent-session-global`
change: add-workspace-home-directory
status: done
relations: []
discovered_by: reviewer-and-architect
discovered_class: spec-signal
discovered_from: ask-cycle-20260525-213500-2
blocker_note: |
  Waiting on user disposition of ask-cycle-20260525-213500-2.
  Provenance: cycle-2 reviewer of gptel-sessions-workspace-consult
  (Finding 1, spec-signal) AND cycle-2 architect end-of-cycle audit
  (arch-cycle-20260525-213500-02, blocking interface-drift against
  load-bearing entry). Both converge on the same UX divergence.
---

## Why this task exists

Both the cycle-2 reviewer of `gptel-sessions-workspace-consult`
and the end-of-cycle Architect audit (finding
`arch-cycle-20260525-213500-02`, blocking, interface-drift against
load-bearing entry) converge on the same UX divergence:

- **Spec** (`specs/workspaces/spec.md:264-270`, "Prefix arg forces
  global save"): `C-u` on the user-facing session-creation command
  binds FORCE-GLOBAL non-nil and routes the new session into the
  global default directory.
- **Design** (`design.md:146-147`): reiterates the prefix-arg
  affordance.
- **Boundary register** (`register/boundary/gptel-sessions-workspace-consult`
  user_facing_command, pre-cycle-2-reconciliation):
  `interactive_form: "P"`, prefix-arg binds FORCE-GLOBAL.
- **Implementation** (cycle 2, commits `87cc457` + `8ce82df`):
  `jf/gptel-persistent-session` uses `current-prefix-arg` for
  *preset selection* (pre-existing UX that predates this work).
  The escape hatch was shipped as a wholly **separate command**
  `jf/gptel-persistent-session-global`. Function-argument contract
  (`force-global` routes correctly) is preserved; the user-facing
  affordance diverges from the spec.

The implementor's deviation is structurally sound (rebinding the
prefix would break preset selection). The cycle-2 architect's
finding routes this to the user as the load-bearing-interface-drift
case.

## User options

| Option | Action | Cost | Risk | Discoverability |
|---|---|---|---|---|
| **A. revise spec + register + design.md to match impl** (architect's recommended) | Update `specs/workspaces/spec.md` "Prefix arg forces global save" scenario to describe the separate `jf/gptel-persistent-session-global` command. Update `design.md` §D3 accordingly. The boundary register's `user_facing_command` is already updated at cycle-2 integrate (see `.orchestrator/cycles/cycle-20260525-213500/reconciliations/boundary-gptel-sessions-workspace-consult.md`). | Small (spec + design.md edits). | None — the impl is sound; the docs catch up. | **Higher**: `M-x jf/gptel-persistent-session-global` shows up in completion. A prefix-arg overload is invisible until learned. |
| **B. re-implement to ALSO accept C-u as force-global toggle** | Modify `jf/gptel-persistent-session`'s interactive form to interpret `C-u` differently from `C-u 4`. E.g., `(interactive "P")` followed by branching: `C-u` → force-global; numeric prefix → preset selection. The separate command can stay or be removed. | Medium (interactive-form refactor; existing preset-selection UX must keep working; test coverage expanded). | Risk of breaking pre-existing preset-selection UX users have learned. | Both affordances available. |
| **C. defer; mark scenario tentative in spec** | Add a note to `specs/workspaces/spec.md:264-270` that the scenario's UX is "tentative — implementation uses separate command pending UX disposition". Block reconciliation of `register/boundary/gptel-sessions-workspace-consult` from `reconciled` (already flipped at cycle-2 integrate) to a `divergent` status for the user_facing_command sub-shape only. Implementations continue to ship the separate command. | Tiny. | The contract stays open across cycles. |

## Recommended disposition

**A (revise spec + design.md).** The architect's recommendation
matches the reviewer's. The implementor's choice has better
discoverability via `M-x` completion (the user sees both the
workspace-aware and the -global variants enumerated); a prefix-arg
overload is invisible until learned and competes for a slot with
the pre-existing preset-selection UX.

## What this task ships

Per the user's disposition:

- **If A**: edit `specs/workspaces/spec.md:264-270` to describe the
  separate-command UX; edit `design.md:146-147` similarly. Commit
  with reference to this task and the architect finding
  `arch-cycle-20260525-213500-02`. The register entry is already
  in shape (cycle-2 reconciled with `escape_hatch_command:
  jf/gptel-persistent-session-global`).
- **If B**: schedule a new task `add-prefix-arg-force-global-toggle`
  in cycle 3+ with the interactive-form refactor + tests covering
  preset-selection-still-works.
- **If C**: edit `specs/workspaces/spec.md` to mark the scenario
  tentative; flip `register/boundary/gptel-sessions-workspace-consult`'s
  `user_facing_command` sub-shape to `divergent` (with a divergence
  note); leave code untouched.

## Verification

Per the chosen path:
- **A**: `grep -n 'prefix arg' specs/workspaces/spec.md` reflects
  the new wording (separate command, not prefix-arg). Both
  `jf/gptel-persistent-session` and `jf/gptel-persistent-session-global`
  are mentioned by name.
- **B**: the interactive-form refactor lands; existing
  preset-selection users see no behaviour change; new tests cover
  the C-u → force-global path. Full gptel/sessions suite green.
- **C**: spec carries the tentative note; register's
  user_facing_command sub-shape carries divergent annotation; no
  code changed.

## Context

- Cycle-2 reviewer findings file:
  `.orchestrator/cycles/cycle-20260525-213500/reviews/gptel-sessions-workspace-consult.md`
  (Finding 1).
- Cycle-2 architect finding file:
  `.orchestrator/cycles/cycle-20260525-213500/findings/arch-cycle-20260525-213500-02.md`.
- Cycle-2 boundary-entry reconciliation note:
  `.orchestrator/cycles/cycle-20260525-213500/reconciliations/boundary-gptel-sessions-workspace-consult.md`.
- This ask carried into cycle-2 integrate as
  `ask-cycle-20260525-213500-2`; resolution will land in the
  cycle-2 handshake artifact's `asks_for_user_resolved` array.
