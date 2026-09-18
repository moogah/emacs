---
name: disposition-edge-drawer-meta-and-placement
description: User decision on the divergent invariants edge-drawer-immediately-after-property-drawer (an EDGES drawer at the pinned spot is parsed as vulpea META) and write-edge-idempotent (a hand-authored drawer elsewhere yields a second drawer); blocks the cycle-2 tasks that write edges onto META-bearing or migration-era notes.
change: workspace-notes-in-vault
status: blocked
task_class: contract
on_critical_path: true
cites_register_entries:
  - register/invariant/edge-drawer-immediately-after-property-drawer
  - register/invariant/write-edge-idempotent
  - register/boundary/edge-writer-api
relations:
  - discovered-from:ask-cycle-1789334430-8
  - discovered-from:ask-cycle-1789334430-13
  - discovered-from:arch-cycle-1789334430-ot-edge-writer-2
  - blocks:int-workspace-commands
  - blocks:int-migration-command
discovered_from: arch-cycle-1789334430-ot-edge-writer-2
discovered_by: architect
discovered_class: interface-drift
blocker_note: user decision — both invariants are status divergent in interfaces.org until dispositioned
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate. Two register invariants cited by
> `og-edge-writer-and-project-note` are `divergent` (see
> .orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md
> and invariant-write-edge-idempotent.md). Cycle-2 tasks `int-workspace-commands`
> (new-in-workspace) and `int-migration-command` write edges onto META-bearing and
> migration-era notes and must not start until this is decided. `int-gptel-org-graph` writes
> onto session notes only (no META today) and may proceed.

## Decision 1 — ask-cycle-1789334430-8: EDGES drawer shadows vulpea META
`vulpea-db--extract-meta` (runtime/straight/repos/vulpea/vulpea-db-extract.el ~:816) takes the
first descriptive list before the first headline and descends into drawers, so an EDGES drawer
placed directly after the property drawer becomes the note's META (probe-confirmed at file and
heading level). Schema fields read via `vulpea-note-meta-get` are shadowed.
- (a) Advise `vulpea-db--extract-meta` to skip drawer descriptive lists (parser-hardening
  slot in config/org-graph/discovery.org; one advice, spec with a META-bearing note carrying
  an EDGES drawer). Keeps D7 placement and the branch byte-copy argument. **Orchestrator
  recommendation.**
- (b) Place the file-level EDGES drawer after META / before the first headline (revises D7;
  weakens the branch byte-copy argument; migration must handle both placements).
- (c) Accept with a policy note: session and project notes carry no META today.

## Decision 2 — ask-cycle-1789334430-13: drawer elsewhere breaks idempotency
`org-graph/write-edge` recognises only a drawer on the line after `:END:`; a hand-authored
EDGES drawer elsewhere under the node gets a second drawer and duplicate tuples (migration-era
files). Proposed: dedupe/append against the node's existing drawer wherever the extractor would
accept it; create at the pinned spot only when none exists. Interacts with Decision 1.

## On disposition
- Record the choices in the next cycle's handshake `asks_for_user_resolved` and flip both
  register entries from `divergent` (reconciled or confirmed with policy note).
- If (a)/(dedupe) are chosen, create an in-change follow-up task
  `og-edge-writer-meta-safe-and-dedupe` (files: config/org-graph/discovery.org,
  config/org-graph/authoring.org, config/org-graph/test/authoring-spec.el) and make
  `int-workspace-commands` / `int-migration-command` blocked-by it instead of this task.
