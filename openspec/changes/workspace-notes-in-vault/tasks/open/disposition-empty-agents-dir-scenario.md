---
name: disposition-empty-agents-dir-scenario
description: User decision on ask-cycle-1789334430-9 — the sessions-branching delta scenario "Empty agents directory" says a child SHALL have an empty .agents/ when the parent has none, but jf/gptel--copy-branch-agents only mkdirs when the parent's exists; the spec passes either way.
change: workspace-notes-in-vault
status: blocked
task_class: test
on_critical_path: false
cites_register_entries:
  - register/vocabulary/session-tree-directory-names
relations:
  - discovered-from:ask-cycle-1789334430-9
  - relates-to:gptel-branch-identity
discovered_from: review-gptel-hidden-agents-F1
discovered_by: reviewer
discovered_class: spec-signal
blocker_note: user decision — relax the scenario or make the mkdir unconditional
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate (reviewer spec-signal on
> `gptel-hidden-agents` plus implementor discovery disc-gptel-hidden-agents-3).

## Options
- (a) Relax the scenario to "branching SHALL NOT fail and the child SHALL list no agents" and
  tighten the test to assert the listing, not the directory. Cheapest; matches the code.
  **Orchestrator recommendation.**
- (b) Make the child `.agents/` mkdir unconditional in `jf/gptel--copy-branch-agents`
  (config/gptel/sessions/branching.org) and assert the directory exists.

## On disposition
Either choice is a one-file edit plus a spec tweak in
config/gptel/sessions/test/filesystem/hidden-agents-dir-spec.el (or the branching spec). Fold
into `gptel-branch-identity` (which edits branching.org) rather than a separate task.
