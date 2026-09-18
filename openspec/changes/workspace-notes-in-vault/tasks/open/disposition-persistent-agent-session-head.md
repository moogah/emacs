---
name: disposition-persistent-agent-session-head
description: User decision on ask-cycle-1789334430-12 — PersistentAgent sessions bypass jf/gptel--create-session-core (no #+title/#+filetags, no session-created hook) while the sessions-persistence delta says every session SHALL emit them at creation.
change: workspace-notes-in-vault
status: blocked
task_class: contract
on_critical_path: false
cites_register_entries:
  - register/shape/session-file-head
  - register/boundary/session-lifecycle-hooks
relations:
  - discovered-from:ask-cycle-1789334430-12
  - relates-to:int-workspaces-gptel
  - relates-to:int-migration-command
discovered_from: review-gptel-session-keywords-and-hooks-F1
discovered_by: reviewer
discovered_class: spec-signal
blocker_note: user decision — spec wording vs producer routing
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate. `register/shape/session-file-head`
> was reconciled recording PersistentAgent as a deliberate second producer variant (regions 1
> and 6 only: drawer + body; no keywords; no hook) pending this decision.

## Options
- (a) Narrow the delta spec (openspec/changes/workspace-notes-in-vault/specs, sessions-persistence
  "SHALL emit at creation") to user-facing sessions. Agent transcripts live under `.agents/`,
  are hidden from enumeration and unindexed by org-graph, so keywords and the hook buy nothing.
  Implementor and reviewer both lean this way. **Orchestrator recommendation.**
- (b) Route agent creation through the core body builder (`jf/gptel--initial-session-body`)
  and run the hook; do it in `int-workspaces-gptel` or a small follow-up. Costs: filetag
  semantics for hidden transcripts, and every hook subscriber must tolerate agent sessions.

## On disposition
(a): edit the delta spec sentence; append a confirmation line to the register entry's
status_note; close. (b): create follow-up `gptel-persistent-agent-through-core` (files:
config/gptel/tools/persistent-agent.org, config/gptel/sessions/commands.org, specs) and
re-point `int-migration-command`'s agent-dir step if it assumes the variant.
