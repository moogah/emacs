---
name: disposition-cycle2-design-confirmations
description: Four design confirmations for cycle-2 tasks (asks 4-7 — D8 parent-ID read, D3 breaking home-builder signature, INIT-AND-COMMIT? fate, boundary-lint replacement); the target tasks proceed as designed unless the user objects.
change: workspace-notes-in-vault
status: blocked
task_class: doc
on_critical_path: false
relations:
  - discovered-from:ask-cycle-1789334430-4
  - discovered-from:ask-cycle-1789334430-5
  - discovered-from:ask-cycle-1789334430-6
  - discovered-from:ask-cycle-1789334430-7
  - relates-to:int-gptel-org-graph
  - relates-to:ws-inversion-points
  - relates-to:ws-scaffold-and-anchor
  - relates-to:int-scaffold-and-lint
discovered_from: plan-anchor-validation-cycle-1789334430
discovered_by: architect
discovered_class: interface-drift
blocker_note: user confirmation; does NOT block the related tasks — each carries the default in its Cycle 1 updates stanza
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate from the plan-phase asks. These are
> confirmations, not open designs: the orchestrator has written the recommended reading into
> each target task's "Cycle 1 updates" stanza and the tasks are NOT blocked on this file.
> Object here (or in the next cycle's handshake) to change the default.

1. **ask-4 (int-gptel-org-graph).** D8 reads the parent's file-level `:ID:` "from the parent's
   drawer head", but `jf/gptel--read-session-drawer-head` returns only `:GPTEL_*:` keys.
   Default: the subscriber does its own read of the file-level property drawer (or the
   session-created payload gains a `:parent-id` key in gptel-branch-identity). Confirm.
2. **ask-5 (ws-inversion-points).** D3 changes `workspace-home-builder`'s argument from NAME
   to the workspace record; tabs.org:212-218 promised custom builders keep working. Default:
   breaking by design; update that prose. Confirm.
3. **ask-6 (ws-scaffold-and-anchor).** Fate of `INIT-AND-COMMIT?` once only mkdir + git init
   remain. Default: retire the switch (register `workspace-scaffold-pipeline` speculates
   retired-or-vestigial). Confirm.
4. **ask-7 (int-scaffold-and-lint).** D10 deletes `gptel-integration-spec.el`; the
   directionality lint must be REPLACED by `boundary-lint-spec.el` that greps tangled .el for
   foreign prefixes on all three sides (workspaces, gptel/sessions, org-graph; bare
   `workspace`, not `workspace-`). Cycle-1 audit confirmed the gap is wider than stated:
   neither config/workspaces/test nor config/org-graph/test greps today. Default: hard
   precondition for closing the change. Confirm.
