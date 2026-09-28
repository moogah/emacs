---
name: doc-sync-cycle2
description: Bring proposal.md, design.md and the delta specs in line with the code and this cycle's user decisions; add the two missing delta specs (workspaces, workspace-integrations)
change: workspace-portable-bookmarks
status: ready
task_class: doc
on_critical_path: true
cycle: cycle-1790453714
cites_register_entries:
  - register/vocabulary/buffer-reincarnation-fallback-chain
  - register/shape/workspace-integration-anchor-payload
  - register/vocabulary/workspace-creation-context
  - register/invariant/bookmark-jump-not-captured-as-working-state
relations:
  - discovered-from:eoc-1790453714-02
  - discovered-from:meta-discovery/doc-sync-lag
discovered_from: [eoc-1790453714-02, meta-discovery/doc-sync-lag]
discovered_by: architect
discovered_class: interface-drift
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714. Blocks `opsx-sync` / `opsx-archive`. USER RULE 2026-09-26: doc corrections where the code is
> right are applied and listed, not asked.

## Steps
1. Apply the 31 corrections listed in `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § Doc corrections (proposal.md 11,
   design.md 13, specs/portable-bookmarks 5, specs/workspace-git-worktrees 2). Verify each against the code at HEAD before applying.
2. design.md D9 per USER ask-cycle-1790453714-3: a jump-triggered restore IS remembered like any find-file; the anti-save predicate was
   removed; what remains is paint-then-show ordering and outer-jump-only target scoping (see the reconciled
   `register/invariant/bookmark-jump-not-captured-as-working-state`).
3. Add `specs/workspaces/spec.md` (MODIFIED: Buffer reincarnation decline gate; workspace-new / anchoring name, basename and remote
   refusals; persistence: remote home loaded broken, never contacted; workspace-purge: strict scope, post-prompt re-check, remote
   unregister-only) and `specs/workspace-integrations/spec.md` (MODIFIED: Anchor payload contract — USER ask-cycle-1790453714-5: NARROW
   the clause: workspaces itself adds no domain keys; a caller invoking a :menu command directly MAY add keys that command documents,
   e.g. `:source-repo`; Creation-time dispatch context vocabulary — six members). Follow the main specs' requirement/scenario style.
4. proposal § Capabilities › Modified Capabilities names workspaces and workspace-integrations; note the core hardening scope.
5. Also correct the README.org Restore / Save-triggers drift only if it falls out naturally (otherwise it stays in `.tasks/`).

## Verification
- `openspec validate workspace-portable-bookmarks` (or the repo's equivalent) passes; every binding user decision of cycles 1-2 is
  greppable in the file it names.
