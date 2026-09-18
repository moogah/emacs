---
name: disposition-design-doc-corrections
description: User disposition of four design.md drifts found in cycle-1789334430 (D4 stage numbers, D10 spec path, D5 hook-vs-registry ordering, D6 parser seam); orchestrator recommends applying all four as doc-only corrections.
change: workspace-notes-in-vault
status: blocked
task_class: doc
on_critical_path: false
relations:
  - discovered-from:ask-cycle-1789334430-1
  - discovered-from:ask-cycle-1789334430-2
  - discovered-from:ask-cycle-1789334430-3
  - discovered-from:ask-cycle-1789334430-10
  - discovered-from:arch-cycle-1789334430-eoc-1
  - discovered-from:arch-cycle-1789334430-ot-parser-hardening-1
discovered_from: arch-cycle-1789334430-eoc-1
discovered_by: architect
discovered_class: interface-drift
blocker_note: user decision — confirm or veto each correction; no code change is involved
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate. Blocked on the user. When
> dispositioned, apply the accepted edits to `openspec/changes/workspace-notes-in-vault/design.md`
> inline (no worktree needed) and close this task.

## Decisions requested
1. **ask-cycle-1789334430-1 — D4 stage numbering.** D4 numbers the removed scaffold stages
   "4-6"; the live pipeline (config/workspaces/scaffold.org:157-224) has five stages and the
   removed ones are 3-5. Register entry `workspace-scaffold-pipeline` already carries the
   correct numbering. **Recommend: apply.**
2. **ask-cycle-1789334430-2 — D10 spec path.** D10 lists `workspace-routing-spec.el` under
   config/workspaces/test/; it lives at config/gptel/sessions/test/workspace-routing-spec.el.
   **Recommend: apply.**
3. **ask-cycle-1789334430-3 / eoc-1 — D5 hook ordering.** D5 (design.md ~:150) says the
   session-created hook runs after the registry entry exists. It runs BEFORE: the hook fires at
   the end of `jf/gptel--create-session-core` (commands.el ~:660) after the file, the file-level
   :ID: and the sibling are on disk, with no visiting buffer; registration happens caller-side
   in the chat-mode binder at find-file time. `register/boundary/session-lifecycle-hooks`
   (reconciled this cycle) pins the caller-side reading. Options: (a) amend D5 to "after the
   sibling file and org :ID: are on disk; before the buffer is displayed or registered" and
   require cycle-2 subscribers never to read `jf/gptel--session-registry` at hook time;
   (b) move registration into the core. **Recommend (a).**
4. **ask-cycle-1789334430-10 / ot-parser-hardening-1 — D6 parser seam.** D6 (design.md ~:193)
   names `vulpea-db--parse-with-temp-buffer` / `org-graph--parse-without-local-variables`. The
   shipped seam is `:around org-graph--vulpea-parse-safely` on the dispatcher
   `vulpea-db--parse-file` (covers temp-buffer, find-file and forced find-file). The register
   entry is reconciled; only the design text is stale. **Recommend: apply.**

## Verification
```
grep -n 'stages 4\|4–6\|4-6' openspec/changes/workspace-notes-in-vault/design.md
grep -n 'workspace-routing-spec' openspec/changes/workspace-notes-in-vault/design.md
grep -n 'registry entry' openspec/changes/workspace-notes-in-vault/design.md
grep -n 'parse-with-temp-buffer\|parse-without-local-variables' openspec/changes/workspace-notes-in-vault/design.md
```
