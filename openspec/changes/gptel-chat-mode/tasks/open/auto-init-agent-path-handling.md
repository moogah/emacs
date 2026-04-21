---
name: auto-init-agent-path-handling
description: Fix nested per-branch agent path handling (session-id, session-dir, parent-session-id) and add real assertions to the agent-path spec
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-auto-init
---

## Files to modify
- `config/gptel/sessions/commands.org` (`jf/gptel--auto-init-session-buffer`
  agent-path branch and buffer-local-var setup)
- `config/gptel/sessions/commands.el` (re-tangled)
- `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el`
  (tighten assertions; add flat + nested coverage)

## Implementation steps
1. **Carve out a dedicated nested-path regex** that matches
   `.../<session-id>/branches/<branch>/agents/<agent>/session.org`
   and captures session-id, branch-name, and agent-name explicitly.
   Keep the existing flat regex
   (`.../<session-id>/agents/<agent>/session.org`) as a separate
   branch.
2. In the nested branch:
   - `session-dir` = the session-id directory (walk up three levels
     from `session.org` past `agents/<agent>/branches/<branch>`).
   - `session-id` = from the path capture (not
     `jf/gptel--session-id-from-directory` on the agent directory).
   - `branch-name` = from the path capture (not hardcoded `"main"`).
   - Do NOT invoke `jf/gptel--update-current-symlink` when the agent
     directory has no `branches/` subdirectory — or redirect it to
     the branch directory explicitly.
3. **Populate `jf/gptel--parent-session-id`**: after reading
   `metadata.yml` via `jf/gptel--read-session-metadata`, pull
   `:parent_session_id` and `setq-local` the buffer-local when
   non-nil. Also expose via `jf/gptel--get-parent-session-id` helper
   if one already exists.
4. Re-tangle.
5. **Tighten existing agent-path test** at
   `auto-init-chat-mode-spec.el:123-158`:
   - Change the path to
     `.../foo-20260420000000/branches/main/agents/researcher-20260420120000-explore/session.org`.
   - Replace `(expect jf/gptel--session-id :to-be-truthy)` with
     `(expect jf/gptel--session-id :to-equal "foo-20260420000000")`.
   - Replace hardcoded `branch-name = "main"` assertion with a real
     capture check against the path.
6. Add a **separate flat-layout spec** for
   `.../foo-20260420000000/agents/researcher-.../session.org` (no
   `branches/` in the path) asserting `session-id = "foo-..."` and
   `branch-name = "main"` (the expected default for legacy flat
   layout).
7. Add a **parent-session-id spec**: open a branch session whose
   `metadata.yml` declares a `parent_session_id`; assert the
   buffer-local is populated correctly.

## Design rationale
The current agent-path branch sets `session-dir = agent-dir` and
relies on `jf/gptel--session-id-from-directory` walking up from that
directory, which returns the agent's own directory name instead of
the parent session-id. This corrupts the registry key, creates a
stray `current → branches/main` symlink inside the agent directory,
and hardcodes `branch-name` to `"main"` regardless of the real
branch. The existing spec masks this because it only asserts
`session-id :to-be-truthy`.

`jf/gptel--parent-session-id` is listed as a buffer-local in
Decision 17 step 2 and in both specs, but no code path populates it
— branching writes `parent_session_id` into `metadata.yml`, but
auto-init never reads it back.

**Blocking follow-up** — `sessions-activities` (agents under a
specific branch) and `sessions-branching` (needs reliable
parent-session lookup) both depend on this fix. Re-pointed from
`sessions-auto-init`.

## Verification
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes,
  including the three new/tightened specs.
- Manual: open a branch-nested agent session; confirm
  `jf/gptel--session-id` equals the parent session-id, `branch-name`
  equals the real branch, `jf/gptel--parent-session-id` is populated,
  and no stray symlinks are created.
- `grep -n ":to-be-truthy" config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el`
  returns no hits against session-id (replaced with `:to-equal`).

## Context
- Review of `sessions-auto-init` (2026-04-21, orch-review-1776774164),
  Findings #2, #3, #4 (all blocking, grouped).
- design.md §Decision 17 step 2.
- `openspec/specs/gptel/sessions-persistence.md` — agent-path shape
  is under-specified; this task may surface a spec update too.
- architecture.md §`sessions/commands`.
