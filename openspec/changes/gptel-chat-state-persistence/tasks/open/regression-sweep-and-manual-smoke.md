---
name: regression-sweep-and-manual-smoke
description: End-to-end regression of all gptel tests plus manual smoke test of create, save-with-menu-change, reopen, branch, and agent creation.
change: gptel-chat-state-persistence
status: ready
relations:
  - "blocked-by:delete-metadata-module"
---

## Files to modify

None (verification task — no code changes). If the sweep reveals bugs, open follow-up tasks rather than modifying here.

## Implementation steps

1. Run `./bin/run-tests.sh` (full suite) — confirm green.
2. Run `./bin/run-tests.sh -d config/gptel` — confirm green.
3. `grep -rn "metadata\\.yml" config/` — confirm no remaining live references. Stray comments that document the removal are acceptable; flag anything that looks like a reader or writer.
4. `grep -rn "metadata\\.yml" openspec/` — confirm specs and current change's own docs are the only references, and those are explicit "removed" / historical mentions.
5. Manual smoke — create a fresh session:
   - `M-x jf/gptel-persistent-session` → pick a preset.
   - Inspect the resulting `session.org`: should start with `:PROPERTIES:\n:GPTEL_PRESET: <name>\n:END:\n#+begin_user\n\n#+end_user\n`.
   - Confirm no `metadata.yml` in `branches/main/`.
6. Manual smoke — `gptel-menu` persistence:
   - In the session buffer, `M-x gptel-chat-menu` (or `C-c C-,`), add a tool, apply.
   - `C-x C-s` (save).
   - Inspect the file on disk: the drawer now contains `:GPTEL_TOOLS:` with the new tool listed.
   - Close the buffer (`C-x k`), reopen the file.
   - Check `gptel-tools` buffer-local: the added tool is still there.
7. Manual smoke — branch:
   - From the existing session, `M-x jf/gptel-branch-session`.
   - Inspect the new branch directory: `session.org` has the same drawer; `branch-metadata.yml` present; `metadata.yml` NOT present.
8. Manual smoke — agent:
   - Trigger a `PersistentAgent` sub-agent creation (e.g., from a tool call that creates one).
   - Inspect the agent `session.org`: drawer contains `:GPTEL_PRESET:` AND `:GPTEL_PARENT_SESSION_ID:`.
   - Confirm no `metadata.yml` in the agent directory.
9. If any smoke step fails, create a follow-up task (via `/opsx-tasks`) describing the failure; do not patch under this task.
10. If all steps pass, mark task complete and hand off for archive.

## Design rationale

A purely behavioral task: once module deletion is complete, we must confirm the whole path works end-to-end. The manual smoke covers the four session shapes (create, save-with-overrides, branch, agent) because purely-automated tests stub at the upstream boundary — the integration test is the user-observable "it works on my machine" evidence.

## Verification

- Full `./bin/run-tests.sh` passes.
- Manual smoke steps all succeed.
- No `metadata.yml` produced or read anywhere during the smoke.

## Context

- proposal.md §Impact (tests and manual verification)
- architecture.md §"Testing Approach" (Running Tests)
- design.md §"Migration Plan" (no migration required; smoke verifies acceptably degraded path for drawerless files)
