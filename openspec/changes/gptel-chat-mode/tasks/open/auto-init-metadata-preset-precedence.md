---
name: auto-init-metadata-preset-precedence
description: Ensure metadata.yml preset wins over the chat-mode hook's property-drawer re-apply
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-auto-init
---

## Files to modify
- `config/gptel/sessions/commands.org` (`jf/gptel--auto-init-session-buffer`
  ordering, or a new guard variable for the chat-mode hook)
- `config/gptel/sessions/commands.el` (re-tangled)
- `config/gptel/chat/menu.org` or the file that defines
  `gptel-chat--apply-declared-preset` (if a hook-side skip is chosen)
- `config/gptel/sessions/test/commands/preset-application-spec.el`
  (add a real-mode integration spec; remove any stub that replaces
  `gptel-chat-mode` with a no-op lambda that skips its hook)

## Implementation steps
Choose ONE of the two remediation paths below; the review called out
both as viable. Prefer the ordering swap (simpler), escalate to a
hook-side skip if ordering causes other problems (e.g. preset depends
on mode being active).

### Path A — swap the order (simpler)
1. In `jf/gptel--auto-init-session-buffer`, move the
   `jf/gptel--ensure-mode-once` call BEFORE reading `metadata.yml` and
   applying its preset. Result: chat-mode activation (and its hook's
   drawer-preset re-apply) runs first; metadata.yml's apply runs last
   and wins.

### Path B — hook-side skip flag
1. Introduce a dynamic/lexical let-bound flag, e.g.
   `gptel-chat--auto-init-in-progress`, bound to `t` in
   `jf/gptel--auto-init-session-buffer` around the mode activation +
   preset apply.
2. `gptel-chat--apply-declared-preset` checks the flag and early-returns
   when set (auto-init is the authoritative preset source).
3. Document the flag contract in a docstring on both functions.

### Common to both paths
4. Re-tangle.
5. Write a **real-mode** integration spec in
   `preset-application-spec.el`:
   - Create a session directory with `metadata.yml` declaring preset A.
   - Create `session.org` whose `:PROPERTIES:` drawer declares preset B.
   - Open the file, triggering real `find-file-hook` and real
     `gptel-chat-mode` activation (do NOT stub the mode).
   - Assert the buffer-local preset values match preset A, not B.
6. Remove or refactor any existing test that stubs `gptel-chat-mode` as
   a no-op lambda — those masked this bug.

## Design rationale
Decision 16 point 2 and the MODIFIED "Auto-initialization enables
`gptel-chat-mode`" scenario in
`openspec/specs/gptel/sessions-persistence.md` both require metadata.yml
to be authoritative over the property drawer. The current code applies
metadata first, then activates chat-mode, and chat-mode's hook
(`gptel-chat--apply-declared-preset` at `config/gptel/chat/menu.el:185`)
re-reads the drawer with the same buffer-local setter, silently
clobbering the metadata values. The `preset-application-spec.el` suite
only passed because it stubbed `gptel-chat-mode` as a no-op, preventing
the hook from running.

**Blocking follow-up** — `sessions-activities` and `sessions-branching`
both rely on correct preset on session open (activities re-opens on
resume; branching opens newly-created branches whose preset is
inherited from metadata.yml). Re-pointed from `sessions-auto-init`.

## Verification
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes,
  including the new real-mode integration spec.
- Manual: create a session directory with divergent preset in
  metadata.yml and drawer; opening the file applies the metadata
  preset.
- `grep -n "cl-letf.*gptel-chat-mode.*lambda" config/gptel/sessions/test/`
  returns no hits in preset-application-spec (the stub is gone).

## Context
- Review of `sessions-auto-init` (2026-04-21, orch-review-1776774164),
  Finding #1 (blocking drift).
- design.md §Decision 16 point 2.
- `openspec/specs/gptel/sessions-persistence.md` MODIFIED scenario.
- `config/gptel/chat/menu.el:185` — the hook that currently clobbers.
