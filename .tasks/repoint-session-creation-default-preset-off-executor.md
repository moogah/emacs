---
name: repoint-session-creation-default-preset-off-executor
description: The interactive session-creation commands (jf/gptel-persistent-session and -global) still default the preset to 'executor, which is no longer a registered preset (its <name>/preset.el never existed; the legacy executor.md was deleted in cycle-1781944619). A no-prefix invocation creates a session bound to an unresolvable preset (scope resolution logs "No scope configuration available for preset: executor"). Repoint the default to a registered preset and update the ~10 specs asserting :GPTEL_PRESET: executor.
source: gptel-fragment-presets (cycle-1781944619, delete-old-presets)
status: externalised
task_class: bug
cites_register_entries:
  - register/boundary/preset-org-to-registration
relations:
  - "discovered-from:delete-old-presets"
discovered_from: delete-old-presets
discovered_by: reviewer
discovered_class: dead-branch
---

## Why this exists

Author-blind review of `delete-old-presets` (cycle-1781944619), corroborated by the
inline implementor's own discovery, found that `config/gptel/sessions/commands.el`
defaults the session-creation preset to `'executor` in three places
(`jf/gptel-persistent-session` docstring ~514, the interactive non-prefix path ~545,
and the argument fallback `(or preset-name 'executor)` ~549; plus the `-global`
sibling ~614). After cycle-1781944619 deleted `executor.md`, and given there has
never been an `executor/preset.el` subdir, `'executor` resolves to an **unregistered**
preset. A default-path session therefore records `:GPTEL_PRESET: executor` for a
preset gptel can't resolve, yielding no scope defaults / no system prompt (the log
`No scope configuration available for preset: executor` appears throughout
`config/gptel/test-results.txt`).

## Why externalised (not in-change)

- The fix lives in `config/gptel/sessions/commands.org` — **outside** the
  `gptel-fragment-presets` write-set (this change never touched sessions/commands).
- It is **functionally pre-existing**: `'executor` was already unregistered before the
  deletion (the flat `.md` was inert under the new registration since cycle 2);
  deleting `executor.md` removed the last on-disk trace but did not change runtime
  resolution. The reviewer reads it as "triggered by this task"; the orchestrator's
  resolution is that the deletion is the natural moment to fix it, but the fix itself
  is a separate behavior decision.
- It is **not a named `gptel-fragment-presets` proposal outcome** (the proposal flipped
  the *workspace* initial preset — done by workspace-flip — not the general
  session-creation default).
- It **cascades**: ~10 spec files assert `:GPTEL_PRESET: executor` literally
  (`session-creation-spec.el`, `branching-integration-spec.el`,
  `work-root-emission-spec.el`, `identity-keys-emission-spec.el`,
  `session-org-creation-spec.el`, `no-duplicate-drawer-spec.el`,
  `workspace-integration-spec.el`, `filesystem-test.el`, etc.). Repointing the default
  requires a coordinated spec update — design thought, not a one-symbol edit.

## Open design question (resolve with maintainer before implementing)

Two viable directions:
1. **Repoint the default** to a registered preset (`'workspace-assistant`, which
   `workspace-assistant/preset.org` explicitly states "replaces the legacy executor")
   — and update the asserting specs to the new default.
2. **Re-author `executor`** as a new-style `executor/preset.el` so the long-standing
   default name keeps resolving.

Direction (1) aligns with "fresh-start the preset set"; (2) preserves the historical
default name. Pick one with the maintainer.

## Files to modify (direction 1)

- `config/gptel/sessions/commands.org` / `.el` — replace the three `'executor`
  defaults in `jf/gptel-persistent-session` (+ `-global`) with the chosen registered
  preset.
- The ~10 specs above asserting `:GPTEL_PRESET: executor` — update to the new default
  (use the explicit-preset argument where a test wants to pin `executor` specifically,
  or switch the expectation).

## Verification

- `grep -rn "or preset-name 'executor\|'executor)))" config/gptel/sessions/commands.el` (expect the new default).
- `./bin/run-tests.sh -d config/gptel/sessions`
- A default-path session no longer logs `No scope configuration available for preset: executor`.

## Context pointers

- Reviewer finding: `.orchestrator/cycles/cycle-1781944619/reviews/delete-old-presets.md` (Finding 1, advisory).
- Inline implementor discovery: `delete-old-presets` task body, Discovery `executor-default-still-unregistered`.
- `workspace-assistant/preset.org` ("replaces the legacy executor .md").
