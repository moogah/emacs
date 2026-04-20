---
name: remove-stale-diagnostics-module
description: Delete config/gptel/diagnostics (old troubleshooting code, no live loaders)
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `config/gptel/diagnostics.org` (delete)
- `config/gptel/diagnostics.el` (delete — generated)
- `config/gptel/diagnostics/` directory (delete — contains only
  `reference-data/` artifacts from prior debugging sessions)

## Implementation steps
1. Confirm (via `grep -rn 'jf-gptel-diagnostics\|gptel/diagnostics' .`
   scoped outside `.beads/`, `openspec/`, and `config/gptel/diagnostics/`)
   that no live code requires, loads, or references the module. As of
   2026-04-20 there are zero such references — the module is dead code.
2. Delete `config/gptel/diagnostics.org` and the tangled
   `config/gptel/diagnostics.el`.
3. Delete the `config/gptel/diagnostics/` subtree (contains only
   `reference-data/` with stale debugging artifacts).
4. Run the full test suite: `./bin/run-tests.sh` — nothing should break
   because nothing references the module.
5. Start Emacs: `./bin/emacs-isolated.sh` — no load errors.

## Design rationale
`config/gptel/diagnostics.{org,el}` was old troubleshooting code from
earlier gptel/text-property debugging. It is not in
`jf/enabled-modules`, not `require`d anywhere, and the only module-level
`(provide 'jf-gptel-diagnostics)` is never consumed. The sessions-
filesystem review surfaced it because it hardcodes `"session.md"` as a
buffer-name lookup (`diagnostics.el:474-475`), which would silently fail
after the rename — but per user direction (orchestrator session
2026-04-20), the correct fix is to remove the dead module entirely
rather than patch its string literals.

Removing it also cleans up the associated `reference-data/` directory
which contains `session.md` artifacts from old debugging runs (also
tied to the legacy file format).

## Verification
- `ls config/gptel/diagnostics*` returns no files.
- `find config/gptel/diagnostics/ -type f` returns no files (directory
  gone).
- `./bin/run-tests.sh` — all tests still pass.
- `./bin/emacs-isolated.sh` — boots without errors.
- `grep -rn 'jf-gptel-diagnostics' . --exclude-dir=.git --exclude-dir=.beads`
  returns nothing.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #3 — original finding was to fix the buffer-name lookup
- User direction during review processing: remove the whole module
  instead (it is dead code from prior troubleshooting)
- diagnostics.el:474-475 (the legacy `"session.md"` buffer lookup that
  prompted the finding)
