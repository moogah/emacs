---
name: emacs-agent-wrapper-targets
description: "bin/emacs-agent target start/list/stop and per-target tools/call/call-write/eval/state"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:emacs-agent-wrapper-live"
  - "blocked-by:data-profile-early-override"
  - "enables:behavioral-smoke-bare-target"
  - "enables:behavioral-smoke-scratch-profile"
  - "enables:bootstrap-skill-and-claude-md"
---

## Files to modify
- bin/emacs-agent (modify)
- README.md or RECIPES section in the skill (modify/new) — target recipes

## Implementation steps
1. `target start <name> [--worktree PATH] [--bare] [--data live|scratch:<dir>|none]`:
   refuse name `server`; worktree mode runs `<PATH>/bin/emacs-isolated.sh
   --daemon=<name>` with the profile env vars exported; bare mode runs
   `emacs -Q --daemon=<name>`; wait until the socket answers `(+ 1 1)` or time out.
2. `target list`: socket names in `$TMPDIR/emacs$(id -u)/` excluding `server`, with
   a liveness probe each.
3. `target stop <name>`: `(kill-emacs)` over the socket; verify the socket is gone.
4. `target <name> tools|call|call-write|eval|state` — same envelopes as live;
   `state` evaluates `(jf/frame-state)` (loading target-support first on `-Q`).
5. Apply the default data profile chosen in design Open Questions; print it on start.

## Design rationale
Isolated targets are addressed by socket name and the socket directory is the
registry; the wrapper adds lifecycle, the data profile and the same tier surface as
the live target.

## Verification
- bash -n bin/emacs-agent
- Manual: start a bare target, list it, eval `(+ 1 2)`, stop it, confirm it is gone

## Context
- design.md § Decisions 'D3', 'D13', 'D15'
- specs/isolated-emacs-targets/spec.md
