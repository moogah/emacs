---
name: emacs-agent-wrapper-live
description: "bin/emacs-agent: live target subcommands (tools, call, call-write, eval) with explicit sockets"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-manifest"
  - "blocked-by:agent-action-log"
  - "enables:emacs-agent-wrapper-targets"
---

## Files to modify
- bin/emacs-agent (new) — bash
- config/emacs-agent/dispatcher.org (modify) — `jf/agent-eval` entry used by the eval tier (logs, returns envelope)
- config/emacs-agent/test/behavioral/wrapper-guard-spec.el (new) or bin/test/ shell checks

## Implementation steps
1. Implement subcommands:
   `emacs-agent live tools | call <tool> '<json>' | call-write <tool> '<json>' | eval '<form>'`.
   - Resolve `live` to socket `server`; always pass `-s` to `emacsclient`; if the
     socket is not served, exit non-zero with "no live session is running".
   - No subcommand form omits the target; a bare `emacs-agent call ...` errors.
   - Pass JSON and forms safely: encode the argument as an Elisp string literal
     (escape backslashes and quotes) inside `(jf/agent-call "call" "tool" "<json>")`.
   - Print the dispatcher's JSON envelope; exit status 0 on ok, 1 on tool error,
     2 on usage/connection errors.
2. Tests: point the wrapper at a fake socket directory (`TMPDIR` override) and assert
   the implicit-target and missing-live-session refusals; quoting round-trip for JSON
   containing quotes, backslashes and newlines.
3. Document the recommended Claude Code permission rules in the script's `--help`
   (allow `live call`, ask `live call-write` or allow per user, ask `live eval`).

## Design rationale
The policy needs a chokepoint that permission rules can see: distinct subcommands
let Claude Code allow, ask or deny each tier. Bare `emacsclient --eval` silently
reaches the live session, so the wrapper must always name the socket.

## Verification
- bash -n bin/emacs-agent
- ./bin/tangle-org.sh config/emacs-agent/dispatcher.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D13. `bin/emacs-agent` is the enforcement chokepoint'
- specs/agent-target-policy/spec.md § 'Two Target Kinds', § 'Wrapper as the Single Agent Entry Point'
