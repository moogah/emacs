---
name: behavioral-smoke-bare-target
description: "Guarded real-daemon smoke: bare target start → eval → frame-state → helpers → stop"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:emacs-agent-wrapper-targets"
  - "blocked-by:target-support-control-helpers"
---

## Files to modify
- config/emacs-agent/test/behavioral/bare-target-smoke-spec.el (new)

## Implementation steps
1. Skip (Buttercup `assume`/pending) when `emacs`/`emacsclient` are unavailable or
   in constrained CI.
2. Use a unique target name; start via `bin/emacs-agent target start <name> --bare`;
   eval; load target-support; `jf/type-text` into a scratch buffer; read
   `state`; stop; assert the socket is gone. Always stop in `after-each`.
3. Never touch the `server` socket.

## Design rationale
One real end-to-end path proves the transport and lifecycle; bulk coverage stays
in fast unit specs.

## Verification
- ./bin/run-tests.sh -d config/emacs-agent/test/behavioral

## Context
- design.md § Decisions 'D7. Testing approach'
- specs/isolated-emacs-targets/spec.md
- specs/frame-state-observation/spec.md
