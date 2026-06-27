---
name: implement-target-registry-and-lifecycle
description: Implement the in-memory target registry plus spawn / inspect / dispose of isolated Emacs daemon targets
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:add-emcp-dep-and-build-server-adapter"
  - "blocked-by:add-emacs-isolated-daemon-flag"
---

## Files to modify
- config/mcp-broker/broker-targets.org (new)
- config/mcp-broker/test/unit/broker-targets-spec.el (new)

## Implementation steps
1. Define a target struct (`cl-defstruct`) with fields: `id` (the server-socket
   name), `process` (the `make-process` object for the spawning command), `mode`
   (`worktree` | `bare`), `status` (`starting` | `running` | `dead`), `start-time`.
2. Hold the registry as a module-level hash table id→struct, with accessors:
   register, lookup (returns nil for unknown), list-all, mark-dead, unregister.
3. `jf/mcp-broker--start-target (mode &optional id)`:
   - generate a unique `id` if not supplied (e.g. `mcp-<counter>` — avoid Math.random;
     use an incrementing counter var);
   - `worktree` mode → `make-process` running `bin/emacs-isolated.sh --daemon=ID`;
     `bare` mode → `make-process` running `emacs -Q --daemon=ID`;
   - register the target as `starting`; flip to `running` once `emacsclient -s ID
     --eval t` succeeds (a readiness probe with a short retry/backoff);
   - return the id.
4. `jf/mcp-broker--inspect-targets ()` → list of plists {id, mode, running?, uptime}.
   Probe liveness (process-live-p and/or an emacsclient ping); mark crashed targets
   `dead`.
5. `jf/mcp-broker--dispose-target (id)`: send `(kill-emacs)` via emacsclient (fallback
   to killing the process), unregister, release resources. Disposing an unknown id is
   a no-op error result, not a crash.
6. Orphan cleanup: register a `kill-emacs-hook` on the broker that disposes every
   registered target, so broker shutdown leaves no orphan daemons. Also add a
   best-effort startup reaper for stale sockets matching our naming scheme.
7. Unit specs: mock `make-process`/`emacsclient` invocation via `cl-letf` scoped to
   the functions under test; assert registry transitions, unknown-id handling, and
   that shutdown disposes all registered targets. (No real daemons here — that is the
   behavioral smoke task.)

## Design rationale
The socket name IS the target id (D3), so addressing and identity collapse to one
handle. The registry is in-memory and non-persistent because targets are ephemeral.
Crash containment (spec: a crashing target must not take down the broker) is why
liveness is probed and `inspect` reports `dead` rather than erroring. Orphan cleanup
on `kill-emacs-hook` satisfies the spec's "broker shutdown leaves no orphans."

## Verification
- `./bin/run-tests.sh -d config/mcp-broker` runs broker-targets unit specs green.
- `grep -n "Math.random\|(random" config/mcp-broker/broker-targets.el` returns
  nothing (ids come from a deterministic counter).
- Spec coverage: maps to isolated-emacs-targets spec §§ Start, Inspect, Dispose,
  Crash Containment, and the no-orphans scenario.

## Context
design.md § Decisions 'D3. Targets are isolated Emacs daemons addressed by
server-socket name'; specs/isolated-emacs-targets/spec.md.
