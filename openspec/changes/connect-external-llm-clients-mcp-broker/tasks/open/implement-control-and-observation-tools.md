---
name: implement-control-and-observation-tools
description: Define the emcp-deftool handlers for lifecycle, control, and observation, wired to the registry and async dispatch
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:implement-async-dispatch-and-timeout"
  - "blocked-by:implement-target-support-and-frame-state"
---

## Files to modify
- config/mcp-broker/broker-tools.org (new)
- config/mcp-broker/test/unit/broker-tools-spec.el (new)

## Implementation steps
1. Using `emcp-deftool`, define the tool surface. Each handler is async
   (`:async t`) and receives `send-result`; it validates the target id against the
   registry first (unknown/dead → structured error, no action), then acts:
   - **Lifecycle**: `start_target` (mode: worktree|bare → `jf/mcp-broker--start-target`,
     also seeds support via `jf/mcp-broker--seed-target`), `inspect_targets`
     (→ registry inspect), `dispose_target` (→ dispose).
   - **Control**: `eval_elisp` (dispatch the client form, return printed value),
     `run_command` (dispatch `(call-interactively 'CMD)`), `type_text` (dispatch
     `(jf/mcp-target--type-text TEXT)`), `send_keys` (dispatch `(execute-kbd-macro
     (kbd KEYS))`).
   - **Observation**: `frame_state` (dispatch `(jf/mcp-target--frame-state)` and relay
     the JSON).
2. Route every target-touching tool through `jf/mcp-broker--dispatch` (async + timeout)
   and resolve `send-result` from its callback. Shape results/errors with the helpers
   from the dispatch task.
3. Assemble the server: collect the tool list and pass it to
   `jf/mcp-broker--build-server`. Expose `jf/mcp-broker-start` / `jf/mcp-broker-stop`
   entry points (start binds the loopback host/port from defcustoms; stop disposes
   targets and stops the server). Do NOT add this module to `jf/enabled-modules`.
4. `eval_elisp` errors must be reported as structured tool errors (`isError`), never
   crash broker or target (spec).
5. Unit specs (mock dispatch + registry): assert each tool builds the expected
   target-side form and passes it to dispatch; assert unknown-target tools return a
   structured error and call dispatch zero times; assert `eval_elisp` error shaping.

## Design rationale
Each tool is a thin `emcp-deftool` handler: validate id → build target-side form →
async dispatch → resolve `send-result`. This keeps the protocol surface declarative
and pushes all the real behavior into the (already tested) registry/dispatch/support
layers. The vocabulary is kept close to gptel's tool shape to leave a future
shared-interface door open (non-goal here).

## Verification
- `./bin/tangle-org.sh config/mcp-broker/broker-tools.org` succeeds.
- `./bin/run-tests.sh -d config/mcp-broker` runs broker-tools unit specs green.
- Spec coverage: emacs-control-tools spec (all five requirements), frame-state-
  observation §Display-Free, isolated-emacs-targets §§ Start/Inspect/Dispose as the
  tool entry points; mcp-broker §Permissive (no confirmation in any handler).

## Context
design.md § Decisions 'D6. Tool surface → emcp-deftool handlers'; specs/
emacs-control-tools/spec.md, specs/frame-state-observation/spec.md.
