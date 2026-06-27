---
name: implement-async-dispatch-and-timeout
description: Implement the async make-process emacsclient dispatch channel with per-call timeout and routing/error handling
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:implement-target-registry-and-lifecycle"
---

## Files to modify
- config/mcp-broker/broker-core.org (modify) — add the dispatch + routing layer
- config/mcp-broker/test/unit/broker-dispatch-spec.el (new)

## Implementation steps
1. Implement `jf/mcp-broker--dispatch (id form callback)`:
   - look up `id` in the registry; unknown/dead → immediately `(funcall callback
     <structured-error>)` and return (no process spawned);
   - otherwise launch an ASYNC `make-process` running `emacsclient -s ID --eval
     (prin1-to-string form)`, capturing stdout into a buffer;
   - on the process sentinel (exit), parse stdout and `(funcall callback <result>)`.
2. Per-call timeout: start a `run-at-time` timer (configurable, default e.g. 30s) that,
   if the process has not finished, kills it and calls `callback` with a structured
   timeout error. Ensure the sentinel and the timer cannot both resolve the callback
   (guard with a one-shot flag).
3. NEVER use `call-process`/synchronous `emacsclient` — a synchronous call would
   freeze the single-threaded broker on a hung target. Add a comment marking this as
   a load-bearing invariant.
4. Result/error shaping: provide helpers that wrap a value or an error into the MCP
   tool-result alist shape emcp expects (`((content . [((type . "text") (text . …))]))`
   and `(isError . t)` for errors), so tool handlers (next task) stay thin.
5. Unit specs (mock the boundary): stub `make-process` to invoke a recorded sentinel
   with canned stdout; assert the callback receives the parsed result; assert
   unknown-id short-circuits with an error and spawns nothing; simulate a never-exiting
   process and assert the timeout path fires exactly one callback.

## Design rationale
This is the mechanism behind the spec's "a hung target does not freeze the broker"
and "an unresponsive call times out" scenarios. emcp's tool model is genuinely
non-blocking (handlers get a `send-result` callback; the HTTP transport defers
responses), so the broker stays responsive as long as WE never block — hence async
`make-process` + a timeout that resolves the callback. The one-shot guard prevents a
double-resolution race between the sentinel and the timer.

## Verification
- `./bin/run-tests.sh -d config/mcp-broker` runs dispatch unit specs green, including
  the timeout and unknown-id cases.
- `grep -n "call-process" config/mcp-broker/broker-core.el` returns nothing in the
  dispatch path (async only).
- Spec coverage: mcp-broker spec §§ Target Registry and Routing, Non-Blocking
  Concurrent Execution.

## Context
design.md § Decisions 'D4. Async proxy with per-call timeout (the load-bearing
rule)'; specs/mcp-broker/spec.md.
