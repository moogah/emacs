---
name: add-test-helpers-and-behavioral-smoke
description: Add shared Buttercup test helpers (fake-target fixture) and one guarded real-daemon end-to-end smoke suite
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:implement-control-and-observation-tools"
---

## Files to modify
- config/mcp-broker/test/helpers-spec.el (new) — shared fixtures/matchers
- config/mcp-broker/test/behavioral/broker-smoke-spec.el (new)

## Implementation steps
1. Extract the ad-hoc mocks used by the unit specs into a shared
   `helpers-spec.el`: a `fake-target` fixture that records the forms sent to dispatch
   and returns canned JSON/results, plus any custom matchers. Refactor the earlier
   unit specs (broker-targets, broker-dispatch, broker-tools) to use it where it
   reduces duplication.
2. Write ONE behavioral smoke spec that exercises the real path end-to-end against a
   real daemon:
   - start a bare target (`emacs -Q --daemon`) via the real lifecycle code;
   - `eval_elisp` a simple form and assert the printed value;
   - `frame_state` and assert the JSON shape on a real buffer;
   - `dispose_target` and assert the socket no longer responds.
3. Guard the smoke suite so constrained CI passes when no suitable Emacs/daemon is
   available: detect availability in a `before-all` and skip (Buttercup pending) with
   a clear message rather than failing.
4. Keep the smoke minimal (one happy-path trip); the bulk of coverage stays in the
   fast mocked unit specs.
5. Confirm discovery: the suite runs under `./bin/run-tests.sh -d config/mcp-broker`
   (and `-f buttercup`).

## Design rationale
Hybrid testing (decided): fast mocked-boundary unit specs for logic, plus a single
real-daemon smoke for integration confidence on the riskiest part (the actual
daemon/emacsclient round-trip). The smoke is guarded/skippable so it never makes CI
flaky or blocks environments without a spawnable Emacs. This matches the repo's
"behavioral tests mock only at the boundary" philosophy without paying daemon startup
cost on every test.

## Verification
- `./bin/run-tests.sh -d config/mcp-broker` is green: unit specs always run; the smoke
  either runs (when a daemon is spawnable) or reports as pending/skipped.
- When run locally with Emacs available, the smoke completes a full
  start→eval→frame-state→dispose trip against a real daemon.
- Spec coverage: behavioral validation of mcp-broker, emacs-control-tools,
  frame-state-observation, and isolated-emacs-targets together.

## Context
design.md § Decisions 'D7. Testing approach — hybrid (mock the boundary + one
real-daemon smoke)'.
