---
name: implement-target-support-and-frame-state
description: Build the target-side support library and the display-free frame-state JSON serializer, seeded into each target at spawn
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:add-emcp-dep-and-build-server-adapter"
---

## Files to modify
- config/mcp-broker/broker-frame-state.org (new) — tangles target-side helpers
- config/mcp-broker/broker-target-support.el (new, generated) — seeded into targets
- config/mcp-broker/test/unit/frame-state-spec.el (new)

## Implementation steps
1. In `broker-frame-state.org`, implement target-side functions (these run INSIDE a
   target, not the broker):
   - `jf/mcp-target--frame-state ()` → returns an alist serialized via `json-serialize`
     describing: current buffer name + contents (bounded — see step 2), window
     configuration (list of displayed buffer names), point position, minibuffer state
     (active? + contents when active), and the last N `*Messages*`/echo-area lines.
   - `jf/mcp-target--type-text (text)` → inserts literal TEXT at point in the current
     buffer, preserving whitespace.
   - (helpers for send-keys/run-command may also live here, or in the tools task.)
2. Bound buffer contents: cap at a configurable max (e.g. 100KB); when truncated,
   include a `truncated: t` (and original length) marker in the JSON, per the spec's
   "large buffer content is truncated with an indicator" scenario.
3. Arrange that the org file tangles to `broker-target-support.el` (a flat file the
   broker can `load` into a daemon). Keep it dependency-light so it loads cleanly into
   a bare `emacs -Q` daemon.
4. Provide a broker-side seeding helper `jf/mcp-broker--seed-target (id)` that loads
   `broker-target-support.el` into the daemon via `emacsclient -s ID --eval (load
   "…/broker-target-support.el")`. (Wire the call into spawn during the tools task /
   integration; expose it here.)
5. Unit specs: call `jf/mcp-target--frame-state` in-process against a temp buffer
   setup and assert the JSON structure; assert truncation marker appears for an
   oversized buffer; assert `type-text` preserves whitespace exactly.

## Design rationale
Observation is display-free by serializing frame state to JSON (borrowed from
llm-test) rather than screenshotting — it needs no Xvfb and is machine-parseable so
clients can assert on it. The helpers must exist IN the target, so the broker seeds a
small support library at spawn; keeping it dependency-light lets even `-Q` targets
gain the helpers. `eval-elisp` needs no seeding; only frame-state/type-text/key
helpers do.

## Verification
- `./bin/tangle-org.sh config/mcp-broker/broker-frame-state.org` succeeds and produces
  `broker-target-support.el`.
- `./bin/run-tests.sh -d config/mcp-broker` runs frame-state unit specs green,
  including the truncation-indicator case.
- Spec coverage: frame-state-observation spec §§ Display-Free Frame State, Bounded
  Observation Output.

## Context
design.md § Decisions 'D5. Targets are seeded with a small support library at spawn';
specs/frame-state-observation/spec.md.
