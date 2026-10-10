---
name: target-support-frame-state
description: "jf/frame-state: display-free JSON observation with bounded buffer contents"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-scaffold-module"
  - "enables:target-support-control-helpers"
---

## Files to modify
- config/emacs-agent/target-support.org (new)
- config/emacs-agent/test/unit/frame-state-spec.el (new)

## Implementation steps
1. Specs first: given a synthetic current buffer, `jf/frame-state` returns JSON
   with buffer name, contents, point, window list (displayed buffer names),
   minibuffer active/contents, and the last N `*Messages*` lines; a buffer larger than
   `jf/frame-state-max-chars` is truncated with `"truncated": true`; the result
   round-trips through `json-parse-string`.
2. Implement in `target-support.org` (inert definitions only: no server start, no
   hooks) so it is safe to load anywhere and loadable into `-Q` targets with a single
   `(load ".../target-support.el")`.

## Design rationale
The display-free observation contract for isolated testing, borrowed from
llm-test. Bounded output keeps large buffers from producing unbounded responses.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/target-support.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D5. A target-side support module provides observation + control helpers'
- specs/frame-state-observation/spec.md
