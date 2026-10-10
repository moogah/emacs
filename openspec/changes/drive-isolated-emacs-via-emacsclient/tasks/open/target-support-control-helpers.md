---
name: target-support-control-helpers
description: "Control helpers jf/type-text, jf/send-keys, jf/run-command for isolated targets"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:target-support-frame-state"
  - "enables:behavioral-smoke-bare-target"
---

## Files to modify
- config/emacs-agent/target-support.org (modify)
- config/emacs-agent/test/unit/control-helpers-spec.el (new)

## Implementation steps
1. Specs first: `jf/type-text` inserts literally; `jf/run-command` calls a named
   command interactively; `jf/send-keys` runs a `kbd` string via `execute-kbd-macro`
   (frame-independent assertions only).
2. Implement; each returns a small status value.
3. Confirm none of the three appears in either preset (presets-spec already asserts
   this; add a manifest assertion if missing).

## Design rationale
Drive helpers for isolated testing only — never exposed on the live session
through the presets. Frame-dependent behavior uses an attached frame (D6).

## Verification
- ./bin/tangle-org.sh config/emacs-agent/target-support.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D5', 'D6'
- specs/emacs-control-tools/spec.md
