---
name: add-require-org-to-mode
description: Add explicit require org to chat mode for batch-context safety
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/mode.org`
- `config/gptel/chat/mode.el` (tangled)

## Implementation steps
1. `mode.el` does `(define-derived-mode gptel-chat-mode org-mode ...)`
   without an explicit `(require 'org)`.
2. In practice, Emacs autoloads `org-mode` on demand, so activation
   works. But minimal batch contexts that have not autoloaded org
   could fail.
3. Add `(require 'org)` after the commentary block in `mode.org`.
4. Re-tangle and re-test.

## Design rationale
Tests currently pass because `gptel-chat-parser` and the test
runner pull in many things; org happens to be available. This is
load-order luck. An explicit `require` makes the dependency
visible and immune to harness changes.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat` passes.
- `head -20 config/gptel/chat/mode.el` shows `(require 'org)`.

## Context
- Review of `mode-definition` task Finding #6.
