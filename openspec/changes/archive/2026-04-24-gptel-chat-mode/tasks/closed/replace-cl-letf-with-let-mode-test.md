---
name: replace-cl-letf-with-let-mode-test
description: Replace cl-letf with plain let in mode-activation test for plain variable binding
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (around
  the mode-activation `it` block)

## Implementation steps
1. Locate the `(cl-letf ((gptel-chat-mode-hook (list (lambda () ...)))) ...)`
   binding (around line 124).
2. `cl-letf` is for generalized places (`(symbol-function 'foo)`,
   `(alist-get 'k m)`, etc.). For a plain variable symbol, plain
   `let` is equivalent and idiomatic.
3. Replace with `(let ((gptel-chat-mode-hook ...)) ...)`.

## Design rationale
Cargo-culted use of `cl-letf` misleads readers expecting a
generalized-place binding. Plain `let` is the right tool here.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- `grep -n cl-letf config/gptel/chat/test/parser/buffer-format-spec.el`
  shows no remaining uses for plain variables (any remaining
  `cl-letf` should be for actual generalized places).

## Context
- Review of `mode-definition` task Finding #5.
