---
name: parser-remove-redundant-goto-char
description: Remove dead goto-char in assistant branch of gptel-chat--parse-buffer
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:parser
---

## Files to modify
- `config/gptel/chat/parser.org` (assistant branch in
  `gptel-chat--parse-buffer`)
- `config/gptel/chat/parser.el` (tangled)

## Implementation steps
1. Locate the assistant branch in `gptel-chat--parse-buffer` (around
   parser.el line 380-386). It does
   `(goto-char end-line-start)` then `(goto-char (line-end-position))`.
2. The first `goto-char` is dead — after `gptel-chat--scan-assistant-body`
   returns, point is already on `end-line-start`'s line, so
   `(line-end-position)` already refers to the right line.
3. Remove the redundant `(goto-char end-line-start)` so the assistant
   branch mirrors the user branch.
4. Re-tangle and re-run parser tests to confirm no behaviour change.

## Design rationale
The user branch correctly skips the redundant goto. Asymmetric style
suggests one of the branches was edited in isolation; removing the
dead call makes the two branches parallel and easier to maintain.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.

## Context
- Review of `parser` task Finding #5.
