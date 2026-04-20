---
name: parser-case-insensitive-test
description: Add parser test for case-insensitive delimiter matching
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:parser
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (extend)

## Implementation steps
1. Add a Buttercup `it` block exercising mixed-case delimiters in
   the same buffer:
   `"#+BEGIN_USER\nq\n#+End_User\n#+begin_assistant\na\n#+END_ASSISTANT\n"`.
2. Assert the parser produces two turns (user then assistant) with
   the expected `:role` and `:content` / `:segments`.

## Design rationale
`case-fold-search t` is bound in the parser and Decisions 4 and 14
both commit to case-insensitive matching. A future refactor that
replaces the regexes or wraps them in a `cond` could silently
regress to case-sensitive without any test failing. Pinning the
invariant with a test is cheap insurance.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- New scenario covers at least one `BEGIN_*` and one `End_*`
  variant.

## Context
- Review of `parser` task Finding #6.
