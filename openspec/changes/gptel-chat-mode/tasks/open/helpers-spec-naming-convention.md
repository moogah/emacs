---
name: helpers-spec-naming-convention
description: Reconcile helpers-spec.el naming with Buttercup discovery convention
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:parser
---

## Files to modify
- `config/gptel/chat/test/helpers-spec.el` (rename or restructure)
- `config/gptel/chat/test/parser/buffer-format-spec.el` (update load
  if renamed)
- Any other test files that load it (currently only
  `buffer-format-spec.el`)

## Implementation steps
Decide on ONE of the two options and apply it:

**Option A (recommended)**: Rename to a non-`-spec` suffix.
1. Rename file from `helpers-spec.el` to `test-helpers.el` (or
   `fixtures.el`).
2. Update the `load`/`require` call in `buffer-format-spec.el`.
3. The `provide` form already uses `gptel-chat-test-helpers`, no
   change there.

**Option B**: Put the test directory on `load-path` at test-run
time so callers can `(require 'gptel-chat-test-helpers)` directly,
and keep the `-spec.el` name (with a docstring noting it contains
no `describe` forms by design).

## Design rationale
The `-spec.el` suffix is the Buttercup discovery convention. A file
named `helpers-spec.el` that contains only a macro and `provide`
will be loaded by Buttercup, find zero `describe` forms, and
contribute nothing to the test report — harmless but misleading. A
future contributor will look at the name and expect tests.

## Verification
- File rename: `ls config/gptel/chat/test/` shows the chosen name.
- All test runs pass: `./bin/run-tests.sh -d config/gptel/chat`.
- No reference to the old filename remains:
  `grep -rn helpers-spec config/gptel/chat/test/` is empty (if
  renamed).

## Context
- Review of `parser` task Finding #7.
