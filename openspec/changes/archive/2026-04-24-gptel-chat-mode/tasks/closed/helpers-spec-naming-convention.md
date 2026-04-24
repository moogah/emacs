---
name: helpers-spec-naming-convention
description: Reconcile helpers-spec.el naming with Buttercup discovery convention
change: gptel-chat-mode
status: done
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

## Review
- **Session:** orch-review-1776789773 (2026-04-21), agent `a378c6344a6bf751b`
- **Verdict:** clean with one doc follow-up
- **Findings:**
  - [follow-up] `openspec/changes/gptel-chat-mode/architecture.md:212,284,311`
    — architecture.md still names the fixture `helpers-spec.el` after
    the rename. Doc drift, not merge-blocking. Follow-up task
    `update-architecture-md-helpers-rename` created.
- **Checked and ruled out:**
  - **Loader callsites**: all three (`buffer-format-spec.el`,
    `escape-round-trip-spec.el`, `message-construction-spec.el`)
    consistently use `expand-file-name "../test-helpers.el"` anchored
    on `load-file-name`.
  - **Provide symbol**: `gptel-chat-test-helpers` unchanged — stable
    feature symbol.
  - **Git history**: tracked as R094 true rename; `git log --follow`
    works.
  - **Buttercup discovery**: new name no longer matches `*-spec.el`,
    so it won't be auto-loaded as a spec — correct, the three specs
    explicitly `load` it.
  - **Downstream impact**: no open task depends on this.
- **Verification:** `./bin/run-tests.sh -d config/gptel/chat --report`
  shows 282/282 passing (no drop since rename).
- **Follow-ups:** `update-architecture-md-helpers-rename` (ready, doc-only)
- **Dependents repointed:** none
