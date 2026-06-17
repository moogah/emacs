---
name: scope-relative-resolution-tests
description: Verify relative paths resolve against default-directory (work root); no validator change
change: gptel-work-root-default-directory
status: blocked
relations:
  - blocked-by:binder-default-directory
---

## Files to modify
- `config/gptel/scope/test/` (new spec, or extend an existing path-resolution spec) —
  e.g. `config/gptel/scope/test/validation/path-resolution-spec.el`

## Implementation steps
1. This task adds VERIFICATION ONLY — no production code change. Grounding confirmed
   the tools already resolve relative paths against the ambient `default-directory`:
   - filesystem tools use bare `(expand-file-name filepath)`
     (`scope-filesystem-tools.org:81,124,172`);
   - bash resolves against `default-directory` (`scope-validation.org:299` passes
     `directory default-directory`; `:420` does `(expand-file-name path directory)`).
   The scope spec is therefore "affirm, not change."
2. Add Buttercup specs that bind `default-directory` to a work root and assert that a
   RELATIVE path argument resolves under it before validation:
   - With `default-directory = /Users/x/proj/` and `paths.read` containing
     `/Users/x/proj/**`, a relative `config/x.el` validates as the absolute
     `/Users/x/proj/config/x.el` and is ALLOWED.
   - With the SAME relative path but `default-directory = /Users/x/other/`, it resolves
     to `/Users/x/other/config/x.el` (resolution follows the work root, not the
     bookkeeping dir).
3. Assert the tool execution path also uses that `default-directory` (the bash
   validator reads `directory default-directory`; confirm a relative-path command's
   extracted op is judged against the work-root-resolved absolute).
4. Do NOT duplicate the absolute-path / fail-closed coverage — that is already green in
   the validation suite (195/195) and verified.

## Design rationale
Fixing `default-directory` (the binder task) is what makes "the path the validator
judges" equal "the path the model meant." This task locks that behavior with tests so
a future regression in tool path-resolution is caught. The validator's allow/deny
logic, deny-precedence, and fail-closed behavior are unchanged — only WHICH directory
relative paths resolve against is being pinned (design D4).

## Design pattern
Follow the existing `config/gptel/scope/test/validation/path-resolution-spec.el` and
`path-validation-spec.el` structure (Buttercup `describe`/`it`/`expect`). Use a
`let`/`cl-letf`-bound `default-directory` per the project's behavioral-test convention
(mocks scoped to the function under test).

## Verification
- Run: `./bin/run-tests.sh -d config/gptel/scope` — all green, including the new specs.
- The new specs fail if `default-directory` is ignored (e.g. if a tool ever hardcodes a
  resolution root).

## Context
design.md § Decisions 'D4 — Tools inherit the work root through the existing buffer context'
specs/scope/spec.md § 'Requirement: Working-directory path resolution and cwd–scope agreement'
