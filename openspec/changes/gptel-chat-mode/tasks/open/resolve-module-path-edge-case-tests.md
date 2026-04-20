---
name: resolve-module-path-edge-case-tests
description: Add characterization tests for jf/resolve-module-path edge cases (empty, leading slash, .., absolute)
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:fix-resolve-module-path-extension
---

## Files to modify
- `config/core/test/resolve-module-path-spec.el` (modify — add `it`
  blocks characterizing edge-case behaviour)

## Implementation steps
1. Open `config/core/test/resolve-module-path-spec.el` (added in
   `b7c77c8`).
2. Add characterization `it` blocks documenting the *observed* output
   for each of the following inputs (`:to-equal` with the expanded
   path string, not tautological matchers):
   - `""` (empty string) → `<jf/emacs-dir>/config/.el`
   - `"/abs"` (leading slash) → `<jf/emacs-dir>/abs.el` (because
     `expand-file-name` treats a leading `/` as absolute)
   - `"core/defaults/"` (trailing slash) → `<jf/emacs-dir>/config/core/defaults/.el`
   - `"../etc/passwd"` (parent-ref) → `<jf/emacs-dir>/../etc/passwd.el`
     (parent of repo + .el)
   - Anything else that looks suspicious — document, don't fix.
3. Frame these as **characterization tests**, not aspirational
   contracts: the function performs no validation. If the user wants
   stricter input handling, that is a separate change.
4. Run the spec and confirm it passes:
   `./bin/run-tests.sh -d config/core/test`.
5. Run the full suite to confirm no collateral damage:
   `./bin/run-tests.sh`.

## Design rationale
`jf/resolve-module-path` was simplified to a single `expand-file-name`
call with no validation. Several unusual inputs produce surprising
paths silently. None of these are regressions (the old function
accepted them too) but nothing in the test suite pins the behaviour.
Characterization tests lock in current semantics so future refactors
can't silently change them.

Reviewer note: explicitly characterize — don't fix. Stricter
validation is a design question for a separate task.

## Verification
- The new `it` blocks pass, each using `:to-equal` with the full
  expected path string.
- `./bin/run-tests.sh -d config/core/test` reports all specs pass.
- No tautological assertions (e.g. `:to-match "/config/"`).

## Context
- Review of fix-resolve-module-path-extension (orchestrator session
  2026-04-20) Finding #1
- `config/core/test/resolve-module-path-spec.el:37-77`
- `init.org:70-82` (function definition)
