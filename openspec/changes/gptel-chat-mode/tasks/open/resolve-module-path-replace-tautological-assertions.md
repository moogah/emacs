---
name: resolve-module-path-replace-tautological-assertions
description: Replace tautological .el/config/ string matchers with full :to-equal checks
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:fix-resolve-module-path-extension
---

## Files to modify
- `config/core/test/resolve-module-path-spec.el` (modify — lines
  roughly 64-77, the `:to-match "\.el\\'"` and `:to-match "/config/"`
  assertions)

## Implementation steps
1. Locate the two low-signal `it` blocks asserting "always ends in
   `.el`" and "always beneath `/config/`" in the spec.
2. Either:
   - Replace with `:to-equal` full-expected-path checks (strongest —
     same info density, catches any deviation).
   - Or delete them if equivalent coverage is already provided by the
     happy-path `:to-equal` tests.
3. Prefer deletion unless you can articulate a specific regression
   class the tautological form catches that the full-path tests
   don't.
4. Run `./bin/run-tests.sh -d config/core/test` and confirm all specs
   still pass.

## Design rationale
A test that would still pass if the function were rewritten
incorrectly (so long as it preserved a substring) is worse than no
test — it creates false confidence. The two matchers flagged by
review only guarantee that `.el` and `config/` appear anywhere in the
output; they cannot catch wrong base directory, wrong separator, or
missing concatenation.

Coordinate with `resolve-module-path-edge-case-tests` if landing both
(both touch the same spec file).

## Verification
- Spec file contains no `:to-match "\.el\\'"` or `:to-match "/config/"`
  assertions unless paired with `:to-equal` coverage that makes them
  tautological.
- `./bin/run-tests.sh -d config/core/test` reports all specs pass.

## Context
- Review of fix-resolve-module-path-extension (orchestrator session
  2026-04-20) Finding #2
- `config/core/test/resolve-module-path-spec.el:64-77`
