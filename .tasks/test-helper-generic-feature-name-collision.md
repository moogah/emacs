---
name: test-helper-generic-feature-name-collision
description: scope/test/helpers-spec.el provides the generic feature 'helpers-spec; any second test-helper file reusing that name collides in the full buttercup batch (require becomes a no-op). Make the convention collision-proof.
source: openspec/changes/gptel-fragment-presets
status: open
relations:
  - "discovered-from:fragment-core"
  - "related-to:investigate-cross-suite-pollution-scope-tests"
  - "related-to:test-suite-shared-global-state-isolation"
---

> Distinct mechanism from the two related items: those concern *runtime* global
> mutable state (e.g. `gptel--known-tools`) leaking between specs in one process.
> This is a *load-time* `provide`/`require` feature-symbol collision that no-ops a
> `require` and crashes the buttercup load outright. Same family ("full-suite-only
> failures"), different fix (unique feature names).

## What

`config/gptel/scope/test/helpers-spec.el` does `(provide 'helpers-spec)`, and
~20 scope specs `(require 'helpers-spec (expand-file-name "helpers-spec.el"
scope-test-dir))`. Because `require` short-circuits on an already-provided
feature *regardless of the FILENAME argument*, ANY other `helpers-spec.el` that
provides the same generic symbol and loads earlier in the recursive-alphabetical
buttercup batch will satisfy `helpers-spec` first and make the scope `require` a
silent no-op — leaving `helpers-spec-make-scope-config` undefined and crashing
the buttercup load.

This exact collision was introduced and inline-fixed within cycle-1781883616
(`config/gptel/presets/test/helpers-spec.el` initially provided `'helpers-spec`;
fixed to `'presets-helpers-spec`). See
`.orchestrator/cycles/cycle-1781883616/findings/` (arch-cycle-1781883616-1).

## Why this is parked in .tasks/

The fix for *this* change is shipped; the landmine is a pre-existing
test-infrastructure convention owned by the gptel/scope test maintainer, not the
prompt-fragments change.

## Suggested resolution (pick one)

1. Rename `scope/test/helpers-spec.el`'s feature to a unique
   `scope-helpers-spec` (and update the ~20 `require` sites), reserving no generic
   name. Lowest-magic, mechanical.
2. Establish a convention + lint: every `*/test/helpers-spec.el` provides a
   dir-derived unique feature (`<subsystem>-helpers-spec`); add a buttercup
   meta-spec that asserts no two helper files provide the same feature symbol.
3. Have specs `load` the helper by path instead of `require`-by-feature, so the
   filename is authoritative.

## Verification

- `grep -rl "(provide 'helpers-spec)" config/` returns ≤1 file (or a meta-spec
  guards uniqueness).
- Full `./bin/run-tests.sh` buttercup process loads without a
  `void-function helpers-spec-make-scope-config` crash.
