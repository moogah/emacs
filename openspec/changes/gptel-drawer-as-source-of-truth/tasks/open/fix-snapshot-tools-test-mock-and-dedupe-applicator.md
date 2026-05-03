---
name: fix-snapshot-tools-test-mock-and-dedupe-applicator
description: The two modify-list tool tests in `snapshot-rendering-spec.el` `let`-bind `gptel-tools` but the implementation reads `(default-value 'gptel-tools)`, so the tests pass for the wrong reason in batch (where the global default is nil). Fix the mock scoping. Then extract the snapshot-key emission shared by `--snapshot-lines` and `--apply-to-drawer` so they can no longer drift silently when a new key is added.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:extend-render-drawer-text-with-preset-snapshot
---

## Files to modify

- `config/gptel/test/snapshot-rendering-spec.el` — fix the `let` → `cl-letf` (or equivalent) on the two modify-list tool tests
- `config/gptel/scope-profiles.org` (and re-tangled `.el`) — extract the shared snapshot-key emission

## Why

**Finding 1 (blocking).** The tests at lines 123 and 130 use `(let ((gptel-tools '(base-A base-B))))` and `(let ((gptel-tools nil)))`. But `jf/gptel-scope-profile--resolve-tool-names` reads the base via `(default-value 'gptel-tools)`. For a `defcustom`, `default-value` ignores `let` dynamic bindings — it returns the global default. In a batch test process where `gptel-tools` is globally nil, the `(:append (extra-C))` test passes because `(append nil '(extra-C))` is `(extra-C)`, not because the merge with `(base-A base-B)` was exercised. The tests are inert; a regression in the merge logic would not be caught.

**Finding 2 (advisory).** `--apply-to-drawer` (scope-profiles.el:436–460) re-implements the same six-key snapshot-mapping that `--snapshot-lines` (scope-profiles.el:274–307) already owns — same key list, same `plist-member` guard for `:tools`, same numeric guards. Adding a seventh snapshot key requires editing two places. This is exactly the kind of duplication that makes the cross-mode idempotency invariant decay silently.

## Implementation steps

1. In `config/gptel/test/snapshot-rendering-spec.el`, replace each `let`-binding of `gptel-tools` in the modify-list tool tests with one of:
   - `cl-letf` that overrides the `default-value` accessor at the symbol level, or
   - `cl-letf` that shadows `gptel--modify-value` directly (cleaner if the merge function is the unit under test), or
   - `setq gptel-tools VALUE` paired with an `unwind-protect` (or `before-each`/`after-each`) cleanup to restore the original default.
2. Re-run `./bin/run-tests.sh -d config/gptel/test` and confirm the two scenarios now actually exercise the documented base values. Sanity-check by deliberately breaking `gptel--modify-value` and confirming the tests fail.
3. In `config/gptel/scope-profiles.org`, extract the snapshot-key mapping into a shared form. Two viable shapes:
   - A small alist `((:model . :GPTEL_MODEL) (:backend . :GPTEL_BACKEND) ...)` with per-key value-formatter functions, iterated by both producers.
   - Have `--apply-to-drawer` parse `(jf/gptel-scope-profile--snapshot-lines preset-spec)` back to property pairs and call `org-set-property` per pair. Tools needs special handling either way (multivalued).
4. Re-tangle: `./bin/tangle-org.sh config/gptel/scope-profiles.org`.
5. Re-run `./bin/run-tests.sh -d config/gptel` to confirm no regressions.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/test
./bin/run-tests.sh -d config/gptel
grep -nE "default-value 'gptel-tools|let \(\(gptel-tools" config/gptel/test/snapshot-rendering-spec.el
```

Expect: no remaining `let`-binding of `gptel-tools` in the test file. All snapshot-rendering specs pass. No regressions elsewhere.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/extend-render-drawer-text-with-preset-snapshot.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-boundary-scope-profile-applicator` (the `cross_stage_invariants` clause).
