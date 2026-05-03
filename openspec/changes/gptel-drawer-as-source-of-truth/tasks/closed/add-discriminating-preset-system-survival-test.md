---
name: add-discriminating-preset-system-survival-test
description: The "preset :system survives when drawer omits :GPTEL_SYSTEM:" scenario in `preset-wiring-spec.el` only asserts that `gptel--system-message` did not become buffer-local — but the spy on `gptel--apply-preset` is a no-op, so no value was ever installed. The test passes even if production code accidentally erased the binding. Add a real installation step so the contract is actually exercised.
change: gptel-drawer-as-source-of-truth
status: done
relations:
  - discovered-from:verify-and-spec-drawer-overlay-wins
---

## Files to modify

- `config/gptel/chat/test/menu/preset-wiring-spec.el` — strengthen the "preset :system survives" scenario (currently lines 655–676), or add a sibling scenario that establishes a real preset-system value
- (optional) `config/gptel/sessions/test/commands/preset-application-spec.el` — add an end-to-end variant if a unit-level fix is awkward

## Why

**Finding 1 (advisory).** The current test asserts `(local-variable-p 'gptel--system-message) :to-be nil` after `gptel-chat--apply-declared-preset` runs with `gptel--apply-preset` spied as a no-op. Because nothing installed a value, the post-state ("not buffer-local") is indistinguishable between "overlay correctly left it alone" and "overlay killed the binding." The test comment itself acknowledges the gap. The contract the invariant `register/invariant/drawer-overlay-wins-over-preset` asserts — *the preset's `:system` value survives the overlay pass when the drawer omits `:GPTEL_SYSTEM:`* — is not actually exercised. A future refactor that calls `(kill-local-variable 'gptel--system-message)` in the overlay path would still pass.

## Implementation steps

1. Choose one of two locations:
   - **Unit (preferred):** in `preset-wiring-spec.el`, replace the no-op spy on `gptel--apply-preset` for this scenario with a stub that performs `(set (make-local-variable 'gptel--system-message) "preset-system")` — i.e., simulates what the real preset setter does. Then run the overlay and assert `gptel--system-message` equals `"preset-system"` after.
   - **End-to-end:** in `preset-application-spec.el`, add a scenario that registers a preset with `:system "preset-system"`, drives a session through the application path with a drawer that omits `:GPTEL_SYSTEM:`, and asserts the buffer-local `gptel--system-message` equals `"preset-system"` after.
2. Confirm the new test fails if you (temporarily) add `(kill-local-variable 'gptel--system-message)` to the overlay path, and passes after reverting — i.e., it is a genuine discriminator for the contract.
3. Re-run `./bin/run-tests.sh -d config/gptel/chat` (and `config/gptel/sessions` if you went the e2e route).

## Verification

```bash
./bin/run-tests.sh -d config/gptel/chat
./bin/run-tests.sh -d config/gptel/sessions
```

Expect: green; the new test discriminates against the kill-local-variable failure mode.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/verify-and-spec-drawer-overlay-wins.md` (Finding 1).

Cited register entries: `interfaces.org#register-invariant-drawer-overlay-wins-over-preset`, `interfaces.org#register-invariant-drawer-system-key-write-exclusion`.

## Observations

- **Unit-level fix selected.** Per the task body's preference and to avoid
  collisions with the parallel task touching
  `preset-application-spec.el`, the strengthened scenario lives in
  `config/gptel/chat/test/menu/preset-wiring-spec.el` only.
- **The strengthened scenario.** Replaced the no-op spy on
  `gptel--apply-preset` (inherited from the surrounding `before-each`)
  with a per-`it` `:and-call-fake` stub that *simulates* the real
  preset setter: it invokes the buffer-local setter the production
  code passes in, installing
  `gptel--system-message := "preset-system"` buffer-locally.  After
  `gptel-chat--apply-declared-preset` runs (which calls
  `gptel-chat--apply-drawer-overrides` with a tuple whose `system`
  field is nil), the test now asserts both
  `(local-variable-p 'gptel--system-message) → t` *and*
  `gptel--system-message ≡ "preset-system"`.  The post-state is no
  longer ambiguous: the only way both assertions hold is if the
  overlay left the preset-installed binding intact.
- **Probe / discriminator outcome.** I temporarily added
  `(kill-local-variable 'gptel--system-message)` to
  `gptel-chat--apply-drawer-overrides` (in `config/gptel/chat/menu.el`,
  inside the `pcase-let` body, before the `(when system ...)` clause)
  and re-ran `./bin/run-tests.sh -d config/gptel/chat`.  The new
  scenario failed with:

  ```
  preset :system survives when drawer omits :GPTEL_SYSTEM:
    Expected `(local-variable-p 'gptel--system-message)' to be `eq' to
    `t', but instead it was `nil'.
  Ran 377 specs, 1 failed, in 3.06s.
  ```

  After reverting the probe, all 377 specs passed in 2.35s.  The new
  test is therefore a genuine discriminator for the
  `register/invariant/drawer-overlay-wins-over-preset` contract on the
  `:GPTEL_SYSTEM:` key — it would catch a refactor that erases the
  preset-installed system message in the overlay path, which the
  prior assertion (only `(local-variable-p ...) :to-be nil`) could
  not.
- **No production-code changes.** The probe was applied and reverted
  on `menu.el` only as a verification step; the committed diff
  contains no `menu.el` change.
- **Verification scope.** Ran `./bin/run-tests.sh -d config/gptel/chat`;
  did not run the `config/gptel/sessions` suite because the unit-level
  fix does not touch that module (the optional e2e variant in
  `preset-application-spec.el` was explicitly skipped to avoid
  collision with a parallel task).

## Discoveries

- **Spy-override pattern (informational).** Buttercup permits a
  per-`it` `spy-on` to override a `before-each` `spy-on` for the same
  symbol within the same `describe` block, which is what this fix
  relies on.  The pattern is already present elsewhere in
  `preset-wiring-spec.el` (e.g. the `gptel-org--entry-properties`
  spies are re-configured per scenario), so this is a consistent
  idiom for the file rather than a new convention.
- **Asymmetric contract clarified.** The strengthened scenario, taken
  together with its sibling
  "drawer-authored :GPTEL_SYSTEM: still wins on read (back-compat)",
  now exercises both arms of the asymmetric `:GPTEL_SYSTEM:` contract:
  drawer omits → preset survives; drawer includes → drawer wins.
  Previously only the second arm had a real assertion; the first arm
  was structurally a no-op test.  No register entry needs updating —
  `register/invariant/drawer-overlay-wins-over-preset` and
  `register/invariant/drawer-system-key-write-exclusion` already
  describe the contract correctly; this task closes the
  test-coverage gap, not a spec gap.
