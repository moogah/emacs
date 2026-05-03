---
name: add-discriminating-preset-system-survival-test
description: The "preset :system survives when drawer omits :GPTEL_SYSTEM:" scenario in `preset-wiring-spec.el` only asserts that `gptel--system-message` did not become buffer-local — but the spy on `gptel--apply-preset` is a no-op, so no value was ever installed. The test passes even if production code accidentally erased the binding. Add a real installation step so the contract is actually exercised.
change: gptel-drawer-as-source-of-truth
status: ready
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
