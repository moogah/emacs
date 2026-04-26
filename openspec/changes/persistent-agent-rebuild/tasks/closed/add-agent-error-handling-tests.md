---
name: add-agent-error-handling-tests
description: Buttercup specs for ERRS, ABRT terminal states and overlay-leak prevention
change: persistent-agent-rebuild
status: needs-review
relations:
  - blocked-by:add-persistent-agent-test-fixtures
  - blocked-by:rebuild-persistent-agent-module
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/error-handling-spec.el` (new)

## Implementation steps

1. **File header**:
   ```elisp
   ;;; error-handling-spec.el --- Persistent-agent error / abort tests -*- lexical-binding: t; -*-

   (require 'buttercup)
   (require 'jf-persistent-agent-test-helpers)
   (require 'gptel-persistent-agent)
   ```

2. **Required `it` cases**:

   - `it "ERRS deletes the overlay and returns an error string"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Error handling" → "Network failure cleanup"
     - Hand-construct a fake FSM for ERRS:
       ```elisp
       (let* ((calls nil)
              (main-cb (lambda (text) (push text calls)))
              (host-buf (generate-new-buffer "*test-overlay-host*"))
              (overlay (with-current-buffer host-buf (make-overlay 1 1)))
              (handler (jf/gptel-persistent-agent--make-on-errs main-cb))
              (fsm (gptel-make-fsm :info (list :context overlay
                                              :error '(some-error "boom")))))
         (unwind-protect
             (progn
               (funcall handler fsm)
               (expect (length calls) :to-equal 1)
               (expect (car calls) :to-match "^Error: agent request failed")
               (expect (car calls) :to-match "boom")
               (expect (overlay-buffer overlay) :to-be nil))
           (kill-buffer host-buf)))
       ```

   - `it "ABRT deletes the overlay and returns an abort string"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Error handling" → "User abort cleanup"
     - Same shape with `--make-on-abrt`. Assert:
       - `main-cb` called exactly once.
       - The string contains "abort" (case-insensitive — assert with regex).
       - Overlay is deleted (`(overlay-buffer overlay)` is nil).

   - `it "DONE handler also deletes the overlay"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Error handling" → "Overlay never leaks"
     - Same hand-built FSM pattern with `--make-on-done`. Even on the happy path, the overlay must be deleted.

   - `it "the parent overlay is gone after every terminal state"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Error handling" → "Overlay never leaks"
     - Loop over `(make-on-done make-on-errs make-on-abrt)` constructors. For each:
       - Build a fresh overlay in a fresh host buffer.
       - Build the handler closure.
       - Drive it with a fake FSM info containing `:context overlay`.
       - Expect `(overlay-buffer overlay)` is nil afterward.
     - This is the loop variant of the three earlier asserts; keep it because the spec scenario is "any terminal state" not "each one."

   - `it "ERRS handler is robust to nil overlay"`
     - Defensive case: pass an FSM with `:context nil`. Expect the handler does not signal an error. `main-cb` is still invoked with an error string. (The handler uses `(when (overlayp overlay) (delete-overlay overlay))` per design — confirm that guard works.)
     - This isn't a formal spec scenario but it's defensive behavior implied by the implementation. Comment: `;; Defensive: handler must not blow up if context isn't an overlay`.

   - `it "main-cb is invoked exactly once per terminal state"`
     - Drive each handler. Assert `(length calls)` is 1 after each invocation. (Already covered by the per-state tests, but a single combined assertion guards against double-firing if a future maintainer adds a redundant call.)

3. **Test isolation**: each `it` creates its own buffer + overlay, cleans up in `unwind-protect`. No fixtures needed beyond what's already in `helpers-spec.el`.

## Design rationale

The spec contract is: every terminal FSM state deletes the overlay before invoking `main-cb`. If this fails, the user sees stale "Calling Tools..." text in their parent buffer indefinitely. Asserting on the overlay's deletion in EVERY terminal handler — including DONE, which is structurally a happy-path test — locks down the cleanup invariant.

The "robust to nil overlay" defensive test reflects a real concern: if `gptel-request` is ever called without `:context`, or if the overlay gets garbage-collected mid-flight, the handler must not crash. The implementation uses `(overlayp overlay)` to guard; this test makes that guard load-bearing.

## Design pattern

The hand-built FSM pattern is identical to the send/completion tests. Lift the helper or inline; either works. If lifted, put it in `helpers-spec.el` from task 3 — but flagging that here would change task 3's scope. For this task, inline the pattern; refactor later if the duplication grows.

For asserting on string content, use Buttercup matchers:
- `(expect s :to-match REGEX)` for partial / pattern match.
- `(expect s :to-equal STR)` for exact equality.

For asserting on absence of crashes: simply running the test without an `error` is the assertion. Optionally wrap in `(expect (lambda () (...)) :not :to-throw)` if Buttercup supports that matcher (verify in your install).

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` includes the new file; all `it` blocks pass.
- Each `it` block has a leading scenario-mapping comment (where applicable; defensive cases are explicitly marked as not-from-spec).
- The "no overlay leaks across terminal states" loop test passes for all three handler types.

**Done means**: 6 `it` blocks, all green, terminal-state cleanup contract pinned to handler behavior.

## Context

specs/persistent-agent/spec.md (delta) § "Error handling" — every scenario in this requirement is covered.
design.md § "Layer 2" → terminal-state handlers (`--make-on-done`, `--make-on-errs`, `--make-on-abrt`)
architecture.md § "Components" → "Persistent-agent tool (rebuilt)"
