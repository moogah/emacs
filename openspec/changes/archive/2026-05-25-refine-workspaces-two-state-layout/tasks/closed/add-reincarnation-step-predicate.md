---
name: add-reincarnation-step-predicate
description: Add a workspace--valid-reincarnation-steps defconst and workspace--reincarnation-step-p predicate to symmetrize closed-set discipline with workspace-state-slot. The fallback chain (bookmark → filename → name → error-buffer) is currently a closed set in name only — step symbols appear only in comments. Closes end-of-cycle architect Finding 2 + on-touch Finding 3.
change: refine-workspaces-two-state-layout
status: done
relations:
  - "discovered-from:bookmark-reincarnation"
discovered_by: architect
discovered_class: invariant-gap
---

## Files to modify

- `config/workspaces/layouts.org` (modify) — add the defconst + predicate near the existing `workspace--valid-state-slots` / `workspace--state-slot-p` pair.
- `config/workspaces/test/buffer-reincarnation-spec.el` (modify) — add a unit test asserting the closed set's membership and the four shipped step symbols.

## Implementation steps

1. **Add the constants and predicate** in `layouts.org`'s "Buffer reincarnation" section, mirroring the workspace-state-slot pattern:

   ```elisp
   (defconst workspace--valid-reincarnation-steps
     '(bookmark filename name error-buffer)
     "Closed, ordered set of reincarnation-chain step symbols.
   The order is load-bearing in production via the `or' chain in
   `workspace--deserialize-buffer'. Adding a fifth member is a
   register-level change (see register/vocabulary/buffer-reincarnation-
   fallback-chain), not a code-level addition.")

   (defun workspace--reincarnation-step-p (step)
     "Return non-nil if STEP names a valid reincarnation-chain step."
     (memq step workspace--valid-reincarnation-steps))
   ```

2. **Add the test** in `buffer-reincarnation-spec.el`:

   ```elisp
   (describe "reincarnation step closed-set"
     (it "names exactly four steps in chain order"
       (expect workspace--valid-reincarnation-steps
               :to-equal '(bookmark filename name error-buffer)))
     (it "predicate accepts every shipped step"
       (dolist (s workspace--valid-reincarnation-steps)
         (expect (workspace--reincarnation-step-p s) :to-be-truthy)))
     (it "predicate rejects unrelated symbols"
       (expect (workspace--reincarnation-step-p 'mystery) :to-be nil)
       (expect (workspace--reincarnation-step-p nil) :to-be nil)))
   ```

3. **Reconcile the register entry** — update `register/vocabulary/buffer-
   reincarnation-fallback-chain`'s `closed_set_enforcement` field to point
   at the new predicate (the `closed_set: true` flag is no longer
   aspirational). This is a documentation update, not a status flip.

## Design rationale

End-of-cycle architect Finding 2 surfaced a closed-set discipline asymmetry: this cycle added `workspace--state-slot-p` for one of the two new vocabulary entries but not the other. The asymmetry is regression-vector: a future contributor adding a fifth fallback step (e.g. `template`) is more likely to add it to the `or` chain in `workspace--deserialize-buffer` without updating the register entry. The predicate is structural enforcement of the closed-set claim.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/run-tests.sh -d config/workspaces
```

- `grep -n "workspace--valid-reincarnation-steps\|workspace--reincarnation-step-p" config/workspaces/layouts.el` shows both defined.
- Test count: 110 → 113 (or higher).

## Context

- Finding IDs: `arch-cycle-20260524-200631-eoc-2` (end-of-cycle, advisory) and `arch-cycle-20260524-200631-on-touch-bookmark-reincarnation-3` (on-touch, advisory; same shape).
- Findings files: `.orchestrator/cycles/cycle-20260524-200631/findings/end-of-cycle-audit.md`, `.orchestrator/cycles/cycle-20260524-200631/findings/on-touch-bookmark-reincarnation.md`.
- Related register entry: `register/vocabulary/buffer-reincarnation-fallback-chain` (reconciled in cycle 1; this task closes the closed-set-enforcement gap noted in its reconciliation).
