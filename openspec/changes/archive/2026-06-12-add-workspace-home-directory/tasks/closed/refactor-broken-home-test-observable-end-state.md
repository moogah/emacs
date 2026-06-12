---
name: refactor-broken-home-test-observable-end-state
description: Reframe workspace-purge broken-home spec to assert observable end-state (no error + clean registry) instead of mocking delete-directory globally
change: add-workspace-home-directory
status: done
relations:
  - discovered-from:workspace-delete-and-purge
merge_commit: b840bad9c0b579c25cbc086ef66ad95ef40e15c8
implementor_commit: 8a4b55dddc6212bd83b9d84512e8a1e38ac10603
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: workspace-delete-and-purge
     discovered_by: reviewer (workspace-delete-and-purge, Finding 2,
       severity: advisory, direction: sub-par-code)
     discovered_class: sub-par-code
     review_file: .orchestrator/cycles/cycle-20260526-191802/reviews/workspace-delete-and-purge.md
     reconciled_into: n/a — pure test-quality cleanup -->

## Why this task exists

The cycle-4 `workspace-delete-and-purge` reviewer (advisory Finding 2)
identified that the broken-home test in
`config/workspaces/test/workspace-delete-purge-spec.el:258-276`
**pins the implementation's call shape**, not its behavioural
contract.

Current shape:

```elisp
;; broken-home test (workspace-delete-purge-spec.el)
(let (delete-calls)
  (cl-letf (((symbol-function 'delete-directory)
             (lambda (&rest _) (cl-incf delete-calls))))
    (workspace-purge "name"))
  (expect delete-calls :to-equal 0))
```

The cycle-4 implementation guards the call:

```elisp
(when (file-directory-p home)
  (delete-directory home t))
```

The test asserts the **inner shape** of the guard (`delete-directory`
is not called). The spec scenario from the task body said:

> Setup: registry entry whose `:home` points at a path that does
> not exist. Invoke. Assert: no error; registry entry removed; no
> attempt made to delete a nonexistent path.

The first two are observable end-state. The third ("no attempt to
delete") has a behavioural form: assert that the call completes
without error and that the registry is clean. A future refactor that
swaps the guard for, say, `(ignore-errors (delete-directory home t))`
— same end-state, arguably cleaner — would **fail the current test**
for stylistic reasons.

Project reviewer overlay (`config/.claude/orchestrator/roles/reviewer.md`,
"Shared mutable state" + "Test conventions") calls out global
monkey-patching as a smell. `cl-letf`-scoped to the
test body is better than `defun`-shadowing, but the test surface
still observes the global call shape rather than per-function
contract.

## Files to modify

- `config/workspaces/test/workspace-delete-purge-spec.el` — modify
  the broken-home test (~lines 258-276).

## Implementation steps

1. Read the existing broken-home spec.

2. Replace the `cl-letf` + `delete-calls` assertion with observable-
   end-state assertions:

   ```elisp
   (it "no-ops on filesystem when :home no longer exists"
     ;; Setup: registry entry whose :home points at a removed path.
     (let* ((parent (make-temp-file "wdp-bh-" t))
            (home (expand-file-name "missing-ws" parent)))
       (puthash "missing-ws"
                (workspace--make "missing-ws" home)
                workspace--registry)
       ;; Verify home does NOT exist (precondition).
       (expect (file-directory-p home) :to-be nil)
       ;; Stub yes-or-no-p; current-prefix-arg lets safeguard pass.
       (let ((current-prefix-arg '(4)))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) t)))
           ;; Should complete without error.
           (workspace-purge "missing-ws")))
       ;; Observable end-state:
       (expect (gethash "missing-ws" workspace--registry) :to-be nil)
       (expect (file-directory-p parent) :to-be t)  ; parent untouched
       ;; (Don't need a delete-calls counter — the no-error assertion
       ;; above is the behavioural contract.)
       (delete-directory parent t)))
   ```

3. Tangle test file? No — `.el` test files are hand-written
   (only `config/<subsystem>/<module>.org` literates; test files in
   `config/<subsystem>/test/` are not literate). Verify by:
   `ls config/workspaces/test/*.org 2>/dev/null` should return nothing.

4. Run: `./bin/run-tests.sh -d config/workspaces`.

## Design rationale

Behavioural tests assert observable contracts. Mock-and-count tests
assert implementation shape, which couples the test to a single
correct implementation rather than the family of correct
implementations.

The replacement assertion ("no error + registry clean + parent
untouched") covers every meaningful behavioural property of the
scenario:

- **No error**: caught by the implicit assertion that the
  `workspace-purge` call completes (Buttercup will raise if it
  signals).
- **Registry clean**: `gethash :to-be nil`.
- **No filesystem damage**: `parent untouched` (a directory above
  `:home` that should never be a delete target regardless of
  implementation).

A future refactor that replaces the explicit `file-directory-p` guard
with `(ignore-errors (delete-directory home t))` or any equivalent
form would pass these assertions because the end-state is the same.

## Verification

- `./bin/run-tests.sh -d config/workspaces` → 224 specs, 0 failed
  (no count change; one test refactored).
- Manual: `grep -A 30 "no-ops on filesystem" config/workspaces/test/workspace-delete-purge-spec.el`
  should show no `delete-calls` counter and no `cl-letf` on
  `delete-directory`.

## Context

- Reviewer Finding: `.orchestrator/cycles/cycle-20260526-191802/reviews/workspace-delete-and-purge.md` § Finding 2
- Project reviewer overlay: `.claude/orchestrator/roles/reviewer.md`
  § "Shared mutable state" + "Test conventions"
- Core test guidance: CLAUDE.md § "Test levels — Behavioural"
  ("Tests declare preconditions explicitly rather than building up
  stateful test infrastructure")

## Observations

- **Departure from task-body template (intentional)**: the template
  in the task body wrapped the call in `(let ((current-prefix-arg
  '(4))) ...)` and re-built the registry entry from scratch via
  `puthash ... (workspace--make ...)`. Neither is needed for the
  broken-home scenario because the seeded `:home` is under
  `wdp-spec--tmp-parent` (which the `before-each` binds as
  `workspaces-default-parent-directory`), so the scope safeguard does
  not fire and no prefix arg is required. Using the file's existing
  `wdp-spec--make-workspace` helper (with `broken=t`) keeps the test
  consistent with the four sibling tests in the file (all of which
  go through the same helper). The behavioural contract asserted is
  identical to the template's: no error + registry clean + parent
  untouched + flush exactly once.

- **One extra assertion added vs. task template**: I added
  `(expect 'workspace--flush-state :to-have-been-called-times 1)` to
  match the four sibling `workspace-purge` happy-path / safeguard
  tests in this `describe` group — every other purge-succeeds test
  in the file pins the flush count. Dropping it on the broken-home
  test only would weaken the regression net for "broken purge
  forgets to flush". Buttercup spec-count is unchanged (224); only
  the assertion list inside the same `it` differs.

- **Test count delta**: 0. Baseline 224 → post-refactor 224, both
  with 0 failed. Verified by running `./bin/run-tests.sh -d
  config/workspaces` before and after the change.

- **Out-of-scope observation (latent, not a defect)**: the
  precondition `(expect (file-directory-p wdp-spec--tmp-parent) :to-be
  t)` is redundant in principle — `wdp-spec--reset` creates this dir
  via `make-temp-file ... t` in the `before-each`. I kept it because
  the SAME assertion is then re-checked AFTER the purge to prove the
  parent was not collaterally deleted; reading them as a pair is
  what makes "parent untouched" a clear assertion. The cost is one
  extra micro-assertion; the benefit is clarity. No action needed.

## Discoveries

(none)
