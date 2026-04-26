---
name: coordinator-lock
description: Implement org-graph-coordinator/with-file-lock with sequential lock semantics so concurrent agent writes to the same file serialize cleanly.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:test-helpers"
---

## Files to modify

- `config/org-graph/test/coordinator/lock-semantics-spec.el` (new) — Buttercup spec, written first.
- `config/org-graph/org-graph.org` (modify) — fill the `Coordinator` subtree.

## Implementation steps

1. Write the spec first. `describe "org-graph-coordinator/with-file-lock"`:
   - acquires lock, runs body, releases lock (assert via inspecting the private `org-graph-coordinator--locks` hash before/during/after).
   - releases lock when body raises (use `condition-case` around the macro call; assert lock cleared).
   - locks on distinct paths are independent (a held lock on path-A does not prevent acquiring path-B).
   - canonicalizes paths: `~/foo` and the absolute equivalent map to the same lock entry.
   - timeout: a second acquire attempt while held raises `org-graph-coordinator-timeout` after `org-graph-coordinator-timeout` seconds. Test by stubbing `accept-process-output` to advance time without real waiting.

   These are sequential simulations — DO NOT spawn timers or real concurrent calls (per architecture decision: lock-semantics-only coverage).

2. Implement in the `Coordinator` subtree:
   ```elisp
   (defvar org-graph-coordinator--locks (make-hash-table :test 'equal))

   (defun org-graph-coordinator--canonicalize (path)
     (file-truename (expand-file-name path)))

   (defmacro org-graph-coordinator/with-file-lock (path &rest body)
     "Run BODY with PATH locked; queue if PATH is already locked.
     Distinct PATHs do not block each other. Releases on error."
     (declare (indent 1) (debug t))
     `(let* ((canon (org-graph-coordinator--canonicalize ,path))
             (deadline (+ (float-time) org-graph-coordinator-timeout)))
        (while (gethash canon org-graph-coordinator--locks)
          (when (> (float-time) deadline)
            (error "org-graph-coordinator: timeout waiting for lock on %s" canon))
          (accept-process-output nil 0.05))
        (puthash canon t org-graph-coordinator--locks)
        (unwind-protect
            (progn ,@body)
          (remhash canon org-graph-coordinator--locks))))
   ```

3. Define a small helper for tests: `org-graph-coordinator--locked-p (path)` returning t if path is currently locked. Used by tests to assert state without grovelling the hash directly.

4. Run tests until green: `./bin/run-tests.sh -d config/org-graph/test/coordinator`.

## Design rationale

Emacs is single-threaded for elisp, so "concurrency" here means overlapping callbacks driven by gptel tool dispatch and timers (design.md §D5). A cooperative lock with `accept-process-output` polling is the established pattern in this codebase (filesystem-scope serialization uses the same shape). `unwind-protect` makes error-recovery automatic — no manual cleanup needed in callers.

Path canonicalization via `file-truename` matters because gptel tools might be handed a relative path or a symlinked path; without canonicalization, two writes that resolve to the same file would slip past the lock.

The 5-second timeout default protects against a genuinely-stuck lock (e.g. a runaway tool that never releases). `accept-process-output` with a 50ms slice keeps the wait responsive without burning CPU.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test/coordinator` — green.
- `grep -n "defmacro org-graph-coordinator/with-file-lock" config/org-graph/org-graph.el` — matches.
- `grep -n "unwind-protect" config/org-graph/org-graph.el` — at least one match in the coordinator section.

## Context

- design.md §D5
- architecture.md §Components §org-graph-coordinator
- architecture.md §Testing Approach §Coordinator tests
- specs/org-graph/spec.md §Agent-Facing Graph Tools (concurrent-write scenario)
