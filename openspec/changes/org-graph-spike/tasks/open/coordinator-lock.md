---
name: coordinator-lock
description: Implement org-graph-coordinator/with-file-lock with sequential lock semantics so concurrent agent writes to the same file serialize cleanly.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:test-helpers
---

## Files to modify
- `config/org-graph/coordinator.el` ← via `config/org-graph/org-graph.org`
  (Coordinator section)
- `config/org-graph/test/coordinator-spec.el` (new)

## Implementation steps
1. Module-private hash table mapping canonicalized absolute paths to lock
   state.
2. `org-graph-coordinator/with-file-lock (path &rest body)` macro:
   - Canonicalize `path` via `expand-file-name` + `file-truename`.
   - If unlocked: mark locked, run BODY, release in `unwind-protect`.
   - If locked: cooperative busy-wait via `accept-process-output` with a short
     timeout, polling lock state, up to a configurable ceiling (default 5s),
     then signal a structured error.
   - Distinct paths are independent; errors and non-local exits release the
     lock.
3. Emacs elisp is single-threaded — "concurrency" here is overlapping
   callbacks from gptel tool dispatch and timers, not OS threads. A
   cooperative lock is the idiomatic pattern (used for filesystem-scope
   serialization elsewhere in the codebase).
4. Write `coordinator-spec.el` (sequential, in-process): BODY runs to
   completion under the lock; a second acquire on the same path waits until
   the first releases (inspect the in-process lock table, do not spawn real
   timers); an error in BODY releases the lock; locks on distinct paths are
   independent.

## Design rationale
D5: a per-file cooperative lock prevents overlapping agent writes from
corrupting the same file, without an OS `flock` or an async write pipeline
(overkill at the spike's handful-of-calls-per-minute scale). The coordinator
is location-agnostic; it serializes any write routed through it. In the
re-centered design its natural context is per-workspace agent writes already
directory-scoped by `GPTEL_WORK_ROOT` (RE-5).

## Design pattern
`unwind-protect` for guaranteed release; `accept-process-output` for the
cooperative wait. Macro hygiene: evaluate `path` once. See design.md D5 for
the full semantics.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — coordinator spec passes all
  four behaviors (runs, queues, error-releases, distinct-independent).
- The macro releases the lock on a thrown error (assert the path is unlocked
  after a `condition-case`-wrapped failing BODY).

## Context
design.md § Decisions D5; architecture.md § Components (org-graph-coordinator).
</content>
