---
name: isolate-test-helper-load-time-restore
description: Suppress or redirect the load-time workspace--restore call in broken-home-runtime-spec.el (and the same pre-existing pattern in broken-home-load-spec.el) so it does not read the developer's real persistence state
change: add-workspace-home-directory
status: ready
relations:
  - discovered-from:broken-home-tolerance
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: broken-home-tolerance
     discovered_by: reviewer (broken-home-tolerance, Finding 3,
       severity: advisory)
     discovered_class: sub-par-code
     reconciled_into: n/a — pure test-isolation cleanup -->

## Why this task exists

The cycle-3 spec `config/workspaces/test/broken-home-runtime-spec.el`
loads `workspaces.el` at the top level via `(load workspaces-el nil t)`.
That evaluates `workspaces.el`'s top-level
`(when (fboundp 'workspace--restore) (workspace--restore))` block
immediately — BEFORE any test's `before-each` redirects
`workspace--state-directory` to a tmp dir. The
`broken-home-runtime-spec--with-state-file` macro only enters scope
inside `it` clause bodies.

Result: at file-load time, the restore reads from the developer's
**real** on-disk persistence file (`~/emacs-workspaces/state/...`).
The `before-each (clrhash workspace--registry)` blanks the registry
between tests, but does not isolate `*Messages*` or the brief
load-time puthashes.

The new spec's relative-path `it` clause adds a `*Messages*` substring
assertion. The pre-baseline message buffer is now load-bearing for
test correctness.

This is a **pre-existing pattern**: `broken-home-load-spec.el` has the
same structure. The cycle-3 spec inherited it. The fix should land
across both files to keep them consistent.

## Files to modify

- `config/workspaces/test/broken-home-runtime-spec.el` (modify)
- `config/workspaces/test/broken-home-load-spec.el` (modify, same fix)

## Implementation steps

Two options (per the cycle-3 reviewer's Finding 3):

**Option A** (recommended — cheapest local fix): `cl-letf`-stub
`workspace--restore` to a no-op around the top-level
`(load workspaces-el nil t)` call. The `fboundp` guard at the
top of `workspaces.el`'s autorestore block will see the stubbed
function still defined; the restore body becomes a no-op.

```elisp
(cl-letf (((symbol-function 'workspace--restore) (lambda () nil)))
  (load workspaces-el nil t))
```

**Option B** (redirect persistence around the load):
`cl-letf` `workspace--state-directory` to a tmp dir around the load
so the auto-restore at load time hits the tmp dir, not the user's
real state. More involved but more honest about what's being
isolated.

Pick A unless the load-time restore is exercising behaviour the spec
actually needs to test.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

Run on a machine with at least one broken workspace in real
`~/emacs-workspaces/state/` (or simulate by populating a fixture).
Without the fix, the relative-path `it` clause's `*Messages*`
substring search can false-pass / false-fail; with the fix, the
baseline is deterministic.

## Cited register entries

None — pure test-isolation cleanup.

## Notes for the implementor

Pre-existing pattern; not a cycle-3 regression. Low risk; one-line
`cl-letf` per file. Pair the two file changes in one commit.
