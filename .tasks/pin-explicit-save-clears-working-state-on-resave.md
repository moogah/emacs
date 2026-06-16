---
name: pin-explicit-save-clears-working-state-on-resave
description: Strengthen the cycle-2 cross-producer shape-equivalence test in layouts-spec.el so it pre-populates :working-state on an existing layout before workspace-save-layout, then asserts :working-state is nil afterward. As shipped the variant-2 spec pre-populates :etc but does NOT pre-populate :working-state, leaving the canonical-helper-preserves + explicit-clear-step pairing under-pinned for the re-save case.
source: refine-workspaces-two-state-layout
status: ready
relations:
  - "discovered-from:unify-layout-construction-paths"
discovered_by: reviewer
discovered_class: test-gap
---

## Files to modify

- `config/workspaces/test/layouts-spec.el` (modify) — add a one-line pre-populate of `:working-state` to the variant-2 portion of the `"layout-v2-plist producer shape equivalence"` describe block, plus a one-line post-assertion that `:working-state` is nil after `workspace-save-layout`.

## Implementation steps

1. Locate the variant-2 portion of the `"layout-v2-plist producer shape equivalence"` describe block in `layouts-spec.el` (added by cycle 2 commit `4d1afee`).
2. Before the `(workspace-save-layout "magit")` line, add a pre-populate step that puts a sentinel value into the magit layout's `:working-state` (parallel to the existing `:etc` pre-populate).
3. After `(workspace-save-layout "magit")`, add an assertion that `:working-state` of the magit layout is now nil.

```elisp
;; Inject working-state alongside :etc.
(plist-put layout :working-state '(sentinel . working-pre-save))
...
;; After workspace-save-layout: :working-state cleared (canonical
;; helper preserves it via the :saved-state cond arm; the explicit
;; clear-step in workspace-save-layout compensates).
(expect (plist-get layout :working-state) :to-be nil)
```

The existing `:etc` round-trip assertion stays.

## Design rationale

The cycle-2 unify-layout-construction-paths task changed `workspace-save-layout` from "construct fresh layout (wipes :working-state)" to "funnel through canonical helper (preserves :working-state) + explicit clear step". The test pre-populates `:etc` and asserts it round-trips, which exercises the canonical helper's preservation. But the test does NOT pre-populate `:working-state` and assert it gets cleared — the regression vector named explicitly in the task brief and in the implementor's report.

Adding two lines closes the test gap. As shipped the regression would only catch fragmentation in `:etc`; a regression in the explicit-clear-step (e.g. a refactor that drops the `plist-put :working-state nil` line) would not be caught for the re-save case (it IS caught for the initial-save case by the pre-existing line-382 invariant test, but that doesn't exercise the re-save path through the funnel).

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

Specific assertion: variant-2 now exercises both `:etc` preservation AND `:working-state` clear-on-resave through the canonical helper. Test count grows by 1 (or stays the same if the assertion is added inline).

## Context

- Cycle 2 review: `.orchestrator/cycles/cycle-20260525-082618/reviews/unify-layout-construction-paths.md` (Finding 2, advisory).
- Cycle 2 commit: `4d1afee` (unify-layout-construction-paths) introduced the funnel + explicit-clear pairing.
- Related register entry: `register/shape/layout-v2-plist` (resolved producer_fragmentation_note in cycle 2).
- Pre-existing test at `layouts-spec.el:~382` ("leaves the named layout's :working-state nil") covers initial-save but not re-save.
