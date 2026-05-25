---
name: extract-workspace-explicit-save-into-group
description: Extract a workspace--explicit-save-into-group helper that workspace-save and workspace-save-layout both call. After cycle 2's unify-layout-construction-paths landed, workspace-save-layout became structurally near-identical to workspace-save (both: pre-set recent pointer → call workspace--autosave-current-layout :saved-state → optionally clear :working-state). The project overlay's architect.severity-overrides.duplication: blocking posture justifies a follow-up extraction.
source: refine-workspaces-two-state-layout
status: ready
relations:
  - "discovered-from:unify-layout-construction-paths"
discovered_by: reviewer
discovered_class: duplication
---

## Files to modify

- `config/workspaces/layouts.org` (modify) — extract `workspace--explicit-save-into-group GROUP-NAME` helper containing the pre-set-recent + call-canonical-helper pattern.
- `config/workspaces/persistence.org` (modify) — `workspace-save` calls the new helper with the current recent group's name (or stays at its current `workspace--autosave-current-layout :saved-state` direct call if the helper is purely for non-current target groups).
- `config/workspaces/test/layouts-spec.el` (modify) — the cross-producer shape-equivalence test should keep passing without modification; the extraction is a refactor, not a behavior change.

## Implementation steps

1. **Read both call sites side-by-side**:
   - `workspace-save` in `persistence.org`.
   - `workspace-save-layout` in `layouts.org` (post-cycle-2 funnel state).

2. **Identify the shared shape**: pre-set the recent layout-group pointer to the target name, call `workspace--autosave-current-layout :saved-state`, clear `:working-state` afterward.

3. **Extract the helper** in `layouts.org` near the other layout helpers:

   ```elisp
   (defun workspace--explicit-save-into-group (ws group-name)
     "Capture current frame into WS's GROUP-NAME layout-group as the
   saved baseline.  Routes through `workspace--autosave-current-layout'
   :saved-state so the layout shape stays canonical."
     (setf (workspace--recent-group ws) group-name)
     (workspace--autosave-current-layout :saved-state)
     ;; Explicit save always clears working-state, including on re-save
     ;; of an existing group (the canonical helper preserves prior
     ;; :working-state in the existing-group cond arm).
     (let* ((group (workspace--find-group ws group-name))
            (layout (workspace--group-recent-layout group)))
       (when layout (plist-put layout :working-state nil))))
   ```

4. **Replace both call sites** with the new helper. `workspace-save` calls it with `(workspace--recent-group ws)`'s current value (re-saving the current group). `workspace-save-layout` calls it with the user-supplied name.

5. **Verify the extraction is structurally equivalent** by running the cycle-2 cross-producer shape-equivalence test (it pre-populates `:etc` and asserts round-trip through all three variants).

## Design rationale

After cycle-2's `unify-layout-construction-paths` (commit `df89ce2`), `workspace-save-layout` is structurally near-identical to `workspace-save`. The overlay's `architect.severity-overrides.duplication: blocking` posture treats this class with prejudice — extracting a named helper makes the canonical shape grep-able and prevents drift if a future maintainer touches one path without the other.

This was reviewer-flagged as advisory during cycle 2 (per `.orchestrator/cycles/cycle-20260525-082618/reviews/unify-layout-construction-paths.md`), not as a blocking finding — the duplication is small and the structural correctness is pinned by the cross-producer shape-equivalence test. Doing this in a separate small change keeps cycle 2's scope clean and gives the helper a dedicated review.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/tangle-org.sh config/workspaces/persistence.org
./bin/run-tests.sh -d config/workspaces
```

Specific assertions:

- `grep -n "workspace--autosave-current-layout :saved-state" config/workspaces/*.el` shows only one or two call sites — both inside `workspace--explicit-save-into-group`.
- Cross-producer shape-equivalence test continues to pass.
- Test count unchanged (refactor only).

## Context

- Cycle 2 review: `.orchestrator/cycles/cycle-20260525-082618/reviews/unify-layout-construction-paths.md` (Finding 3, advisory).
- Cycle 2 merge: commit `df89ce2` introduced the duplication by funneling `workspace-save-layout` through the canonical helper.
- Related register entry: `register/shape/layout-v2-plist` (reconciled in cycle 1; cycle 2 resolved its `producer_fragmentation_note`).
- Architect overlay: `.claude/orchestrator/config.yaml` line 70 (`architect.severity-overrides.duplication: blocking`).
