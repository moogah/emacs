---
name: remove-dead-after-tab-switch-placeholder
description: Remove the placeholder workspace--after-tab-switch defun and its two advice-add lines in tabs.org. The cycle's two-state-layout task added workspace--persistence-after-tab-switch (the targeted advice the placeholder was waiting to host); the placeholder body is now unreachable (let-bind a name, when name truthy, return nil — provably nil for both branches). Closes end-of-cycle architect Finding 4.
change: refine-workspaces-two-state-layout
status: ready
relations:
  - "discovered-from:two-state-layout"
discovered_by: architect
discovered_class: dead-branch
---

## Register entries cited by this task

- `register/boundary/autosave-guard-pipeline` (reconciled). Stage-1
  attachment surface is the `tab-bar-select-tab` / `tab-bar-switch-to-
  tab` advice list. Removing the dead `:after` advice simplifies that
  surface to one `:after` (the live `workspace--persistence-after-tab-
  switch`), eliminating advice load-order ambiguity. No structural
  contract change; the entry's stage-1 entry-point count drops from
  "placeholder + targeted" to "targeted only" on tab-switch.

## Files to modify

- `config/workspaces/tabs.org` (modify) — delete the `workspace--after-tab-switch` defun and its two `advice-add` lines.
- `config/workspaces/test/tabs-spec.el` (modify) — update the "tab-switch advice / no-ops on tabs not present in the workspaces registry" test to assert against `workspace--persistence-after-tab-switch` directly (the live :after advice). The test should still pin the "no-op off-workspace" behavior but against the actual advice that ships work, not the placeholder.
- `config/workspaces/persistence.org` (modify, doc-only) — remove or update the comment at line 241 referencing the now-deleted placeholder.

## Implementation steps

1. **Remove the placeholder** in `tabs.org`:

   ```elisp
   ;; DELETE:
   (defun workspace--after-tab-switch (&rest _)
     ...)
   (advice-add 'tab-bar-switch-to-tab :after #'workspace--after-tab-switch)
   (advice-add 'tab-bar-select-tab    :after #'workspace--after-tab-switch)
   ```

2. **Re-target the test** in `tabs-spec.el`. The "no-ops on tabs not present in the workspaces registry" test was added in v1 to pin the placeholder's `(when name nil)` early-return. With the placeholder gone, the test should pin the same property against `workspace--persistence-after-tab-switch`:

   ```elisp
   (it "workspace--persistence-after-tab-switch no-ops on non-workspace tabs"
     ;; The cycle-1 two-state-layout task installed this :after advice;
     ;; it should be a no-op when the current tab is not a workspace tab.
     ...)
   ```

3. **Verify the advice surface** is unchanged from the user's perspective:
   - Before: 3 advices on `tab-bar-select-tab` (1 :before from persistence, 2 :after — placeholder + persistence-after).
   - After: 2 advices (1 :before from persistence, 1 :after from persistence).
   - The placeholder did nothing; its removal cannot change behavior.

4. **Update the comment** in `persistence.org` (around line 241) that references the placeholder.

## Design rationale

The placeholder pre-dated this cycle (v1 added it as an extension point). The cycle's `two-state-layout` task filled the implicit extension slot with `workspace--persistence-after-tab-switch` (a SEPARATE function with a SEPARATE :after advice). The result: two `:after` advices on the same primitive, one of which is unconditionally `nil`.

The architect's end-of-cycle Finding 4 noted this becomes load-bearing for future maintenance: the two-advice composition makes advice load-order non-obvious, and a future contributor extending the placeholder would have to discover (and avoid) collision with the targeted advice. Removing the dead branch simplifies the surface and makes the cycle's persistence-advice path the unambiguous canonical home.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/tabs.org
./bin/tangle-org.sh config/workspaces/persistence.org
./bin/run-tests.sh -d config/workspaces
```

- `grep -n "workspace--after-tab-switch" config/workspaces/` returns nothing.
- Test count unchanged (the re-targeted test is the same count, against a different advice).
- The "tab-switch advice / survives a tab-bar-select-tab round-trip" test (which doesn't reference the placeholder by name) still passes.

## Context

- Finding ID: `arch-cycle-20260524-200631-eoc-4` (end-of-cycle, advisory, call-graph-dead-branch).
- Findings file: `.orchestrator/cycles/cycle-20260524-200631/findings/end-of-cycle-audit.md`.
- Note: the placeholder is **pre-existing** (not introduced this cycle). The cycle made it visibly dead by filling the extension slot. Architect's recommendation was Option A (remove); Option B (keep with a comment clarifying the deferred catalog-pattern-9 intent) and Option C (informational, no action) were also discussed. This task picks Option A.
- Related to catalog pattern 9 ("per-workspace activities-style hooks") which is explicitly a design.md non-goal. If that pattern is ever ported, the new code path adds its own targeted advice; reviving the placeholder is not the right shape.
