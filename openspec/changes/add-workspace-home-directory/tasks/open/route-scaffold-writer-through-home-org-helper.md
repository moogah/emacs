---
name: route-scaffold-writer-through-home-org-helper
description: Route scaffold.el's home.org writer through the workspace-home-org-path helper instead of inlining (expand-file-name "home.org" home), matching the cycle-3 inline-fix pattern applied to tabs.el
change: add-workspace-home-directory
status: ready
relations:
  - discovered-from:arch-cycle-20260526-171719-01
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: arch-cycle-20260526-171719-01
     discovered_by: architect (end-of-cycle audit)
     discovered_class: duplication
     finding_file: .orchestrator/cycles/cycle-20260526-171719/findings/arch-cycle-20260526-171719-01.md -->

## Why this task exists

End-of-cycle architect finding `arch-cycle-20260526-171719-01`
identified a duplication of the `(expand-file-name "home.org" home)`
literal: cycle-3's inline-fix at commit `c6c1b22` routed `tabs.el`'s
`workspace-default-home-builder` through the canonical helper
`workspace-home-org-path` (from `home-org.el`), but `scaffold.el`'s
`workspace--scaffold-write-home-org` was missed by the same
refactor and still inlines the literal at line 27.

`register/boundary/home-org-read-pipeline` names
`workspace-home-org-path` as the stage-1 producer of the pipeline.
A second writer that bypasses the helper is a duplication of the
boundary's path-resolution responsibility.

Severity is `advisory` (overlay-default `duplication: blocking`
explicitly overridden in the finding because the inlined site is
N=1 and `scaffold.el` is the lone hold-out).

## Files to modify

- `config/workspaces/scaffold.org` (and the regenerated
  `config/workspaces/scaffold.el`).

## Implementation steps

1. In `scaffold.org`, locate `workspace--scaffold-write-home-org`.
   The current implementation looks like:

   ```elisp
   (defun workspace--scaffold-write-home-org (workspace-home name)
     (let ((path (expand-file-name "home.org" workspace-home)))
       (unless (file-exists-p path)
         (with-temp-file path
           ...))))
   ```

2. Replace the inline `(expand-file-name "home.org" workspace-home)`
   with `(workspace-home-org-path workspace-home)`. The helper is in
   `config/workspaces/home-org.el` and is already loaded by the
   workspaces module's load order — confirm `home-org` precedes
   `scaffold` in the loader.

3. Tangle:

   ```bash
   ./bin/tangle-org.sh config/workspaces/scaffold.org
   ```

4. Run the workspaces test suite:

   ```bash
   ./bin/run-tests.sh -d config/workspaces
   ```

   Expected: 209+ specs pass; no behavioural change (the helper
   returns the same string for the same input by definition).

## Cited register entries

- `register/boundary/home-org-read-pipeline` (load_bearing: false,
  confirmed) — defines `workspace-home-org-path` as stage-1.
- `register/invariant/home-org-user-authored-after-creation`
  (load_bearing: true) — the broader invariant whose enforcement
  this routing supports.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces

# Confirm the inlined literal is gone:
grep -n 'expand-file-name "home.org"' config/workspaces/scaffold.el
# Expected: zero matches.

# Confirm the helper is routed through:
grep -n 'workspace-home-org-path' config/workspaces/scaffold.el
# Expected: exactly one match (in workspace--scaffold-write-home-org).
```

## Notes for the implementor

This is a mechanical one-token swap (`expand-file-name` →
`workspace-home-org-path` with arity adjustment). Low risk; no
behavioural change; the test suite asserts the writer's output
shape, not its internal call sequence.

The cycle-3 inline-fix established the pattern for `tabs.el`; this
task ports the same pattern to the one remaining site.

## Context

- Architect finding: `.orchestrator/cycles/cycle-20260526-171719/findings/arch-cycle-20260526-171719-01.md`
- Cycle-3 inline-fix commit: `c6c1b22`
