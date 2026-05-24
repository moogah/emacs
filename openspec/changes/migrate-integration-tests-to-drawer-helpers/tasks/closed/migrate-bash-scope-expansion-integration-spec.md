---
name: migrate-bash-scope-expansion-integration-spec
description: Rewrite `bash-integ--make-scope-config` to call `helpers-spec-make-scope-config` directly. Also unblocks two named "gptel callback contract" failures that are masked by the YAML error today.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el` (modify) — rewrite local helper at line 89

## Why

Two named failures from the failing-test list live in this file:
- `run_bash_command integration: gptel callback contract empty-drawer deny-all defaults route per-violation denial through callback` (line 465)
- `run_bash_command integration: gptel callback contract tool is registered as async in gptel--known-tools` (line 521)

Both are casualties of the YAML symptom — they error at fixture-construction time and never reach their real assertions. After this migration, run them in isolation; either they pass (the YAML error was the only obstacle), or they surface a real underlying defect (file a `.tasks/` item to track).

## Implementation steps

1. **Rewrite** `bash-integ--make-scope-config` (lines 89–125):
   ```elisp
   (defun bash-integ--make-scope-config (read-paths write-paths deny-paths
                                         &optional _bash-deny cloud-auth)
     "Build a scope-config plist for bash-scope-expansion specs."
     (helpers-spec-make-scope-config
      :read read-paths
      :write write-paths
      :execute '()
      :modify '()
      :deny deny-paths
      :auth-detection (or cloud-auth "warn")))
   ```
   The `_bash-deny` parameter is preserved for call-site compatibility but ignored (bash-tools deny lists were retired in cycle-3).
2. **Update call sites** in the file: they currently destructure the result of the YAML-parsing helper; now they receive the plist directly. Most call sites should be unchanged because the function name and signature are preserved; the body change is the only edit.
3. **Run the two named failures in isolation** after the migration to confirm the assertion-layer status (pass → done; fail → log the residual defect as a follow-up `.tasks/` item; do NOT in-scope-fix it here).

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "run_bash_command integration|bash-scope-expansion" | head -20
grep -n "jf/gptel-scope-yaml" config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el
```

Expect: no YAML matches; the two named "gptel callback contract" tests either pass or fail on a real assertion (not on `void-function`).

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 3, plus the cross-reference to Workstream C #3 and #4
