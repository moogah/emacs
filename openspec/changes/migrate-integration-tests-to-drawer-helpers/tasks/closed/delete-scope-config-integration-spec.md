---
name: delete-scope-config-integration-spec
description: Delete the file entirely — every scenario tests YAML parsing pipeline behavior that was deleted in cycle-3.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/scope-config-integration-spec.org` (delete)
- `config/gptel/scope/test/integration/scope-config-integration-spec.el` (delete)

## Why

The file's commentary (lines 20–24 and 70–71 of the .org) explicitly describes it as testing the YAML parsing pipeline: "every test uses `jf/gptel-scope-yaml--parse-string` and `jf/gptel-scope-yaml--merge-schema-defaults`." All seven scenarios are YAML-specific:

| Scenario | Tests |
|---------|-------|
| 1. Parse round-trip | YAML → plist → YAML cycle (the YAML parser is gone) |
| 2. Schema merge with defaults | `--merge-schema-defaults` (function is gone) |
| 3. Path validator permissive | Uses deprecated `helpers-spec--scope-with-paths` + on-disk YAML loader |
| 4. Path validator restrictive | Same deprecated helpers |
| 5. Cloud validator | Calls deleted YAML parser directly |
| 6. Contract satisfaction | Calls deleted YAML parser |
| 7. YAML boolean normalization | Tests YAML-specific keyword coercion (`:true` → `t`) |

None of these test current behavior. The post-YAML scope-config plist shape is already covered by:
- `config/gptel/scope/test/validation/path-validation-spec.el` (path validator behavior — uses `helpers-spec-make-scope-config` directly)
- `config/gptel/scope/test/validation/cloud-auth-spec.el` (cloud auth — same)
- `config/gptel/scope/test/drawer/*-spec.el` (drawer reader contract)
- `config/gptel/scope/interfaces.el` + the contract validator (plist shape invariant)

Deletion is clean — no behavior loses coverage.

## Implementation steps

1. **Delete** the literate source:
   ```bash
   rm config/gptel/scope/test/integration/scope-config-integration-spec.org
   ```
2. **Delete** the tangled `.el` (the tangler doesn't clean up after a source delete):
   ```bash
   rm config/gptel/scope/test/integration/scope-config-integration-spec.el
   ```
3. **Audit** for any cross-references (other tests `require`-ing this file's `provide` symbol, the orchestrator's register, etc.):
   ```bash
   grep -rn "scope-config-integration-spec" config/ openspec/ .orchestrator/ 2>/dev/null
   ```
   Likely zero matches (it's a test-only file with no provide name that other tests depend on). Address any survivors before closing the task.

## Verification

```bash
ls config/gptel/scope/test/integration/scope-config-integration-spec.* 2>&1 | head -5
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "scope-config-integration\|Scope config integration" | head -10
```

Expect: `ls` reports "no such file"; no specs from this file run; failure count drops by the file's spec count (~8 from the current report).

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 8
- The deleted scenarios cover removed behavior (cycle-3 `register/invariant/scope-no-security-key-in-plist` + the deletion of `--merge-schema-defaults`)
