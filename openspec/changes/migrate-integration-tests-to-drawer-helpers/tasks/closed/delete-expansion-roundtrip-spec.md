---
name: delete-expansion-roundtrip-spec
description: Delete the file entirely — tests "expand → write scope.yml → reload" round-trip; both the on-disk YAML format and the YAML loader are gone, and the drawer round-trip is already covered by `config/gptel/scope/test/drawer/*-spec.el`.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/expansion-roundtrip-spec.el` (delete)

## Why

The file's documented invariant (line 22) is: "expand scope → write scope.yml → reload scope.yml → contract valid via `jf/gptel-scope-yaml--load-schema`." Cycle-3 retired:

- The on-disk `scope.yml` format (writers now use `jf/gptel-scope--write-pattern-to-drawer` → `org-entry-put-multivalued-property`)
- The YAML loader (the "reload" step has no reader)

The file's helper `roundtrip--reload-and-validate` (lines 107–118) calls `helpers-spec-load-scope-config` (line 110), which is now a deprecated stub. There is no path from the current writer output to a YAML-style reload, because the writer doesn't produce YAML.

The behavior the file *would* test in a post-YAML world — that the writer produces a drawer that round-trips to a valid scope config — is already covered by:

- `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` — write-side drawer idempotency
- `config/gptel/scope/test/drawer/*-spec.el` — drawer reader / writer contract
- The validator tests (`test/validation/`) — feed the loader output to validators; that asserts the loader's plist shape

Deletion is clean — no behavior loses coverage. The migration target (rewrite to drawer round-trip) would duplicate `test/drawer/*` for no incremental coverage.

## Implementation steps

1. **Delete** the file:
   ```bash
   rm config/gptel/scope/test/integration/expansion-roundtrip-spec.el
   ```
2. **Audit** for cross-references:
   ```bash
   grep -rn "expansion-roundtrip-spec" config/ openspec/ .orchestrator/ 2>/dev/null
   ```
   Expected: zero matches (test-only file, no shared provide).

## Verification

```bash
ls config/gptel/scope/test/integration/expansion-roundtrip-spec.el 2>&1
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "roundtrip|round-trip" | head -10
./bin/run-tests.sh -d config/gptel/scope/test/drawer 2>&1 | tail -5
```

Expect: file is gone; no round-trip specs run from the integration directory; the drawer test directory still passes (the surviving coverage for what this file used to test).

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 9
- Surviving drawer round-trip coverage: `config/gptel/scope/test/drawer/`
