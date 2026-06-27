---
name: migrate-parallel-tool-callback-spec
description: Rewrite `parallel--make-empty-scope-config` (and one inline scope construction) to call `helpers-spec-make-scope-config` directly.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/parallel-tool-callback-spec.el` (modify) — rewrite local helper at line 105, plus one inline construction at line 402

## Implementation steps

1. **Rewrite** `parallel--make-empty-scope-config` (lines 105–128). Currently:
   ```elisp
   (defun parallel--make-empty-scope-config ()
     (jf/gptel-scope-yaml--merge-schema-defaults
      (jf/gptel-scope-yaml--parse-string "paths:\n  read: []\n  write: []\n  …")))
   ```
   New body:
   ```elisp
   (defun parallel--make-empty-scope-config ()
     "Build the empty/permissive scope-config plist for parallel-tool-callback specs."
     (helpers-spec-make-scope-config
      :read '() :write '() :execute '() :modify '() :deny '()
      :auth-detection "warn"))
   ```
2. **Rewrite the inline construction at line 402–416.** Same shape: extract the path lists from the inline YAML string, pass them to `helpers-spec-make-scope-config` directly. Drop the YAML string literal.
3. **Verify queue / expansion helpers still exist.** The file references `jf/gptel-scope--process-expansion-queue`, `jf/gptel-scope--expansion-active`, `jf/gptel-scope--expansion-queue`. Confirm via `grep -n "(defun jf/gptel-scope--process-expansion-queue\|(defvar jf/gptel-scope--expansion-active\|(defvar jf/gptel-scope--expansion-queue" config/gptel/scope/scope-expansion.el`. Cycle-3 may have renamed them; flag the task if so (no rename expected, but a quick check is cheap insurance).

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "parallel|transient collision" | head -20
grep -n "jf/gptel-scope-yaml" config/gptel/scope/test/integration/parallel-tool-callback-spec.el
```

Expect: no matches for `jf/gptel-scope-yaml`; the `Parallel tool callback:` describe blocks (five `it` blocks based on the current failure list) all print green.

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 2
