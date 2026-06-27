---
name: surgical-bash-parser-contract-layers-spec
description: Delete the YAML-dependent "Scope config layers" describe (Layers 1–2, ~140 lines); migrate fixture builders for the remaining real bash-parser wiring tests (Layers 3–6).
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/bash-parser-contract-layers-spec.org` (modify — literate source)
- `config/gptel/scope/test/integration/bash-parser-contract-layers-spec.el` (regenerated)

## Why

The file has two architecturally distinct sections:

1. **"Scope config layers" describe (lines 189–332 in the .org)** — Layers 1–2 test the deleted YAML schema sections (`security`, `bash_tools`). All assertions reference behavior that was deleted in cycle-3 (`register/invariant/scope-no-security-key-in-plist`). No corresponding post-YAML behavior; nothing to migrate to.

2. **Layers 3–6 describes (lines 350–412 in the .org)** — permissive/restrictive/corpus/error-shape tests that exercise the real bash-parser ↔ scope-validation contract. These remain valuable; only the fixture helpers need updating.

## Implementation steps

1. **Delete** the entire "Scope config layers" `describe` block in the .org source (lines 189–332 in the current source layout). Search for the literal text `(describe "Scope config layers"` and remove that form through its closing `)`.
2. **Rewrite** the fixture builders (lines 205–224 in the source — but those will move after the delete, so locate them relative to the surviving describes):
   ```elisp
   (defun contract-layers-spec--permissive-config ()
     (helpers-spec-make-scope-config
      :read '("/**") :write '("/**") :execute '("/**") :modify '("/**")
      :deny '() :auth-detection "warn"))

   (defun contract-layers-spec--restrictive-config ()
     (helpers-spec-make-scope-config
      :read '() :write '() :execute '() :modify '() :deny '()
      :auth-detection "deny"))
   ```
3. **Audit remaining inline call sites** of `helpers-spec--scope-with-paths` / `helpers-spec-make-scope-yml` / `helpers-spec-load-scope-config` (the agent found 6 in this file before the delete; after the delete most should be inside the removed L1-L2 describe). For any survivors in L3–L6, rewrite to use the fixture builders or call `helpers-spec-make-scope-config` directly.
4. **Re-tangle**:
   ```bash
   ./bin/tangle-org.sh config/gptel/scope/test/integration/bash-parser-contract-layers-spec.org
   ```

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "contract layers|contract-layers" | head -20
grep -n "jf/gptel-scope-yaml\|helpers-spec-load-scope-config\|helpers-spec--scope-with-paths\|Scope config layers" config/gptel/scope/test/integration/bash-parser-contract-layers-spec.org config/gptel/scope/test/integration/bash-parser-contract-layers-spec.el
wc -l config/gptel/scope/test/integration/bash-parser-contract-layers-spec.org
```

Expect: tangle succeeds; no matches for deleted helpers or for the deleted describe-block name; the file is ~140 lines shorter; the surviving Layer 3–6 describes pass.

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 7
