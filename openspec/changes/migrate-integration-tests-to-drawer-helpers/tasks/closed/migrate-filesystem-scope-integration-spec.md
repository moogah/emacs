---
name: migrate-filesystem-scope-integration-spec
description: Rewrite `fs-integ--load-config-from-yaml` to call `helpers-spec-make-scope-config` directly, eliminating the dead-YAML-helper chain.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/filesystem-scope-integration-spec.el` (modify) — rewrite one local helper (line 119), drop the YAML-construction helper at line 88

## Why

The file's only YAML dependency is concentrated in two local helpers:
- `fs-integ--make-scope-yaml` (lines 88–117) — builds a YAML string
- `fs-integ--load-config-from-yaml` (lines 119–123) — parses + merges via the dead `jf/gptel-scope-yaml--{parse-string,merge-schema-defaults}`

All test bodies call `fs-integ--load-config-from-yaml` and feed the result to `(spy-on 'jf/gptel-scope--load-config :and-return-value …)`. The plist shape is the same as the loader's output, so `helpers-spec-make-scope-config` is a drop-in replacement.

## Implementation steps

1. **Delete** `fs-integ--make-scope-yaml` (lines 88–117). It exists only to feed the YAML loader.
2. **Rewrite** `fs-integ--load-config-from-yaml` (lines 119–123) to accept the same three path-list arguments directly (renaming if helpful, e.g. `fs-integ--make-config`):
   ```elisp
   (defun fs-integ--make-config (read-paths write-paths deny-paths)
     "Build a scope-config plist with the given path lists."
     (helpers-spec-make-scope-config
      :read read-paths
      :write write-paths
      :execute '()
      :modify '()
      :deny deny-paths
      :auth-detection "warn"))
   ```
3. **Update call sites** (lines 159, 188, 211, 230, 271, 294, 315, 344, and any others — grep within the file): each call currently looks like:
   ```elisp
   (yaml (fs-integ--make-scope-yaml READ WRITE DENY))
   (config (fs-integ--load-config-from-yaml yaml))
   ```
   Collapse to:
   ```elisp
   (config (fs-integ--make-config READ WRITE DENY))
   ```
4. **Drop the intermediate `yaml` let-binding** wherever it was only used to thread into `--load-config-from-yaml`.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -E "filesystem-scope|Filesystem scope integration" | head -20
grep -n "jf/gptel-scope-yaml\|fs-integ--make-scope-yaml\|fs-integ--load-config-from-yaml" config/gptel/scope/test/integration/filesystem-scope-integration-spec.el
```

Expect: no matches for the dead helpers (only the new `fs-integ--make-config` survives); every `Filesystem scope integration:` `it` block prints green.

## Context

- Migration target: `helpers-spec-make-scope-config` at `config/gptel/scope/test/helpers-spec.el:170`
- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 1
