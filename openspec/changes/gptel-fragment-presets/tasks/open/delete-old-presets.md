---
name: delete-old-presets
description: Verify no snapshot/count test depends on the presets directory, delete the legacy .md presets, drop the yaml dependency if now unused, and refresh test-report snapshots.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:registration-rewrite"
  - "blocked-by:preset-workspace-assistant"
  - "blocked-by:preset-system-explorer"
  - "blocked-by:workspace-flip"
---

## Files to modify

- `config/gptel/presets/*.md` (delete) — `executor.md`, `explore.md`, `plan.md`,
  `research.md`, `zettelkasten.md`, `perplexity-researcher.md`, `minimal.md`,
  `system-explorer.md`, `test-agent-basic.md`, `test-agent-fs-scope.md`,
  `test-agent-bash-scope.md`.
- Any test asserting a preset **count** or loading `jf/gptel-presets-directory`
  (modify) — align to the new preset set.
- `config/gptel/preset-registration.org/el` (delete, if fully superseded by the
  presets sub-module registration).
- `test-report` snapshot files (refresh).

## Implementation steps

1. **Pre-deletion safety grep** (resolve design §Open Question):
   - `grep -rn "jf/gptel-presets-directory" config/ --include=*.el | grep -v runtime`
   - `grep -rn "Registered .* presets\|preset count\|test-agent-" config/gptel --include=*.el | grep -v runtime`
   - Inspect any `test-report`/snapshot that enumerates presets.
   - If a test (e.g. a scope or registration spec) loads a `test-agent-*` preset
     **as a fixture from this directory**, give it a dedicated fixture under the
     test tree instead of relying on a shipped preset, then proceed.
2. Delete the legacy `.md` presets listed above.
3. Remove `config/gptel/preset-registration.{org,el}` if the presets sub-module
   fully replaces it; update `gptel.org` load order accordingly.
4. Drop `(require 'yaml)` from the preset path; confirm no remaining consumer:
   `grep -rn "require 'yaml\|yaml-parse" config/gptel --include=*.el | grep -v runtime`.
5. Refresh test-report snapshots:
   `./bin/run-tests.sh --report` / `make test-report` and commit the updated
   snapshot files.
6. Run the full gptel suite; confirm no orphaned references to deleted presets.

## Design rationale

Fresh-start the preset set (proposal.md "remove all"); the new presets and the
fragment pipeline have replaced the legacy `.md` files and the workspace
`executor` coupling. Deletion is deliberately last and gated on the safety grep
so a fixture dependency cannot silently break the suite (design Risks).

## Verification

- `ls config/gptel/presets/*.md 2>/dev/null` (expect none).
- `grep -rn "executor" config/gptel --include=*.el | grep -v runtime` (expect no
  live references; research/corpus data files under bash-parser are unrelated).
- `./bin/run-tests.sh -d config/gptel`
- `git diff --stat` shows refreshed test-report snapshots only where expected.

## Context pointers

- Old presets: `config/gptel/presets/*.md`.
- `test-agent-*` references found in tests are buffer-name/agent-name strings, not
  file loads (verify in step 1): `config/gptel/tools/test/persistent-agent/error-handling-spec.el`,
  `config/gptel/tools/test/test-org-roam-integration.el`.
- Design Risks/Migration Plan step 6–7.

## Cycle 2 updates (cycle-1781885402)

> registration-rewrite already did part of this task's deletion work — **scope reduced**.

- **Already deleted by registration-rewrite:** `config/gptel/preset-registration.org`
  and `.el` (the whole YAML parse/normalize/coerce module), and `(require 'yaml)` was
  removed from the **preset path**. Do not re-attempt those deletions.
- **Remaining scope for this task:** delete the old `.md` presets themselves (`executor`,
  `explore`, `plan`, `research`, `zettelkasten`, `perplexity-researcher`, `minimal`,
  `system-explorer.md`, `test-agent-*`) AFTER the snapshot/count-test grep (design step 6),
  and the **global** yaml-dep check (verify no OTHER module still requires `yaml` before
  dropping it project-wide — registration-rewrite only cleared the preset path).
- `register/boundary/preset-org-to-registration` is **confirmed**; the new registration
  ignores flat `.md` files in `presets/` (only descends `<name>/preset.el` subdirs), so
  the old `.md` presets are already inert at runtime — deletion is cleanup, not behavior.
