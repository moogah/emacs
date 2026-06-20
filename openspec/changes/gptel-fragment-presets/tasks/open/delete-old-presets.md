---
name: delete-old-presets
description: Verify no snapshot/count test depends on the presets directory, delete the legacy .md presets, drop the yaml dependency if now unused, and refresh test-report snapshots.
change: gptel-fragment-presets
status: ready
relations:
  - "blocked-by:registration-rewrite"
  - "blocked-by:preset-workspace-assistant"
  - "blocked-by:preset-system-explorer"
  - "blocked-by:workspace-flip"
  - "blocked-by:wire-fragment-sources-load"
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

## Cycle 4 updates (cycle-1781941375)

> ALL blockers cleared — this task is now **ready** and is the last remaining
> substantive in-change task (alongside the test-quality followup
> `harden-fragment-txt-golden`). It is the change's final cleanup cycle.

- **Blockers satisfied:** `wire-fragment-sources-load`, `migrate-prelude-preamble`,
  and `workspace-flip` all landed done+reviewed this cycle (eda66755, a63f90e0,
  b4d68f82). registration-rewrite + both presets were already done. Status flipped
  blocked → ready.
- **DO NOT delete `config/gptel/presets/registration.{org,el}`.** That is the ACTIVE
  presets sub-module and now hosts BOTH loaders: `jf/gptel-preset-register-all`
  (`<name>/preset.el` discovery) AND the new `jf/gptel-fragment-sources-directory` /
  load-sources-all (flat `presets/sources/*.el` discovery, added by
  wire-fragment-sources-load). The deletion target named in step 3 was the OLD
  top-level `config/gptel/preset-registration.{org,el}` (YAML pipeline) — already
  deleted by registration-rewrite in cycle 2. Re-confirm it's gone; do not touch the
  active `presets/registration.*`.
- **New source files exist under `presets/sources/`** (`environment.*`,
  `emacs-prelude.*`, `agent-preamble.*` + committed `.txt`). These are live fragment
  sources, NOT legacy presets — do not delete them. Only the flat legacy `.md` files
  directly under `config/gptel/presets/` are in scope.
- `register/boundary/preset-org-to-registration` gained a cycle-4 addendum (the
  sources-discovery sibling loader); `register/boundary/sources-directory-load` is now
  confirmed. Neither expands this task's deletion scope.
- Safety grep (design step 1) still stands before any deletion.

## Cycle 3 updates (cycle-1781900938)

> Both new presets landed; this task picked up a new blocker.

- **New blocker: `wire-fragment-sources-load`** (created this cycle). Do not delete legacy
  presets / drop `yaml` until the fragment SOURCE-load wiring is real — deleting while the
  env (and next cycle's prelude/preamble) seams are still dark would conflate two failures.
  Current `blocked_by`: `workspace-flip`, `wire-fragment-sources-load` (the two presets and
  registration-rewrite blockers are now satisfied).
- **Both new presets exist and register** (`workspace-assistant`, `system-explorer` via
  `<name>/preset.el`). The legacy `system-explorer.md` (flat file) is now safely deletable —
  it is shadowed by the new `system-explorer/preset.el` and inert at runtime.
- `register/boundary/preset-org-to-registration` gained an addendum (forwards `:backend`
  unchanged; render-locus = load-time defconst) and `register/shape/preset-config-plist`
  was reconciled (`:backend` = gptel backend NAME STRING). Neither changes this task's
  deletion scope, but the `:backend "Claude"` form is what the surviving presets carry.
- Pre-deletion safety grep (design step 1) still stands; additionally confirm no test loads
  a legacy `.md` preset as a fixture now that the new pipeline is the only live path.
