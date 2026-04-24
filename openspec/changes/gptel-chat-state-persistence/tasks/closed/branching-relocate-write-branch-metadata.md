---
name: branching-relocate-write-branch-metadata
description: Move jf/gptel--write-branch-metadata out of the to-be-deleted sessions/metadata.org into sessions/branching.org so that delete-metadata-module can run without breaking branch creation.
change: gptel-chat-state-persistence
status: needs-review
relations:
  - "discovered-from:delete-metadata-module"
---

## Files to modify

- `config/gptel/sessions/branching.org` (modify) — add `jf/gptel--write-branch-metadata` (currently in `sessions/metadata.org`) to this module, drop `(require 'gptel-session-metadata)` if present, leave the existing call site at `branching.el:172` (now resolves to the locally-defined function).
- `config/gptel/sessions/branching.el` (tangled output — regenerate via `./bin/tangle-org.sh`).
- `config/gptel/sessions/metadata.org` (modify) — remove the `jf/gptel--write-branch-metadata` defun (and its sole branch-metadata-related companion `jf/gptel--read-branch-metadata`, which has no callers and would otherwise be dead code lingering until `delete-metadata-module` runs). The other functions in metadata.org stay until `delete-metadata-module` deletes the whole module.
- `config/gptel/sessions/metadata.el` (tangled output).

## Implementation steps

1. Read `config/gptel/sessions/metadata.org`'s defun for `jf/gptel--write-branch-metadata` (currently the last user-defined function before `provide`). Note: it depends on `jf/gptel--branch-metadata-file-path` (defined in `sessions/filesystem.org`) and `jf/gptel--log` (defined in `sessions/logging.org`).
2. Open `config/gptel/sessions/branching.org`. Add `jf/gptel--write-branch-metadata` near the existing branch-creation logic (the function that calls it lives around `branching.org` line ~286–289 / `branching.el:172`). Place the defun just above its caller, or in a "Branch metadata helpers" subsection — maintainer's choice. Do NOT add `(require 'gptel-session-metadata)`; instead ensure `branching.org` already requires (or add) `'gptel-session-filesystem` (for the path helper) and `'gptel-session-logging` (for `jf/gptel--log`). Both are likely already required.
3. Tangle: `./bin/tangle-org.sh config/gptel/sessions/branching.org`. Verify generated `branching.el` defines `jf/gptel--write-branch-metadata`.
4. In `config/gptel/sessions/metadata.org`, delete the `jf/gptel--write-branch-metadata` defun block. Also delete the now-dead `jf/gptel--read-branch-metadata` defun block (no callers; verified via `grep -rn 'jf/gptel--read-branch-metadata' config/ --include='*.el'` returning only its definition site).
5. Tangle: `./bin/tangle-org.sh config/gptel/sessions/metadata.org`.
6. Run `./bin/run-tests.sh -d config/gptel/sessions` — branch-creation tests must stay green; the void-function error that surfaced under `delete-metadata-module`'s aborted run must not reappear.
7. Run `./bin/run-tests.sh -d config/gptel` as a broader check.

## Design rationale

`metadata.el` historically conflated two concerns: (a) session-level `metadata.yml`, the target of the `gptel-chat-state-persistence` change chain; and (b) per-branch `branch-metadata.yml`, which tracks parent-branch lineage for non-main branches. design.md §Risk explicitly flagged the conflation risk and intended `branch-metadata.yml` to remain — but the mitigation ("task list names each file explicitly") only named on-disk files, not the helper functions.

The architecturally honest fix is to relocate the lone live branch-metadata writer into the branching module, where its only caller already lives. This keeps `branch-metadata.yml`'s behavior identical (per design intent), unblocks `delete-metadata-module` (the remaining four functions in `metadata.el` are dead code), and tightens module boundaries.

`jf/gptel--branch-metadata-file-path` (in `filesystem.org`) is left in place — it's a generic path helper, used by the function being relocated. No need to move it; if a later cleanup wants tighter cohesion it can move it inside `branching.org` too.

The other dead functions in `metadata.el` (`is-agent-session-p`, `is-branch-session-p`, `get-parent-session-id`) are deleted as part of `delete-metadata-module`, not here.

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/branching.org`
- `./bin/tangle-org.sh config/gptel/sessions/metadata.org`
- `grep -n 'jf/gptel--write-branch-metadata' config/gptel/sessions/branching.el config/gptel/sessions/metadata.el` — defined in branching.el, gone from metadata.el.
- `grep -rn 'jf/gptel--read-branch-metadata' config/ --include='*.el'` — no matches.
- `./bin/run-tests.sh -d config/gptel/sessions` — green (no new failures vs baseline `.orchestrator/baseline-1777059480.txt`).

## Context

- `delete-metadata-module` (now blocked-by this task) — its aborted attempt produced 10 `void-function jf/gptel--write-branch-metadata` failures, all rooted at `branching.el:172`.
- design.md §Risk ("branch-metadata.yml could be mistaken for metadata.yml...") — the mitigation was insufficient; this task is the corrective.
- Current location: `config/gptel/sessions/metadata.el:85` defines `jf/gptel--write-branch-metadata`; `branching.el:172` calls it.
