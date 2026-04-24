---
name: delete-metadata-module
description: Delete the sessions metadata module (metadata.org, metadata.el) and supporting constants/filesystem helpers after all callers have been updated.
change: gptel-chat-state-persistence
status: blocked
relations:
  - "blocked-by:sessions-auto-init-drop-metadata"
  - "blocked-by:session-creation-drawer-prepopulate"
  - "blocked-by:branching-drop-metadata-copy"
  - "blocked-by:activities-integration-metadata-guard"
  - "blocked-by:persistent-agent-drop-metadata-write"
  - "blocked-by:branching-relocate-write-branch-metadata"
---

## Files to modify

- `config/gptel/sessions/metadata.org` (delete) — the entire module.
- `config/gptel/sessions/metadata.el` (delete) — tangled output.
- `config/gptel/sessions/constants.org` (modify) — remove `jf/gptel-session--metadata-file` constant.
- `config/gptel/sessions/filesystem.org` (modify) — remove `jf/gptel--metadata-file-path` helper.
- `config/gptel/gptel.org` (modify, if applicable) — remove `jf/load-module` call for `sessions/metadata.el` from the gptel loader.
- `config/gptel/sessions/test/` — remove any dedicated metadata-module tests (if they exist) — most likely under `sessions/test/` at the top level or a `metadata/` subdir.
- `config/gptel/tools/persistent-agent.org` (modify) — remove stale prose references to the deleted module: the `metadata.el        - Read metadata from metadata.yml + scope.yml` line in the module-layout diagram, the `T->>FS: Write metadata.yml (includes parent_session_id)` step in the sequence diagram, and the two `metadata.yml` entries in the ASCII directory trees (session and agent dirs). Re-tangle. Discovered during review of `persistent-agent-drop-metadata-write` — that task's verification grep was scoped to `.el`/spec only.

## Implementation steps

1. Verify all callers are updated: the four blocking tasks above must be merged/complete. Run `grep -rn "gptel-session-metadata\|jf/gptel--read-session-metadata\|jf/gptel--write-session-metadata\|jf/gptel--metadata-file-path\|jf/gptel--update-metadata-timestamp\|jf/gptel-session--metadata-file" config/` — expect all matches to be in the files about to be deleted or in comments.
2. Delete `config/gptel/sessions/metadata.org` and `config/gptel/sessions/metadata.el`.
3. Remove `jf/gptel-session--metadata-file` (the constant `"metadata.yml"`) from `constants.org`. Tangle.
4. Remove `jf/gptel--metadata-file-path` from `filesystem.org`. Tangle.
5. If `gptel.org` (the chat/sessions loader) has an explicit `jf/load-module` call for `sessions/metadata.el`, remove it. Tangle.
6. Search for and remove any `sessions/test/metadata/` directory or `test-metadata-*.el` files if they exist. Verify with `find config/gptel/sessions/test -name '*metadata*'`.
7. In `config/gptel/tools/persistent-agent.org`, remove the stale `metadata.yml`/`metadata.el` mentions in the architecture diagram (around the module-layout block), the sequence diagram (the `T->>FS: Write metadata.yml ...` step), and both ASCII directory trees (session dir and agent dir entries). Re-tangle with `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.
8. Run `./bin/run-tests.sh -d config/gptel/sessions` — everything should still pass (all call sites are updated).
9. Run `./bin/run-tests.sh -d config/gptel` as a broader regression check.

## Design rationale

Module deletion is the final step after every caller has been updated. Performing it last keeps the intermediate states of the repo load-safe: tasks that drop individual call sites can run in parallel / any order, and the module itself is only removed once all imports are gone (design.md §Decision 6 establishes that metadata.yml has no remaining readers or writers).

## Verification

- `grep -rn "gptel-session-metadata\|jf/gptel--read-session-metadata\|jf/gptel--write-session-metadata\|jf/gptel--metadata-file-path\|jf/gptel--update-metadata-timestamp\|jf/gptel-session--metadata-file" config/` — no matches in live code (comments noting removal in design docs are acceptable).
- `ls config/gptel/sessions/metadata.*` — no such files.
- `./bin/run-tests.sh -d config/gptel` — full gptel test suite passes.
- `./bin/run-tests.sh` — global suite still green.

## Context

- proposal.md §What Changes (BREAKING module deletion)
- architecture.md §Components (Deleted — metadata.org / metadata.el)
- design.md §Decision 6 (rationale for removal)
