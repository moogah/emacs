---
name: persistent-agent-drop-metadata-write
description: Remove the agent metadata.yml write from persistent-agent and cut its dependency on gptel-session-metadata, so delete-metadata-module can safely run.
change: gptel-chat-state-persistence
status: ready
relations:
  - "discovered-from:delete-metadata-module"
---

## Files to modify

- `config/gptel/tools/persistent-agent.org` (modify) — drop `(require 'gptel-session-metadata)`; delete the `metadata.yml` write block in the agent-creation flow; update the Key Dependencies prose section and the Testing Considerations checklist item that references metadata.yml.
- `config/gptel/tools/persistent-agent.el` (tangled output — regenerate via `./bin/tangle-org.sh`).
- `config/gptel/test/persistent-agent-spec.el` (modify) — drop `(require 'gptel-session-metadata)`; delete the entire `describe "metadata.yml"` block (2 `it` + 1 `xit`). Keep all other specs as-is.
- `config/gptel/test/session-restoration-spec.el` (delete) — tests the metadata.yml-authoritative auto-init path that no longer exists in production. The drawer-authoritative auto-init is already covered by `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` and `preset-application-spec.el`.

## Implementation steps

1. Verify no reader of agent metadata.yml exists (should already be true — confirmed by orchestrator pre-flight):
   ```
   grep -rn '"metadata\.yml"\|jf/gptel-session--metadata-file\|jf/gptel--metadata-file-path' config/gptel/ --include='*.el' | grep -v test/
   ```
   Only the write-site in `persistent-agent.el`, the to-be-deleted module/constant/helper, and docstring/log-message mentions should appear.
2. In `persistent-agent.org`:
   - Section "Dependencies" (around the top `require` block that contains `gptel-session-registry`): remove the `(require 'gptel-session-metadata)` line.
   - Section that writes agent metadata.yml (around tangled `persistent-agent.el:180-191`; `persistent-agent.org:618-629`): delete the whole `;; Write metadata.yml with session metadata` block, including the `(let ((metadata-file ...) (timestamp ...)) (with-temp-file metadata-file ...))` form and its trailing `jf/gptel--log` call.
   - Prose doc sections:
     - "Integration Points / Key Dependencies": delete the `*gptel-session-metadata*: Metadata file read/write` bullet.
     - "Testing Considerations / How to test": delete the `Check metadata.yml includes =parent_session_id=` bullet (or rewrite to reference the drawer, but deletion is simpler — nothing reads the agent metadata anyway).
3. Tangle: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`. The tangled `.el` must also drop `(require 'gptel-session-metadata)` and the metadata write block.
4. In `config/gptel/test/persistent-agent-spec.el`:
   - Drop `(require 'gptel-session-metadata)` from the top-of-file require block.
   - Delete the entire `(describe "metadata.yml" ...)` block (currently the 3 specs: "writes version, session_id, and timestamps"; `xit` "records parent_session_id from parent buffer"; "records preset name and type as agent"). The enclosing `describe "PersistentAgent"` stays.
5. `git rm config/gptel/test/session-restoration-spec.el`.
6. Run `./bin/run-tests.sh -d config/gptel/test` — remaining specs must stay green.
7. Run `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — drawer-authoritative auto-init tests must still pass (they already do; this is a guard against accidentally removing coverage this task was supposed to keep).

## Design rationale

Design.md §Decision 6 ("metadata.yml has no remaining readers or writers") was taken on faith when the task breakdown was drawn up; the persistent-agent write-site was missed. User option A resolves the gap by honouring the design in spirit — nothing in live code reads `metadata.yml` (confirmed via the grep above), and the agent's preset/parent linkage is already recoverable via buffer-local session vars set at agent-buffer creation time, so the on-disk sidecar is unused bookkeeping.

`session-restoration-spec.el` predates the drawer migration and tests a read path (`jf/gptel--auto-init-session-buffer` reading `metadata.yml`) that no longer exists. It happens to still pass because its mocks satisfy the pre-drawer code path, but it exercises no production code after `sessions-auto-init-drop-metadata`. Parallel coverage for the new contract lives in `sessions/test/commands/auto-init-chat-mode-spec.el` and `preset-application-spec.el`.

Retaining a single `xit`-flagged parent_session_id bug (noted in persistent-agent-spec.el lines 186-193) is moot under this task — there is no write file to assert against — so the `xit` is removed along with its sibling specs.

## Verification

- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`
- `./bin/run-tests.sh -d config/gptel/test` — green.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — green.
- `grep -n "gptel-session-metadata\|jf/gptel-session--metadata-file\|metadata\\.yml" config/gptel/tools/persistent-agent.el config/gptel/test/persistent-agent-spec.el` — no matches. (Log-message and prose mentions must all be gone for these two files.)
- `ls config/gptel/test/session-restoration-spec.el` — no such file.

## Context

- design.md §Decision 6 (rationale for metadata.yml removal — expand in the review for this task, if tangible gap found)
- Predecessor tasks: `sessions-auto-init-drop-metadata`, `session-creation-drawer-prepopulate`, `branching-drop-metadata-copy`, `activities-integration-metadata-guard` (all done) — same pattern, different subsystem.
- Unblocks: `delete-metadata-module` (its `blocked-by:` is repointed to include this task).
