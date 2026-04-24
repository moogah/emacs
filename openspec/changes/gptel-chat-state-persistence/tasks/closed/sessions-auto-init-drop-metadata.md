---
name: sessions-auto-init-drop-metadata
description: Remove metadata.yml reading from jf/gptel--auto-init-session-buffer so session state comes exclusively from the session.org drawer via gptel-chat-mode activation.
change: gptel-chat-state-persistence
status: done
relations:
  - "blocked-by:chat-drawer-overrides-overlay"
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — remove the metadata-read block and the post-activation `gptel--apply-preset` call in `jf/gptel--auto-init-session-buffer`.
- `config/gptel/sessions/test/commands/preset-application-spec.el` (rewrite) — assertions switch from "metadata.yml wins" to "drawer is authoritative".
- `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` (modify) — fixtures stop creating `metadata.yml`; create `session.org` with a drawer instead.

## Implementation steps

1. In `commands.org`, simplify `jf/gptel--auto-init-session-buffer`:
   - Keep path extraction, `(jf/gptel--ensure-mode-once)` invocation, buffer-local session vars (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`), registry registration, `jf/gptel-autosave-enabled`, and current-symlink update.
   - Delete the entire `(let* ((metadata (condition-case parse-err ...)) ...) ...)` block that reads `metadata.yml` and applies the preset / parent-session-id.
   - Drop `(require 'gptel-session-metadata)` from the module header.
   - Remove the now-obsolete explanatory comment block about "metadata.yml is authoritative, runs LAST."
2. `jf/gptel--parent-session-id` is now set by the chat-mode hook during `(jf/gptel--ensure-mode-once)` — no auto-init-level action needed.
3. Tangle: `./bin/tangle-org.sh config/gptel/sessions/commands.org`.
4. Rewrite `preset-application-spec.el`:
   - Delete the fixture helper that writes `metadata.yml`. Replace with a helper `jf-gptel-test--write-session-with-drawer (branch-dir preset &optional parent-id)` that writes `session.org` with a pre-populated drawer.
   - Delete all `describe "metadata.yml preset path"` sections whose entire intent was "metadata wins."
   - Add/modify `describe "drawer-driven auto-init"` with scenarios:
     - Opening a session.org with `:GPTEL_PRESET: coding` in the drawer → `gptel--apply-preset` called with `coding`.
     - Drawer deltas (GPTEL_TOOLS) are overlaid after preset apply.
     - `GPTEL_PARENT_SESSION_ID` sets `jf/gptel--parent-session-id`.
     - Auto-init does not read metadata.yml (spy that `jf/gptel--read-session-metadata` is not called — after this change the symbol is gone, so instead spy on `insert-file-contents` for paths matching `metadata\\.yml$` and assert never-called).
     - `gptel-mode` minor-mode is NOT enabled.
5. In `auto-init-chat-mode-spec.el`, update any `before-each` fixtures that wrote `metadata.yml`; have them write a `session.org` with a drawer instead (use the same helper from step 4).
6. Run `./bin/run-tests.sh -d config/gptel/sessions/test/commands`.

## Design rationale

With the drawer authoritative (design.md §Decision 5, 6), auto-init's only remaining job is path-level setup: extract ids, set buffer-locals, register, symlink. The preset and parent-session-id come from the drawer via chat-mode activation — auto-init doesn't need to do that work a second time.

The "metadata.yml wins" test contract was the sole behavioral argument for keeping metadata.yml. Reversing it is the whole point of the change (design.md §Decision 9). Tests are rewritten, not deleted — coverage of "what authoritative source wins?" is preserved, just pointing at the drawer now.

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org`
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — all specs pass.
- `grep -n "metadata.yml\|jf/gptel--read-session-metadata" config/gptel/sessions/commands.el` — no matches.
- Manual: open an existing session.org with a drawer, verify preset is applied via chat-mode hook (inspect `gptel--preset` buffer-local); verify no `metadata.yml` read attempt (Emacs `*Messages*` should not mention metadata).

## Context

- proposal.md §What Changes (BREAKING auto-init, BREAKING test rewrites)
- specs/gptel/sessions-persistence.md §"Auto-initialization enables `gptel-chat-mode`" (MODIFIED)
- architecture.md §"`jf/gptel--auto-init-session-buffer` (simplified)"
- design.md §Decisions 6, 9

## Review

Reviewed inline (orch-review-1777056605).

Looked at:
- Implementation commit `d139996` + follow-up `5380d4f` (stale-spec removal).
- `jf/gptel--auto-init-session-buffer` in `commands.el:138` — retained path detection, regex matching, directory validation, `ensure-mode-once`, path-derived buffer-locals, registry registration, `jf/gptel-autosave-enabled`, and `current` symlink update. Metadata read block, parent-session-id extraction, and post-activation `gptel--apply-preset` call all removed. The explanatory comment block explaining why the function's scope shrank is concise and grounds the reader in Decisions 5/6/9.
- `(require 'gptel-session-metadata)` dropped from `commands.org`.
- `preset-application-spec.el`: old "metadata.yml wins" block replaced with drawer-driven scenarios — real-mode integration (drawer preset drives apply-preset), overlay-hook-fires, GPTEL_PARENT_SESSION_ID populates `jf/gptel--parent-session-id`, negative spy that no `insert-file-contents` targets `metadata.yml$`, and a `(gptel-mode 1)` never-called assertion. Shared helper `jf-gptel-test--write-session-with-drawer` matches the task's step 4 contract.
- `auto-init-chat-mode-spec.el`: fixtures switched from stubbed `insert-file-contents` returning metadata.yml content to either real `session.org` on disk with a drawer (parent-session-id scenario) or simple in-memory buffers (unit-level path tests).
- The `5380d4f` follow-up correctly deleted a stale spec that lived under `config/gptel/test/` (outside the task's file-modify list) and whose sole intent was the "metadata wins" contract. Coverage is preserved in `preset-application-spec.el`.
- state.json: `post_merge_buttercup_failed: 23` = baseline. `merge_conflict` was on `(require 'gptel-session-metadata)` removal + commentary rewrite in `commands.org`; resolution composed both commentary rewrites and re-tangled — correct outcome.

Findings: none. The merge-regression recovery path (missing stale spec, caught by the regression sweep, cleaned up in a follow-up commit) is exactly the kind of signal the orchestrator pipeline is designed to surface and resolve; no process change needed.
