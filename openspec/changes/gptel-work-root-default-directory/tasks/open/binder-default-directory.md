---
name: binder-default-directory
description: Binder sets buffer-local default-directory from GPTEL_WORK_ROOT, branch-dir fallback
change: gptel-work-root-default-directory
status: ready
relations: []
---

## Files to modify
- `config/gptel/sessions/commands.org` (modify) — the `gptel-chat-mode-hook` binder
  (`jf/gptel--bind-session-buffer`, ≈`commands.org:260-300`)

## Implementation steps
1. The binder already scans the drawer (`drawer-alist`) and `setq-local`s four
   identity vars (`jf/gptel--session-id`, `jf/gptel--session-dir`,
   `jf/gptel--branch-name`, `jf/gptel--branch-dir`) before a `condition-case` block
   that does registry + autosave work.
2. Add ONE `setq-local` for `default-directory`, placed alongside the four identity
   vars and *before* the `condition-case` registry block (so it survives a registry
   failure, matching the existing var-setting discipline):
   ```elisp
   (setq-local default-directory
               (file-name-as-directory
                (expand-file-name
                 (or (cdr (assoc "GPTEL_WORK_ROOT" drawer-alist))
                     jf/gptel--branch-dir))))
   ```
3. Confirm `jf/gptel--branch-dir` is already bound at that point (it is — it's one of
   the four set just above). The `(or … branch-dir)` makes keyless sessions a no-op:
   `branch-dir` is already what `find-file` set `default-directory` to.
4. `file-name-as-directory` + `expand-file-name` normalize the value to an absolute
   directory with a trailing separator regardless of how it was written.
5. Tangle + validate: `./bin/tangle-org.sh config/gptel/sessions/commands.org`.

## Design rationale
The binder is the single content-addressed setup seam (Threads A/F, already
implemented): it runs from `gptel-chat-mode-hook` after `kill-all-local-variables`,
so `setq-local` lands. It already reads the drawer, so reading one more key costs
nothing and avoids a second hook. The drawer is NOT a local-variables mechanism, so
the `hack-local-variables` ordering concern (a dir-locals artifact) does not apply
(design D2). Keyless fallback to `branch-dir` is graceful degradation — byte-for-byte
today's behavior for sessions that predate the key (design D3).

This is the READ side shared by BOTH chat and agent buffers: because
`gptel--handle-tool-use` runs every tool call inside `(with-current-buffer info:buffer)`,
this one buffer-local set reaches all tools (design D4 — no separate task needed).

## Design pattern
Place the new `setq-local` immediately after the existing
`(setq-local jf/gptel--branch-dir branch-dir)` line and before the
`(condition-case err …)` that does `jf/gptel--register-session` / autosave.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/commands.org` passes.
- Buttercup spec under `config/gptel/sessions/test/commands/` (a fixture `session.org`
  drawer can be hand-crafted — independent of the writer task):
  - WHEN a drawer declaring `:GPTEL_WORK_ROOT: /Users/x/proj` is activated THEN
    buffer-local `default-directory` is `/Users/x/proj/`.
  - WHEN the drawer omits `:GPTEL_WORK_ROOT:` THEN `default-directory` equals
    `jf/gptel--branch-dir` (file's own directory).
  - WHEN the value lacks a trailing separator THEN `default-directory` has one.
- Run: `./bin/run-tests.sh -d config/gptel/sessions`.

## Context
design.md § Decisions 'D2 — Read in the binder', 'D3 — Keyless fallback', 'D4 — Tools inherit the work root through the existing buffer context'
specs/sessions-persistence/spec.md § 'Requirement: Default-directory resolution on session activation'
