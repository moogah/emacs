---
name: buffer-membership
description: Add the bufferlo dependency, wire its per-tab buffer scope to workspaces, and implement workspace-remove-buffer. Verify kill-buffer semantics remain globally destructive (Story A).
change: add-workspaces-package
status: blocked
relations:
  - "blocked-by:tab-integration"
---

## Files to modify

- `config/workspaces/buffer-membership.org` (new) — bufferlo dependency, integration, and `workspace-remove-buffer`.
- `config/workspaces/workspaces.org` (modify) — load `buffer-membership.el` before any consumers; document `kill-buffer` non-interference.
- `config/workspaces/test/buffer-membership-spec.el` (new) — Buttercup behavioral tests.

## Implementation steps

1. In `buffer-membership.org`, add the bufferlo dependency via straight:
   ```elisp
   (use-package bufferlo
     :straight t
     :init (bufferlo-mode 1))
   ```
2. Verify and document the bufferlo surfaces we depend on (these are stable in current bufferlo; pin to the surface here so future bufferlo changes are caught):
   - `bufferlo-buffer-list` — current-tab buffer list (used by `consult` and similar via filter hooks).
   - `bufferlo-remove` — remove a buffer from current tab's local list without killing it. This is the engine for `workspace-remove-buffer`.
   - `bufferlo-mode-line-prefix` / similar — display surface (not required for MVP).
   - bufferlo's automatic membership-on-display behavior — required for the "Displayed buffers become members" scenario.
3. Bridge buffer membership to our workspace registry:
   - On `workspace-new` completion, the bufferlo per-tab list starts empty for the new tab. The home builder's display calls (e.g. `switch-to-buffer`) cause bufferlo to add those buffers automatically. Confirm with a test.
   - On `tab-bar-switch-to-tab` (already-advised in `tab-integration`), bufferlo handles the swap internally — no extra glue needed.
   - Add a hook on `kill-buffer-hook` that, when the buffer being killed is a file buffer, syncs its absence into the workspace's `:buffer-files` (so persistence (later task) does not try to restore a killed buffer).
4. Implement `workspace-remove-buffer`:
   - Prompts for a buffer (default current) using `read-buffer` *filtered to the current workspace's membership*.
   - Calls `bufferlo-remove` for the current tab.
   - Updates the workspace plist's `:buffer-files` via `workspace--remove-buffer-file` (so persistence does not re-add it on next save).
   - Bound under the workspaces prefix (the prefix is decided in the persistence/cutover discussion; tentatively `C-x w r`).
5. Confirm `kill-buffer` is NOT shadowed, advised, or wrapped by us anywhere. Add an assertion-style Buttercup spec:
   - Display a file buffer in two workspaces (via `switch-to-buffer` on each).
   - Call `kill-buffer` on it from one workspace.
   - Assert: buffer is dead; not in `bufferlo-buffer-list` of either workspace; not in `:buffer-files` of either workspace's plist.
6. Buttercup specs in `buffer-membership-spec.el`, mapping 1:1 to spec scenarios:
   - **Displayed buffers become members** — visit a file, confirm membership.
   - **Non-file buffers can be members** — display `*Messages*`, confirm membership (live but not persisted).
   - **Membership is per-workspace** — visit same file in two workspaces, confirm both lists contain it.
   - **Buffers from other workspaces are filtered out** — file member of only workspace A, switch to B, confirm `read-buffer`'s candidates exclude it.
   - **Remove from current workspace only** — `workspace-remove-buffer` removes from A but not B; underlying buffer still live.
   - **Remove the only membership** — `workspace-remove-buffer` removes from only-member workspace; underlying buffer still live.
   - **kill-buffer kills globally** — as in step 5.
   Use `cl-letf` to scope any necessary mocks (e.g. `read-buffer` to bypass interactive prompts) to the function-under-test (design.md §D9).
7. Smoke test:
   ```
   ./bin/emacs-isolated.sh -nw --eval "(progn
     (workspace-new \"a\")
     (find-file \"~/.emacs.d/init.el\")
     (workspace-new \"b\")
     (find-file \"~/.emacs.d/init.el\")
     (message \"a-list: %s b-list: %s\"
              (length (bufferlo-buffer-list (workspace--tab-for \"a\")))
              (length (bufferlo-buffer-list (workspace--tab-for \"b\"))))
     (kill-emacs))"
   ```

## Design rationale

`bufferlo`'s per-tab scope maps directly onto our 1-tab-per-workspace invariant; using it avoids re-implementing filtered `buffer-list`, completing-read integrations, and the menu hooks that `consult` already calls into (design.md §D2). The only glue we own is keeping the workspace plist's `:buffer-files` in sync with the live bufferlo state so the persistence task has a consistent source of truth for what to save.

Story A (do not shadow `kill-buffer`) is preserved structurally: we add no advice, no overrides, no defalias. `workspace-remove-buffer` is purely additive (design.md §D3). The kill-buffer-globally test is included to *prevent regression* — a future task or third-party package adding a shadow would break that spec immediately.

Non-file buffers (`*Messages*`, magit-status, `*scratch*`) participate in membership while live but are not persisted across restarts (design.md §D4) — captured in spec Requirement: Per-machine persistence and restoration ("non-file buffers that cannot be restored are omitted").

## Verification

- `./bin/tangle-org.sh config/workspaces/buffer-membership.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces` — all `buffer-membership-spec.el` specs pass; previously-passing specs still pass.
- `grep -rn "advice-add.*kill-buffer\|defalias.*kill-buffer" config/workspaces/` returns NO matches.
- Smoke command in step 7 reports both lists contain the shared file (length ≥ 1 each).

## Context

- design.md §D2 "Use `bufferlo` for buffer-list scoping"
- design.md §D3 "`kill-buffer` is left untouched; `workspace-remove-buffer` is the additive command (Story A)"
- design.md §D4 "Persist via `frameset.el` for window configs; reference buffers by file path"
- design.md §D9 "Testing approach"
- specs/workspaces/spec.md Requirements: Workspace-scoped buffer membership, workspace-remove-buffer command, kill-buffer remains globally destructive
