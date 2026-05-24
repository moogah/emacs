---
name: keybindings-and-prefix
description: Bind workspaces commands under a single user-facing prefix (proposed C-x w) and document them. Resolves design.md Open Question 1.
change: add-workspaces-package
status: blocked
relations:
  - "blocked-by:persistence"
---

## Files to modify

- `config/workspaces/workspaces.org` (modify) — `global-set-key` calls under the chosen prefix.
- `config/workspaces/docs/README.org` (new) — usage and keybinding reference (mirrors `config/activities/docs/README.org` shape).

## Implementation steps

1. Confirm the prefix is free in the running configuration. `C-x w` is currently a 2-key prefix (`C-x n w` is `widen`; `C-x w` itself appears unbound). Run inside isolated Emacs:
   ```
   ./bin/emacs-isolated.sh --eval "(message \"binding: %s\" (key-binding (kbd \"C-x w\")))"
   ```
   Expected: `nil` or a prefix-command stub. If a future config change has claimed it, raise to the user before binding.
2. Bind in `workspaces.org`:
   ```elisp
   (global-set-key (kbd "C-x w n") #'workspace-new)
   (global-set-key (kbd "C-x w s") #'workspace-switch)
   (global-set-key (kbd "C-x w l") #'workspace-switch-layout)
   (global-set-key (kbd "C-x w L") #'workspace-save-layout)
   (global-set-key (kbd "C-x w D") #'workspace-delete-layout)
   (global-set-key (kbd "C-x w R") #'workspace-switch-to-recent-layout)
   (global-set-key (kbd "C-x w r") #'workspace-remove-buffer)
   (global-set-key (kbd "C-x w S") #'workspace-save-state)
   ```
3. Write `config/workspaces/docs/README.org` covering:
   - Conceptual model (workspaces ↔ tabs; layouts within a workspace; home is special).
   - Command reference table with the bindings above.
   - Customization: `workspace-home-builder` (default behavior and example override).
   - Persistence: where state lives, how restart works, that non-file buffers are not persisted.
   - Story A note: `kill-buffer` is still globally destructive; `workspace-remove-buffer` is the "I'm done here" command.
   - Deferred features section (timestamped layout revisions, git observations) so users do not file feature requests for already-planned-work.

## Design rationale

Centralizing the bindings in one place (and one task) lets the user see the full surface in a single diff and reject the prefix choice without churning prior tasks. The docs file mirrors the existing `config/activities/docs/README.org` convention so the codebase has a uniform "where do I read about this module" expectation.

This task is deliberately last among the implementation tasks (before cutover) because the binding choice depends on what commands actually exist — it should not lead the implementation.

## Verification

- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces` — all specs still pass.
- Isolated Emacs: `(describe-bindings)` shows the `C-x w` prefix populated; each binding invokes its named command without error.

## Context

- design.md §"Open Questions" item 1 "Keybinding prefix"
- proposal.md §What Changes (command surface)
- Comparable convention: `config/activities/docs/README.org`
