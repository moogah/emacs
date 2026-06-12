---
name: workspaces-transient-menu
description: Context-aware workspace-menu transient (three states); promote C-x w to open it; registry-driven Integrations group
change: workspace-integration-menu
status: blocked
relations:
  - blocked-by:integration-registry-core
---

## Files to modify
- `config/workspaces/workspaces-transient.org` (new — the transient + predicates)
- `config/workspaces/workspaces.org` (modify — load submodule; promote `C-x w` binding; retire individual chords)
- `config/workspaces/test/transient-menu-spec.el` (new — Buttercup)

## Implementation steps

1. **Create `config/workspaces/workspaces-transient.org`** (literate headers,
   `:tangle workspaces-transient.el`, `(provide 'workspaces-transient)`).
   It uses the latest `transient` (loaded via straight in `config/transient`).

2. **Context predicates** (read the current tab's workspace once):
   ```elisp
   (defun workspace--menu-current () (when-let ((n (workspace--current-name))) (gethash n workspace--registry)))
   (defun workspace--menu-healthy-p () (let ((w (workspace--menu-current))) (and w (not (workspace--broken-p w)))))
   (defun workspace--menu-broken-p  () (let ((w (workspace--menu-current))) (and w (workspace--broken-p w))))
   (defun workspace--menu-in-ws-p   () (and (workspace--menu-current) t))
   ```

3. **`transient-define-prefix workspace-menu`** with context-gated groups:
   - **Entry** (always): `n` workspace-new, `s` workspace-switch, `o` workspace-restore.
   - **Layouts + State** (`:if workspace--menu-healthy-p`): `l` switch-layout,
     `L` save-layout, `T` recent-layout, `S` workspace-save, `r` workspace-revert.
   - **Manage / Recover** (`:if workspace--menu-in-ws-p`): `D` delete, `P` purge,
     `R` re-anchor (these are recovery in a broken ws, manage in a healthy one).
   - **Integrations** (`:if workspace--menu-healthy-p`): dynamic via
     `:setup-children` + `transient-parse-suffixes`, walking
     `workspace--integrations` for entries with a `:menu` (KEY . COMMAND).
     Mirror the pattern at `config/gptel/skills/skills-transient.org:291-351`.
     Each suffix invokes the command on a freshly-built current-workspace
     payload: `(workspace--integration-payload name home 'menu-invoke)` —
     build it from `workspace--menu-current` at invoke time.

4. **Promote `C-x w` (decided)** in `config/workspaces/workspaces.org`: replace
   the block of individual `(global-set-key (kbd "C-x w <key>") …)` forms
   (~workspaces.org:361-372) with a single
   `(global-set-key (kbd "C-x w") #'workspace-menu)`. Every command stays
   reachable through the menu and via `M-x`. Preserve old mnemonics as the
   menu's suffix keys where practical (n/s/S/etc.).

5. **Wire loading**: add the `(jf/load-module … workspaces-transient.el …)` line
   near the end of the workspaces.org loader (AFTER `tabs.el` and
   `integrations.el`, since the transient needs `workspace--current-name`,
   `workspace--registry`, and `workspace--integrations`).

6. **Tests** `config/workspaces/test/transient-menu-spec.el` (Buttercup,
   unit-level — do NOT drive the interactive UI): with no current workspace,
   `healthy-p`/`broken-p`/`in-ws-p` are nil and the Integrations
   `:setup-children` builder yields no suffixes; with a healthy-workspace
   fixture and two registered `:menu` integrations, the builder yields two
   suffixes; with a broken-workspace fixture, `broken-p` is t, `healthy-p` nil
   (Integrations + operational suppressed, recovery shown). Stub
   `workspace--current-name`/`workspace--registry`/`workspace--broken-p` via
   `cl-letf`; rebind `workspace--integrations` via `let`.

## Design rationale
A single transient as the front door (design Decision 5, user-confirmed
promote). Group `:if` predicates give the three states declaratively;
`:setup-children` is the established repo idiom for registry-driven dynamic
menus, so integrations inject without the menu naming them. Broken-state
suppression falls out of the same predicate that makes `workspace-sessions-dir`
return nil for broken workspaces — the menu just makes it visible.

## Verification
- `./bin/tangle-org.sh config/workspaces/workspaces-transient.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces`
- Manual: `C-x w` opens the menu; entries match the current context.
- Done when: three states render the correct groups; Integrations group is
  registry-driven and healthy-only; `C-x w` opens the menu.

## Context
design.md § Decision 5; spec `workspaces` (ADDED: Workspace transient menu);
spec `workspace-integrations` (Integrations populate the workspaces transient;
On-demand integration invocation).
