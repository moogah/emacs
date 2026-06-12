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

## Observations
- Confirmed the real signatures: `workspace--current-name` (tabs.el),
  `workspace--registry` (hash-table defvar in tabs.el), `workspace--broken-p`
  (data-model.el), and accessors `workspace--name` / `workspace--home`
  (data-model.el). The predicate skeleton in the task body matched the actual
  API; `workspace--menu-broken-p` was given an explicit trailing `t` so it
  returns a boolean rather than the broken-p value.
- The `:menu` payload command is invoked at *menu-invoke* time, not render
  time: `workspace--menu-invoke-integration` rebuilds
  `(workspace--integration-payload name home 'menu-invoke)` from the live
  current workspace each time the suffix fires. Note
  `workspace--integration-payload` takes `(name home context)` (3 args) and
  derives `:sessions-dir` internally — no path logic duplicated in the menu.
- Factored the dynamic-children logic into a NAMED top-level helper
  `workspace--menu-integration-children` (returns `(KEY DESC THUNK)` specs),
  with `workspace--menu-setup-integration-children` as the thin
  `transient-parse-suffixes` wrapper — so the builder is unit-testable
  without driving the transient UI (mirrors the skills-transient idiom but
  extracts the builder for testability).
- Preserved the pre-existing `C-x w b` -> `workspace-remove-buffer` chord as a
  `b` suffix in the Manage / Recover group (it was in the retired chord block
  but not enumerated in the task's group spec; dropping it would have lost a
  binding).
- Updated the two stale keybinding assertions in
  `config/workspaces/test/workspace-delete-purge-spec.el` (they asserted the
  retired `C-x w D` / `C-x w P` chords). New assertions verify `C-x w` opens
  `workspace-menu` and that `workspace-delete` / `workspace-purge` remain
  `commandp` (M-x reachable). This file is owned by the delete/purge work but
  the assertions test the C-x w contract this task owns.
- Final: `./bin/run-tests.sh -d config/workspaces` -> 277 specs, 0 failed
  (baseline on this branch was 276/0; net +1 after replacing 2 chord-binding
  tests with 3 menu-contract tests and adding the new transient-menu-spec).

## Discoveries
- discovery_id: disc-workspaces-transient-menu-1
  class: load-order-hazard
  description: >
    `config/workspaces/workspaces` loads BEFORE `config/transient` in
    init.el's `jf/enabled-modules` (line ~111 vs ~120). `config/transient.el`
    installs the straight override of the built-in `transient` and relies on
    `transient` NOT already being loaded (`use-package transient :demand t`).
    An eager `(require 'transient)` at the top of `workspaces-transient.el`
    pulls in the stale built-in transient, which then defeats the straight
    override — concretely causing `transient-showcase` to fail at init with
    `(void-function transient--set-layout)`, aborting the whole test run
    (exit 255). The skills-transient module CAN `(require 'transient)` safely
    only because gptel loads AFTER `config/transient`.
  affected_register_entry: none (init load-order invariant, not a register entry)
  recommendation: >
    Any workspaces-subtree module that uses transient MUST defer transient
    symbols behind `with-eval-after-load 'transient` and must NOT
    `(require 'transient)` at load time, until/unless the
    `workspaces`-before-`transient` ordering in init.el is changed. This task
    wraps the `transient-define-prefix` and the `transient-parse-suffixes`
    callback in `with-eval-after-load 'transient`; `C-x w` binds the
    `workspace-menu` symbol so the keybinding is valid before the prefix is
    defined (transient loads later in the same init pass).
