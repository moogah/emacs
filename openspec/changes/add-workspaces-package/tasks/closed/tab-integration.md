---
name: tab-integration
description: Wire workspaces to tab-bar-mode. Implement workspace-new / workspace-switch commands and the home builder defcustom. Each workspace ↔ one tab, tagged with a :workspace-name tab parameter.
change: add-workspaces-package
status: done
relations:
  - "blocked-by:workspace-data-model"
---

## Files to modify

- `config/workspaces/tabs.org` (new) — tab-bar integration + workspace identity commands.
- `config/workspaces/workspaces.org` (modify) — load `tabs.el`; surface the `workspace-home-builder` defcustom.
- `config/workspaces/test/tabs-spec.el` (new) — Buttercup integration tests.
- `config/workspaces/test/home-spec.el` (new) — Buttercup specs for the home builder.

## Implementation steps

1. In `workspaces.org` (the top-level file), define the public defcustom for the home builder:
   ```elisp
   (defcustom workspace-home-builder #'workspace-default-home-builder
     "Function called to build the `home' layout for a newly-created workspace.
   Called with one argument WORKSPACE-NAME, in the context of the freshly
   activated workspace. Any buffers it displays become members of the new
   workspace."
     :type 'function
     :group 'workspaces)

   (defun workspace-default-home-builder (_workspace-name)
     "Default `workspace-home-builder': show a single window on *scratch*."
     (delete-other-windows)
     (switch-to-buffer (get-buffer-create "*scratch*")))
   ```
2. Create `config/workspaces/tabs.org`. Maintain the in-memory workspace registry:
   ```elisp
   (defvar workspace--registry (make-hash-table :test 'equal)
     "WORKSPACE-NAME → workspace plist. Mutated by all workspace-* commands.")
   ```
3. Implement `workspace--current-name`: read the `:workspace-name` parameter from the current `tab-bar--current-tab` via `alist-get`.
4. Implement `workspace-new`:
   - Prompt for name (or accept arg).
   - If an existing tab carries this `:workspace-name`, select that tab and return.
   - Otherwise: `tab-bar-new-tab`, then `tab-bar-rename-tab NAME`, then set the new tab's `:workspace-name` parameter via `tab-bar--current-tab` mutation (use the documented `tab-bar-tab-name-function` /parameter alist surface).
   - Insert a fresh workspace plist (via `workspace--make`) into `workspace--registry`.
   - Activate the workspace's `home` layout-group: call `workspace-home-builder` with the name; capture the resulting window configuration as a layout; `workspace--upsert-group` it as `"home"` and set recent-group to `"home"`.
5. Implement `workspace-switch`:
   - `completing-read` over workspaces in registry that currently have a backing tab.
   - Find the matching tab by scanning `tab-bar-tabs`; `tab-bar-select-tab` on its 1-indexed position.
6. Implement a `tab-bar-switch-to-tab` advice (`:after`) that, when the just-selected tab carries `:workspace-name`, no-ops for now (subsequent tasks will hook layout-restore and buffer-membership-activation here). The advice MUST be a no-op for tabs without `:workspace-name` so it does not interfere with `activities-tabs-mode` during the side-by-side period (design.md §D8).
7. Add Buttercup specs in `tabs-spec.el`:
   - Scenario: Creating a workspace creates a tab (assert `tab-bar-tabs` length grows and new tab carries `:workspace-name`).
   - Scenario: Switching tabs switches workspaces (`workspace--current-name` updates).
   - Scenario: Workspace names are unique (second `workspace-new` of same name re-selects the existing tab; registry size unchanged).
8. Add Buttercup specs in `home-spec.el`:
   - Scenario: New workspace uses the home builder — set `workspace-home-builder` to a spy lambda that opens a particular buffer; assert that buffer is visible and is the workspace's first layout in the `home` group.
   - Scenario: Home layout cannot be deleted — call `workspace-delete-layout` for `"home"` (this command lands in `layout-commands`; until then assert via `workspace--group-name-reserved-p`).
   - Scenario: Re-saving home overwrites without invoking the builder — set the spy, then call `workspace-save-layout "home"` (lands in `layout-commands`; until then assert via direct data-layer call) and verify spy is NOT invoked again.

   For scenarios that depend on commands landed in `layout-commands`, mark them with `xit` (Buttercup pending) and add a comment pointing to that task; they unblock when that task lands.
9. In `tabs.org` add `(provide 'workspace-tabs)`. Load it from `workspaces.org`.
10. Test in isolated Emacs:
    ```
    ./bin/emacs-isolated.sh -nw --eval "(progn (workspace-new \"alpha\") (workspace-new \"beta\") (workspace-switch \"alpha\") (message \"current: %s\" (workspace--current-name)) (kill-emacs))"
    ```

## Design rationale

Talking to `tab-bar-mode` directly (rather than `activities-tabs-mode`) is the load-bearing decision from design.md §D1 — it gives us control over tab parameters, which is the only place to durably tag "this tab belongs to a workspace." The `:workspace-name` parameter is the discriminator that lets workspace commands and the eventual layout/membership hooks ignore tabs owned by other systems during side-by-side development (design.md §D8).

The home builder runs in the context of the freshly-activated workspace so any buffers it displays become members via the buffer-membership path landing in a later task (design.md §D6).

## Verification

- `./bin/tangle-org.sh config/workspaces/tabs.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces` — `tabs-spec.el` and `home-spec.el` pass (xit-marked scenarios stay pending until later tasks).
- `grep -n ":workspace-name" config/workspaces/tabs.el` — confirms the parameter is set, not just read.
- Isolated Emacs smoke command in step 10 exits 0 and prints `current: alpha`.

## Context

- design.md §D1 "Build on `tab-bar-mode` directly"
- design.md §D6 "Reserve `home` as a layout-group name; configurable builder"
- design.md §D8 "Side-by-side development; single hard-cutover commit"
- specs/workspaces/spec.md Requirements: Workspace identity and tab visibility, Per-workspace home layout
