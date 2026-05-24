## REMOVED Requirements

### Requirement: All activities-extensions behaviors

The `activities-extensions` capability — the project-aware workflow shell layered on top of `activities.el` (metadata system, lifecycle hooks, git/projectile integration, org-roam document creation, gptel session integration, transient UI) — SHALL be removed from the configuration.

**Reason**: `activities.el` itself is fundamentally limited to a single "default" and "last" configuration per activity, with no support for the arbitrary number of named layouts the workflow now requires. The `activities-extensions` prototype is built on top of those limitations and is not worth carrying forward. The replacement `workspaces` package (see `openspec/specs/workspaces.md`) provides the multi-layout model directly, with per-workspace buffer scoping that `activities-extensions` never had.

**Migration**: No automated migration. The cutover is staged:

1. The `workspaces` package lands first and is exercised alongside the still-loaded `activities` / `activities-extensions` modules. During this period users continue to use activities as before for any in-flight work.
2. A single cutover commit then:
   - Removes the `config/activities/` subtree
   - Removes the `activities/activities` entry from `jf/enabled-modules` in `init.org`
   - Removes the `activities` and `perspective` `use-package` blocks from `config/core/window-management.org`
   - Removes `config/gptel/sessions/activities-integration.org` and its loader entry
   - Drops the `activities` and `perspective` dependencies from `straight.el`
3. Users with active activities at cutover time are expected to recreate the equivalent workspaces by hand. No data conversion tool is planned.

The `activities-extensions.md` main spec SHALL be archived (moved or deleted from `openspec/specs/`) when this change is archived.

#### Scenario: Post-cutover activities-extensions commands are unbound
- **WHEN** the cutover commit has landed and Emacs is restarted
- **THEN** none of the `activities-ext-*` commands (`activities-ext-create`, `activities-ext-show-projects`, `activities-ext-open-document`, `activities-ext-validate`, `activities-ext-add-project`, `activities-ext-buffer-activity`, `activities-ext-buffer-all-activities`) are defined
- **AND** the keybindings under `C-x C-a` that they registered are unbound or rebound by another package
- **AND** the corresponding `activities-extensions` features (metadata system, project worktree creation, org-roam document creation hook, gptel session integration, transient UI) are absent from the running Emacs

#### Scenario: Post-cutover the modules are gone from the configuration
- **WHEN** the cutover commit has landed
- **THEN** `config/activities/` does not exist
- **AND** `init.org`'s `jf/enabled-modules` does not list `activities/activities`
- **AND** `config/core/window-management.org` does not include a `(use-package activities ...)` or `(use-package perspective ...)` block
- **AND** `config/gptel/sessions/activities-integration.org` does not exist
- **AND** the `activities` and `perspective` packages are not declared as dependencies via `straight.el`
