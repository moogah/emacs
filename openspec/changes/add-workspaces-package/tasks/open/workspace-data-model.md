---
name: workspace-data-model
description: Implement the pure, side-effect-free data layer (workspace, layout-group, layout plists) with constructor / accessor / mutator helpers. Buttercup-tested. No Emacs UI yet.
change: add-workspaces-package
status: blocked
relations:
  - "blocked-by:scaffold-workspaces-package"
---

## Files to modify

- `config/workspaces/data-model.org` (new) — pure data layer.
- `config/workspaces/workspaces.org` (modify) — load `data-model.el` via `jf/load-module`.
- `config/workspaces/test/data-model-spec.el` (new) — Buttercup unit tests.

## Implementation steps

1. In `config/workspaces/data-model.org` define the in-memory representation as plists (matching the persistence shape in design.md §D5):
   - **Workspace**: `(:name STR :recent-layout-group STR :buffer-files (FILE ...) :layout-groups (LAYOUT-GROUP ...))`
   - **Layout-group**: `(:name STR :layouts (LAYOUT ...))`
   - **Layout**: `(:timestamp INT :frameset BLOB :git-state nil)`
   The `:git-state` slot is always present and always `nil` in MVP (forward-compat slot; design.md §D5).
2. Implement pure constructors and accessors. Use `workspace--` (double-dash) prefix for internals:
   ```elisp
   (defun workspace--make (name) ...)             ; → new workspace plist, no groups
   (defun workspace--name (ws) ...)
   (defun workspace--layout-groups (ws) ...)
   (defun workspace--recent-group (ws) ...)
   (defun workspace--set-recent-group (ws name) ...)  ; returns updated ws (non-destructive)
   (defun workspace--find-group (ws name) ...)        ; → group or nil
   (defun workspace--upsert-group (ws name layout) ...)
   (defun workspace--remove-group (ws name) ...)
   (defun workspace--buffer-files (ws) ...)
   (defun workspace--add-buffer-file (ws path) ...)
   (defun workspace--remove-buffer-file (ws path) ...)
   ```
   And group-level helpers:
   ```elisp
   (defun workspace--group-make (name layout) ...)
   (defun workspace--group-name (g) ...)
   (defun workspace--group-layouts (g) ...)
   (defun workspace--group-recent-layout (g) ...)   ; → most recent layout (MVP: only layout)
   (defun workspace--group-add-layout (g layout) ...)
   ```
   And layout-level helpers:
   ```elisp
   (defun workspace--layout-make (frameset &optional timestamp) ...)
   (defun workspace--layout-frameset (l) ...)
   (defun workspace--layout-timestamp (l) ...)
   ```
3. Enforce the reserved name in pure data: `workspace--group-name-reserved-p` returns t for `"home"`. Mutators that delete groups should consult this (the actual rejection lives in the command layer; the predicate lives here so it is testable in isolation).
4. Provide the file with `(provide 'workspace-data-model)`.
5. In `workspaces.org`, add a `* Submodules` section that `jf/load-module`s `data-model.el`.
6. Write Buttercup specs in `config/workspaces/test/data-model-spec.el` covering:
   - constructor returns a workspace with the given name and empty groups
   - `workspace--upsert-group` adds a group, then replaces it on second call with same name
   - `workspace--set-recent-group` is non-destructive (original ws unchanged)
   - `workspace--find-group` returns nil for unknown names
   - `workspace--remove-group` removes the named group; layout count drops by one
   - `workspace--group-name-reserved-p` returns t for `"home"` and nil for arbitrary names
   - `workspace--add-buffer-file` deduplicates (adding the same path twice yields one entry)
   - `workspace--layout-make` defaults `:timestamp` to `(time-convert nil 'integer)` when not supplied; `:git-state` is always `nil`
7. Tangle and run tests as in Verification below.

## Design rationale

Plists at every level keep the persistence file readable by hand and avoid a struct-vs-plist conversion at the disk boundary (design.md §D5). Pure functions in this layer make the bulk of the package testable without spinning up tab-bar, bufferlo, or frameset machinery — only the integration tasks need those.

The reserved-name predicate lives in pure data so the layout-commands task can rely on it deterministically without re-implementing the policy at the UI layer (design.md §D6).

## Verification

- `./bin/tangle-org.sh config/workspaces/data-model.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces` — all `data-model-spec.el` specs pass.
- `grep -n "(defun workspace--" config/workspaces/data-model.el` enumerates only `workspace--`-prefixed helpers (no public `workspace-*` symbols escape from this layer).

## Context

- design.md §D5 "Persistence schema is forward-compatible by design"
- design.md §D6 "Reserve `home` as a layout-group name; configurable builder"
- design.md §D9 "Testing approach: Buttercup, co-located, behavioral-spec mapping"
- specs/workspaces/spec.md Requirement: Per-workspace named layouts
