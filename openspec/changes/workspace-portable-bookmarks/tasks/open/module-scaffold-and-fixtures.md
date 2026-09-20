---
name: module-scaffold-and-fixtures
description: Create the portable-bookmarks module skeleton, load it from the package loader, and add Buttercup test fixtures
change: workspace-portable-bookmarks
status: ready
task_class: module-load
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/boundary/portable-bookmarks-consumer
relations: []
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (new)
- `config/workspaces/portable-bookmarks.el` (new, tangled — commit alongside the `.org`)
- `config/workspaces/workspaces.org` (modify — add the loader line)
- `config/workspaces/workspaces.el` (tangled)
- `config/workspaces/test/portable-bookmarks-fixtures.el` (new)

## Implementation steps

1. Create `config/workspaces/portable-bookmarks.org` with the standard literate
   headers:
   ```org
   #+title: Workspaces — Portable Bookmarks
   #+property: header-args:emacs-lisp :tangle portable-bookmarks.el
   #+auto_tangle: y
   ```
   Add an Overview section stating the module's boundary: it is a *consumer*
   module — it may name `bookmark`, the workspace registry and `git`, and
   workspaces core must never name it. It must NOT require `magit`.

2. Header block (`:comments no`):
   ```elisp
   ;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-
   ```
   Then a requires block: `(require 'cl-lib)` and `(require 'bookmark)`.

3. Add a customization group and two defcustoms/defvars for the property names
   used to stamp records, so the stamped keys are named in exactly one place:
   - the identity property (suggested symbol: `workspace-bookmark-repo`)
   - the repo-relative path property (suggested: `workspace-bookmark-relpath`)
   Use the `workspace-bookmark-` prefix for public symbols and
   `workspace-bookmark--` for internals.

4. Add a stub advice-installation section with the two seams named but not yet
   implemented (later tasks fill them in), plus matching removal so the module
   can be reloaded during development without stacking advice:
   - `bookmark-make-record` — `:filter-return`
   - `bookmark-handle-bookmark` — around/filter-args
   Keep the stubs pass-through: return the record unchanged.

5. End with `(provide 'workspace-portable-bookmarks)` and the closing
   `;;; portable-bookmarks.el ends here` comment, matching `tabs.el`.

6. Register the module in the package loader `config/workspaces/workspaces.org`
   (NOT in `init.org` — `init.org` lists only `("workspaces/workspaces" …)`).
   Add, after `workspaces-mode.el` and before `workspaces-transient.el`:
   ```elisp
   (jf/load-module (expand-file-name "config/workspaces/portable-bookmarks.el" jf/emacs-dir))
   ```
   Load order matters: the module reads `workspace--registry` and
   `workspace--current-name` (from `tabs.el`) and calls `workspace-restore`
   (from `persistence.el`), so it must load after both.

7. Create `config/workspaces/test/portable-bookmarks-fixtures.el` providing
   Buttercup helpers the later tasks all use. Tests in this repo are flat in
   `config/workspaces/test/` (29 existing `*-spec.el` files, no subdirectories).
   Fixtures needed:
   - **temp repo builder** — `make-temp-file` a directory, run `git init`, set
     `user.email`/`user.name` locally (batch git has no global identity), commit
     one file, and optionally `git remote add origin <url>` with a caller-supplied
     URL so SSH/HTTPS normalization can be exercised.
   - **temp worktree builder** — given a temp repo, `git worktree add` a child at
     a given path on a new branch.
   - **fake workspace registry entry** — put a plist with a `:home` pointing at a
     temp directory into `workspace--registry`, and a teardown that removes it.
     Check `data-model.el` for the accessor used to build the plist rather than
     writing the plist shape by hand.
   - **temp bookmark file** — bind `bookmark-default-file` to a temp path and
     `bookmark-alist` to nil so no test can touch the real bookmark list.
   - a teardown helper that deletes every temp directory it created.
   End with `(provide 'portable-bookmarks-fixtures)`.

8. Add a smoke spec (`config/workspaces/test/portable-bookmarks-spec.el`) that
   loads the module and asserts the feature is provided and that the fixtures
   can build a repo with a worktree. This proves the harness before any
   behaviour exists.

9. Tangle and validate both org files:
   `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` and
   `./bin/tangle-org.sh config/workspaces/workspaces.org`.

## Design rationale

The module is a consumer of workspaces, not part of its core: the package's
one-way dependency rule says core names no consumer, while consumers may name
core. Placing it in `config/workspaces/` alongside `git-worktrees.el` follows the
precedent already set for consumer modules living inside the package directory.

It must not require `magit`. The existing `jf/workspace--worktree-children`
enumerates worktrees via `magit-list-worktrees`, and reusing it would make
bookmark resolution inherit git-worktrees' hard magit dependency. This module
calls `git` as a subprocess instead; the duplication is deliberate and flagged in
design.md for later unification.

Stamped property names live in one place because they are written into the user's
bookmark file: renaming them later would orphan existing stamps, so the
indirection is cheap insurance.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0
  (it runs `check-parens`).
- `./bin/tangle-org.sh config/workspaces/workspaces.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — the smoke spec passes and the
  existing 29 spec files still pass.
- `grep -n "portable-bookmarks" config/workspaces/workspaces.el` shows the
  loader line.
- `grep -n "magit" config/workspaces/portable-bookmarks.el` returns nothing.

## Context
- proposal.md § 'What Changes' (new module bullet)
- design.md § D10 'Module placement and load order'
- design.md § D11 'Testing approach'
