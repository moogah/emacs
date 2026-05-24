---
name: scaffold-workspaces-package
description: Create the empty workspaces module (literate org skeleton + loader registration) so subsequent tasks have a home to land code in. No commands or behavior yet.
change: add-workspaces-package
status: ready
relations: []
---

## Files to modify

- `config/workspaces/workspaces.org` (new) — top-level literate module file.
- `config/workspaces/test/.gitkeep` (new) — placeholder so the test directory exists and `./bin/run-tests.sh -d config/workspaces` does not error.
- `init.org` (modify) — register `("workspaces" "Named tab-based workspaces with per-workspace layouts and buffer scoping")` in `jf/enabled-modules`.

## Implementation steps

1. Create `config/workspaces/workspaces.org` with the standard literate header:
   ```org
   #+title: Workspaces
   #+author: Jeff Farr
   #+property: header-args:emacs-lisp :tangle workspaces.el
   #+auto_tangle: y
   ```
2. Add a `* Lexical Binding` section with `;; -*- lexical-binding: t; -*-`.
3. Add a `* Custom Group` section defining:
   ```elisp
   (defgroup workspaces nil
     "Named tab-based workspaces with per-workspace layouts and buffer scoping."
     :group 'convenience
     :prefix "workspace-")
   ```
4. Add a `* Module Initialization` section with `(provide 'workspaces)` and the conventional `;;; workspaces.el ends here` trailer.
5. Create empty `config/workspaces/test/.gitkeep`.
6. In `init.org`, add the entry to `jf/enabled-modules` immediately after `("core/window-management" ...)`. The new module should load before any module that might want to interact with it (none in MVP).
7. Tangle: `./bin/tangle-org.sh config/workspaces/workspaces.org` and `./bin/tangle-org.sh init.org`.
8. Launch isolated Emacs (`./bin/emacs-isolated.sh -nw -batch -e "(message \"workspaces loaded: %s\" (featurep 'workspaces))"`) and confirm the module loads without error.

## Design rationale

The package is intentionally inert at this stage. We want a known-good module shell so subsequent tasks add behavior incrementally and `jf/load-module`'s error-handling path is exercised once, in isolation, before any real code lands (design.md §D8: "side-by-side development; single hard-cutover commit").

The `workspace-` prefix is reserved by `defgroup` here so later defcustoms (notably `workspace-home-builder`) have a canonical home (design.md §D6).

## Verification

- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/tangle-org.sh init.org`
- `./bin/run-tests.sh -d config/workspaces` — succeeds (no tests yet, runner reports zero specs and exits 0).
- `grep -n '"workspaces"' init.el` returns the new module entry.
- Isolated Emacs starts without error: `./bin/emacs-isolated.sh -nw --eval '(kill-emacs (if (featurep (quote workspaces)) 0 1))'` exits 0.

## Context

- design.md §D8 "Side-by-side development; single hard-cutover commit"
- design.md §D6 "Reserve `home` as a layout-group name; configurable builder"
- proposal.md §Impact ("New: `config/workspaces/`")
