---
name: ws-inversion-points
description: Delete the home.org reader; add workspace-display-name-function and make the home builder default to scratch and receive the workspace record.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:int-workspaces-org-graph
---

## Files to modify
- config/workspaces/home-org.org and home-org.el (delete)
- config/workspaces/tabs.org (modify; tangle)
- config/workspaces/persistence.org (modify; tangle — restore fallback)
- config/workspaces/workspaces.org (modify; tangle — remove the home-org load line / require)
- config/workspaces/test/display-name-spec.el (rewrite)
- config/workspaces/test/*home-layout*-spec.el or the spec covering `workspace-default-home-builder` (modify)

## Implementation steps
1. Add `(defvar workspace-display-name-function #'workspace--registry-display-name ...)`
   in `tabs.org` where `workspace--display-name` lives (~:135-145). The
   default returns `(workspace--name ws)`. Rewrite `workspace--display-name`
   to `(or (condition-case nil (funcall workspace-display-name-function ws) (error nil)) (workspace--name ws))`.
   It must never read a file.
2. Change `workspace-default-home-builder` (~:218-229) to switch to
   `*scratch*` in a single window. Make the builder contract explicit: the
   builder is called with the workspace record as its argument (update the
   defcustom docstring and every call site — `workspace-new`, the
   persistence restore fallback ~:614-655, `workspace--capture-home-layout`
   in layouts.org if it invokes the builder).
3. Remove every use of `workspace-home-org-path` / `-exists-p` / `-title`:
   the tab formatter (~:167-194) now goes through `workspace--display-name`
   only; `workspace--new-anchor-existing` (~:356) no longer checks for
   `home.org` (its context decision is finished in `ws-scaffold-and-anchor`;
   here just drop the reader call and leave a TODO-free stub that treats
   any `.git` directory as `anchored-existing` until that task lands).
4. Delete `home-org.org` / `home-org.el`, its load line in `workspaces.org`,
   and its lint allow-list entry if one exists. `grep -rn 'home-org\|home\.org' config/workspaces/*.org`
   must return only prose you intentionally leave (ideally none).
5. Tests: default label is the registry name; an installed function drives
   the tab label while the registry name is unchanged; a function that
   signals falls back to the name; the default builder shows `*scratch*`
   and visits no file under the home; a custom builder receives the record
   and can read `:home` and `:node-id`.

## Design rationale
The package must not read notes: notes now live in the vault and belong
to org-graph, and the package-independence rule forbids workspaces from
naming org-graph. Two inversion points replace the reader — a
display-name function (installed by the integration layer to return the
project note's title) and the already-customisable home builder (installed
to open the project note). Defaulting the builder to `*scratch*` returns
to the pre-`home.org` behaviour and keeps the package self-sufficient.

## Design pattern
Function-valued variable with a safe default and a `condition-case`
fallback, like `tab-bar-tab-name-function`. Keep the tab-bar formatter
wrapper (`workspace--tab-bar-tab-name-format`) as the single consumer of
`workspace--display-name`.

## Verification
- `./bin/tangle-org.sh config/workspaces/tabs.org` etc. validate.
- `./bin/run-tests.sh -d config/workspaces` — green.
- `ls config/workspaces/home-org.*` → no such file.
- `grep -n 'home-org\|home\.org' config/workspaces/tabs.el config/workspaces/persistence.el config/workspaces/workspaces.el` → no matches.

## Context
design.md § D3 'Inversion points replace the home.org reader'
specs/workspaces/spec.md § 'Display-name inversion point', 'Per-workspace home layout', 'Required home directory and identity coupling'
