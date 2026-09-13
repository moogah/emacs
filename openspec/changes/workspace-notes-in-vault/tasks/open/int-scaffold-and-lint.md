---
name: int-scaffold-and-lint
description: Create the config/integrations/ module (loader + three guarded submodules), enable it in init.org, and add the package-boundary lint spec.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:gptel-single-root
  - blocked-by:og-vault-only-discovery
  - enables:int-workspaces-org-graph
  - enables:int-workspaces-gptel
  - enables:int-gptel-org-graph
---

## Files to modify
- config/integrations/integrations.org (new; tangle to integrations.el — loader)
- config/integrations/workspaces-org-graph.org (new; skeleton with guards)
- config/integrations/workspaces-gptel.org (new; skeleton)
- config/integrations/gptel-org-graph.org (new; skeleton)
- config/integrations/test/helpers-spec.el (new — fixtures: fake workspace registry entry, temp vault, stub vulpea)
- config/integrations/test/boundary-lint-spec.el (new)
- config/integrations/test/module-load-spec.el (new)
- init.org (modify; tangle — `("integrations/integrations" "...")` after `org-graph/org-graph`)

## Implementation steps
1. Loader `integrations.org`: header per CLAUDE.md, `(require 'cl-lib)`,
   then `jf/load-module` the three submodules by absolute path (pattern:
   `config/org-graph/org-graph.org` ~:386-396). Prose: this directory is the
   only place two packages may be named; list the boundary rule.
2. Each submodule: lexical-binding header; a top comment block declaring
   the two packages it bridges; `declare-function`/`defvar` for everything
   it touches; body wrapped so it is inert when either package is absent:
   `(with-eval-after-load 'workspaces (with-eval-after-load 'org-graph ...))`
   for registration-time work, plus `featurep` guards inside handlers.
   Leave a `(provide 'jf-integrations-<name>)` and no behaviour yet.
3. `boundary-lint-spec.el`: for each package directory grep the tangled
   `.el` files (read them with `insert-file-contents`; do not shell out) and
   expect zero matches: `config/workspaces/*.el` ∌ `jf/gptel-`, `gptel-`,
   `org-graph`; `config/gptel/sessions/*.el` ∌ `workspace-`, `org-graph`;
   `config/org-graph/*.el` ∌ `workspace-`, `jf/gptel-`. Exclude `test/`
   subdirectories. Print offending file:line on failure.
4. `module-load-spec.el`: loading the loader with none of the three
   features present signals nothing; with all three present the three
   submodule features are provided in order.
5. `init.org`: add the enabled-modules entry after `org-graph/org-graph`;
   adjust the org-graph entry description to drop "plugged into
   workspaces". Tangle init.org (`./bin/tangle-org.sh init.org`).

## Design rationale
The user's rule: workspaces, gptel sessions, and org-graph are independent
packages that never name each other. Something must still connect them,
and that something needs a home that is loaded last and can vanish
without breaking any package. A grep lint turns the rule into a failing
test rather than a convention, extending the existing workspaces
directionality lint to all three prefixes.

## Design pattern
Loader shape from `config/org-graph/org-graph.org`; guarded soft-dependency
style from the (now deleted) `config/org-graph/workspace-integration.org`
(`with-eval-after-load` + `boundp`/`fboundp` checks) — reuse its guard
idioms, not its behaviour. Test fixtures pattern:
`config/org-graph/test/helpers-spec.el`.

## Verification
- `./bin/tangle-org.sh config/integrations/*.org` and `init.org` validate.
- `./bin/run-tests.sh -d config/integrations` — lint and load specs green (lint passes only once `gptel-single-root` and `og-vault-only-discovery` have removed the cross-references).
- `grep -n 'integrations/integrations' init.el` → entry present after org-graph.

## Context
design.md § D1 'Three packages plus one integration layer'; § D10 (integrations test dir)
specs/workspace-graph-integration/spec.md § 'Package independence boundary'
specs/workspace-integrations/spec.md § 'Registry is the published boundary (directionality preserved)'
