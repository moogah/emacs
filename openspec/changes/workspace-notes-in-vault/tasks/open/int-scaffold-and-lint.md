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
- config/workspaces/git-worktrees.org (modify; tangle — :9 stale pointer to the deleted gptel workspace-integration module)

## Implementation steps
> Cycle 1: original step 5 clause ('adjust the org-graph entry description to drop "plugged into workspaces"') shipped via inline fix 34cfb163 (init.org:179-184); removed. Step 3's tokens corrected to the bare forms per .orchestrator/cycles/cycle-1789334430/reconciliations/invariant-packages-independent-no-cross-naming.md (ask-7). Step 6 added for the stale-pointer cleanup that lint will otherwise flag.

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
   `org-graph`; `config/gptel/sessions/*.el` ∌ bare `workspace`, `org-graph`;
   `config/org-graph/*.el` ∌ bare `workspace`, `jf/gptel-`. Grep the BARE
   token `workspace` (not `workspace-`): docstrings and comments are tangled
   too, and the hyphenated form missed init.org's "plugged into workspaces"
   prose this cycle. Exclude `test/` subdirectories. Print offending
   file:line on failure. This spec supersedes the interim gptel-side lint in
   `config/gptel/sessions/test/filesystem/single-root-spec.el:137-146` (leave
   it or delete it — it says the integrations lint absorbs it).
4. `module-load-spec.el`: loading the loader with none of the three
   features present signals nothing; with all three present the three
   submodule features are provided in order.
5. `init.org`: add the enabled-modules entry after `org-graph/org-graph`
   (the org-graph entry comment at init.org:179-183 already announces the
   integration layer). Tangle init.org (`./bin/tangle-org.sh init.org`).
6. Stale pointers the lint will flag: `config/workspaces/git-worktrees.org:9`
   names the deleted `config/gptel/sessions/workspace-integration.org` —
   rewrite it to point at `config/integrations/workspaces-gptel.org`. The
   same pointer in the birth-menu docstring (tabs.org:315 / tabs.el:166) is
   rewritten by `ws-scaffold-and-anchor` step 4 — do NOT edit tabs.org here
   (file conflict); if that task has not merged when the lint first runs,
   a tabs.el:166 hit is the expected residue, and the lint stays red until
   it lands.

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
- `./bin/run-tests.sh -d config/integrations` — lint and load specs green (`gptel-single-root` and `og-vault-only-discovery` are done; the lint passes only once step 6's pointer cleanup and `ws-scaffold-and-anchor`'s tabs.org:315 rewrite have landed).
- `grep -n 'workspace-integration' config/workspaces/git-worktrees.el config/workspaces/tabs.el` → no matches.
- `grep -n 'integrations/integrations' init.el` → entry present after org-graph.

## Context
design.md § D1 'Three packages plus one integration layer'; § D10 (integrations test dir)
specs/workspace-graph-integration/spec.md § 'Package independence boundary'
specs/workspace-integrations/spec.md § 'Registry is the published boundary (directionality preserved)'

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/invariant/packages-independent-no-cross-naming`: speculated → reconciled with an INTERIM GAP. Only the gptel/sessions side has a lint today (single-root-spec.el:137-146); the workspaces side LOST its directionality lint when `gptel-integration-spec.el` was deleted and the org-graph side never had a grep (only the mapatoms check module-load-spec.el:145-162). `boundary-lint-spec.el` is therefore a HARD PRECONDITION for closing the change. `jf/gptel-workspace-initial-preset` (named by presets/workspace-assistant/preset.org:10) may live ONLY under config/integrations/ — `int-workspaces-gptel` defines it; do not stub it here. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-packages-independent-no-cross-naming.md`.
- `register/invariant/sessions-single-root`: speculated → reconciled. `jf/gptel--target-sessions-root`, force-global, `jf/gptel-persistent-session-global` and `config/gptel/sessions/workspace-integration.org` are all deleted; your Design pattern's "(now deleted)" guard-idiom source is reachable via `git show ad69b5dc^:config/gptel/sessions/workspace-integration.org` and `git show b6678ebc^:config/org-graph/workspace-integration.org`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-sessions-single-root.md`.
- `register/boundary/gptel-sessions-workspace-consult`: superseded (confirmed complete) — consumer and producer both removed; there is no session consult for the integration layer to reinstate. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-gptel-sessions-workspace-consult.md`.
- `register/invariant/org-graph-loader-ordered-sequence`: reconciled — module-load-spec.el:127-143 audits the tangled loader's `jf/load-module` lines. **Implication**: copy that audit shape for step 4's `module-load-spec.el` (three submodules, contiguous, in order). See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-org-graph-loader-ordered-sequence.md`.

### User-resolved decisions
- `ask-cycle-1789334430-7` (open; default carried — `disposition-cycle2-design-confirmations`): the deleted `gptel-integration-spec.el` must be REPLACED by `boundary-lint-spec.el` grepping tangled .el on all three sides with bare tokens; hard precondition for closing the change. Step 3 now says exactly this.

### Already-shipped inline fixes
- `34cfb163` (arch on-touch ot-vault-discovery-1): init.org's org-graph `jf/enabled-modules` entry no longer says "plugged into workspaces" and its comment already announces config/integrations/. **Implication for this task**: original step 5's description edit is removed; only the new `integrations/integrations` entry remains to add.

### Meta-discoveries
- `untouched-neighbour-consumer-drift`: the bare-token lint is the mechanised form of this lesson; when it first goes green, also run the same three greps over `*.org` sources and `docs/` once by hand and record survivors in Observations (prose is not tangled, but it is where the next stale pointer will come from).
