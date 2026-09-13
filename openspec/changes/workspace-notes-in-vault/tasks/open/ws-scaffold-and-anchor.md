---
name: ws-scaffold-and-anchor
description: Shrink scaffolding to mkdir + git init, collapse creation contexts to fresh/anchored, offer the birth menu for both, thread the optional node-id through workspace-new.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:ws-node-id-and-payload
  - enables:int-workspaces-org-graph
  - enables:int-scaffold-and-lint
---

## Files to modify
- config/workspaces/scaffold.org (modify; tangle)
- config/workspaces/tabs.org (modify; tangle — anchor flow, contexts, birth-offer gate)
- config/workspaces/workspaces.org (modify; tangle — `workspace-new` signature, delete/purge docstrings)
- config/workspaces/integrations.org (modify; tangle — context vocabulary prose)
- config/workspaces/test/birth-offer-spec.el, integration-dispatch-spec.el, scaffold/anchor specs (modify)

## Implementation steps
1. `scaffold.org`: remove `workspace--scaffold-home-org-content`,
   `workspace--scaffold-write-home-org`, the `make-directory sessions`
   stage, and the `git add`/`git commit` stage. The pipeline is: compute
   path + collision check → `make-directory` → `git init` → register. Keep
   the "stop on failure, leave the partial directory, do not register"
   behaviour for mkdir and git init. Remove the `:init-and-commit?` switch
   if nothing needs it any more.
2. `tabs.org` prefix-arg flow (~:354-373): two cases only — `.git` present →
   register with context `anchored`, no git command; absent → `git init`,
   register with context `fresh`. Delete `anchored-scaffolded` and
   `anchored-existing` everywhere (`grep -rn 'anchored-' config/workspaces/`).
3. `workspace-new` gains an optional `node-id` argument (interactive callers
   pass nil) forwarded to `workspace--make`. `workspace-re-anchor` must
   preserve the existing `:node-id`.
4. Birth offer (`workspace--offer-birth-menu` ~:283-331): offered for both
   `fresh` and `anchored`. Update the prose that justified skipping adopted
   workspaces: the package writes nothing to the home, so the offer itself
   is safe.
5. `workspace-delete` / `workspace-purge` docstrings and prose: drop
   mentions of `home.org` / `sessions/`; state that nothing outside `:home`
   is touched.
6. Specs: default-path scaffold yields a directory containing only `.git/`
   with zero commits (`git rev-list --count HEAD` fails / is 0); anchoring a
   repo writes nothing and dispatches `anchored`; anchoring a non-repo runs
   `git init` and dispatches `fresh`; birth offer presented for `anchored`;
   declining leaves the repo byte-identical; `workspace-new` with a node-id
   registers it; re-anchor keeps the node-id.

## Design rationale
With notes in the vault the home is purely operational, so there is
nothing to scaffold beyond an empty repository and nothing to commit. The
`home.org`-presence discriminator that distinguished the two anchored
contexts has no referent any more; "did the package initialise the repo"
is the only fact integrations need, hence `fresh` vs `anchored`. The birth
offer previously skipped adopted repos to avoid modifying them; since the
offer writes nothing until the user picks a command, both contexts can be
offered.

## Design pattern
Keep the subprocess helper and error-guard shape already in `scaffold.org`;
delete stages rather than gating them. Context symbols are decided in one
place (`tabs.org`) and only consumed by the dispatcher.

## Verification
- `./bin/tangle-org.sh config/workspaces/scaffold.org` and the other touched files validate.
- `./bin/run-tests.sh -d config/workspaces` — green.
- `grep -n 'home.org\|sessions\|anchored-scaffolded\|anchored-existing\|Initial workspace' config/workspaces/scaffold.el config/workspaces/tabs.el config/workspaces/integrations.el` → no matches.

## Context
design.md § D4 'Scaffold shrinks to mkdir + git init; contexts are fresh / anchored'
specs/workspaces/spec.md § 'workspace-new default scaffolding', 'Anchoring an existing directory via prefix arg', 'Birth-time integration offer', 'workspace-delete is unregister-only by default', 'workspace-purge as the destructive deletion command'
specs/workspace-integrations/spec.md § 'Creation-time dispatch'
