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
> Cycle 1: step 2's recursive grep corrected — `grep -rn 'anchored-' config/workspaces/` also matches docs/README.org:248-249 and test/ fixtures; see .orchestrator/cycles/cycle-1789334430/reconciliations/vocabulary-workspace-creation-context.md.

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
   `anchored-existing` everywhere (`grep -n 'anchored-' config/workspaces/*.org config/workspaces/*.el`
   — the enumerating docstring is integrations.org:135 / integrations.el:47, the
   emitters tabs.org:365-369 / tabs.el:216-220, the birth-offer gate
   tabs.org:329 / tabs.el:180; keep the non-birth members `menu-invoke` and
   `purge`). docs/README.org:248-249 is prose — update it too.
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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/boundary/workspace-scaffold-pipeline`: speculated (annotated). `ws-node-id-and-payload` deleted `workspace--sessions-dir` and INLINED stage 4 as the literal `(make-directory (expand-file-name "sessions" home) t)` at scaffold.org:223 / scaffold.el:84 (zero consumers, eoc-5); the pipeline is still the five stages at scaffold.org:205-226. **Implication**: step 1's "make-directory sessions stage" is that literal; the `home.org` writer (`workspace--scaffold-write-home-org` scaffold.org:139, called :222) and the commit stage (:224-226) are as described. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-workspace-scaffold-pipeline.md`.
- `register/invariant/home-holds-no-org-content`: speculated (annotated). Becomes true of the code only when this task lands; re-state it (with the register) when you close. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-home-holds-no-org-content.md`.
- `register/vocabulary/workspace-creation-context`: speculated → reconciled with member `purge` ADDED (built by `workspace--dispatch-purge-integrations`, integrations.el:120, from workspaces.el:201). `anchored-scaffolded` / `anchored-existing` are still emitted (tabs.el:216/220/224, dispatched :236/:280, gate :180) until step 2 collapses them. **Implication**: the context-vocabulary docstring (integrations.org:135 / integrations.el:47; the inputs cite the :191-201 handler prose) should CITE `register/vocabulary/workspace-creation-context` rather than re-enumerate the members (eoc-3). See `.orchestrator/cycles/cycle-1789334430/reconciliations/vocabulary-workspace-creation-context.md`.
- `register/shape/workspace-plist-v3`: speculated → reconciled. `(workspace--make NAME HOME &optional NODE-ID)` already exists (data-model.el:102); `workspace-set-node-id` is in persistence.el:276-295. **Implication**: step 3 only threads `workspace-new`'s new optional arg (tabs.el:287 `(workspace-new name &optional anchor-existing)`) through to `workspace--make`; `workspace-re-anchor` (workspaces.el:63) must copy the existing `:node-id` into the rebuilt record. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-plist-v3.md`.
- `register/shape/workspace-integration-anchor-payload`: speculated → reconciled. Signature `(ws context)`, keys `:name :home :node-id :context`; `:sessions-dir` is GONE, so removing `sessions/` in step 1 has no payload consumer to update. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-integration-anchor-payload.md`.
- `register/invariant/packages-independent-no-cross-naming`: reconciled with an interim gap. The birth-menu docstring you rewrite in step 4 (tabs.org:309-315 / tabs.el:160-166) still names the DELETED `config/gptel/sessions/workspace-integration.org`; drop that pointer in the rewrite — `int-scaffold-and-lint`'s bare-token lint will otherwise flag tabs.el:166. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-packages-independent-no-cross-naming.md`.

### User-resolved decisions
- `ask-cycle-1789334430-6` (open; default carried — object in `disposition-cycle2-design-confirmations`): fate of `INIT-AND-COMMIT?` once only mkdir + git init remain. Default: retire the switch. **Implication for this task**: step 1's "if nothing needs it any more" resolves to yes — delete the `&key init-and-commit?` and its three call sites (tabs.el:219/223/268); `git init` becomes unconditional in the scaffold and the anchor flow (step 2) decides by `.git` presence.

### Meta-discoveries
- `untouched-neighbour-consumer-drift`: prose consumers of `home.org` / `sessions/` / `Initial workspace` exist outside the three tangled files the Verification greps — docs/README.org:46-111 and :248-249 describe the old five-stage scaffold and the three contexts. Grep the tree for the bare tokens and fix the README prose in this task (no other open task owns it).
- `stub-bounded-regression`: keep step 6's un-stubbed scenarios (real temp directory, real `git init`, byte-identical repo after declining) — do not replace them with spies on `workspace--scaffold-git`.
