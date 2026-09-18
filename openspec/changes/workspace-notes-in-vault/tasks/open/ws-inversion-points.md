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
> Cycle 1: step 4's grep corrected — scaffold.org keeps the `home.org` writer (`workspace--scaffold-write-home-org`, scaffold.org:139) until `ws-scaffold-and-anchor` removes it, so a `config/workspaces/*.org`-wide grep would false-positive; see .orchestrator/cycles/cycle-1789334430/reconciliations/invariant-home-holds-no-org-content.md.

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
   and its lint allow-list entry if one exists. `grep -n 'home-org\|home\.org' config/workspaces/tabs.org config/workspaces/persistence.org config/workspaces/workspaces.org config/workspaces/layouts.org`
   must return only prose you intentionally leave (ideally none). `scaffold.org`
   still names `home.org` (its writer) — that is `ws-scaffold-and-anchor`'s
   deletion, not yours; do not edit scaffold.org here.
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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/shape/workspace-plist-v3`: speculated → reconciled. The record slot is `:node-id` (string | nil), read with `workspace-node-id` (data-model.el:178); the setter `workspace-set-node-id` lives in persistence.org (persistence.el:276-295), not data-model. Step 5's "custom builder receives the record" spec should read `(workspace--home ws)` / `(workspace-node-id ws)` from the passed record. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-plist-v3.md`.
- `register/invariant/registry-name-equals-basename`: confirmed. `:node-id` never rekeys the registry; the note explicitly leaves the stale `workspace-home-org-title` wording (tabs.org:144 / tabs.el:81) for THIS task to remove in step 3. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-registry-name-equals-basename.md`.
- `register/invariant/home-holds-no-org-content`: speculated (annotated). The scaffold still writes `home.org` and `sessions/` (scaffold.org:222-223); this task deletes only the READER (`home-org.org`, `workspace-home-org-*` calls at tabs.el:5/81/131/207). The invariant becomes true only after `ws-scaffold-and-anchor`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-home-holds-no-org-content.md`.
- `register/shape/workspace-integration-anchor-payload`: speculated → reconciled. Keys are exactly `:name :home :node-id :context`; no payload consumer reads the display name or the home layout, so the two inversion points introduced here are the only seams the integration layer will install into. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-integration-anchor-payload.md`.

### User-resolved decisions
- `ask-cycle-1789334430-5` (open; default carried — object in `disposition-cycle2-design-confirmations`): D3 changes `workspace-home-builder`'s argument from NAME to the workspace record; tabs.org:211-214 promises custom builders keep working without signature changes. Default: breaking by design. **Implication for this task**: step 2 rewrites that tabs.org:211-214 prose (and the `workspace-default-home-builder` docstring at tabs.org:217-222) to state the new contract; every call site currently passes NAME (`(funcall workspace-home-builder name)` at tabs.el:231).

### Meta-discoveries
- `untouched-neighbour-consumer-drift`: regex/prose consumers are invisible to helper-name greps. After step 4, also run a whole-tree `grep -rn 'home-org\|home\.org' config/ openspec/specs/ --include='*.org' --include='*.el'` and list the survivors (expected: scaffold.org writer, docs/README.org:46-111 scaffold prose — both `ws-scaffold-and-anchor`'s) in Observations rather than editing them here.
- `register-status-lags-commit-prose`: cite the reconciliation note paths above (statuses are persisted in interfaces.org at integrate), not commit messages, when you record Observations.
