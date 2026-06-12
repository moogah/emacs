---
name: wire-create-dispatch
description: Call integration dispatch from both creation entry points with correct context; delete the broken initial-session stub
change: workspace-integration-menu
status: blocked
relations:
  - blocked-by:integration-registry-core
---

## Files to modify
- `config/workspaces/tabs.org` (modify — dispatch from both `workspace--new-*`)
- `config/workspaces/scaffold.org` (modify — delete `workspace--scaffold-initial-session` + its call + prose)
- `config/workspaces/test/integration-dispatch-spec.el` (new — Buttercup)

## Implementation steps

1. **Delete the broken stub in `config/workspaces/scaffold.org`**: remove
   `workspace--scaffold-initial-session` (the `with-temp-file` writing
   `<date>-initial.org` with only `#+TITLE:`, ~scaffold.org:132) AND its call
   in the scaffold pipeline (the stage-5 invocation). `sessions/` is still
   created (the `make-directory` stays); it is just left EMPTY. Update the
   pipeline stage-list prose to drop the "create initial session" stage.

2. **Wire dispatch in `config/workspaces/tabs.org` — `workspace--new-default-path`:**
   after the `puthash ... workspace--registry` + `workspace-home-builder` call,
   add `(workspace--dispatch-create-integrations name home 'fresh)`. Dispatch
   runs AFTER registration (so a failing integration cannot block a clean
   workspace) and AFTER the `Initial workspace` commit (so its semantics are
   unchanged — integration files are intentionally left untracked).

3. **Wire dispatch in `workspace--new-anchor-existing`** with per-case context,
   after registration:
   - case 1 (repo + home.org, register-only) → `'anchored-existing`
   - case 2 (repo, no home.org, scaffolded no-commit) → `'anchored-scaffolded`
   - case 3 (non-repo, full scaffold) → `'fresh`
   Ensure dispatch fires in ALL THREE cases (case 1 currently does no scaffold —
   it must still dispatch so consumers can decide/skip).

4. **Do NOT reorder the commit.** `git commit "Initial workspace"` stays the
   last fail-fast scaffold step before registration. Mid-pipeline scaffold
   failure must still leave no registry entry and run no `:on-create`.

5. **Tests** `config/workspaces/test/integration-dispatch-spec.el` (Buttercup,
   behavioral): register a fake `:on-create` (rebind `workspace--integrations`
   in `before-each`); assert it runs once with the right `:context` for each of
   the three creation paths; a fake handler that `error`s does NOT abort
   creation (workspace exists, tab present, a `*Messages*` notice emitted — spy
   on `message`), and a second handler still runs; mid-scaffold failure runs no
   handler. Mock git/filesystem at the boundary with `cl-letf` per repo
   behavioral-test convention.

## Design rationale
The stub is exactly the broken artifact; deleting it (rather than patching) is
correct because workspaces structurally cannot build a real gptel session
(directionality contract). Dispatch-after-registration preserves the existing
"scaffold failure → no registry entry" semantics (design Decision 4) while
keeping one uniform dispatch site; the push payload means current-ness is
irrelevant. Additive-never-load-bearing: a failing integration leaves a valid
workspace.

## Verification
- `./bin/tangle-org.sh config/workspaces/scaffold.org`
- `./bin/tangle-org.sh config/workspaces/tabs.org`
- `./bin/run-tests.sh -d config/workspaces`
- `grep -rn "scaffold-initial-session\|-initial.org" config/workspaces/scaffold.el config/workspaces/tabs.el` returns nothing
- Done when: dispatch fires in all three contexts with correct labels; a
  throwing handler can't abort creation; failure semantics preserved; stub gone.

## Context
design.md § Decision 4; spec `workspaces` (MODIFIED: workspace-new default
scaffolding; Anchoring an existing directory via prefix arg); spec
`workspace-integrations` (Creation-time dispatch; Integration failures are
visible but never fatal).
