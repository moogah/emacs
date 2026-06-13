---
name: wire-create-dispatch
description: Call integration dispatch from both creation entry points with correct context; delete the broken initial-session stub
change: workspace-integration-menu
status: done
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

## Observations
- Deleted `workspace--scaffold-initial-session` and its stage-5 call in
  `scaffold.org`. The pipeline is now FIVE stages (1 mkdir, 2 git-init gated,
  3 home.org, 4 mkdir sessions/ left empty, 5 git add+commit gated). Stage
  numbering in prose/docstring renumbered (old stage 6 commit → stage 5).
  `make-directory` for `sessions/` stays; sessions/ is just left empty. The
  `git commit "Initial workspace"` step is untouched and remains the final
  fail-fast scaffold step.
- `tabs.org`: added `(require 'workspace-integrations)`. `workspace--new-
  default-path` fires `(workspace--dispatch-create-integrations name home
  'fresh)` after the `puthash` + home-builder. `workspace--new-anchor-
  existing` was refactored so the `cond` now *returns* a per-case context
  symbol (`anchored-existing` / `anchored-scaffolded` / `fresh`) bound in the
  enclosing `let*`; a single dispatch call after registration uses it. This
  keeps one uniform dispatch site while satisfying the per-case label
  requirement. The outer `let` became `let*` because `context` references the
  `cond` over earlier bindings.
- Local variable names at the dispatch sites: `name` and `home` in both
  functions (both already in lexical scope at the post-registration point).
- Behavioral semantics verified by tests: dispatch is strictly downstream of
  the fail-fast scaffold + `puthash`, so a mid-pipeline scaffold `user-error`
  leaves no registry entry and runs no `:on-create` handler; a throwing
  handler is caught inside `workspace--dispatch-create-integrations` and never
  aborts creation (workspace registered, tab present, `*Messages*` notice
  emitted, second handler still runs).
- Stale-test reconciliation (caused directly by the stub removal): updated
  `config/workspaces/test/scaffold-spec.el` (stage-5 "exactly one initial.org"
  → "sessions/ left empty"; anchor-branch porcelain no longer asserts
  `?? sessions/` because git does not track empty directories; mid-failure
  assertion now expects an empty sessions/; six-stage→five-stage prose) and a
  comment in `workspace-new-anchor-spec.el` (case 2 "home.org and
  sessions/<date>-initial.org" → "home.org and an empty sessions/"). These
  were not in the task's "files to modify" list but had to be updated to keep
  `./bin/run-tests.sh -d config/workspaces` green after the contract change.
- New spec `config/workspaces/test/integration-dispatch-spec.el` (6 specs):
  per-context dispatch for all four creation flows, failure-never-fatal,
  scaffold-failure-runs-no-handler. Test suite: 272 specs / 0 failed (266
  baseline + 6).
- Both grep guards pass: no `scaffold-initial-session`/`-initial.org` and no
  `gptel-sessions-` in tabs.el or scaffold.el.

## Discoveries
- discovery_id: disc-wire-create-dispatch-1
  class: interface-drift
  description: >-
    register/boundary/workspace-scaffold-pipeline describes a SIX-stage
    ordered pipeline with INIT-AND-COMMIT? gating stages 2 and 6 and a stage-5
    "create HOME/sessions/<date>-initial.org". After deleting the broken
    initial-session stub, the pipeline is FIVE stages: the
    session-file-creation stage is gone, sessions/ is created empty (mkdir
    stays as stage 4), and the gated commit is now stage 5 (was stage 6). The
    register entry's stage-list contract no longer matches the implementation
    or the renumbered scaffold.org prose/docstrings.
  affected_register_entry: register/boundary/workspace-scaffold-pipeline
  recommendation: >-
    Reconcile the register entry to the five-stage pipeline (drop the initial-
    session stage; sessions/ created-empty as stage 4; gated git add+commit as
    stage 5; INIT-AND-COMMIT? gates stages 2 and 5). The pipeline's load-
    bearing failure semantics are unchanged (commit stays last fail-fast step;
    failure → partial state left in place → no registry entry → no :on-create),
    so only the stage enumeration drifts.

