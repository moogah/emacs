---
name: registry-on-purge-surface
description: Add :on-purge surface and purge-time dispatch to the integration registry
change: add-workspace-git-worktrees
status: ready
relations:
  - enables:purge-teardown-dispatch
  - enables:on-purge-teardown-handler
---

## Files to modify
- config/workspaces/integrations.org (modify) — and its tangled integrations.el
- config/workspaces/test/integration-registry-spec.el (modify)

## Implementation steps
1. In `workspace-register-integration` (a `cl-defun`), add an `on-purge`
   keyword argument alongside `label`, `on-create`, `menu`. Store it in the
   entry plist as `:on-purge`.
2. Relax the "at least one surface" guard so it accepts ANY of `on-create`,
   `menu`, or `on-purge`. Update the `user-error` message text to list all
   three surfaces:
   `"workspace-register-integration: %s has none of :on-create, :menu, :on-purge"`.
3. Add a new dispatch function `workspace--dispatch-purge-integrations`,
   modelled on the existing `workspace--dispatch-create-integrations`:
   - Signature `(name home context)`; build the payload via
     `workspace--integration-payload` (reuse it verbatim — do not duplicate
     payload shape).
   - Iterate `workspace--integrations` in registration order; for each entry
     carrying an `:on-purge` handler, run it through `workspace--run-one-integration`
     EXCEPT that runner reads `:on-create`. Generalise: either add an optional
     surface-key argument to `workspace--run-one-integration` (defaulting to
     `:on-create`) so it can run `:on-purge`, or add a sibling runner. Prefer
     adding the optional key arg to keep one normaliser.
   - `message` each `(failed . REASON)` as
     `"workspace: integration %s purge-teardown failed: %s"`.
   - Return the `(ID . OUTCOME)` alist; NEVER signal.
4. Tangle + validate: `./bin/tangle-org.sh config/workspaces/integrations.org`.
5. Extend `integration-registry-spec.el` with specs for: an on-purge-only
   registration succeeds and is retrievable; a registration with none of the
   three surfaces signals `user-error`; `workspace--dispatch-purge-integrations`
   runs each `:on-purge` once in order, skips entries lacking it, catches a
   throwing handler as `failed`, and never signals.

## Design rationale
Teardown is a genuine, reusable lifecycle event (gptel could later archive
sessions on it), so it belongs as a generic registry surface rather than as
ad-hoc advice from the git module — that would re-introduce the directionality
coupling the registry exists to remove. The birth case needs no new surface
(it reuses `:menu`), so `:on-purge` is the ONLY contract growth in this change.
Reusing the existing dispatch/runner shape keeps the registry uniform.

## Design pattern
Mirror `workspace--dispatch-create-integrations` and
`workspace--run-one-integration` in config/workspaces/integrations.org exactly —
same error-guard, same `ok`/`skipped`/`(failed . reason)` normalisation, same
"additive, never signals" contract. The only differences are the surface key
(`:on-purge`) and the failure message wording.

## Verification
- `./bin/tangle-org.sh config/workspaces/integrations.org` validates (parens OK)
- `./bin/run-tests.sh -d config/workspaces` — new registry specs pass
- Acceptance: `:on-purge` is a declarable, retrievable surface; an
  on-purge-only registration is valid; dispatch runs handlers in order, is
  error-guarded, and never signals.

## Context
design.md § Decisions 'D3 — One new generic registry surface: `:on-purge`'
specs/workspace-integrations/spec.md (MODIFIED registration; ADDED purge-time
teardown dispatch)
