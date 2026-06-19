---
name: preset-system-explorer
description: Author the read-only system-explorer preset (Elisp config block + role fragment) tangling to a registering .el, specializing in environment analysis with no write/modify operations.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:fragment-core"
  - "blocked-by:registration-rewrite"
---

## Files to modify

- `config/gptel/presets/system-explorer/preset.org` (create) — Elisp config +
  role fragment.
- `config/gptel/presets/system-explorer/preset.el` (tangled, registers).
- `config/gptel/presets/test/golden/system-explorer.claude.txt` (create).
- `config/gptel/presets/test/system-explorer-spec.el` (create).

## Implementation steps

1. Write the role fragment, best-practice sections: `Role` (read-only environment
   analyst — installed packages, available commands, configuration,
   troubleshooting), `Background`, `Instructions` (numbered; analyze before
   concluding), `Constraints` (read-only — never write/modify; reiterate near the
   end). Keep the static system prompt free of live machine data — environment
   capture is the dynamic env fragment's / a tool's job, not baked into `:system`
   (design Non-Goals; best-practices static/dynamic separation).
2. Elisp config block: `:backend "Claude"`, `:model 'claude-sonnet-4-6`,
   `:temperature`, `:description`, read-only tool set, and a read-only
   `:scope-profile` (e.g. `system-explorer`) — no write/modify/deny-violating
   capabilities.
3. Tangle → `preset.el` registers via the new pipeline; `:system` = rendered role.
4. Golden snapshot of the rendered Claude role; assert `:to-equal`.
5. Spec: preset registers; config resolves; scope profile is read-only (no write
   paths granted).
6. Tangle; run the presets suite.

## Design rationale

The second preset exercises the read-only end of the behavioral spectrum and a
different scope profile, proving the model generalizes beyond the workspace
helper (proposal.md; design §Decision 9). Reinforces the static/dynamic
separation: the prompt describes *how to reason about* environments; actual
machine state stays dynamic.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets/test`
- `grep -n "gptel-make-preset\|system-explorer" config/gptel/presets/system-explorer/preset.el`
- Confirm no write tools / write scope in the config block.

## Context pointers

- Spec: `specs/prompt-fragments/spec.md`; `specs/gptel/preset-registration.md`.
- Scope profiles: `config/gptel/scope/scope-profiles.org` (read-only profile
  shape).
