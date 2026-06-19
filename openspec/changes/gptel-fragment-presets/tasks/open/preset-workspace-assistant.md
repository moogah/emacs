---
name: preset-workspace-assistant
description: Author the workspace-assistant preset (Elisp config block + role fragment) tangling to a registering .el, following system-prompt best practices, with a golden snapshot of its rendered role.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:fragment-core"
  - "blocked-by:registration-rewrite"
---

## Files to modify

- `config/gptel/presets/workspace-assistant/preset.org` (create) — Elisp config
  block + role fragment sections.
- `config/gptel/presets/workspace-assistant/preset.el` (tangled, registers).
- `config/gptel/presets/test/golden/workspace-assistant.claude.txt` (create).
- `config/gptel/presets/test/workspace-assistant-spec.el` (create).

## Implementation steps

1. Write the role fragment with best-practice sections (proposal.md brief):
   `Role` (general-purpose helper for the user's active workspace — role + task
   up front), `Background` (what a workspace is — static domain knowledge),
   `Instructions` (numbered, imperative), `Constraints` (stay within workspace
   scope; reiterate the critical limits near the end). Keep at the right altitude
   — specific enough to guide novel inputs, expressed as heuristics not rule
   lookups. No tool docs yet (palette tools are a near-future follow-up).
2. Write the Elisp config block with native types: `:backend "Claude"`,
   `:model 'claude-sonnet-4-6`, `:temperature`, `:description`,
   `:confirm-tool-calls`, scope keys (`:scope-profile`, etc. as appropriate for a
   general workspace helper). Mirror the capability set the workspace flow needs
   (no executor-specific SQL tools unless required).
3. Ensure tangling produces a `preset.el` that registers via the new
   registration pipeline; `:system` = rendered role text.
4. Generate the golden snapshot of the rendered Claude role and assert
   `:to-equal (read-golden "workspace-assistant.claude.txt")`.
5. Add a spec: preset registers, resolves expected config keys, scope stored in
   `jf/gptel-preset--scope-defaults`.
6. Tangle; run the presets suite.

## Design rationale

This preset is the forcing function that proves the fragment model end-to-end and
replaces `executor` for the workspace package (design §Decision 9). Authoring it
on best practices (role-first, semantic sections → XML, constraints restated)
validates the renderer against real content.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets/test`
- `grep -n "gptel-make-preset\|workspace-assistant" config/gptel/presets/workspace-assistant/preset.el`
- Manual: `(gptel-get-preset 'workspace-assistant)` resolves in a loaded session.

## Context pointers

- Spec: `specs/prompt-fragments/spec.md`; `specs/gptel/preset-registration.md`.
- Best-practices brief + recommended Claude section ordering: proposal.md.
- Workspace consumer (flipped in a later task):
  `config/gptel/sessions/workspace-integration.org:53`.
