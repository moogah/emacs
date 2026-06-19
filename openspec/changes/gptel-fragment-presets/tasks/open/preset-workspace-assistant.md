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

## Cycle 1 updates (cycle-1781883616)

> fragment-core landed and its interface is now **confirmed** in the register.
> Build against these concrete facts (no longer speculation):

- **Fragment value is a plist** `(:kind SYMBOL :sections ((name . body) ...))`
  — `:kind` ∈ `{static, dynamic}` (default `static`); a section is a cons
  `(name . body)` where `name` is the verbatim trimmed heading string and
  `body` is a whitespace-trimmed string. (`register/shape/fragment` reconciled;
  `register/shape/section` confirmed.)
- **Renderer:** `jf/gptel-fragment-render (fragment backend)` — `claude` only;
  each section → `"<tag>\nbody\n</tag>"`, body **verbatim**, sections joined
  with a **blank line** (`"\n\n"`). Unimplemented backend logs-then-signals
  `jf/gptel-fragment-unimplemented-backend` (`define-error` ⊂ `error`) — catch
  with `condition-case` on `error` if needed.
- **Parser:** `jf/gptel-fragment--parse-source` (Org string **or** file path →
  the fragment plist); batch-loadable (no interactive Org deps).
- **Kind is declared in source** via a `#+fragment_kind: static|dynamic` Org
  keyword (case-insensitive); unrecognized → `static`.
- **Section-name → tag:** `jf/gptel-fragment--section-name-to-tag` downcases,
  trims, and collapses whitespace runs to a single `_` (`Output Format` →
  `output_format`). Multi-word headings are safe.
- **Reviewer note:** a fragment with no sections renders to `""`. Decide
  explicitly whether the composer skips/rejects empty fragments rather than
  silently emitting an empty block.
- **Test-helper lesson (cycle 1 inline fix arch-cycle-1781883616-1):** if this
  task adds a `*/test/helpers-spec.el`, it MUST `(provide ...)` a **module-unique**
  feature (e.g. `presets-helpers-spec`) — never the generic `'helpers-spec`,
  which `scope/test/helpers-spec.el` already owns. Reusing it no-ops the scope
  specs' `require` and crashes the full buttercup load. Also: a directory-scoped
  green run does NOT imply full-suite green; the orchestrator gates on the full
  suite.

## Cycle 2 updates (cycle-1781885402)

> registration-rewrite landed; the preset pipeline is now **confirmed**. Build against
> these concrete facts:

- **Author the preset at `config/gptel/presets/workspace-assistant/preset.el`** (loader
  convention `config/gptel/presets/<name>/preset.el`; preset name = basename). Register
  is idempotent (re-load updates, no duplicate). `register/boundary/preset-org-to-registration`
  **confirmed**.
- **Config block** native Elisp (`register/shape/preset-config-plist`, confirmed):
  `:description`/`:backend` (`claude`)/`:model` required; `:tools`, `:temperature`, scope
  keys (`:paths :shell-commands :bash-tools :scope-profile` → extracted+stripped) + `:mode`
  optional.
- **`:system` = pre-rendered static role text** (rendered at tangle time via
  `jf/gptel-fragment-render`; registration forwards it). No palette tools yet (per design
  Decision 9 / Non-Goals).
