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
