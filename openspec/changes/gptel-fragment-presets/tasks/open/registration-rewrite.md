---
name: registration-rewrite
description: Rewrite preset registration to load tangled preset .el artifacts; delete the YAML frontmatter pipeline; retain native-Elisp scope/mode extraction; rewrite the preset-registration tests.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:fragment-core"
---

## Files to modify

- `config/gptel/presets/registration.org` (create) — new fragment-era
  registration; supersedes `config/gptel/preset-registration.org`.
- `config/gptel/presets/registration.el` (tangled).
- `config/gptel/preset-registration.org/el` (delete or reduce to a shim) — remove
  parse / normalize / coerce; keep nothing YAML-specific.
- `config/gptel/gptel.org` (modify) — load the new registration module in the
  presets sub-module load order.
- `config/gptel/presets/test/registration-spec.el` (create) — rewritten specs.
- Existing `preset-registration` tests (modify/relocate) under the new pipeline.

## Implementation steps

1. New registration model: each preset `.org` tangles to an `.el` that registers
   via `gptel-make-preset`. Registration loads those tangled `.el` artifacts (the
   sub-module loader `require`s each preset's `.el`), not a directory scan + YAML
   parse.
2. Preset name = basename (sans extension), interned. `:system` = the preset's
   rendered role text (from its static role fragment via fragment-core).
3. Retain scope/mode extraction, reading **native Elisp** config:
   - Scope keys `:paths` `:shell-commands` `:bash-tools` `:scope-profile` →
     stored in `jf/gptel-preset--scope-defaults`, stripped from the registration
     plist. Do **not** extract legacy `:org-roam-patterns`.
   - `:mode` → `jf/gptel-preset--mode-defaults` (default `"org-mode"`).
4. **Delete the YAML pipeline**: `jf/gptel-preset--parse-file`,
   `--normalize-keys`, `--coerce-values`. Remove `(require 'yaml)` from the preset
   path (see delete-old-presets task for the global yaml-dep check).
5. Rewrite tests: preset registered from its tangled `.el`; name from basename;
   idempotent re-load updates the entry; scope keys stored+stripped;
   `:scope-profile` extracted; preset with no scope keys unchanged; visible in
   transient menu / inline `@mention` (mock upstream surface as needed).
6. Tangle; run the registration suite and the broader gptel suite.

## Design rationale

Elisp config needs no coercion and tangles directly to a `gptel-make-preset`
call, deleting the parse/normalize/coerce machinery (design §Decision 6;
`specs/gptel/preset-registration.md` ADDED/MODIFIED/REMOVED). Scope/mode side
tables stay so the scope and session subsystems keep their existing lookups.

## Verification

- `grep -rn "yaml-parse-string\|normalize-keys\|coerce-values" config/gptel/presets config/gptel/preset-registration.el` (expect none on the preset path).
- `./bin/run-tests.sh -d config/gptel/presets/test`
- `./bin/run-tests.sh -d config/gptel` (no new failures beyond preset-content gaps handled by later tasks).

## Context pointers

- Spec: `specs/gptel/preset-registration.md` (Preset authored as Org tangled to
  Elisp; Config in an Elisp block; Scope key extraction MODIFIED; YAML reqs
  REMOVED).
- Current code: `config/gptel/preset-registration.org` (whole file is the thing
  being replaced).
- Scope consumer: `config/gptel/scope/scope-profiles.org`
  (`jf/gptel-preset--scope-defaults` lookup).

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
