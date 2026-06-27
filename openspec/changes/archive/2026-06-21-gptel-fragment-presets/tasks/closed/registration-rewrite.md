---
name: registration-rewrite
description: Rewrite preset registration to load tangled preset .el artifacts; delete the YAML frontmatter pipeline; retain native-Elisp scope/mode extraction; rewrite the preset-registration tests.
change: gptel-fragment-presets
status: done
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

## Observations

- **Old YAML pipeline file deleted, not shimmed.** `config/gptel/preset-registration.org`
  and its tangled `.el` are removed entirely (`git rm`). The fragment-era module
  lives at `config/gptel/presets/registration.org` → `registration.el`. The
  feature symbol `gptel-preset-registration` is *preserved* on the new module so
  existing `(require 'gptel-preset-registration ...)` call sites keep resolving;
  only the file path in those `require`s was updated.
- **No dedicated old `preset-registration-spec.el` existed.** The only test that
  exercised the old pipeline's retained surface is
  `config/gptel/scope/test/integration/preset-scope-key-extraction-spec.el`
  (tests `jf/gptel-preset--extract-scope`). I kept that function with an identical
  contract and only repointed its `require` path (`preset-registration.el` →
  `presets/registration.el`) plus a stale doc comment. It passes unchanged. The
  new behavioral specs live in `config/gptel/presets/test/registration-spec.el`.
- **Public registration entry point added: `jf/gptel-preset-register`.** A tangled
  `preset.el` calls `(jf/gptel-preset-register 'NAME :description ... :system ...)`.
  This decouples registration from the composer (per escalation note): `:system`
  arrives pre-rendered (the preset `.org` renders its static role fragment at
  tangle time via `jf/gptel-fragment-render`); registration never renders. The
  helper extracts scope/mode keys then calls `gptel-make-preset`.
- **Loader contract: `<name>/preset.el`.** `jf/gptel-preset-register-all` scans
  `jf/gptel-presets-directory` for *subdirectories* containing `preset.el` and
  `load`s each. This matches the preset-content tasks' authored layout
  (`config/gptel/presets/system-explorer/preset.el`, etc.) and avoids loading
  sibling module files (`fragments.el`, `registration.el`) that live directly in
  `presets/`. Subdirectories without a `preset.el` are skipped. Returns the count.
- **Load order:** `gptel.org` now loads `presets/fragments` → `presets/registration`
  → `(jf/gptel-preset-register-all)` → scope-profiles/consumers, using
  `jf/resolve-module-path` (dotted module ids, per the module-system contract)
  for the two presets modules I introduced. Neighboring `expand-file-name` lines
  left untouched (out of scope).
- **`(require 'yaml)` removed from the preset path** (it was in the registration
  section of `gptel.org`). The remaining `(require 'yaml)` in `scope-profiles.org`
  is profile-YAML (scope subsystem), explicitly out of this task's scope — the
  global yaml-dep removal is the delete-old-presets task.
- **`:org-roam-patterns` handling retained verbatim** from the old extractor: it
  is dropped from *both* scope-defaults and the registration plist, and triggers
  a `display-warning`. It is never treated as a scope key.
- **Doc-only updates** to stale references in `scope-profiles.org` and
  `tools/community-tools.org` (they named the deleted file / the `.md` pipeline).
  I reworded the registration.org intro to avoid the literal deleted-function
  names so the verification grep stays clean on the preset path.
- **Broader-suite baseline:** `./bin/run-tests.sh -d config/gptel` reports 3
  preexisting failures, all in scope bash add-to-scope integration specs
  (`bash-add-to-scope-bug-spec.el`, `bash-multi-violation-expansion-spec.el`;
  root cause `Lisp nesting exceeds max-lisp-eval-depth`). Confirmed identical on
  the pre-change tree via `git stash` — NOT introduced by this task and unrelated
  to preset registration.

## Discoveries

### discovery: preset-config-plist shape pinned (native Elisp, scope keys extracted)
- **discovery_id:** registration-preset-config-plist-shape
- **class:** spec-signal
- **description:** Implemented `register/shape/preset-config-plist` concretely.
  The config a tangled `preset.el` passes to `jf/gptel-preset-register` is a plain
  plist of native-Elisp gptel keys plus `:system` (pre-rendered role text). The
  register helper extracts `:paths :shell-commands :bash-tools :scope-profile`
  into `jf/gptel-preset--scope-defaults` (keyed by name symbol) and *removes* them
  from the plist; extracts `:mode` into `jf/gptel-preset--mode-defaults`
  (default `"org-mode"`, invalid → `"org-mode"`); drops `:org-roam-patterns`
  (warn). `:model` is a symbol authored directly (no interning). No coercion of
  `:true`/`:false` and no snake→kebab normalization occur — those are deleted.
  Everything else (`:description :backend :model :tools :temperature` and any
  other key) passes through verbatim to `gptel-make-preset`.
- **affected_register_entry:** register/shape/preset-config-plist
- **recommendation:** Reconcile speculated → confirmed. One refinement vs. the
  speculated entry: `:system` is supplied by the *preset author* (rendered at
  tangle time), so the registration helper's job is "extract scope/mode + forward";
  it does not itself call `jf/gptel-fragment-render`. The speculated entry's
  phrasing ("Registration sets :system to the rendered role text ... via
  jf/gptel-fragment-render") is accurate at the *pipeline* level but the render
  happens at tangle time in the preset `.el`, not inside the register function.

### discovery: preset-org-to-registration boundary — render at tangle time, register decoupled from composer
- **discovery_id:** registration-boundary-tangle-time-render
- **class:** interface-drift
- **description:** Implemented `register/boundary/preset-org-to-registration`.
  Stage 1 (author + pre-render role) happens in the preset `.org` at tangle time:
  the role fragment is parsed+rendered (`jf/gptel-fragment-render`) and the
  resulting string is inlined as `:system` in the tangled `preset.el`'s
  `jf/gptel-preset-register` call. Stage 2 (extract-scope-keys) and stage 3
  (register) both happen inside `jf/gptel-preset-register` at load time. The
  loader `jf/gptel-preset-register-all` discovers `<name>/preset.el` and `load`s
  each; idempotency is provided by upstream `gptel-make-preset` (assoc/setcdr by
  name). Crucially, registration is **decoupled from the composer** — it forwards
  a single pre-rendered `:system` string and never composes/renders fragments
  itself.
- **affected_register_entry:** register/boundary/preset-org-to-registration
- **recommendation:** Reconcile speculated → confirmed, with the boundary's
  render-locus clarified: the renderer must be batch-loadable at *tangle time*
  (it is — `presets/fragments.el` is batch-safe), but the *registration module
  at load time* does NOT require the renderer. The named load-order risk is
  satisfied: `presets/fragments` loads before `presets/registration` in
  `gptel.org`. The preset name = source basename (the authoring convention is the
  subdir basename, e.g. `system-explorer/preset.el` registers `'system-explorer`);
  the loader keys off the `preset.el` location, and the name symbol is supplied by
  the preset.el's own `jf/gptel-preset-register` call.

### discovery: preset .el location convention is <name>/preset.el (one subdir per preset)
- **discovery_id:** registration-preset-el-location
- **class:** scope-question
- **description:** The loader needs a discovery convention. Because module files
  (`fragments.el`, `registration.el`) live directly in `config/gptel/presets/`, I
  could not scan `presets/*.el` for presets. I adopted the convention the
  preset-content tasks already assume: `config/gptel/presets/<name>/preset.el`
  (one subdirectory per preset). The loader scans subdirectories for `preset.el`.
- **affected_register_entry:** register/boundary/preset-org-to-registration
- **recommendation:** Confirm this convention is consistent across the
  preset-system-explorer / preset-workspace-assistant / delete-old-presets tasks
  (they reference `presets/<name>/preset.org` → `preset.el`, so they agree). If a
  later task wants flat `presets/*-preset.el` instead, the loader's discovery glob
  is the single point to change.
