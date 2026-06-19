---
name: composer
description: Implement the composer that assembles an ordered fragment list into the effective system message, with context-derived default compositions (chat/agent) and an override seam.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:fragment-core"
---

## Files to modify

- `config/gptel/presets/fragments.org` (modify) — add composer + composition API.
- `config/gptel/presets/fragments.el` (tangled).
- `config/gptel/presets/test/composer-spec.el` (create).

## Implementation steps

1. Define a composition as an ordered list of fragment references. A fragment
   reference resolves to either pre-rendered static text or a dynamic function
   (see fragment-core kinds).
2. Implement `jf/gptel-fragment-compose (composition backend &optional context)`:
   - Render each fragment in list order (static → pre-rendered text; dynamic →
     call its function at compose time), join with a blank line.
   - Preserve list order exactly.
3. Implement context-default compositions:
   - `jf/gptel-fragment--default-composition (context)` returns:
     - `chat`  → `[emacs-prelude(static), role, environment(dynamic)]`
     - `agent` → `[agent-preamble(static), role, environment(dynamic)]`
   - Provide an override seam: a caller MAY pass an explicit composition that
     replaces the default (add/remove/reorder). Document that dynamic fragments
     default to the **tail** and non-tail dynamic placement is discouraged.
   - The `role` slot is filled by the active preset's role fragment; absent role
     content collapses to an empty contribution (prelude/preamble still lead).
4. Write `composer-spec.el`:
   - Fragments appear in list order `[A,B,C]`; single-fragment composition.
   - Chat default = prelude, role, env; agent default = preamble, role, env.
   - Author supplies only a role fragment → context default with that role in the
     role position.
   - Dynamic fragment evaluated at compose time reflects live input; defaults to
     tail; explicit non-tail placement is honored.
5. Tangle and run tests.

## Design rationale

Realizes the composition + context-default + static/dynamic-timing requirements
(design.md §Decision 3, §Decision 4). Reproduces today's hard-coded `concat`
order as data, so the common case (config + role only) stays simple while the
override seam keeps extension opt-in.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets/test`
- `grep -n "defun jf/gptel-fragment-compose" config/gptel/presets/fragments.el`

## Context pointers

- Spec: `specs/prompt-fragments/spec.md` (Composition assembles an ordered
  fragment list; Context-derived default composition with an override seam;
  Static/Dynamic fragment requirements).

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
