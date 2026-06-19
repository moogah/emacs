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
