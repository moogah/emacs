---
name: composer
description: Implement the composer that assembles an ordered fragment list into the effective system message, with context-derived default compositions (chat/agent) and an override seam.
change: gptel-fragment-presets
status: done
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

## Observations

- **Empty/absent-role decision (SKIP, not reject):** `jf/gptel-fragment-compose`
  realizes each reference, then joins only the *non-empty* (after `string-trim`)
  results with `"\n\n"`. A reference realizing to `""`/whitespace contributes
  nothing. Rationale: (1) a fragment with no sections renders to `""`
  (fragment-core reviewer note); (2) the `role` slot collapses to an empty
  contribution when a preset supplies no role fragment — the documented common
  case (config-only preset). Rejecting would punish that common case; emitting a
  blank block would produce leading/trailing/double blank lines and a join-unstable
  prefix. Skipping keeps the static prelude/preamble prefix clean and stable for
  prompt caching. Asserted in the "Empty / absent contributions" describe block.

- **nil dynamic result == empty contribution:** a dynamic fn returning nil
  (e.g. the default `environment-fn` = `#'ignore` before it is wired) realizes
  to `""` and is skipped, not coerced to the literal string `"nil"`. Non-nil,
  non-string results are `format`-coerced. This was a real bug caught by the
  "author supplies only a role fragment" test and fixed in `--realize-ref`.

- **`backend` arg is currently inert in `compose`.** Static references are
  already pre-rendered for their backend (tangle-time) and dynamic functions own
  their own formatting, so `compose` does not consult `backend`. It is kept in
  the signature (`(composition backend &optional context)`) per the task contract
  for parity with `jf/gptel-fragment-render` and forward compatibility. Flagged as
  a potential responsibility-leakage candidate in Discoveries — if no backend-aware
  join ever materializes, the integrate phase may drop the parameter.

- **Default-composition content sourced via an override seam of `defvar`s**
  (`jf/gptel-fragment-chat-prelude-text`, `-agent-preamble-text`,
  `-environment-fn`) because the concrete `emacs-prelude` / `agent-preamble` /
  `environment` sources are owned by other (source/integration) tasks and don't
  exist yet. This keeps the composer self-contained and unit-testable now; the
  integration task wires the real tangled text / env fn into these vars without
  touching composer logic. If integration prefers a different wiring mechanism
  (e.g. passing refs in directly), the `defvar` seam is the only thing to revisit.

- **`--default-composition` grew an optional `role-ref` arg** beyond the task's
  stated signature `(context)`. The task says "the role slot is filled by the
  active preset's role fragment" but gave no mechanism to inject it; an arg is the
  minimal seam. Absent `role-ref` → empty static role slot (skipped at compose).
  Out-of-scope question for integration: where the active preset's role ref is
  resolved from (registration? send context?). Noted, not resolved here.

- **Pre-existing baseline failures (external):** `./bin/run-tests.sh -d config/gptel`
  reports 3 failures, all in `config/gptel/scope` (a `:success` callback assertion
  + two `read-paths` nil assertions, plus "Registry write failed" warnings).
  Confirmed pre-existing by stashing this change and re-running `-d config/gptel/scope`:
  still 3 failed. Not caused by composer; not in this task's scope.

## Discoveries

```yaml
- discovery_id: composer-composition-shape-pinned
  class: shape-fragmentation
  description: >
    A composition is pinned as an ordered list of fragment-reference plists.
    A reference is (:kind static :text STRING) or (:kind dynamic :fn FUNCTION),
    built via jf/gptel-fragment-ref-static / -ref-dynamic. :kind deliberately
    mirrors the fragment plist's :kind so the two shapes read symmetrically. A
    static ref carries already-pre-rendered text (consumed verbatim, no
    compose-time render); a dynamic ref carries a 1-arg fn called
    (funcall fn context) at compose time. This is a NEW value shape that was
    seeded but unpinned; pinning up front per the cycle-1 meta-discovery.
  affected_register_entry: register/shape/composition
  recommendation: >
    Confirm register/shape/composition as: "ordered list of fragment references;
    reference = plist (:kind static :text STRING) | (:kind dynamic :fn FN);
    constructors jf/gptel-fragment-ref-static / -ref-dynamic; static text is
    pre-rendered+verbatim, dynamic fn called with the compose context."

- discovery_id: composer-compose-confirmed
  class: spec-signal
  description: >
    register/boundary/composer-compose's realize-fragments + join stages are
    implemented exactly: realize each ref in order (static -> :text verbatim;
    dynamic -> funcall :fn context), join in list order with a blank line
    ("\n\n", matching fragment-core's section join). The resolve-composition
    stage is realized as jf/gptel-fragment--default-composition (context-derived
    chat/agent defaults) + an explicit-composition override seam (caller passes a
    composition directly to compose). Cross-stage invariants hold.
  affected_register_entry: register/boundary/composer-compose
  recommendation: speculated -> confirmed-recommended

- discovery_id: composer-static-prerender-dynamic-compose-held
  class: spec-signal
  description: >
    LOAD_BEARING invariant static-prerender-dynamic-compose holds with no
    deviation. Static refs are consumed verbatim with zero compose-time
    rendering; dynamic refs are the only per-send work; default compositions
    place the single dynamic ref (environment) at the tail, keeping the
    static prelude/preamble+role prefix stable/cacheable. Non-tail dynamic
    placement is mechanically permitted (asserted) but the defaults and docs
    keep it tail-only.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: speculated -> confirmed-recommended (no deviation)

- discovery_id: composer-context-default-composition-held
  class: spec-signal
  description: >
    context-default-composition holds: chat -> [prelude(static), role,
    environment(dynamic)]; agent -> [preamble(static), role,
    environment(dynamic)]. Override seam = caller may pass an explicit
    composition to compose. Absent role collapses to an empty contribution
    (skipped at compose) while prelude/preamble still leads. One refinement:
    the role fragment is injected via an optional role-ref arg to
    --default-composition (the entry implies the role is "supplied" but names
    no mechanism); and the prelude/preamble/env content is sourced through
    defvar seams pending the source tasks.
  affected_register_entry: register/invariant/context-default-composition
  recommendation: >
    speculated -> confirmed-recommended, with a note that role injection is via
    a role-ref parameter and lead/env content via override-seam defvars until
    the source fragments land.

- discovery_id: composer-backend-arg-inert
  class: responsibility-leakage
  description: >
    jf/gptel-fragment-compose accepts a `backend` arg per the task signature but
    does not consult it: static refs are pre-rendered for their backend at tangle
    time and dynamic fns own their formatting. The arg is retained for parity with
    jf/gptel-fragment-render and forward compatibility, but is presently inert.
  affected_register_entry: register/boundary/composer-compose
  recommendation: >
    Decide during integrate whether compose is genuinely backend-agnostic (drop
    the arg) or whether a future backend-aware join justifies keeping it. No
    behavior depends on it today.

- discovery_id: composer-nil-dynamic-empty
  class: invariant-gap
  description: >
    A dynamic fragment fn returning nil (e.g. the default environment fn = ignore
    before wiring) must contribute nothing, not the literal "nil". --realize-ref
    maps nil -> "" before the empty-skip check. Without this, an unwired env seam
    would inject "nil" into every system message. Not covered by the seeded
    entries; recording as a realization detail of the empty-contribution policy.
  affected_register_entry: register/boundary/composer-compose
  recommendation: >
    Fold into composer-compose's join/skip contract: empty == nil-or-trimmed-empty
    text; such references are skipped.
```
