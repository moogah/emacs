---
name: preset-workspace-assistant
description: Author the workspace-assistant preset (Elisp config block + role fragment) tangling to a registering .el, following system-prompt best practices, with a golden snapshot of its rendered role.
change: gptel-fragment-presets
status: done
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

## Observations

- **`:backend` value type — task body vs. register entry.** Implementation step 2
  says `:backend "Claude"` (a string), but the confirmed register entry
  `register/shape/preset-config-plist` declares `:backend` a **symbol** (`claude`),
  its validator rejects non-symbols, and the renderer dispatches on the symbol
  `'claude` (`pcase` in `jf/gptel-fragment-render`). I authored `:backend 'claude`
  (symbol). The register entry is authoritative and the renderer would not match a
  string, so this is the only correct choice. Recorded as a deviation below.
- **No existing `preset.org` to mirror.** The header/`:comments no` convention was
  taken from `registration.org` (the sibling source module). The first babel block
  lands the `lexical-binding` header on line 1 as required.
- **Role-source authored via `string-join` of lines, not a single multi-line string
  literal.** A multi-line string with `* Role`/`* Background` headings at column 0
  inside `#+begin_src` makes Org's tangler treat those lines as Org headings and
  silently *truncate the source block* (the first attempt tangled only 5 of 6
  blocks; the `--role-source` defconst vanished and load failed with
  `void-variable`). Assembling the source with `string-join` keeps every `"* …"`
  marker off column 0 in the `.org` while still presenting real `* Heading` lines
  to `jf/gptel-fragment--parse-source`. This is a tangle-time authoring hazard any
  future fragment-source author will hit.
- **Render locus.** The role renders once at *load time* of `preset.el` (a
  deterministic, batch-safe transform), and the rendered string is cached in a
  `defconst` (`--system`) that registration forwards verbatim. The spec asserts
  `:system` is `eq` to that constant, pinning the static-prerender invariant: no
  per-send rendering. The boundary entry's "tangle time" language is satisfied in
  spirit (rendered before any send, from a static source); a stricter
  "literal-baked-into-.el" reading would require running the renderer as a babel
  build step — see Discoveries.
- **`:mode` omitted** → registration defaults it to `"org-mode"`, the correct
  default for a workspace session whose drawers are Org. Not asserted in the spec
  (covered by registration-spec's mode-default test); left implicit.
- **Golden file** stored with no trailing newline (matches `presets-test-read-golden`
  verbatim-read contract and the existing `core-sample.claude.txt`).

## Discoveries

- class: deviation
  affected_register_entry: register/shape/preset-config-plist
  summary: |
    Task implementation-step 2 specifies `:backend "Claude"` (string); the
    confirmed register shape and the renderer's `pcase` both require the symbol
    `claude`. Authored `:backend 'claude`. No friction with the register entry —
    the deviation is purely against the task body's literal text, which is
    internally inconsistent with the cited (authoritative) register entry.
  status: implemented-as-register-specifies

- class: spec-signal
  affected_register_entry: register/boundary/preset-org-to-registration
  summary: |
    Stage-1 of the boundary ("role fragment pre-renders at TANGLE time inside
    preset.el") is ambiguous about whether the rendered text must be a baked
    string literal in the .el or may be computed by the renderer at .el load
    time. I implemented load-time render-once + defconst caching, which upholds
    static-prerender-dynamic-compose (the spec's actual load-bearing concern: no
    per-send rendering) and keeps the .org readable. A literal-baked variant would
    need a babel `:var`/eval build step to inline the rendered string and would
    duplicate the role text in both the source string and the baked literal. Flag
    for the integrator if "baked literal" is a hard requirement.

- class: scope-question
  affected_register_entry: register/shape/preset-config-plist
  summary: |
    `:confirm-tool-calls` appears in implementation-step 2 and the legacy
    executor.md, but it is NOT a key in the confirmed preset-config-plist shape
    (not required, not optional, no consumer named). With no palette tools wired
    yet (Decision 9 / Non-Goals), a tool-confirmation key has nothing to gate, so
    I omitted it. Revisit when palette tools land for workspace-assistant.

- class: interface-drift
  summary: |
    Authoring a fragment role inside a preset .org is a tangle-time minefield:
    column-0 `*` heading markers in a source-block string get eaten by Org's
    tangler (silent block truncation, not an error). A shared authoring helper or
    a documented `string-join`-of-lines convention in fragments.org / a preset
    template would prevent every future preset author from rediscovering this.
