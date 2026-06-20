---
name: preset-system-explorer
description: Author the read-only system-explorer preset (Elisp config block + role fragment) tangling to a registering .el, specializing in environment analysis with no write/modify operations.
change: gptel-fragment-presets
status: done
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

## Cycle 2 updates (cycle-1781885402)

> registration-rewrite landed; the preset pipeline is now **confirmed**. Build against
> these concrete facts:

- **Author the preset at `config/gptel/presets/system-explorer/preset.el`** (loader
  convention: `config/gptel/presets/<name>/preset.el`; preset name = basename =
  `system-explorer`). `register/boundary/preset-org-to-registration` **confirmed**.
- **Config block** is native Elisp (`register/shape/preset-config-plist`, confirmed):
  required `:description` (string), `:backend` (symbol `claude`), `:model` (symbol);
  optional `:tools`, `:temperature`, and the scope keys `:paths :shell-commands
  :bash-tools :scope-profile` (extracted into `jf/gptel-preset--scope-defaults` and
  stripped) + `:mode`. `:org-roam-patterns` is NOT a scope key.
- **`:system` is the PRE-RENDERED role text** — render the static role fragment at
  **tangle time** via `jf/gptel-fragment-render`; registration forwards it (it does not
  call the renderer itself). For read-only system-explorer, keep scope to read-only ops.

## Observations

- **Authored at `config/gptel/presets/system-explorer/preset.org` → `preset.el`**
  (loader convention `<name>/preset.el`; preset name = basename `system-explorer`).
  Tangles 6 blocks; `./bin/tangle-org.sh` parens-validation passes.
- **Role fragment is read-only and machine-agnostic.** Four sections — `Role` /
  `Background` / `Instructions` (numbered; "analyze before you conclude" is an
  explicit step) / `Constraints` (strictly read-only; reiterated in the closing
  sentence). It describes *how to reason about* an environment and bakes in NO
  live machine data (no OS name, kernel string, or paths) — a spec guard asserts
  the rendered `:system` contains none of `darwin` / `/usr/local/bin` / `homebrew`.
  Live state is left to the dynamic `environment` fragment / tools.
- **Config block** (`register/shape/preset-config-plist`): `:backend 'claude`
  (symbol), `:model 'claude-sonnet-4-6` (symbol), `:temperature 0.3`,
  `:description` string, `:tools '("read_file" "list_directory"
  "search_project_content" "list_project_files")` (all read-only), and
  `:scope-profile "system-explorer"` (the committed read-only profile: no
  write/modify/execute paths, `cloud.auth_detection: deny`).
- **No write capability anywhere.** No `:paths` / `:shell-commands` / `:bash-tools`
  authored; no write/exec/delete tool named (`grep -niE` for
  `create_file|delete_files|execute_*|run_bash_command|write_paths` over
  `preset.el` returns nothing). Read-only on both the tool axis and the scope axis.
- **`:system` pre-render mechanism.** The role fragment is a fragment plist
  (`(:kind static :sections ((NAME . BODY) ...))`) built directly in Elisp and
  rendered once at preset.el *load time* via `jf/gptel-fragment-render … 'claude`.
  Load-time (not per-send) render upholds
  `register/invariant/static-prerender-dynamic-compose`. Golden snapshot
  `test/golden/system-explorer.claude.txt` (no trailing newline) is asserted
  `:to-equal` the registered `:system`.
- **Tests:** `config/gptel/presets/test/system-explorer-spec.el` (buttercup,
  `presets-helpers-spec` only). Directory run: `Ran 63 specs, 0 failed` (9 new).

## Discoveries

### discovery: role-fragment authored as an Elisp plist, not an embedded Org string
- **discovery_id:** system-explorer-role-as-plist-not-org-string
- **class:** spec-signal
- **description:** The cited shapes describe the rendered `:system` as the role
  fragment rendered at tangle time, and `jf/gptel-fragment--parse-source` accepts
  an Org *string* of `*`-headings. Authoring that string verbatim inside an
  `emacs-lisp` babel block is hostile to Org tangling: lines beginning with `*`
  at column 0 inside the block collide with Org heading syntax — the first tangle
  silently *dropped the whole block* ("Tangled 5 code blocks" for 6 blocks), and
  the `,*` structural-escape strips inconsistently (first body line only),
  corrupting the string. I instead built the fragment value directly as the plist
  `(:kind static :sections ((NAME . BODY) ...))` — the exact shape the parser
  emits — and fed it to the public `jf/gptel-fragment-render`. The renderer
  remains the single source of truth for the emitted Claude format; only the
  parser is bypassed (it is just one producer of that plist).
- **affected_register_entry:** register/shape/fragment (and the authoring note in
  register/boundary/preset-org-to-registration stage 1)
- **recommendation:** No entry change required (the renderer contract is
  unaffected and the plist is the parser's own output shape). Worth surfacing to
  sibling preset authors (`workspace-assistant`) and to a future authoring helper:
  preset role text with column-0 headings should NOT be embedded as a raw Org
  string in a babel block. Either author the sections as a plist (as here) or keep
  the role fragment in a separate `.org`/`.txt` sibling that the parser reads at
  tangle time. The boundary entry's stage-1 phrasing ("static role fragment" in
  the `.org`) reads as if the role must be Org-syntax in the preset source; in
  practice the preset author owns the plist and the parser is optional.

### discovery: read-only is enforced on two independent axes (tools AND scope profile)
- **discovery_id:** system-explorer-readonly-two-axes
- **class:** spec-signal
- **description:** "Read-only" for this preset is upheld twice over: (1) the
  `:tools` allowlist names only non-mutating tools, and (2) `:scope-profile
  "system-explorer"` is the committed profile with empty write/modify/execute
  paths and `cloud.auth_detection: deny`. The legacy `system-explorer.md` granted
  `run_bash_command` (a bash tool whose read-only-ness depends *entirely* on the
  scope profile gating it). I deliberately omitted bash from `:tools` so the
  tool-axis guarantee holds independently of scope evaluation — a narrow,
  documented deviation from the legacy tool set in service of the task's
  "read-only `:tools` only" constraint.
- **affected_register_entry:** register/shape/preset-config-plist (scope-key
  extraction consumers) — no change needed
- **recommendation:** None for the register. Flag for `workspace-assistant`/future
  scope-init work: scope defaults are *stored* in `jf/gptel-preset--scope-defaults`
  at registration but I could not verify here (no full gptel load in the
  directory-scoped suite) that a downstream session-init actually *applies* the
  `system-explorer` profile to gate `run_bash_command`. If a future preset wants
  bash, the tool-axis vs scope-axis split should be made explicit in the spec so
  authors know bash safety rides on scope, not on the tool name.
