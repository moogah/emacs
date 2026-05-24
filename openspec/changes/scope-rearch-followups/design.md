## Context

The gptel scope validation rearch (archived `2026-04-17-scope-system-cleanup`) left three specific residuals uncovered during `/opsx:verify`:

- `config/gptel/scope/scope-shell-tools.org` (around the `request_scope_expansion` `gptel-make-tool` block) — the tool takes `tool_name` as its primary argument and constructs `(list :tool tool_name :resource (car patterns) :reason justification :validation-type 'path :patterns patterns)`. The `:validation-type` is a hardcoded literal `'path`. Anything downstream that branches on `:validation-type` sees `path` even when the LLM asked about `run_bash_command`. **A first fix attempt** (commit `d00949f`) introduced a side-table keyed by tool name (`jf/gptel-scope--scoped-tool-operations` populated by `gptel-make-scoped-tool`) plus a lookup that decoded validation-type from the recorded `:operation`. Author-blind review (`.orchestrator/cycles/post-hoc-2026-05-24/reviews/ws-b-scope-shell-tools-validation-type.md`) found the lookup defended a `'path`-default fallback that contradicted the change's own contract, lacked the regression spec the architecture promised, and — more fundamentally — was an elaborate workaround for the wrong primary argument. **The attempt was reverted** (commit `9ed38c6`) and the disposition reshaped to D1 below.
- `config/gptel/scope/scope-expansion.org:640` — `(message "Cannot add command '%s' to scope — use path-based expansion instead" resource)` is the last expression of the bare-command-name branch. No callback is funcalled. The async wrapper waits forever.
- `config/gptel/preset-registration.org:198,203` — `scope-keys` list still contains `:org-roam-patterns`. `config/gptel/scope-profiles.org:38,109,207,245` still references `:org-roam-patterns` in docstrings and the empty-scope fallback. Neither codepath is exercised by live profile YAML (all five `.yml` files have been pruned), but both leak stale structure into generated artifacts.

The rearch itself is stable. These are edge bugs and dead code — a small, bounded cleanup.

## Goals / Non-Goals

**Goals:**
- `request_scope_expansion` produces a violation-info whose `:validation-type` matches the tool it is about.
- Every branch of `jf/gptel-scope--add-bash-to-scope` funcalls the callback exactly once.
- `:org-roam-patterns` is removed from the preset extraction list and from the empty-scope writer. Legacy presets carrying the key get a warning, not silent retention.
- Regression tests exist (one per bug) close to the code under test.

**Non-Goals:**
- No change to validator semantics, pipeline stages, error codes, or the `gptel-make-scoped-tool` macro.
- No attempt to bring back pattern-based (org-roam) validation. It was removed by design.
- No schema migration tooling. If a user's `scope.yml` already has an `org_roam_patterns` block, it stays there untouched — the validators ignore it and the writer just stops *adding* one.
- No factoring of the three fixes into a shared helper. They are in three different modules and have nothing in common.

## Decisions

### D1. Refactor `request_scope_expansion` to take `operation` as its primary argument

**Choice:** Change the LLM-facing signature from `(tool_name, patterns, justification)` to `(operation, patterns, justification)`. `operation` is a closed-enum string: one of `read`, `write`, `modify`, `execute`, or `bash`. The violation-info constructor derives `:validation-type` directly:

```elisp
(lambda (callback operation patterns justification)
  (when (vectorp patterns)
    (setq patterns (append patterns nil)))
  (let* ((op-sym (intern operation))
         (vtype (if (eq op-sym 'bash) 'bash 'path))
         (violation-info (list :resource (car patterns)
                               :reason justification
                               :validation-type vtype
                               :operation op-sym
                               :patterns patterns)))
    (jf/gptel-scope-prompt-expansion violation-info callback patterns nil)))
```

Operations outside the closed set short-circuit with `:success nil :error "unknown_operation"` before reaching `jf/gptel-scope-prompt-expansion`.

**Alternative considered (and attempted, then reverted):** Introduce a side-table keyed by tool name (`jf/gptel-scope--scoped-tool-operations`) populated by `gptel-make-scoped-tool`, with a lookup helper that decodes validation-type from the recorded `:operation`. Implemented in `d00949f`, reverted in `9ed38c6`. The attempt failed for four reasons: an unknown-tool fallback that defaulted to `'path` (contradicting the change's own contract), no regression spec (architecture promised three `it` blocks; zero existed), a docstring that misstated when the registry was populated, and — most fundamentally — the lookup itself was an elaborate workaround for the wrong primary argument. Every other consumer of `:validation-type` in the codebase (`scope-expansion.el:310,510,546`, `scope-validation.el:914,931`, `interfaces.el:111`) derives it from operation directly via the dispatcher at `scope-validation.el:779-785`. Only `request_scope_expansion` had to recover it from a name string, because only `request_scope_expansion` took a name string as its primary argument.

**Alternative considered:** String-match on tool name. Rejected for the same reason the side-table was — hacking around the wrong primary argument is the wrong direction.

**Alternative considered:** Split `request_scope_expansion` into two tools, `request_scope_expansion_for_path` and `request_scope_expansion_for_bash`. Rejected — doubles the LLM tool-list cost and scales poorly if a third validation-type is introduced. Closer to a workaround for a missing arg than a real shape.

**Why:** Scope is fundamentally `(operation, paths)`. The validator dispatcher already classifies `(operation nil → bash, operation non-nil → path)`. Threading `operation` through `request_scope_expansion`'s own signature aligns this one consumer with how every other consumer in the codebase recovers `:validation-type`. The model already has to know which operation it needs (it's about to invoke the corresponding tool) — supplying the operation explicitly is not new cognitive load. No registry, no lookup, no fallback arm.

**Migration note:** This is a breaking LLM-API change. Model prompts that called `request_scope_expansion` with `tool_name "read_file_in_scope"` need to switch to `operation "read"`. The tool is a transient affordance, not a persisted format, so the migration is "redeploy the system prompt that documents the tool."

**Detailed implementation tracker:** `.tasks/refactor-request-scope-expansion-to-take-operation.md` — captures the proposed signature in full, the open questions (drop `:tool` from violation-info?, `scope-rearch-followups` disposition), the regression spec scenarios to add, and the test file location.

### D2. Bare-command-name refusal: structured denial via callback, not `message`

**Choice:** Replace the `(message ...)` call with a `funcall` that delivers a Deny-shaped JSON payload:

```elisp
(funcall callback
         (json-serialize
          `(:success nil
            :error "command_name_not_expandable"
            :message "Cannot expand scope for command name '%s'. Request expansion for a specific file operation (path) instead."
            :user_denied t)))
```

**Alternative considered:** Keep the refusal at `message` and also funcall a success:nil. Rejected — two side effects for one choice, and the `message` is redundant once the LLM gets structured feedback.

**Alternative considered:** Signal `user-error` so the wrapper's `condition-case` catches it. Rejected — the wrapper catches tool *body* exceptions as `tool_exception`, not scope-handler exceptions. Raising here would leak through as an unhandled signal.

**Why:** The handler's contract (documented in the `scope-expansion` spec under "Callback response shapes") is "every action funcalls the wrapper's async callback with a JSON string." This branch violated that. We make it compliant.

### D3. `:org-roam-patterns` removal is a one-line-per-module edit plus a warning

**Choice:** Four touches:

1. `preset-registration.org:188` — remove `:org-roam-patterns` from `scope-keys`. Add a one-line scan-and-warn for legacy keys: if the preset plist contains any of `(:org-roam-patterns :shell-commands :bash-tools)`, `display-warning` once with the preset name.
2. `scope-profiles.org:400` — remove the hardcoded `:org-roam-patterns` block from the empty-scope fallback plist. The written YAML now carries only `paths`, `cloud`, `security`.
3. `scope-profiles.org:19,205,487` — trim the docstrings/comments that advertise org-roam-patterns as a supported section.
4. `scope-expansion.org:414,415,518,524` and `scope-yaml.org:116` — rewrite the example from `:org-roam-patterns ↔ org_roam_patterns` to `:auth-detection ↔ auth_detection`, matching what the main spec already uses.

**Alternative considered:** Delete *all* legacy key handling, including `:shell-commands` and `:bash-tools`. Rejected for this change — `:bash-tools` still appears in the extraction list and may have downstream consumers. Proposal scope is `:org-roam-patterns` only; the other legacy keys are a separate cleanup.

**Alternative considered:** Leave the docstrings; only fix code. Rejected — docstrings are part of the contract new readers will discover. Stale docstrings are how the drift survived the rearch.

**Why:** Small, surgical edits. Each site is load-bearing for exactly one thing (extraction, writing, or documentation), and the `display-warning` preserves feedback for legacy presets without retention.

### D4. One test file per bug, colocated with the module under test

Already committed to in `architecture.md` per the user's answer. Files:
- `config/gptel/scope/test/tool-wrapper/request-scope-expansion-operation-spec.el`
- `config/gptel/scope/test/expansion/add-bash-to-scope-callback-spec.el`
- `config/gptel/scope/test/yaml/empty-scope-fallback-spec.el`

Each spec file holds exactly the scenarios listed in the architecture's scenario mapping. Shared fixtures come from `config/gptel/scope/test/helpers-spec.el`.

### D5. Treat this change as the pilot for "findings-encapsulated" OpenSpec usage

**Choice:** Keep the change's artifacts faithful to the OpenSpec structure (proposal, specs delta, architecture, design, beads) but treat each section from the perspective of a bug-fix rather than a feature:

- `proposal.md` = findings document (what we discovered, not what we want to add)
- `specs/` = delta MODIFICATIONS that bring the spec into line with the corrected behavior (no ADDED Requirements)
- `architecture.md` = test placement and test conventions, no architectural change
- `design.md` = per-bug fix plan with code anchors
- beads = one per bug (plus one per regression test if we want separate tracking)

**Why:** The user flagged this as an experiment. If the format holds up — if it remains reviewable, archivable, and searchable — it's a useful precedent for future verification-driven cleanup. If it doesn't, the cost is a single change folder we can archive like any other.

## Risks / Trade-offs

- **Risk:** D1 is a breaking change to `request_scope_expansion`'s LLM-facing argument schema (`tool_name` → `operation`). Model prompts that documented the old shape will produce malformed calls until updated.
  **Mitigation:** The tool is a transient LLM affordance, not a persisted format — there is no on-disk data to migrate, only the system prompt(s) that document the tool. Land the docs change in the same commit as the implementation. As a safety net, the tool's body short-circuits with a structured `:success nil :error "unknown_operation"` payload for any operation value outside the closed enum (including the LLM passing a tool-name by mistake), so a stale prompt produces a debuggable error rather than a silent mis-route.

- **Risk:** D2's replacement payload invents a new error code `"command_name_not_expandable"` that is not in the canonical set (`denied-pattern`, `not-in-scope`, `parse_incomplete`, `cloud_auth_denied`, `cloud_provider_denied`).
  **Mitigation:** This code is emitted by the expansion-UI *handler*, not a validator. The canonical set governs validator output. Expansion-UI responses are a separate vocabulary (compare `"no_scope_config"` from the wrapper). Document this in the code comment next to the funcall and in the spec scenario.

- **Risk:** D3's warning fires loudly on legacy presets and annoys users who haven't cleaned up.
  **Mitigation:** Use `display-warning` with `:warning` level (not `:error`), and only once per session per preset. Message explicitly says the key is ignored and names the preset.

- **Risk:** The "findings-as-proposal" format confuses someone reading the archive later, expecting a feature description.
  **Mitigation:** The proposal's opening paragraph is explicit about the format. If it works out, we document the pattern somewhere durable (likely the OpenSpec skill's `opsx:new` flow, not this change).

## Migration Plan

No deployment-side migration. Each fix is a local code edit:

1. Ship the three code edits in any order (they are independent).
2. Ship the regression tests (one bead each) with the corresponding fix.
3. Archive the change with `openspec archive scope-rearch-followups` once all beads are closed. `--skip-specs` is NOT needed — the delta specs in this change should flow into the main specs at archive time (unlike the rearch archive, which was a retrofit).

**Rollback:** `git revert` any single fix commit independently. No data on disk is affected (we just stop writing a section that no validator reads).

## Open Questions

- **Should the legacy-key warning list include `:bash-tools`?** Leaning no (out of scope); revisit as a follow-up cleanup if the field is genuinely dead.
- **Is `jf/gptel-preset--scope-defaults` consulted anywhere we're missing?** The existing extraction emits it; the rearch may have left other consumers. Quick grep before finalizing D3, add a bead to update them if found.
