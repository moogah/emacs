## Context

The gptel scope system validates tool invocations against a scope.yml configuration. It has grown across three development cycles (v2 path validation, v3 command categories, v4 bash-parser semantic analysis) without architectural reconciliation. The result is a monolithic scope-core.el (~1100 lines) that mixes macro definition, dispatch logic, path validation, pattern validation, glob matching, allow-once mechanism, error formatting, and a 40+ entry tool categorization const. A second file (scope-shell-tools.el, ~900 lines) duplicates some of scope-core's functionality (glob-to-regex, path matching) and has a circular dependency back to it.

Five concrete bugs exist from this drift (documented in interfaces.org): divergent error codes between path validators, missing violation-info branches, duplicate glob implementations, invalid config writes, and double allow-once checking.

The core insight driving this cleanup: the bash pipeline's stage 6 (file operation validation) and the filesystem tool validator both answer the same question — "is this path allowed for this operation type?" — but were implemented independently with different error codes and different glob matchers.

## Goals / Non-Goals

**Goals:**
- Single `validate-path-operation` function used by all path checking (filesystem tools and bash pipeline stage 6)
- Single glob-to-regex implementation
- Consistent error codes across all validators
- Thin tool wrapper macro with no validation logic
- Tools declare their own operation type (no central categorization const)
- Schema loading consolidated in scope-yaml.el
- Remove dead code (org-roam tools, pattern validation, scope-commands, categories)
- Fix all five documented divergences

**Non-Goals:**
- Changing the 7-stage bash pipeline logic (relocate only)
- Changing scope.yml format
- Adding new validation capabilities
- Changing the expansion UI behavior
- Migrating existing tests to new framework (update paths/imports only)

## Decisions

### Decision 1: Merge scope-shell-tools.el into scope-validation.el rather than keeping separate

**Choice:** Single validation module containing both path and bash validation.

**Rationale:** The circular dependency between scope-core and scope-shell-tools exists because both need path matching. By putting all validation in one module, the shared `validate-path-operation` function is a simple internal call, not a cross-module dependency. The bash pipeline is a consumer of path validation, not an independent system.

**Alternative considered:** Keep bash validation in its own file, import shared path functions. Rejected because it perpetuates the current split that caused the divergence. The bash pipeline is ~400 lines of sequential stage functions — large but cohesive within a single file.

### Decision 2: Operation type declared at macro call site via :operation keyword

**Choice:** Each tool definition includes `:operation read|write|execute|modify` in its `gptel-make-scoped-tool` call.

**Rationale:** The central `tool-categories` const requires updating a separate file when adding any tool. With declaration at the call site, adding a new tool is self-contained — you define the tool and its operation type in the same place.

**Implementation:**
```elisp
;; In scope-filesystem-tools.el
(gptel-make-scoped-tool read_file
  :operation read
  :async t
  ...body...)

(gptel-make-scoped-tool write_file_in_scope
  :operation write
  :async t
  ...body...)
```

The macro uses `:operation` to determine which validation path to take and what operation type to pass to `validate-path-operation`.

**Alternative considered:** Infer operation from tool name convention (e.g., tools ending in `_in_scope` are writes). Rejected — fragile, doesn't extend to execute/modify, and makes the contract implicit.

### Decision 3: Bash tool routing by tool name, not lookup table

**Choice:** The macro recognizes `run_bash_command` by name and routes to the bash pipeline. All other tools with `:operation` go to the filesystem path validator.

**Rationale:** There is exactly one bash tool (`run_bash_command`). A general-purpose routing mechanism for a single special case adds unnecessary abstraction. If a second bash tool is ever needed, the routing can be generalized then.

**Implementation in macro:**
```elisp
(if (string= tool-name "run_bash_command")
    (jf/gptel-scope--validate-bash-command tool-name args config)
  (jf/gptel-scope--validate-filesystem-tool tool-name operation args config metadata))
```

**Alternative considered:** Validation type keyword (`:validation bash|path`) alongside `:operation`. Rejected — reintroduces a second dimension of categorization that was the problem with the original design. The operation type is sufficient.

### Decision 4: Allow-once check moves to validation module entry

**Choice:** Allow-once is checked at the top of `validate-filesystem-tool` and `validate-bash-command`, not in the macro.

**Rationale:** The current bug is that allow-once is checked in both the macro AND the validator. Moving it into the validation module means the macro simply calls the validator and handles the result — it doesn't need to know about allow-once at all. The validator is responsible for all permission decisions.

**Exception:** Allow-once must still bypass missing config. The validation module checks allow-once before attempting config-dependent validation.

### Decision 5: Schema loading moves to scope-yaml.el

**Choice:** `jf/gptel-scope-yaml--load-schema` in scope-yaml.el replaces `jf/gptel-bash--load-schema` from scope-shell-tools.el.

**Rationale:** scope-yaml.el is already the YAML boundary module. Schema loading (read file, parse YAML, normalize keys, merge defaults, reject malformed) is YAML boundary work. Having it in scope-shell-tools was a historical accident — the bash pipeline needed defaults and built its own loader.

**The defaults plist** (currently in scope-shell-tools.el) moves to scope-yaml.el:
```elisp
(defconst jf/gptel-scope-yaml--schema-defaults
  '(:paths (:read () :write () :execute () :modify () :deny ())
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
```

### Decision 6: Use the bash pipeline's glob-to-regex (character-by-character parser)

**Choice:** Keep the more robust character-by-character `glob-to-regex` from scope-shell-tools.el. Delete the simpler regex-replacement version from scope-core.el.

**Rationale:** The character-by-character parser handles edge cases (escaped characters, character classes) that the simple version doesn't. Since both validators will share one implementation, we want the more correct one.

### Decision 7: Phased implementation order

**Phase 1 — Foundation:** Create scope-validation.el with validate-path-operation and glob matching. Write tests against interface contracts. No existing code changes yet.

**Phase 2 — Wire up filesystem tools:** Create scope-tool-wrapper.el with the macro. Update scope-filesystem-tools.el to use `:operation`. Remove scope-core.el's path validation. Tests verify filesystem tools work through new path.

**Phase 3 — Migrate bash pipeline:** Move 7-stage pipeline into scope-validation.el. Replace stage 6 with call to validate-path-operation. Remove scope-shell-tools.el. Tests verify bash validation works through shared path.

**Phase 4 — Clean up:** Remove org-roam tools, scope-commands, tool categorization const. Update expansion.el. Update interfaces.el. Delete old files.

**Rationale:** Each phase produces a working system. Phase 1-2 can be verified independently of bash changes. Phase 3 is the riskiest (moving the pipeline) but by then the shared core is proven.

## Risks / Trade-offs

**[Risk] Large file size for scope-validation.el (~800-1000 lines)**
→ Mitigation: The file is organized into clear sections (see architecture). It's large but cohesive — all validation logic in one place is better than split across two files with circular deps and duplicate functions. The org source file has sections with collapsible headings.

**[Risk] Regression during bash pipeline relocation (Phase 3)**
→ Mitigation: The 7-stage pipeline logic is unchanged. Only stage 6 changes (calls shared function instead of local one). Existing bash pipeline tests verify behavior is preserved. The interface contract tests (glob test cases, error code validation) catch any divergence.

**[Risk] Breaking existing test imports**
→ Mitigation: Tests are reorganized but import paths are explicit `(require 'scope-validation)` etc. Old requires will fail loudly at load time, not silently. Run full test suite after each phase.

**[Risk] Expansion UI assumes pattern validation type exists**
→ Mitigation: Phase 4 removes pattern branches from expansion.el. The transient menu only needs to handle path and bash validation types after cleanup.

## Migration Plan

### Implementation Phases

**Phase 1:** scope-validation.el + tests (no existing code changes)
**Phase 2:** scope-tool-wrapper.el + filesystem tools migration
**Phase 3:** Bash pipeline migration into scope-validation.el
**Phase 4:** Remove dead code, update expansion and interfaces

### Rollback

Each phase is a separate commit. Rollback is `git revert` of the phase commit. Since scope.yml format is unchanged, no user-facing migration is needed.

## Open Questions

1. **Should `format-tool-error` live in the validation module or the wrapper?** It transforms validation results into LLM-facing JSON. Argument for validation: it knows error codes. Argument for wrapper: it's presentation, not validation. Current leaning: validation module, since it needs the error code → resource field mapping.

2. **Should the wrapper handle the allow-once bypass of missing config, or should that be in the validation module?** Current leaning: validation module — the wrapper should not need to know about allow-once at all. The validation module checks allow-once first, and if it matches, returns (:allowed t) without ever loading config.
