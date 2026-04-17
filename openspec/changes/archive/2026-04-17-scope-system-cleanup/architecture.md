## Components

### scope-yaml.el — Configuration Boundary

**Responsibilities:**
- Parse scope.yml to normalized plist (snake_case → kebab-case, vectors → lists, YAML booleans → elisp booleans)
- Write modified plists back to YAML (kebab-case → snake_case)
- **NEW**: Load schema with safe defaults merging (absorbs `load-schema` from scope-shell-tools.el)
- Reject malformed configs (e.g., deprecated `bash_tools.categories` section)

**No dependencies on other scope modules.** Loaded early.

**Schema defaults:**
```elisp
(:paths (:read () :write () :execute () :modify () :deny ())
 :cloud (:auth-detection "warn")
 :security (:enforce-parse-complete t :max-coverage-threshold 0.8))
```

### scope-validation.el — All Validation Logic

**Responsibilities:**
- `validate-path-operation(path operation config)` — shared core function used by both filesystem and bash validation
- `validate-filesystem-tool(tool-name operation args config metadata)` — thin wrapper: extract path from args, call validate-path-operation
- `validate-bash-command(tool-name args config)` — seven-stage pipeline, calls validate-path-operation at stage 6
- Single `glob-to-regex` implementation and `path-matches-patterns` function
- Allow-once mechanism (check, consume, clear)
- Error code constants and violation-info building
- `build-violation-info` handles ALL canonical error codes

**Dependencies:** scope-yaml (for schema loading), bash-parser (for AST extraction in bash pipeline)

**Internal structure (sections within the single file):**
1. Error codes and constants
2. Glob pattern matching
3. Allow-once mechanism
4. Path operation validation (shared core)
5. Filesystem tool validation (thin layer over shared core)
6. Bash validation pipeline (7 stages, stage 6 calls shared core)
7. Violation-info building and error formatting

### scope-tool-wrapper.el — Thin Macro + Dispatch

**Responsibilities:**
- `gptel-make-scoped-tool` macro definition
- Load config (via scope-yaml)
- Normalize arguments (vectors → lists)
- Gather metadata for path tools (via scope-metadata)
- Call validation module entry point
- Handle result: execute tool body on success, format error or trigger expansion on failure

**Does NOT contain:** Validation logic, error codes, glob matching, allow-once checking

**Dependencies:** scope-yaml, scope-validation, scope-metadata, scope-expansion (for async denial handling)

### scope-expansion.el — Expansion UI

**Responsibilities:**
- Transient menu (deny / allow-once / add to scope / edit manually)
- Queue management for multiple async denials
- Scope.yml updates: `add-path-to-scope` (routes to correct paths section by operation type)
- **REMOVED**: `add-pattern-to-scope` (org-roam), `add-bash-to-scope` categories logic

**Dependencies:** scope-yaml (for writing updates), scope-validation (for allow-once list manipulation)

### scope-metadata.el — File Metadata (unchanged)

**Responsibilities:** Gather file metadata (path, existence, git status, type) before validation.

### interfaces.el — Executable Contracts (updated)

**Responsibilities:**
- Canonical error code vocabulary
- Error code → resource field mapping
- Validation result shape validator
- Violation-info shape validator
- Allow-once resource format specification
- Glob test cases (shared fixture for all validators)
- **UPDATED**: Remove pattern validation type, update component diagram

### scope-filesystem-tools.el — Tool Definitions (updated)

**Responsibilities:** Define read_file, write_file_in_scope, edit_file_in_scope using the macro with `:operation` declaration.

**REMOVED:** scope-org-roam-tools.el, scope-commands.el

## Interfaces

### Config Loading Interface

```
scope-yaml.el exports:
  jf/gptel-scope-yaml--load-schema(filepath) → normalized plist with defaults
  jf/gptel-scope-yaml--parse-file(filepath) → raw normalized plist
  jf/gptel-scope-yaml--write-update(filepath section key value) → writes YAML
```

### Validation Entry Points

```
scope-validation.el exports:

  ;; Shared core — called by both filesystem and bash validators
  jf/gptel-scope--validate-path-operation(path operation config)
    → (:allowed t) | (:allowed nil :error CODE :resource PATH :message STR ...)
    operation: read | write | execute | modify

  ;; Filesystem tool entry point
  jf/gptel-scope--validate-filesystem-tool(tool-name operation args config metadata)
    → validation-result
    Extracts path from args, calls validate-path-operation

  ;; Bash tool entry point (7-stage pipeline)
  jf/gptel-scope--validate-bash-command(tool-name args config)
    → validation-result
    Stages 1-5,7 are bash-specific; stage 6 calls validate-path-operation per extracted op

  ;; Allow-once
  jf/gptel-scope--check-allow-once(tool-name resource) → t | nil (consumes on match)
  jf/gptel-scope--grant-allow-once(tool-name resource) → adds to buffer-local list
  jf/gptel-scope--clear-allow-once() → clears all

  ;; Error transformation
  jf/gptel-scope--build-violation-info(tool-name args validation-result validation-type)
    → (:tool STR :resource STR :reason STR :validation-type SYMBOL)
```

### Macro Interface

```
scope-tool-wrapper.el exports:

  (gptel-make-scoped-tool NAME
    :operation read|write|execute|modify  ; declares operation type
    :async t|nil                          ; triggers expansion UI on denial
    :meta t|nil                           ; bypasses validation entirely
    BODY)
```

### Data Flow

```
Tool invocation
  │
  ▼
scope-tool-wrapper (macro)
  ├── load config (scope-yaml)
  ├── normalize args
  ├── gather metadata (scope-metadata, path tools only)
  ├── check allow-once (scope-validation)
  │   └── allowed? → execute tool body
  ├── route by tool type:
  │   ├── path tool → validate-filesystem-tool (scope-validation)
  │   │                └── validate-path-operation (shared core)
  │   ├── bash tool → validate-bash-command (scope-validation)
  │   │                └── 7-stage pipeline
  │   │                    └── stage 6: validate-path-operation (shared core)
  │   └── meta tool → execute immediately
  └── handle result:
      ├── allowed → execute tool body
      └── denied:
          ├── sync → format-tool-error → return to LLM
          └── async → build-violation-info → expansion UI (scope-expansion)
```

## Boundaries

### In Scope

- Restructure scope-core.el into scope-tool-wrapper.el + scope-validation.el
- Merge scope-shell-tools.el bash validation into scope-validation.el
- Unify path validation (single validate-path-operation, single glob-to-regex)
- Move schema loading into scope-yaml.el
- Remove org-roam tools, pattern validation, tool categorization const, scope-commands
- Fix known divergences (error codes, violation-info gaps, allow-once double-check, categories write bug)
- Update interfaces.el contracts
- Update expansion.el (remove org-roam/categories handling)
- Update filesystem-tools.el (macro call with :operation)

### Out of Scope

- Seven-stage bash pipeline logic (preserved as-is, just relocated)
- scope.yml format changes (purely internal restructuring)
- bash-parser integration (unchanged)
- Expansion UI transient menu design (unchanged, just cleaned up)
- New validation features or operation types

### External Dependencies

- `bash-parser` — tree-sitter-based bash parser for AST extraction
- `yaml` — Emacs YAML parsing library
- `gptel` — tool registration via `gptel-make-tool`
- `transient` — expansion UI menu framework

## Testing Approach

### Test Framework

**Buttercup** (BDD) for all new test suites. Existing contract validators in `interfaces.el` remain as importable `defconst` + validator functions — these are structural contracts, not test cases.

### Test Organization

```
config/gptel/scope/test/
├── validation/
│   ├── path-operation-spec.el      — shared core validator
│   ├── filesystem-spec.el          — filesystem tool validation
│   ├── bash-pipeline-spec.el       — 7-stage pipeline (stage 6 uses shared core)
│   ├── glob-matching-spec.el       — single glob implementation
│   ├── allow-once-spec.el          — allow-once mechanism
│   ├── error-codes-spec.el         — error code consistency across validators
│   └── violation-info-spec.el      — build-violation-info for all error codes
├── tool-wrapper/
│   ├── macro-spec.el               — macro dispatch, arg normalization
│   ├── config-loading-spec.el      — config loading delegation to scope-yaml
│   └── routing-spec.el             — routing to correct validator
├── expansion/
│   ├── expansion-ui-spec.el        — transient menu actions
│   └── scope-update-spec.el        — scope.yml write operations
├── helpers-spec.el                 — shared test infrastructure
└── interfaces.el                   — executable contracts (defconst, validators, glob test cases)
```

### Naming Conventions

- Test files: `*-spec.el` (Buttercup)
- Describe blocks: `(describe "scope-validation: validate-path-operation" ...)`
- It blocks: `(it "allows read when path matches paths.read" ...)`
- Prefix describes with module name for grep-ability

### Running Tests

```bash
# All scope tests
./bin/run-tests.sh -d config/gptel/scope

# Validation module tests only
./bin/run-tests.sh -d config/gptel/scope/test/validation

# Tool wrapper tests only
./bin/run-tests.sh -d config/gptel/scope/test/tool-wrapper

# Via make
make test-buttercup-directory DIR=config/gptel/scope
```

### Test Patterns

**Mocking:** `cl-letf` for function-level mocking scoped to individual tests. Spy-on for call verification.

```elisp
;; Mock scope-yaml to return test config without touching filesystem
(before-each
  (spy-on 'jf/gptel-scope-yaml--load-schema
          :and-return-value test-config))
```

**Shared fixtures from interfaces.el:**
```elisp
(require 'scope-interfaces)

(describe "glob matching"
  (dolist (case scope/interface--glob-test-cases)
    (let ((pattern (nth 0 case))
          (path (nth 1 case))
          (expected (nth 2 case)))
      (it (format "pattern %s %s path %s"
                   pattern (if expected "matches" "rejects") path)
        (expect (jf/gptel-scope--path-matches-pattern path pattern)
                :to-equal expected)))))
```

**Contract validation:**
```elisp
;; Verify validator output matches interface contract
(it "returns well-formed validation result"
  (let ((result (jf/gptel-scope--validate-path-operation path op config)))
    (expect (scope/interface--validate-validation-result result) :to-be nil)))
```

### Scenario Mapping

Each spec scenario maps to one or more `(it ...)` blocks:

| Spec Requirement | Test File | Pattern |
|---|---|---|
| Single path operation validator scenarios | validation/path-operation-spec.el | One `it` per scenario |
| Single glob implementation scenarios | validation/glob-matching-spec.el | Data-driven from interface--glob-test-cases |
| Consolidated error codes scenarios | validation/error-codes-spec.el | Cross-validator consistency checks |
| Allow-once mechanism scenarios | validation/allow-once-spec.el | One `it` per scenario |
| Bash pipeline preserved scenarios | validation/bash-pipeline-spec.el | Integration tests calling pipeline |
| Thin wrapper macro scenarios | tool-wrapper/macro-spec.el | Mock validation module |
| No central categorization scenarios | tool-wrapper/routing-spec.el | Verify no lookup table |

**Key integration test:** Both filesystem and bash validators produce identical results for the same path+operation+config, verifying no drift between the two entry points.

## Dependencies

- **bash-parser** (config/bash-parser/) — tree-sitter-based bash parser. Used by stage 1-4 of bash pipeline.
- **yaml.el** — YAML parsing. Used by scope-yaml.el.
- **gptel** — Tool registration framework. gptel-make-tool creates tools that the macro wraps.
- **transient.el** — UI framework for expansion menu.
- **cl-lib** — Common Lisp compatibility (cl-letf, cl-loop, cl-block).
- **scope-metadata.el** — File metadata gathering (unchanged, consumed by wrapper).

## Constraints

- **No scope.yml format changes** — this is internal restructuring only. Existing scope.yml files must continue to work.
- **Bash pipeline preserved** — the 7-stage pipeline is working and tested. It relocates into scope-validation.el but its logic is unchanged.
- **Load order** — scope-yaml must load before scope-validation (needs schema loading). scope-validation must load before scope-tool-wrapper (needs validators). scope-tool-wrapper must load before tool definition files.
- **Buffer-local state** — allow-once list and branch-dir are buffer-local. This constraint is preserved.
- **No caching** — scope.yml is read fresh every tool call. This is by design.
