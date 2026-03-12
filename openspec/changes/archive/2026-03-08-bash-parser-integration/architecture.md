# Architecture

## Components

### Bash Parser Module (config/bash-parser/)
**Responsibility**: Parse bash command strings into structured AST with semantic extraction capabilities.

Graduated from experiments, this is a self-contained module providing:
- **Core Parser** (`core/core.el`) - Tree-sitter parsing, token inventory, parse completeness
- **Plugin System** (`analysis/plugins.el`) - Registry and orchestration for semantic extractors
- **File Operations Plugin** (`plugins/file-ops.el`) - Extract read/write/execute/modify operations
- **Cloud Auth Plugin** (`plugins/cloud-auth.el`) - Detect AWS/GCP/Azure authentication
- **Security Plugin** (`plugins/security.el`) - Pattern-based security rule matching
- **Coverage Analysis** (`analysis/coverage.el`) - Calculate semantic coverage metrics
- **Command Semantics** (`semantics/semantics.el`) - Database of command-to-operation mappings

**Public API**:
- `jf/bash-parse` - Parse command string → AST with tokens
- `jf/bash-extract-semantics` - Extract semantics → plugin results + coverage

### Validation Pipeline (config/gptel/tools/scope-shell-tools.org)
**Responsibility**: Orchestrate multi-stage validation using bash-parser and scope.yml configuration.

**Validation stages**:
1. **Parse** - Call `jf/bash-parse` to get AST
2. **Extract** - Call `jf/bash-extract-semantics` to get file ops, cloud auth, security results
3. **Parse completeness** - Enforce or warn based on `security.enforce_parse_complete`
4. **Pipeline commands** - Validate all commands in chain against bash_tools categories
5. **Command categorization** - Check deny list, read_only, safe_write, dangerous
6. **File path validation** - Match extracted paths against operation-specific scope patterns
7. **Cloud auth policy** - Enforce `cloud.auth_detection` and `allowed_providers`
8. **Execute** - Run command with timeout and output truncation if all validations pass

**New functions**:
- `jf/gptel-scope--validate-command-semantics` - Main validation orchestrator
- `jf/gptel-scope--validate-file-operations` - Operation-specific path matching
- `jf/gptel-scope--validate-pipeline-commands` - Extract and validate all pipeline commands
- `jf/gptel-scope--validate-cloud-auth` - Enforce cloud policy
- `jf/gptel-scope--check-parse-completeness` - Handle incomplete parses

**Removed functions**:
- `jf/gptel-bash--parse-command` - Replaced by bash-parser

### Scope Schema Loader (config/gptel/tools/scope-shell-tools.org)
**Responsibility**: Load and validate scope.yml v4 documents.

**Schema v4 structure**:
```elisp
{:paths {:read [...]
         :write [...]
         :execute [...]  ; NEW
         :modify [...]   ; NEW
         :deny [...]}
 :bash-tools {:categories {:read-only [...]
                           :safe-write [...]
                           :dangerous [...]}
              :deny [...]}
 :cloud {:auth-detection "warn"       ; NEW: "allow", "warn", "deny"
         :allowed-providers [...]}     ; NEW: ["aws", "gcp", "azure"]
 :security {:enforce-parse-complete t ; NEW
            :max-coverage-threshold 0.8}} ; NEW
```

**New functions**:
- `jf/gptel-scope--validate-schema-v4` - Validate loaded YAML structure
- `jf/gptel-scope--load-cloud-config` - Load cloud section with defaults
- `jf/gptel-scope--load-security-config` - Load security section with defaults

## Interfaces

### Bash Parser → Validation Pipeline

**Input to bash-parser**:
```elisp
(jf/bash-parse "cat /workspace/file.txt | grep foo")
```

**Output from bash-parser**:
```elisp
{:command-name "cat"
 :positional-args ("/workspace/file.txt")
 :tokens [...]
 :parse-complete t
 :parse-errors nil
 :pipeline [...]}  ; Contains "grep foo" command
```

**Input to semantic extraction**:
```elisp
(jf/bash-extract-semantics parsed-command)
```

**Output from semantic extraction**:
```elisp
{:parse-complete t
 :parse-errors nil
 :coverage {:total-tokens 8
            :claimed-tokens 8
            :coverage-ratio 1.0}
 :domains {:filesystem [{:operation :read
                         :path "/workspace/file.txt"
                         :absolute-path "/workspace/file.txt"
                         :command-name "cat"}]
           :cloud-auth nil}  ; or provider info if detected
 :plugin-results [...]}
```

### Validation Pipeline → Scope Schema

**Query pattern matching**:
```elisp
(jf/gptel-scope--match-path-pattern "/workspace/file.txt"
                                     '("/workspace/**" "/tmp/**"))
;; => t (if matches any pattern)
```

**Operation-specific validation**:
```elisp
(jf/gptel-scope--validate-operation :read "/workspace/file.txt" scope-config)
;; Checks: paths.read or paths.write (write includes read)
;;         NOT paths.deny

(jf/gptel-scope--validate-operation :execute "/workspace/script.sh" scope-config)
;; Checks: paths.execute only
;;         NOT paths.deny
```

### Error Response Format

**Structured errors** returned to gptel tool system:
```elisp
;; Parse incomplete
{:success nil
 :error "incomplete_parse"
 :parse-errors [...]
 :partial-tokens [...]
 :message "..."}

;; Path out of scope
{:success nil
 :error "path_out_of_scope"
 :path "/etc/passwd"
 :operation :read
 :required-scope "paths.read"
 :allowed-patterns ["/workspace/**"]
 :message "..."
 :suggestion "Use request_scope_expansion to request access"}

;; Pipeline command denied
{:success nil
 :error "command_denied"
 :command "rm"
 :pipeline-position 1
 :full-command "ls | xargs rm"
 :message "..."
 :reason "Command in deny list"}

;; Cloud auth denied
{:success nil
 :error "cloud_auth_denied"
 :provider :aws
 :allowed-providers [:gcp]
 :message "..."
 :suggestion "Add 'aws' to cloud.allowed_providers or set auth_detection to 'allow'"}
```

## Boundaries

### In Scope (This Change)

**Integration layer**:
- Validation pipeline orchestration in scope-shell-tools.org
- Schema v4 loading and validation
- Operation-specific path matching
- Pipeline command extraction and validation
- Cloud auth policy enforcement
- Enhanced error message construction

**Testing**:
- Integration tests for validation pipeline
- Tests for schema v4 loading
- Tests for all validation stages
- Characterization tests for legacy validation logic (before replacement)
- Migration of 683 bash-parser tests

**Migration**:
- Move bash-parser from experiments to production
- Update gptel module loader
- Update example presets to schema v4

### Out of Scope

**Not changing**:
- Bash-parser internals (already tested, proven)
- Scope expansion UI (scope-expansion.org)
- Preset registration system
- Session management

**Future work**:
- Automatic v3 → v4 schema migration tool
- User-facing documentation updates
- Migration guide for existing scope.yml files

### Internal vs External

**Internal** (implementation details):
- Validation stage ordering
- Path resolution algorithm
- Coverage threshold comparison logic
- YAML to Elisp normalization

**External** (stable APIs):
- `run_bash_command` tool signature (unchanged)
- `request_scope_expansion` tool signature (unchanged)
- scope.yml v4 schema (new, but stable once released)
- Bash-parser public API (`jf/bash-parse`, `jf/bash-extract-semantics`)

## Testing Approach

### Test Framework

**ERT (Emacs Lisp Regression Testing)** - Built-in Emacs test framework.

**Note**: While **Buttercup** is now the preferred framework for new test suites (BDD-style with setup/teardown and spies), this change continues using ERT for consistency with existing bash-parser tests. Future test suites should prefer Buttercup.

**Why ERT for this change:**
- Native to Emacs, no external dependencies
- Integrates with existing test infrastructure (`make test`, `./bin/run-tests.sh`)
- Bash-parser already has 683 ERT tests (proven compatibility)
- Supports test organization (`ert-deftest`), assertions (`should`, `should-not`), and test discovery
- No migration cost for existing tests

### Test Organization

**Location**: `config/gptel/tools/test/` (next to scope-shell-tools.org)

**Test files**:
- `test-scope-validation-pipeline.el` - End-to-end validation pipeline tests
- `test-scope-validation-file-paths.el` - Operation-specific path validation tests
- `test-scope-validation-pipelines.el` - Pipeline command extraction and validation tests
- `test-scope-validation-cloud-auth.el` - Cloud authentication policy tests
- `test-scope-schema-v4.el` - Schema v4 loading and validation tests
- `test-scope-shell-tools-integration.el` - Integration tests with bash-parser
- `test-scope-shell-tools-legacy.el` - Characterization tests for existing validation logic

**Bash-parser tests location**: `config/bash-parser/test/` (moved from experiments, structure preserved)

### Naming Conventions

**Test files**: `test-<module>-<capability>.el` format
- Example: `test-scope-validation-file-paths.el`

**Test functions**: `test-<functionality>-<scenario>` format
- Example: `test-file-path-validation-read-operation-in-scope`
- Example: `test-pipeline-validation-dangerous-command-in-chain`

**Scenario mapping**: Test function names reference spec scenarios
- Use scenario slug in test name where possible
- Add comment linking to spec: `; Scenario: specs/scope-validation-file-paths/spec.md § "Extract read operation"`

### Running Tests

**Primary method**: Makefile-based test runner

```bash
# All gptel tests
make test-gptel

# Specific test file
make test-file FILE=config/gptel/tools/test/test-scope-validation-pipeline.el

# Pattern matching
make test-pattern PATTERN='^test-file-path-'

# All bash-parser tests (after migration)
make test-bash-parser
```

**Alternative method**: Shell script wrapper

```bash
# All gptel tools tests
./bin/run-tests.sh -d config/gptel/tools/test

# Specific capability
./bin/run-tests.sh -p '^test-cloud-auth-'

# With snapshot for regression tracking
./bin/run-tests.sh -d config/gptel/tools/test --snapshot
```

**Continuous integration**: All tests run on push via GitHub Actions

### Test Patterns

#### Test Data Setup

**Inline test data** for clarity:
```elisp
(ert-deftest test-file-path-validation-read-in-scope ()
  "Scenario: Read operation matches paths.read"
  (let ((scope-config '(:paths (:read ("/workspace/**") "/tmp/**")
                                :deny ("/etc/**")))
        (file-op '(:operation :read
                   :path "/workspace/file.txt"
                   :absolute-path "/workspace/file.txt")))
    (should (jf/gptel-scope--validate-file-operation file-op scope-config))))
```

**Fixture scope.yml documents** for complex scenarios:
```elisp
(defconst test-scope-v4-full
  '(:paths (:read ("/workspace/**")
            :write ("/tmp/**")
            :execute ("/workspace/scripts/**")
            :modify ("/workspace/config/**")
            :deny ("/etc/**" "~/.ssh/**"))
    :bash-tools (:categories (:read-only ("ls" "cat" "grep")
                              :safe-write ("mkdir" "touch")
                              :dangerous ())
                 :deny ("rm" "sudo" "chmod"))
    :cloud (:auth-detection "warn"
            :allowed-providers ("aws" "gcp"))
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8)))
```

#### Mocking/Stubbing

**Bash-parser calls**: Use real bash-parser (integration tests), not mocks
- Bash-parser is proven with 683 tests, safe to depend on
- Integration tests validate actual parse → validate flow

**File system operations**: Mock `file-truename` and `expand-file-name` for path resolution tests
```elisp
(cl-letf (((symbol-function 'file-truename) (lambda (path) path))
          ((symbol-function 'expand-file-name) (lambda (path dir) (concat dir "/" path))))
  ;; Test path resolution logic
  ...)
```

**Scope.yml loading**: Provide in-memory scope config, don't load from disk
```elisp
;; Don't: (jf/gptel-scope-load-config "/path/to/scope.yml")
;; Do:    (let ((scope-config '(:paths ...))) ...)
```

#### Common Helpers

**Location**: `config/gptel/tools/test/test-helper.el`

**Parse and validate helper**:
```elisp
(defun test-validate-command (command directory scope-config)
  "Parse COMMAND and run through validation pipeline with SCOPE-CONFIG."
  (let ((parsed (jf/bash-parse command))
        (semantics (jf/bash-extract-semantics (jf/bash-parse command))))
    (jf/gptel-scope--validate-command-semantics
      command directory scope-config parsed semantics)))
```

**Scope config builder**:
```elisp
(defun test-make-scope-config (&rest overrides)
  "Build scope config with defaults and OVERRIDES."
  (let ((defaults '(:paths (:read () :write () :execute () :modify () :deny ())
                    :bash-tools (:categories (:read-only () :safe-write () :dangerous ())
                                 :deny ())
                    :cloud (:auth-detection "warn" :allowed-providers ())
                    :security (:enforce-parse-complete t :max-coverage-threshold 0.8))))
    (jf/merge-plist defaults overrides)))
```

**Assertion helpers**:
```elisp
(defun should-validate (command directory scope-config)
  "Assert COMMAND validates successfully."
  (let ((result (test-validate-command command directory scope-config)))
    (should (plist-get result :success))))

(defun should-reject (command directory scope-config expected-error)
  "Assert COMMAND fails with EXPECTED-ERROR."
  (let ((result (test-validate-command command directory scope-config)))
    (should-not (plist-get result :success))
    (should (equal expected-error (plist-get result :error)))))
```

### Scenario Mapping

**100% scenario coverage goal**: Every scenario in specs maps to at least one test.

**Mapping strategy**:

1. **Direct mapping** (most common):
   - Spec: "Scenario: Extract read operation" → Test: `test-file-ops-extract-read-operation`
   - One test per scenario, exact scenario name in test function

2. **Grouped scenarios** (related edge cases):
   - Spec: Multiple "Extract X operation" scenarios → Test: `test-file-ops-extract-all-operation-types`
   - One parameterized test covering multiple related scenarios
   - Comment documents which scenarios covered

3. **Integration scenarios** (multi-component):
   - Spec: "Pipeline with file paths validated together" → Test: `test-integration-pipeline-and-file-paths`
   - Combines validation stages, tests end-to-end flow

4. **Characterization tests** (legacy code):
   - Before refactoring existing validation logic, capture current behavior
   - Test: `test-legacy-directory-validation-behavior`
   - Ensures refactored code maintains compatibility

**Test organization by spec**:

```
config/gptel/tools/test/
├── test-scope-validation-file-paths.el
│   ├── test-file-ops-extraction-*        # specs/scope-validation-file-paths scenarios 1-5
│   ├── test-path-resolution-*            # scenarios 6-9
│   ├── test-operation-specific-*         # scenarios 10-20
│   └── test-deny-patterns-*              # scenarios 21-23
├── test-scope-validation-pipelines.el
│   ├── test-pipeline-extraction-*        # specs/scope-validation-pipelines scenarios 1-5
│   ├── test-pipeline-validation-*        # scenarios 6-10
│   └── test-pipeline-bypass-closed-*     # scenarios 11-13
├── test-scope-validation-cloud-auth.el
│   ├── test-cloud-detection-*            # specs/scope-validation-cloud-auth scenarios 1-5
│   ├── test-cloud-modes-*                # scenarios 6-15
│   └── test-cloud-provider-filtering-*   # scenarios 16-20
├── test-scope-schema-v4.el
│   ├── test-schema-loading-*             # specs/scope-schema-v4 scenarios 1-10
│   ├── test-schema-validation-*          # scenarios 11-15
│   └── test-schema-defaults-*            # scenarios 16-20
└── test-scope-shell-tools-integration.el
    ├── test-integration-full-pipeline-*  # End-to-end validation flows
    └── test-integration-error-messages-* # Error format validation
```

**Scenario coverage tracking**:
- Each test includes comment with spec reference
- Example: `; Scenario: specs/scope-validation-file-paths/spec.md § "Extract read operation"`
- Enables grep-based coverage verification: `grep -r "Scenario: specs/" config/gptel/tools/test/`

### Characterization Testing Strategy

**Goal**: Capture current validation behavior before refactoring to ensure compatibility.

**Approach**:
1. **Phase 1** (before refactoring): Write characterization tests
   - Test current regex-based validation
   - Test current directory-only validation
   - Test current pipeline bypass behavior
   - Capture what the code DOES (not what it SHOULD do)

2. **Phase 2** (during refactoring): Run characterization tests
   - Tests fail when behavior changes
   - Intentional changes: Update tests to new behavior
   - Unintentional changes: Fix implementation

3. **Phase 3** (after refactoring): Archive characterization tests
   - Keep as regression suite or remove if new tests cover
   - Document what behavior changed and why

**Example characterization test**:
```elisp
(ert-deftest test-legacy-pipeline-bypass ()
  "Characterization: Current implementation only validates base command in pipeline.
   This behavior is INTENTIONALLY CHANGED by bash-parser integration.
   Test documents OLD behavior for comparison."
  :expected-result :failed  ; Will fail after refactoring
  (let ((scope-config test-scope-v3-config))
    ;; Old behavior: "ls | xargs rm" allowed (only "ls" validated)
    (should-validate "ls | xargs rm" "/workspace" scope-config)))
```

After refactoring, update test:
```elisp
(ert-deftest test-pipeline-full-validation ()
  "New behavior: All pipeline commands validated.
   Replaces test-legacy-pipeline-bypass (characterization test)."
  (let ((scope-config test-scope-v4-config))
    ;; New behavior: "ls | xargs rm" rejected (rm in deny list)
    (should-reject "ls | xargs rm" "/workspace" scope-config "command_denied")))
```

## Dependencies

### Internal Dependencies

**Bash-parser module** (`config/bash-parser/`):
- All core, plugin, and analysis modules
- Public API: `jf/bash-parse`, `jf/bash-extract-semantics`
- Version: Stable (683 passing tests)

**Scope core** (`config/gptel/tools/scope-core.el`):
- Scope loading infrastructure
- Pattern matching utilities
- May need extension for v4 schema fields

**GPtel core** (`config/gptel/gptel.el`):
- Module loading system
- Tool registration system

### External Dependencies

**Built-in Emacs libraries**:
- `treesit` (Emacs 29+) - Tree-sitter parsing backend
- `cl-lib` - Common Lisp extensions
- `ert` - Testing framework

**No new external dependencies** - bash-parser already requires tree-sitter.

### Test Dependencies

**ERT** (built-in) - Test framework

**Test infrastructure**:
- `Makefile` - Test runner and Emacs invocation
- `bin/run-tests.sh` - CLI wrapper for test execution
- `config/core/testing.el` - Test discovery and batch execution

## Constraints

### Performance Constraints

**Tree-sitter overhead**: Parsing adds ~10-50ms per command
- Acceptable: Commands execute infrequently (user-driven)
- Mitigation: No caching needed, parsing is fast enough

**Plugin execution**: Linear with plugin count (currently 3: file-ops, cloud-auth, security)
- Acceptable: <5ms total for plugin execution
- Scalability: System supports up to ~10 plugins before optimization needed

**Test suite size**: 683 bash-parser tests + ~200 new integration tests = ~883 tests
- Target: Full suite runs in <60 seconds
- Mitigation: Parallel test execution where possible

### Compatibility Constraints

**Breaking change**: No backward compatibility with scope.yml v3 schema
- Users MUST manually update scope.yml files
- No automatic migration provided
- Clear migration guide required in documentation

**Tree-sitter requirement**: Emacs 29+ only
- This is already a requirement for bash-parser
- Not a new constraint

**Module load order**: bash-parser must load before scope-shell-tools
- Enforced in `config/gptel/gptel.org` module loader
- Tests verify load order

### Technical Constraints

**Emacs Lisp limitations**:
- No true concurrency - validation stages run sequentially
- Acceptable: Validation is fast enough (~50ms total)

**YAML parsing**: Must handle snake_case → kebab-case normalization
- YAML convention: `auth_detection`, `allowed_providers`
- Elisp convention: `:auth-detection`, `:allowed-providers`
- Handled by scope-core YAML loader

**Path resolution**: Must handle symlinks, relative paths, shell variables
- `file-truename` resolves symlinks
- `expand-file-name` resolves relative paths
- Shell variables ($VAR) not expanded (security boundary - only literal paths validated)

### Testability Constraints

**Integration tests require bash-parser**: Cannot mock bash-parser in integration tests
- Acceptable: Bash-parser is stable and fast
- Unit tests can mock if needed for edge cases

**Scope.yml loading in tests**: Tests use in-memory scope config, not disk files
- Faster test execution
- No file system side effects
- Easier to test edge cases

**Coverage target**: 100% of spec scenarios = ~135+ tests
- Achievable with test helpers and parameterization
- Prioritize critical paths if time-constrained
