## Components

### Core Parser (bash-parser-core)
- **Responsibility**: Parse bash commands into structured representation with token inventory
- **Key Changes**: Add token tracking (ID, type, value, position), add parse-complete flag
- **Output**: Enhanced parsed command structure with :tokens list and :parse-complete flag

### Token System (bash-parser-tokens)
- **Responsibility**: Define token types, track token inventory, provide token utilities
- **Not a separate module**: Token handling is integrated into parser core
- **Token Types**: :command-name, :positional-arg, :flag, :flag-arg, :command-substitution, :separator, :redirection, :pipe

### Plugin Registry (bash-parser-plugins)
- **Responsibility**: Register plugins, orchestrate plugin execution, aggregate results
- **Key Functions**:
  - `jf/bash-register-plugin` - Register new plugin
  - `jf/bash-extract-semantics` - Main entry point, runs all applicable plugins
  - `jf/bash--get-applicable-plugins` - Filter plugins by predicates
- **State**: `jf/bash-semantic-plugins` - List of registered plugins with priorities

### Coverage System (bash-parser-coverage)
- **Responsibility**: Calculate semantic coverage, identify unclaimed tokens, provide visualization
- **Key Functions**:
  - `jf/bash-calculate-coverage` - Compute coverage metrics from tokens and claimed IDs
  - `jf/bash-visualize-coverage` - Human-readable coverage display
- **Coverage Metrics**: Total tokens, claimed tokens, coverage ratio, unclaimed tokens, coverage by type

### Filesystem Plugin (bash-parser-file-ops refactored)
- **Responsibility**: Extract filesystem operations, claim relevant tokens
- **Refactoring**: Wrap existing extraction logic in plugin protocol
- **Key Functions**:
  - `jf/bash-plugin-filesystem` - Plugin implementation
  - `jf/bash-extract-file-operations` - Backward-compatible wrapper (DEPRECATED)

### Cloud Auth Plugin (bash-parser-cloud-auth)
- **Responsibility**: Extract cloud authentication scope (AWS, GCP, Azure)
- **Pattern Database**: `jf/bash-cloud-auth-patterns` - Command patterns for auth detection
- **Key Functions**:
  - `jf/bash-plugin-cloud-auth` - Plugin implementation
  - Pattern matching for aws-vault, aws, gcloud, az commands

## Interfaces

### Plugin Protocol
```elisp
;; Plugin registration
(jf/bash-register-plugin
  :name 'plugin-name
  :priority 100
  :extractor #'plugin-function
  :predicates (list #'predicate-fn))

;; Plugin function signature
(defun plugin-function (parsed-command)
  "PARSED-COMMAND: Enhanced parse result with :tokens and :parse-complete
   Returns: jf/bash-plugin-result struct or nil"
  ...)

;; Plugin result structure
(make-jf/bash-plugin-result
  :domain :keyword              ; :filesystem, :cloud-auth, etc.
  :operations (list ...)        ; Domain-specific operation plists
  :claimed-token-ids (list ...) ; Token IDs understood by plugin
  :metadata (list ...))         ; Domain-specific metadata
```

### Main API
```elisp
;; New unified extraction API
(jf/bash-extract-semantics parsed-command)
  => (:parse-complete t/nil
      :parse-errors (...)
      :coverage (:total-tokens N :claimed-tokens M :coverage-ratio R ...)
      :domains ((:filesystem . ops) (:cloud-auth . ops) ...)
      :plugin-results (...))

;; Deprecated (backward compatible wrapper)
(jf/bash-extract-file-operations parsed-command &optional var-context)
  => (list of file operation plists)
```

### Token Inventory Structure
```elisp
;; Enhanced parsed command
(:command-name "aws-vault"
 :positional-args (...)
 :flags (...)
 :tokens [(:id 0 :type :command-name :value "aws-vault" :start 0 :end 9)
          (:id 1 :type :positional-arg :value "exec" :start 10 :end 14)
          ...]
 :parse-complete t
 :parse-errors nil)
```

### Data Flow
```
Bash String
    ↓
jf/bash-parse (enhanced with tokens)
    ↓
Parsed Command (with :tokens, :parse-complete)
    ↓
jf/bash-extract-semantics
    ├─→ Plugin 1 (filesystem)    → Result 1 + claimed token IDs
    ├─→ Plugin 2 (cloud-auth)    → Result 2 + claimed token IDs
    └─→ Plugin N (future)        → Result N + claimed token IDs
    ↓
Aggregate results + Calculate coverage
    ↓
Semantic Analysis Result
```

## Boundaries

### In Scope
- Token inventory tracking in parser
- Parse completeness flag
- Plugin registry and orchestration
- Coverage calculation and reporting
- Filesystem plugin refactoring
- Cloud authentication plugin (AWS, GCP, Azure)
- Breaking API change to unified semantic extraction

### Out of Scope (Future Extensions)
- Cloud resource extraction (S3 buckets, log groups, etc.) - beyond authentication
- Database connection extraction (psql, mysql, mongo)
- Network operation extraction (curl, wget, ssh)
- Container operation extraction (docker, podman)
- Language-specific code parsing (python -c, node -e) - not bash injection

### Internal vs External
- **Internal**: Token structures, plugin registry, coverage calculation algorithms
- **External**: Plugin protocol (stable interface for third-party plugins), main extraction API

### Integration Points
- **Parser Core**: Must emit token inventory
- **gptel Scope System**: Consumer of semantic extraction API (needs migration from old API)
- **Existing File Operations**: Refactored as plugin, old API deprecated

## Testing Approach

### Test Framework
**ERT (Emacs Lisp Regression Testing)** - Continue using ERT for consistency with existing bash-parser tests. ERT provides:
- Test definition via `ert-deftest`
- Assertion macros (`should`, `should-not`, `should-error`)
- Test discovery and execution
- Integration with existing test infrastructure

### Test Organization
**Location**: `config/experiments/bash-parser/test/`

Test files co-located with existing bash-parser tests in dedicated test directory. New test files:
- `test-bash-parser-plugins.el` - Plugin registry, orchestration, protocol
- `test-bash-parser-coverage.el` - Coverage calculation and reporting
- `test-bash-parser-tokens.el` - Token inventory (if separate tests needed, may be integrated into core tests)
- `test-bash-parser-filesystem-plugin.el` - Filesystem plugin refactored implementation
- `test-bash-parser-cloud-auth.el` - Cloud authentication plugin

### Naming Conventions
- **Test Files**: `test-bash-parser-*.el` format
- **Test Functions**: `test-<functionality>` format
- **Example**: `test-plugin-registration`, `test-coverage-calculation`, `test-cloud-auth-aws-vault`

### Running Tests
**Primary Method**: `make test-bash-parser`

Extend existing Makefile target to discover and run all `test-bash-parser-*.el` files in test directory. Makefile handles:
- Emacs invocation with isolated runtime
- Test file discovery
- Batch execution with output formatting

**Alternative** (already supported): `./bin/run-tests.sh -d config/experiments/bash-parser`

### Test Patterns

#### Test Data Setup
- **Inline command strings**: Most tests use inline bash command strings for clarity
- **Fixture commands**: Complex commands stored in test constants
- **Mock variable contexts**: Use alists for testing variable resolution

#### Mocking/Stubbing
- **Plugin mocking**: Create minimal test plugins for orchestration tests
- **No external mocking needed**: Parser and plugins are pure functions
- **Stub patterns**: Use simple alists for pattern databases in tests

#### Common Helpers
- **Parse-and-extract helper**: Combine `jf/bash-parse` and `jf/bash-extract-semantics` for tests
- **Token finder**: Helper to find tokens by type or value in token inventory
- **Coverage assertion**: Helper to assert expected coverage ratio

#### Assertion Patterns
```elisp
;; Assert token presence
(should (equal (plist-get token :type) :command-name))

;; Assert coverage
(should (= (plist-get coverage :coverage-ratio) 0.85))

;; Assert plugin result structure
(should (jf/bash-plugin-result-p result))
(should (eq (jf/bash-plugin-result-domain result) :filesystem))

;; Assert claimed tokens
(should (member token-id claimed-ids))
```

### Scenario Mapping

Each spec scenario maps to one or more ERT test cases:

**Pattern 1: Direct Mapping** (most common)
- Spec: "Scenario: Register plugin"
- Test: `test-plugin-registration`
- Verifies: Plugin added to registry with correct fields

**Pattern 2: Multiple Scenarios in One Test** (related scenarios)
- Spec: "Scenario: Full coverage" + "Scenario: Partial coverage"
- Test: `test-coverage-calculation-ratios`
- Verifies: Multiple coverage ratios in single test with subtests

**Pattern 3: Parameterized Test** (systematic coverage)
- Spec: "Scenario: Claim command name" + "Scenario: Claim flag tokens"
- Test: `test-filesystem-plugin-token-claiming` with loop over test cases
- Verifies: Token claiming for multiple command patterns

**Pattern 4: Integration Test** (multiple components)
- Spec: Multiple scenarios across plugins and coverage
- Test: `test-semantic-extraction-end-to-end`
- Verifies: Full pipeline from parse to coverage report

**TDD Approach**:
1. Write failing test from spec scenario
2. Implement minimum code to pass test
3. Refactor with tests passing
4. Use `ert-deftest` with descriptive names matching scenarios

## Dependencies

### Internal Dependencies
- `bash-parser-core` - Parser with token tracking (modified)
- `bash-parser-protocol` - Forward declarations (unchanged)
- `cl-lib` - Common Lisp extensions for struct definitions

### External Dependencies
- **None** - Plugin architecture is self-contained within bash-parser

### Test Dependencies
- `ert` - Emacs Lisp Regression Testing framework (built-in)
- `bash-parser` modules - Code under test

## Constraints

### Performance Constraints
- **Token overhead**: Minimal - token tracking adds ~10% memory per parsed command
- **Plugin execution**: Linear with plugin count - acceptable for 2-10 plugins
- **Coverage calculation**: O(n) where n = token count - acceptable for typical commands (<100 tokens)

### Compatibility Constraints
- **Breaking API change**: `jf/bash-extract-file-operations` deprecated in favor of `jf/bash-extract-semantics`
- **Backward compatibility wrapper**: Provided for gradual migration
- **Parser output structure**: Extended but backward compatible (new fields added, old fields unchanged)

### Technical Constraints
- **Emacs Lisp limitations**: No true multithreading - plugins execute sequentially
- **Token granularity**: Trade-off between coverage precision and complexity - chosen word-level granularity
- **Shared claiming**: Multiple plugins can claim same tokens - requires careful coverage calculation

### Testability Constraints
- **Pure functions**: Parser and plugins are pure (no side effects) - highly testable
- **Test isolation**: Each test file independent - can run in any order
- **Test data**: Uses inline command strings - no external file dependencies
