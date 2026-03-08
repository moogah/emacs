# Scope Validation Test Suite

Comprehensive test coverage for gptel scope validation system with organized structure by test type.

## Directory Structure

```
test/
├── unit/                    # Unit tests (50-60 tests)
│   ├── core/               # Core validation functions
│   ├── schema/             # Schema parsing and loading
│   ├── pipeline/           # Pipeline command extraction
│   └── cloud/              # Cloud auth detection
├── integration/             # Integration tests (144+ tests)
│   ├── schema.el           # Schema loading and validation
│   ├── cloud-auth.el       # Cloud authentication policies
│   ├── pipelines.el        # Pipeline command validation
│   └── file-paths.el       # File operation path validation
├── behavioral/              # Behavioral tests (10-15 tests)
│   └── end-to-end/         # Complete validation workflows
├── helpers-spec.el          # Shared test infrastructure
└── README.md               # This file
```

## Test Organization

### Unit Tests

Test individual functions in isolation with mocked dependencies.

**Location**: `unit/`

**Examples**:
- Schema path pattern matching
- Command categorization
- Cloud provider detection
- Parse coverage calculation

**Framework**: Buttercup (preferred)

**Naming**: `*-spec.el` files

### Integration Tests

Test module interactions and orchestration.

**Location**: `integration/`

**Coverage**:
- `schema.el` - Schema loading and validation (35+ scenarios)
- `cloud-auth.el` - Cloud authentication policy enforcement (30+ scenarios)
- `pipelines.el` - Pipeline command extraction and validation (25+ scenarios)
- `file-paths.el` - Operation-specific path validation (45+ scenarios)

**Framework**: ERT (existing tests)

**Naming**: Short descriptive names without `test-` prefix

### Behavioral Tests

Test end-to-end workflows from user perspective.

**Location**: `behavioral/`

**Examples**:
- Complete validation pipeline (parse → validate → enforce)
- Error message generation and scope expansion requests
- Session integration with scope validation

**Framework**: Buttercup (preferred)

**Naming**: `*-spec.el` files

## Shared Infrastructure

### Test Helpers (`helpers-spec.el`)

Provides common utilities for all test types:

**Custom Matchers**:
- `:to-be-validation-success` - Assert validation success
- `:to-be-validation-error` - Assert specific error type
- `:to-have-file-operation` - Check file operation extraction
- `:to-have-cloud-auth` - Check cloud auth detection
- `:to-have-parse-coverage` - Verify parse coverage ratio

**Mock Utilities**:
- `helpers-spec-setup-session` - Create mock gptel session
- `helpers-spec-teardown-session` - Clean up mock session
- `helpers-spec-mock-process-output` - Mock process execution

**Fixture Creation**:
- `helpers-spec-make-scope-yml` - Create temporary scope file
- `helpers-spec-make-minimal-scope` - Minimal valid scope
- `helpers-spec-make-scope-with-cloud-deny` - Cloud auth denied
- `helpers-spec-make-scope-with-allowed-providers` - Provider filtering

**Usage Example**:
```elisp
(require 'helpers-spec)

(describe "My validation test"
  (before-each
    (helpers-spec-setup-session))

  (after-each
    (helpers-spec-teardown-session))

  (it "validates command successfully"
    (let ((result (validate-command "ls /workspace")))
      (expect result :to-be-validation-success))))
```

## Running Tests

### By Test Type

```bash
# All tests (both ERT and Buttercup)
./bin/run-tests.sh -d config/gptel/tools/test

# Unit tests only (Buttercup)
make test-scope-unit

# Integration tests only (ERT)
make test-scope-integration

# Behavioral tests only (Buttercup)
make test-scope-behavioral
```

### By Capability

```bash
# Schema validation tests
make test-scope-schema

# Cloud authentication tests
make test-scope-cloud-auth

# Pipeline validation tests
make test-scope-pipelines

# File path validation tests
make test-scope-file-paths
```

### Direct Make Targets

```bash
# Run specific test file
make emacs-test-eval EVAL_CMD="(ert-run-tests-batch-and-exit 'test-scope-schema)"

# Run with verbose output
./bin/run-tests.sh -d config/gptel/tools/test -v
```

## Writing New Tests

### Unit Test Template (Buttercup)

```elisp
;;; unit/core/my-function-spec.el --- Tests for my-function -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'helpers-spec)
(require 'my-module)

(describe "my-function"
  (before-each
    ;; Setup
    )

  (after-each
    ;; Teardown
    )

  (describe "with valid input"
    (it "returns expected result"
      (expect (my-function "input") :to-equal "output")))

  (describe "with invalid input"
    (it "signals error"
      (expect (my-function nil) :to-throw 'wrong-type-argument))))
```

### Integration Test Template (ERT)

```elisp
;;; integration/my-integration.el --- Integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-module-a)
(require 'my-module-b)

(ert-deftest test-my-integration-success ()
  "Test successful integration between modules."
  (let ((result (module-a-call-module-b "input")))
    (should (equal result "expected-output"))))

(ert-deftest test-my-integration-failure ()
  "Test integration failure handling."
  (should-error (module-a-call-module-b nil)))
```

### Behavioral Test Template (Buttercup)

```elisp
;;; behavioral/my-workflow-spec.el --- End-to-end workflow tests -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'helpers-spec)
(require 'my-validation-system)

(describe "Complete validation workflow"
  (before-each
    (helpers-spec-setup-session))

  (after-each
    (helpers-spec-teardown-session))

  (describe "when validating safe command"
    (it "allows execution"
      (let ((result (validate-and-execute "ls /workspace")))
        (expect result :to-be-validation-success)
        (expect (plist-get result :executed) :to-be t))))

  (describe "when validating denied command"
    (it "blocks execution and returns structured error"
      (let ((result (validate-and-execute "rm -rf /")))
        (expect result :to-be-validation-error "command_denied")
        (expect (plist-get result :executed) :to-be nil)))))
```

## Test Coverage Goals

Current status and goals:

| Test Type     | Current | Goal | Status |
|---------------|---------|------|--------|
| Integration   | 144     | 144  | ✓      |
| Unit          | 0       | 50+  | Planned|
| Behavioral    | 0       | 10+  | Planned|
| **Total**     | **144** | **200+** | In Progress |

## Architecture Integration

### Bash Parser Integration

Unit and integration tests use bash-parser for:
- AST extraction and token claiming
- Semantic plugin execution (file-ops, cloud-auth)
- Parse completeness validation
- Coverage ratio calculation

**Key functions tested**:
- `jf/bash-parser-parse-string` - Parse bash commands
- `jf/bash-parser-semantic-extract` - Run semantic plugins
- `jf/bash-parser-coverage-ratio` - Calculate token coverage

### Scope System Integration

Tests validate seven-stage validation pipeline:
1. Parse (bash-parser)
2. Extract semantics (plugins)
3. Parse completeness (security check)
4. Pipeline validation (command extraction)
5. Command categorization (deny list check)
6. File operation validation (path scoping)
7. Cloud auth policy (provider filtering)

**Key modules tested**:
- `jf-gptel-scope-shell-tools.el` - Main validation orchestrator
- `jf-gptel-scope-core.el` - Schema loading and matching
- `gptel-scope-profiles.el` - Profile management

## Continuous Integration

Tests run automatically on:
- Pre-commit hooks (fast unit tests)
- Pull request validation (all tests)
- Main branch push (all tests + snapshot comparison)

**Snapshot testing**:
```bash
# Capture baseline
./bin/run-tests.sh -d config/gptel/tools/test --snapshot

# Compare changes
git diff config/gptel/tools/test/test-results.txt
```

## Resources

- OpenSpec: `openspec/changes/bash-parser-integration/`
- Architecture: `openspec/changes/bash-parser-integration/architecture.md`
- Behavioral Specs: `openspec/changes/bash-parser-integration/specs/`
- CLAUDE.md: See "Testing Infrastructure" and "Scope validation test organization"
