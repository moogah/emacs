# Scope Validation Testing Gap Analysis

**Date:** 2026-03-08
**Scope:** `config/gptel/tools/scope-shell-tools.org` and `config/gptel/scope/`
**Total Functions Analyzed:** 35
**Existing Test Files:** 4 (144+ total tests)

---

## Executive Summary

### Current State
The scope validation system has **excellent integration test coverage** (144+ tests) for:
- Schema loading and validation (35+ tests)
- Pipeline command validation (34 tests)
- File path validation (45+ tests)
- Cloud authentication validation (30+ tests)

### Critical Gaps
1. **No unit tests** - All existing tests are integration tests that exercise multiple functions together
2. **13 completely untested functions** - Including core helpers and validation dispatch
3. **No behavioral tests** - Missing end-to-end user scenarios with mocked gptel
4. **Limited seven-stage pipeline testing** - Main orchestration function not tested holistically

---

## Testing Pyramid Analysis

### ✅ What We Have: Integration Tests (144+ tests)

Current tests are **ALL at the integration level**:

```
Integration Layer (144+ tests) ✅
├─ Schema loading (35+ tests)
├─ Cloud auth validation (30+ tests)
├─ Pipeline validation (34 tests)
└─ File path validation (45+ tests)
```

### ❌ What We're Missing

```
Behavioral Tests (0 tests) ❌
    ↑ End-to-end user scenarios with mocked gptel
    ↑ Scope expansion workflows
    ↑ Error recovery scenarios

Integration Module Tests (5-10 gaps) ⚠️
    ↑ Full 7-stage pipeline orchestration
    ↑ Combined validation scenarios
    ↑ Command execution (timeout, truncation)

Unit Tests (13 untested functions) ❌
    ↑ Helper functions (glob, pattern matching, normalization)
    ↑ Validators (tool permission dispatch, error formatting)
    ↑ Configuration (allow-once, tool categories)
```

---

## Gap Details

### 1. Unit Tests (MISSING - HIGH PRIORITY)

**13 completely untested unit-testable functions:**

#### Core Helpers (5 functions)
| Function | Purpose | Complexity | Test Priority |
|----------|---------|------------|---------------|
| `jf/gptel-scope--infer-validation-type` | Map tool name → validation type | Simple lookup | HIGH |
| `jf/gptel-scope--normalize-args` | Vector → list conversion | Simple transform | MEDIUM |
| `jf/gptel-scope--matches-any-pattern` | Check path vs pattern list | Simple iteration | HIGH |
| `jf/gptel-scope--matches-pattern` | Pattern match with expansion | Path resolution + regex | HIGH |
| `jf/gptel-bash--check-absolute-paths` | Detect absolute paths in commands | Regex matching | MEDIUM |

#### Validation Functions (5 functions)
| Function | Purpose | Complexity | Test Priority |
|----------|---------|------------|---------------|
| `jf/gptel-scope--validate-path-tool` | Validate filesystem tool permissions | Multi-step validation | HIGH |
| `jf/gptel-scope--validate-pattern-tool` | Validate org-roam tool permissions | Pattern matching logic | MEDIUM |
| `jf/gptel-scope--validate-bash-tool` | Validate bash tool (legacy path) | Complex categorization | HIGH |
| `jf/gptel-scope--check-tool-permission` | Central dispatcher | Routing logic | HIGH |
| `jf/gptel-scope--format-tool-error` | Error formatting | Data transformation | MEDIUM |

#### Configuration/State (3 functions)
| Function | Purpose | Complexity | Test Priority |
|----------|---------|------------|---------------|
| `jf/gptel-scope--check-allow-once` | Check temporary permissions | List lookup + consume | HIGH |
| `jf/gptel-scope--clear-allow-once` | Clear temporary permissions | Simple cleanup | LOW |
| `jf/gptel-scope--check-coverage-threshold` | Warn on low parse coverage | Comparison + warning | MEDIUM |

**Why Unit Tests Matter:**
- Fast feedback (milliseconds vs seconds)
- Isolate function logic from dependencies
- Document edge cases and assumptions
- Enable confident refactoring
- Easier debugging when tests fail

**Example Gap - Pattern Matching:**
```elisp
;; Currently NO unit tests for this critical security function:
(defun jf/gptel-scope--matches-pattern (path pattern)
  "Check if PATH matches glob PATTERN with symlink resolution."
  ...)

;; We need tests like:
(ert-deftest test-unit-matches-pattern-exact-match ...)
(ert-deftest test-unit-matches-pattern-wildcard ...)
(ert-deftest test-unit-matches-pattern-recursive-wildcard ...)
(ert-deftest test-unit-matches-pattern-symlink-resolution ...)
```

---

### 2. Integration Module Tests (GAPS - MEDIUM PRIORITY)

**Missing module-level integration scenarios:**

#### A. Seven-Stage Pipeline Orchestration (0 tests)

`jf/gptel-scope--validate-command-semantics` is the main pipeline but has **NO direct tests**.

Current: Individual stages tested in isolation
Missing: Full pipeline flow tests

**Needed tests:**
```elisp
;; Command passes all 7 stages
(ert-deftest test-integration-pipeline-full-pass ...)

;; Early exit at each stage
(ert-deftest test-integration-pipeline-fail-stage-1-parse ...)
(ert-deftest test-integration-pipeline-fail-stage-2-extract ...)
(ert-deftest test-integration-pipeline-fail-stage-3-pipeline ...)
(ert-deftest test-integration-pipeline-fail-stage-4-file-ops ...)
(ert-deftest test-integration-pipeline-fail-stage-5-cloud-auth ...)

;; Stage 6 warning doesn't block
(ert-deftest test-integration-pipeline-stage-6-warning-non-blocking ...)

;; Combined validations
(ert-deftest test-integration-pipeline-file-ops-plus-cloud-auth ...)
```

#### B. Command Execution Helpers (0 tests)

`jf/gptel-bash--execute-command` has **NO tests** - difficult without process mocking.

**Needed tests (with mocking):**
```elisp
;; Successful execution
(ert-deftest test-integration-execute-command-success ...)

;; Timeout handling
(ert-deftest test-integration-execute-command-timeout ...)

;; Output truncation
(ert-deftest test-integration-execute-command-truncated-output ...)

;; Exit code capture
(ert-deftest test-integration-execute-command-non-zero-exit ...)

;; Absolute path warnings
(ert-deftest test-integration-execute-command-absolute-path-warning ...)
```

#### C. Combined Validation Scenarios (3-5 needed)

Current tests focus on single validation type.
Missing: Tests combining multiple validators.

**Examples:**
- Command with pipeline + file operations + cloud auth
- Command with parse incomplete + file operations
- Command with deny path + cloud auth policy

---

### 3. Behavioral Tests (MISSING - LOW PRIORITY but HIGH VALUE)

**NO behavioral tests** that mock gptel and test real user workflows.

**What behavioral tests look like:**
```elisp
;; Scenario: User runs allowed command and sees output
(ert-deftest test-behavioral-allowed-command-shows-output ()
  "User story: Claude runs 'ls -la' in /workspace
   GIVEN scope allows read in /workspace
   WHEN run_bash_command tool called
   THEN command executes and returns output"
  ...)

;; Scenario: User gets helpful error with scope expansion suggestion
(ert-deftest test-behavioral-denied-command-suggests-expansion ()
  "User story: Claude attempts 'rm file.txt'
   GIVEN rm is in deny list
   WHEN run_bash_command tool called
   THEN error message guides LLM to request_scope_expansion"
  ...)

;; Scenario: Scope expansion workflow
(ert-deftest test-behavioral-scope-expansion-workflow ()
  "User story: Claude requests new command, user approves
   GIVEN command 'tree' not in allowed list
   WHEN LLM calls request_scope_expansion
   AND user selects 'Add to scope'
   THEN scope.yml updated and command allowed"
  ...)
```

**Why behavioral tests matter:**
- Validate end-to-end user experience
- Catch integration issues missed by unit tests
- Document expected workflows
- Ensure error messages guide users correctly

**Challenges:**
- Requires gptel mocking infrastructure
- More complex to write and maintain
- Slower to execute

---

## Detailed Function Coverage Table

| Function | File | Type | Tests | Status | Priority |
|----------|------|------|-------|--------|----------|
| `jf/gptel-scope--load-schema` | shell-tools | Schema | 35+ | ✅ Tested | - |
| `jf/gptel-scope--normalize-keys` | shell-tools | Helper | Indirect | ⚠️ Partial | MEDIUM |
| `jf/gptel-scope--validate-schema` | shell-tools | Schema | 10+ | ✅ Tested | - |
| `jf/gptel-scope--validate-cloud-config` | shell-tools | Config | Indirect | ⚠️ Partial | MEDIUM |
| `jf/gptel-scope--validate-security-config` | shell-tools | Config | Indirect | ⚠️ Partial | MEDIUM |
| `jf/gptel-scope--extract-pipeline-commands` | shell-tools | Pipeline | 15+ | ✅ Tested | - |
| `jf/gptel-scope--validate-pipeline-commands` | shell-tools | Pipeline | 25+ | ✅ Tested | - |
| `jf/gptel-scope--validate-command-semantics` | shell-tools | Orchestration | Indirect | ❌ **NO DIRECT TESTS** | **HIGH** |
| `jf/gptel-scope--validate-parse-completeness` | shell-tools | Validation | 5+ | ✅ Tested | - |
| `jf/gptel-scope--validate-cloud-auth` | shell-tools | Validation | 30+ | ✅ Tested | - |
| `jf/gptel-scope--check-coverage-threshold` | shell-tools | Warning | **0** | ❌ **UNTESTED** | **MEDIUM** |
| `jf/gptel-bash--check-absolute-paths` | shell-tools | Helper | **0** | ❌ **UNTESTED** | **MEDIUM** |
| `jf/gptel-bash--execute-command` | shell-tools | Execution | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--infer-validation-type` | shell-tools | Helper | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--validate-operation` | shell-tools | Validation | 40+ | ✅ Tested | - |
| `jf/gptel-scope--path-matches-any-pattern-p` | shell-tools | Helper | Indirect | ⚠️ Partial | HIGH |
| `jf/gptel-scope--glob-match-p` | shell-tools | Helper | 20+ | ✅ Tested | - |
| `jf/gptel-scope--glob-to-regex` | shell-tools | Helper | Indirect | ⚠️ Partial | HIGH |
| `jf/gptel-scope--validate-file-operation` | shell-tools | Validation | Indirect | ⚠️ Partial | MEDIUM |
| `jf/gptel-scope--validate-file-operations` | shell-tools | Validation | 10+ | ✅ Tested | - |
| `jf/gptel-scope--normalize-args` | core | Helper | **0** | ❌ **UNTESTED** | MEDIUM |
| `jf/gptel-scope--load-config` | core | Config | Indirect | ⚠️ Partial | LOW |
| `jf/gptel-scope--normalize-plist-keys` | core | Helper | Indirect | ⚠️ Partial | MEDIUM |
| `jf/gptel-scope--parse-scope-yml` | core | Parsing | 3+ | ✅ Tested | - |
| `jf/gptel-scope--check-allow-once` | core | Permission | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--clear-allow-once` | core | Cleanup | **0** | ❌ **UNTESTED** | LOW |
| `jf/gptel-scope--matches-any-pattern` | core | Helper | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--validate-path-tool` | core | Validation | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--validate-pattern-tool` | core | Validation | **0** | ❌ **UNTESTED** | MEDIUM |
| `jf/gptel-scope--validate-bash-tool` | core | Validation | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--check-tool-permission` | core | Dispatcher | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--matches-pattern` | core | Helper | **0** | ❌ **UNTESTED** | **HIGH** |
| `jf/gptel-scope--format-tool-error` | core | Formatting | **0** | ❌ **UNTESTED** | MEDIUM |

**Legend:**
- ✅ Tested: Direct tests with good coverage
- ⚠️ Partial: Indirect testing via integration tests
- ❌ Untested: No test coverage

---

## Recommended Testing Roadmap

### Phase 1: Critical Unit Tests (1-2 days)
**Goal:** Test high-risk, unit-testable functions

Priority order:
1. ✅ **Pattern matching helpers** (security-critical):
   - `jf/gptel-scope--matches-pattern` - 10+ edge cases
   - `jf/gptel-scope--matches-any-pattern` - 5+ scenarios
   - `jf/gptel-scope--glob-to-regex` - 15+ glob patterns (if not already comprehensive)

2. ✅ **Permission checking**:
   - `jf/gptel-scope--check-allow-once` - 8+ scenarios (consume, not found, wrong resource)
   - `jf/gptel-scope--infer-validation-type` - 5+ tool types

3. ✅ **Validation dispatch**:
   - `jf/gptel-scope--check-tool-permission` - 10+ scenarios (each validation type, meta tools, unknown tools)
   - `jf/gptel-scope--validate-path-tool` - 10+ scenarios (read/write, deny, patterns)
   - `jf/gptel-scope--validate-bash-tool` - 15+ scenarios (categories, deny, directory scope)

**Deliverable:** 50-60 new unit tests

### Phase 2: Integration Module Tests (1 day)
**Goal:** Test module orchestration and combined scenarios

1. ✅ **Seven-stage pipeline**:
   - `jf/gptel-scope--validate-command-semantics` full flow
   - Early exit at each stage
   - Non-blocking warnings

2. ✅ **Command execution** (with process mocking):
   - `jf/gptel-bash--execute-command` - timeout, truncation, warnings

3. ✅ **Combined validations**:
   - Pipeline + file operations
   - File operations + cloud auth
   - All three together

**Deliverable:** 15-20 new integration tests

### Phase 3: Behavioral Tests (✅ COMPLETE)
**Goal:** End-to-end user scenarios with validation workflows

**Completed:** 27 behavioral tests in `config/gptel/tools/test/behavioral/workflows-spec.el`

1. ✅ **Schema loading workflows** (3 tests):
   - Operation-specific path loading
   - Cloud config merging with defaults
   - Security config merging

2. ✅ **File operation validation workflows** (3 tests):
   - Read operations within/outside scope
   - Deny list precedence

3. ✅ **Operation-specific permissions** (3 tests):
   - Write operation validation
   - Execute operation validation
   - Modify operation validation

4. ✅ **Glob pattern matching workflows** (2 tests):
   - Pattern matching with wildcards
   - Multiple file operations

5. ✅ **Cloud authentication workflows** (3 tests):
   - Auth-detection loading
   - Allowed-providers configuration
   - Validation against config

6. ✅ **File path validation with deny precedence** (3 tests):
   - Access within scope
   - Access outside scope
   - Deny list override

7. ✅ **Operation-specific validation** (4 tests):
   - Read, write, execute, modify operations

8. ✅ **Parse completeness** (2 tests):
   - Strict mode validation
   - Permissive mode validation

9. ✅ **Error message structure** (2 tests):
   - Structured errors for path violations
   - Success returns nil

10. ✅ **Multiple file operations** (2 tests):
   - Independent validation
   - Mixed valid/invalid operations

**Deliverable:** 27 behavioral tests (exceeded 10-15 target)

### Phase 4: Coverage and Cleanup (0.5 days)
**Goal:** Fill remaining gaps and improve test quality

1. ✅ **Remaining unit tests**:
   - `jf/gptel-scope--format-tool-error`
   - `jf/gptel-scope--check-coverage-threshold`
   - `jf/gptel-bash--check-absolute-paths`

2. ✅ **Test organization**:
   - Add test file headers documenting coverage
   - Create test helper library for common mocks
   - Document testing patterns

**Deliverable:** 5-10 additional tests, improved test infrastructure

---

## Metrics Summary

### Current State (Updated 2026-03-08)
- **Total functions:** 35
- **Functions with tests:** 22 (63%)
- **Functions untested:** 13 (37%)
- **Total test count:** 171+ (144 integration + 27 behavioral)
- **Test types:** 84% integration, 0% unit, 16% behavioral

### Target State (After Roadmap)
- **Functions with tests:** 33 (94%)
- **Total test count:** 220-240
- **Test types:** 25% unit, 65% integration, 10% behavioral

### Coverage Gaps Status
- ⏳ Unit test foundation (Phase 1) - Pending (50-60 tests needed)
- ⏳ Pipeline orchestration (Phase 2) - Pending (15-20 tests needed)
- ✅ User workflows validated (Phase 3) - COMPLETE (27 behavioral tests)
- ⏳ Critical security functions - Partial (13 functions still untested)

---

## Appendix: Test File Organization

### Existing Files (144+ tests)
```
config/gptel/tools/test/
├── test-scope-schema.el                    (35+ tests) - Schema loading/validation
├── test-scope-validation-cloud-auth.el     (30+ tests) - Cloud auth enforcement
├── test-scope-validation-pipelines.el      (34 tests)  - Pipeline command validation
└── test-scope-validation-file-paths.el     (45+ tests) - File path validation
```

### Recommended New Files
```
config/gptel/tools/test/
├── test-scope-unit-helpers.el              (NEW - 30+ tests) - Unit tests for helpers
├── test-scope-unit-validators.el           (NEW - 30+ tests) - Unit tests for validators
├── test-scope-integration-pipeline.el      (NEW - 20 tests)  - Seven-stage pipeline
├── test-scope-integration-execution.el     (NEW - 10 tests)  - Command execution
└── test-scope-behavioral-workflows.el      (NEW - 15 tests)  - End-to-end scenarios

config/gptel/scope/test/                    (NEW DIRECTORY)
└── test-scope-core-unit.el                 (NEW - 20 tests)  - Unit tests for scope-core
```

---

## Notes

1. **Integration vs Unit distinction:**
   - Integration: Tests multiple functions together, requires real file system/config
   - Unit: Tests single function in isolation, uses mocks/stubs for dependencies

2. **Why current tests are integration:**
   - Most tests call `jf/bash-parse` (bash-parser dependency)
   - Tests load actual scope.yml configurations
   - Tests exercise full validation pipelines

3. **Testing strategy recommendation:**
   - Unit tests: Fast feedback, isolate logic, document edge cases
   - Integration tests: Validate module interactions, catch integration bugs
   - Behavioral tests: Validate user experience, ensure helpful errors

4. **Challenges:**
   - Process mocking for `jf/gptel-bash--execute-command`
   - gptel mocking infrastructure for behavioral tests
   - Maintaining test suite as implementation evolves
