# run_bash_command Behavioral Tests

Modular test suite for the `run_bash_command` tool's seven-stage validation pipeline.

## Organization

Tests are organized by validation stage and concern:

| File | Stage | Tests | Description |
|------|-------|-------|-------------|
| `parse-completeness-spec.el` | 2 | 10 | Parse completeness enforcement |
| `deny-list-validation-spec.el` | 3 | 10 | Deny list bypass prevention |
| `no-op-allowance-spec.el` | 5 | 6 | No-op allowance short-circuit |
| `operation-specific-paths-spec.el` | 6 | 14 | Operation-specific path validation |
| `cloud-authentication-spec.el` | 7 | 11 | Cloud authentication policy |
| `integration-scenarios-spec.el` | Multi | 11 | Complex integration scenarios |
| `path-resolution-spec.el` | - | 10 | Working directory and path resolution |
| `resource-limits-spec.el` | - | 9 | Timeout and resource limits |
| `error-messages-spec.el` | - | 10 | Error message structure |

**Total: 91 tests across 9 files**

## Seven-Stage Validation Pipeline

1. **Session directory detection** - Locate scope.yml
2. **Parse completeness** (stage 2) - Reject incomplete bash syntax
3. **Deny list** (stage 3) - Block prohibited commands
4. **Working directory resolution** - Resolve relative paths
5. **No-op allowance** (stage 5) - Allow zero-operation commands
6. **Operation-specific paths** (stage 6) - Validate read/write/execute/modify permissions
7. **Cloud authentication** (stage 7) - Enforce cloud provider policies

## Running Tests

### All tests in this directory
```bash
./bin/run-tests.sh -d config/gptel/tools/test/behavioral/run-bash-command
```

### Specific test file
```bash
./bin/run-tests.sh -d config/gptel/tools/test/behavioral/run-bash-command -p parse-completeness
```

### Via Makefile
```bash
make test-buttercup-directory DIR=config/gptel/tools/test/behavioral/run-bash-command
```

### Individual file (from repository root)
```bash
emacs -batch -L config/gptel/tools \
  -l buttercup \
  -l config/gptel/tools/test/behavioral/run-bash-command/parse-completeness-spec.el \
  -f buttercup-run-discover
```

## Test Structure

Each test file follows the same pattern:

```elisp
;;; <name>-spec.el --- <Description> -*- lexical-binding: t; -*-

;; Commentary section explaining the stage and behaviors tested

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies with correct paths for subdirectory
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

(describe "run_bash_command: <Category>"
  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "test description"
    ;; Test implementation
    ))

(provide '<module-name>-spec)
```

## Dependencies

- **Framework**: Buttercup (BDD-style testing)
- **Helpers**: `helpers-spec.el` (parent directory)
- **Implementation**: `scope-shell-tools.el` (tools directory)

## Test Patterns

### Scope Configuration
```elisp
(let* ((scope-yml (helpers-spec-make-scope-yml "<yaml>"))
       (scope-config (helpers-spec-load-scope-config scope-yml)))
  ;; Test using scope-config
  (delete-file scope-yml))
```

### Bash Parser Mocking
```elisp
(helpers-spec-mock-bash-parse
 "command string"
 '("list" "of" "commands")  ; Extracted commands
 t)  ; parse-complete flag
```

### Bash Semantics Mocking
```elisp
(helpers-spec-mock-bash-semantics
 (list (helpers-spec--make-file-op :read "/path" :command-name "cat"))
 nil  ; Cloud auth detection
 '(:ratio 1.0))  ; Coverage metrics
```

### Validation
```elisp
(let ((result (jf/gptel-scope--validate-command-semantics
               "command"
               "/working/dir"
               scope-config)))
  (expect (plist-get result :error) :to-equal "expected-error"))
```

## Selective Test Execution

Run tests by priority or stage:

```bash
# All security-critical tests (stage 2, 3, 7)
./bin/run-tests.sh -p 'parse-completeness|deny-list|cloud-auth'

# Permission validation tests (stage 6)
./bin/run-tests.sh -p 'operation-specific-paths'

# Integration and edge cases
./bin/run-tests.sh -p 'integration|error-messages|resource-limits'
```

## Benefits of Modular Organization

1. **Navigation**: Easy to find tests for specific validation stages
2. **Maintainability**: Smaller, focused files (~100-250 lines each)
3. **Selective execution**: Run only security tests or specific stages
4. **Parallel development**: Multiple developers can work on different test files
5. **Clear boundaries**: One file per concern/stage
6. **Follows existing patterns**: Matches bash-parser test organization

## Related Documentation

- Behavioral spec: `openspec/specs/gptel/scope.md`
- Implementation: `config/gptel/tools/scope-shell-tools.org`
- Test framework: `config/core/testing.el`
