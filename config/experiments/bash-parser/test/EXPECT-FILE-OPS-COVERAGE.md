# :expect-file-ops Coverage Summary

This document tracks the addition of comprehensive `:expect-file-ops` expectations to corpus test files.

## Goal

Add comprehensive test cases with complete `:expect-file-ops` showing all file operations including:
- Pattern flow tracking (`:pattern`, `:pattern-source`)
- Loop context (`:loop-context`, `:loop-variable`)
- Conditional context (`:conditional`, `:branch`, `:test-condition`)
- Heredoc context (`:heredoc-content`)
- Nested operations (`:from-substitution`, `:indirect`)

## Current Coverage

### corpus-parse-command-substitution.el
- **Total tests**: 23
- **Tests with :expect-file-ops**: 2
- **Key examples added**:
  - `cmdsub-nested-003`: Find pattern flow to cat read (pattern generation + consumption)
  - `cmdsub-multiple-002`: Multiple substitutions in cp (dynamic source/dest)

### corpus-parse-combined-patterns.el
- **Total tests**: 39
- **Tests with :expect-file-ops**: 1
- **Key examples added**:
  - `combined-for-cmdsub-001`: Loop + substitution with pattern flow through loop variable

### corpus-parse-conditional.el
- **Total tests**: 18
- **Tests with :expect-file-ops**: 1
- **Key examples added**:
  - `cond-complex-001`: Test condition file metadata check

### corpus-parse-for-loop.el
- **Total tests**: 27
- **Tests with :expect-file-ops**: 1
- **Key examples added**:
  - `forloop-real-008`: Loop context with cp operations

### corpus-parse-heredoc.el
- **Total tests**: 23
- **Tests with :expect-file-ops**: 2
- **Key examples added**:
  - `heredoc-context-001`: Heredoc in git commit (no file ops - content only)
  - `heredoc-context-007`: Heredoc with redirect (file creation)

### test-corpus-file-operations.el
- **Total tests**: 98 (original) + 5 (integration) = 103
- **Integration tests added**: 5
- **Key examples added**:
  - `integration-001`: Loop + substitution + conditional + nested substitution (all context markers)
  - `integration-002`: Pipeline with pattern substitution
  - `integration-003`: Loop + conditional + directory operations
  - `integration-004`: Heredoc piped to while loop with dynamic writes
  - `integration-005`: Conditional with pipeline and file operations

## Validation Infrastructure

### New Files
- `test-expect-file-ops-validation.el`: Validates corpus structure
- Added `jf/test-corpus-file-ops-validate-file()` to test-corpus-file-operations.el
- Added `jf/test-corpus-file-ops-validate-all()` for comprehensive validation

### Validation Functions
```elisp
;; Count tests with expectations per corpus
(jf/test-corpus-file-ops-validate-file "command-substitution")

;; Validate all corpus files
(jf/test-corpus-file-ops-validate-all)
```

## Next Steps (Not required for this bead - future work)

To achieve full comprehensive coverage, future work could add:

1. **More command-substitution examples** (currently 2/23):
   - Nested substitutions with pattern flow
   - Backtick syntax examples
   - Complex pipeline scenarios

2. **More combined-patterns examples** (currently 1/39):
   - All xargs patterns (5 tests)
   - All command-sub in redirects (4 tests)
   - All find-exec patterns (5 tests)

3. **More conditional examples** (currently 1/18):
   - Compound tests with multiple file checks
   - Command substitution in test conditions
   - Multi-branch elif chains

4. **More for-loop examples** (currently 1/27):
   - All real-world patterns (8 tests)
   - Glob expansion patterns
   - Command substitution in loop lists

5. **More heredoc examples** (currently 2/23):
   - Heredoc in different contexts (SQL, Python, etc.)
   - Indented heredoc patterns
   - Multi-stage pipeline with heredoc

## Impact

The :expect-file-ops expectations serve as:
1. **Test validation**: Verify file operations extraction is working correctly
2. **Documentation**: Show expected behavior for each semantic pattern
3. **Regression prevention**: Catch changes that break file operations detection
4. **Feature completeness**: Ensure all context markers are being added

## Verification Commands

```bash
# Run bash parser tests
make test-bash-parser

# Run validation tests specifically
make emacs-test-eval EVAL_CMD="(load \"test-expect-file-ops-validation.el\") (ert-run-tests-batch)"

# In Emacs interactive
(require 'test-corpus-file-operations)
(jf/test-corpus-file-ops-validate-all)
```
