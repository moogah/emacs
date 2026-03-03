# Bash Parser Tests

This directory contains ERT (Emacs Regression Testing) tests for the bash-parser module.

## Test Files

### Core Parser Tests
- `bash-parser-test.el` - Original parser tests using `test-corpus.el` (60 tests)
- `test-corpus.el` - Test corpus for basic parsing (commands, flags, args, dangerous patterns)

### Semantic Analysis Tests
- `test-file-operations-corpus.el` - Corpus-based file operations tests (60 tests)
- `file-operations-corpus.el` - Test corpus showing which files are created/read/modified/deleted
- `test-file-operations.el` - Core file operations extraction tests (33 tests)
- `test-command-semantics.el` - Command semantics database tests
- `test-security-validator.el` - Sandbox security validation tests
- `test-parser-extension.el` - Parser backward compatibility tests
- `test-glob-matching.el` - Glob pattern matching tests

### Test Results
- `CORPUS-TEST-RESULTS.md` - Detailed analysis of corpus test results
- `test-results.txt` - Complete test suite results (tracked in git)

## Running Tests

### Recommended: Run All Tests with Snapshot

The easiest way to run the complete test suite (286 tests) and capture results:

```bash
# From repository root
./config/experiments/bash-parser/test/run-all-tests-snapshot.sh
```

This will:
- Run all 286 tests across all test files
- Capture output to `test-results.txt` (tracked in git)
- Display summary with pass/fail counts
- Exit with code 0 if all pass, 1 if any fail

**View results:**
```bash
cat config/experiments/bash-parser/test/test-results.txt
```

**See what changed after making modifications:**
```bash
git diff config/experiments/bash-parser/test/test-results.txt
```

**Commit baseline or updated results:**
```bash
git add config/experiments/bash-parser/test/test-results.txt
git commit -m "Update test results baseline"
```

**Current Status:**
- **286 total tests**
- **262 passing (91.6%)**
- **24 failing (8.4%)**

Test suite includes:
- ✅ Original parser tests (60/60 passing) - Basic parsing, pipelines, chains
- ✅ Core extraction tests (most passing) - File operations, redirections
- ⚠️ Some corpus tests (41/60 passing) - Archives, variables need work
- ⚠️ Some glob tests failing - Pattern metadata edge cases

### Run All Tests

```bash
/Applications/Emacs.app/Contents/MacOS/Emacs -batch \
  -l config/experiments/bash-parser/bash-parser.el \
  -l config/experiments/bash-parser/test/test-glob-matching.el \
  -f ert-run-tests-batch-and-exit
```

### Run Specific Test Suite

```bash
# Run file operations corpus tests (60 tests - clear examples)
./config/experiments/bash-parser/test/run-file-operations-corpus-tests.sh

# Run glob matching tests
./config/experiments/bash-parser/test/run-glob-tests.sh

# Run file operations tests (33 core tests)
./config/experiments/bash-parser/test/run-file-operations-tests.sh

# Run specific test pattern manually
/Applications/Emacs.app/Contents/MacOS/Emacs -batch \
  --eval "(setq treesit-extra-load-path '(\"/Users/jefffarr/emacs/runtime/tree-sitter\"))" \
  -l config/experiments/bash-parser/bash-parser.el \
  -l config/experiments/bash-parser/test/test-glob-matching.el \
  --eval '(ert-run-tests-batch-and-exit "^test-glob-")'
```

### Interactive Testing

From within Emacs:

```elisp
;; Load the module
(load-file "config/experiments/bash-parser/bash-parser.el")

;; Load the test file
(load-file "config/experiments/bash-parser/test/test-glob-matching.el")

;; Run all tests interactively
(ert t)

;; Run specific test suite
(ert "^test-glob-")

;; Run single test
(ert "test-glob-single-level-wildcard-match")
```

## Test Status

### Glob Matching Tests (test-glob-matching.el)

Status: **READY - AWAITING IMPLEMENTATION** (Bead: emacs-v9d5)

All tests are marked with `:expected-result :failed` until the glob matching functions are implemented in bash-parser.org (bead emacs-v9d5).

Required functions:
- `jf/bash-glob-match-p` - Main pattern matching function
- `jf/bash--glob-to-regex` - Convert glob pattern to regex (helper)
- `jf/bash--match-segments` - Segment-based matching (helper)

**Current State:** Tests will pass unexpectedly because functions don't exist yet. Once functions are implemented:
1. Tests will start failing (because functions exist but may have bugs)
2. Fix implementation until all tests pass
3. Remove the `:expected-result :failed` annotation from each test

## Test Naming Convention

Tests follow the pattern: `test-<component>-<scenario-slug>`

Examples:
- `test-glob-single-level-wildcard-match` - Maps to spec scenario "Single-level wildcard match"
- `test-glob-recursive-wildcard-match` - Maps to spec scenario "Recursive wildcard match"
- `test-glob-character-class-match` - Maps to spec scenario "Character class match"

Each test includes a docstring with:
1. Scenario reference from spec (e.g., "bash-sandbox-security § 'Single-level wildcard match'")
2. Brief description of what is being tested

## Spec References

Tests map to scenarios defined in:
- `openspec/changes/bash-parser-file-ops/specs/bash-sandbox-security/spec.md`
- `openspec/changes/bash-parser-file-ops/specs/bash-file-operations/spec.md`
- `openspec/changes/bash-parser-file-ops/specs/bash-command-semantics/spec.md`

See `openspec/changes/bash-parser-file-ops/architecture.md` for testing approach and framework details.

## Test Coverage

### Glob Matching Tests

The test suite covers:

1. **Single-level wildcards (*)**: Matches within one path segment, doesn't cross directory boundaries
2. **Recursive wildcards (**)**: Matches zero or more directory levels
3. **Character classes ([])**: Matches single character from set
4. **Question marks (?)**: Matches exactly one character
5. **Path boundaries**: Patterns must match from start, respect directory structure
6. **Literal characters**: Special regex chars are escaped properly
7. **Combined wildcards**: Multiple wildcard types in one pattern
8. **Edge cases**: Empty paths/patterns, root paths, exact matches

Total: 30+ test cases covering all glob matching scenarios from spec.

### File Operations Extraction Tests (test-file-operations.el)

Status: **COMPLETED - PARTIAL PASS** (Bead: emacs-5rst)

**Test Results:** 25 of 33 tests passing (76% pass rate)

**Passing Tests:**
- Basic extraction (simple read, multiple files, no operations)
- Redirection extraction (output, append, input, multiple)
- Path types (absolute, relative)
- Glob patterns (simple, recursive)
- Confidence levels
- Metadata inclusion
- Deduplication (same operation)
- Main function (simple, pipeline, chain)
- Variable resolution (with context, with braces, multiple variables)

**Known Issues (8 failing tests with follow-up beads):**
1. **emacs-xbsj** (P1): Unresolved variable marking
   - Tests: test-variable-simple-variable-reference, test-variable-unresolved-variable, test-variable-empty-variable-context, test-variable-partial-resolution
   - Issue: Variables without context not marked with :unresolved metadata

2. **emacs-zppa** (P1): Variable assignment tracking in chains
   - Tests: test-variable-assignment-and-usage-in-chain, test-variable-multiple-assignments
   - Issue: VAR=value assignments not tracked across && chains

3. **emacs-qhtn** (P2): Touch command semantics
   - Tests: test-extraction-command-chain
   - Issue: touch command not producing :create operation

4. **emacs-wxn1** (P2): Pipeline redirection extraction
   - Tests: test-extraction-pipeline-with-multiple-file-operations
   - Issue: Redirections in pipeline commands not extracted

**Test Coverage:**
- 33 test cases covering all file operations extraction scenarios from spec
- Tests extraction from: redirections, positional args, exec blocks
- Tests multi-command constructs: pipelines, chains
- Tests variable handling: detection, resolution, tracking, assignments
- Tests operation deduplication and metadata

**Running Tests:**
```bash
./config/experiments/bash-parser/test/run-file-operations-tests.sh
```

**Next Steps:**
- Fix P1 issues (emacs-xbsj, emacs-zppa) for core variable functionality
- Fix P2 issues (emacs-qhtn, emacs-wxn1) for complete command coverage
- All tests should pass before marking Wave 6 complete
