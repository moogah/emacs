# Corpus Tests

Data-driven test generation (34 runner tests + 135 generated).

## Philosophy

Corpus tests use **data-driven test generation** to provide comprehensive coverage from curated test data. Instead of hand-writing individual test functions, corpus tests define data structures that runners convert into executable tests.

This approach enables:
- Rapid test case addition (add data, not code)
- Consistent test structure across many scenarios
- Easy visualization of test coverage
- Separation of test data from test logic

## Organization

- `data/` - Corpus data files (test case definitions)
- `runners/` - Test runners that load and execute corpus
- `corpus-index.el` - Corpus registry and loader

## Test Files

**Data files (7):**
- `corpus-parse-command-substitution.el` - Command substitution patterns
- `corpus-parse-conditional.el` - Conditional patterns
- `corpus-parse-for-loop.el` - For loop patterns
- `corpus-parse-heredoc.el` - Heredoc patterns
- `corpus-parse-process-substitution.el` - Process substitution patterns
- `corpus-parse-combined-patterns.el` - Combined patterns
- `corpus-parse-llm-scenarios.el` - LLM-generated scenarios

**Runners (3):**
- `test-corpus-parse.el` (4 tests) - Parse corpus tests
- `test-corpus-file-operations.el` (0 explicit, ~98 generated) - File operations corpus
- `test-corpus-script-execution.el` (30 tests) - Script execution corpus

Total: 34 runner tests (6.2% of suite) + 135 corpus-generated tests

## Running Tests

```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/corpus
```

## How Corpus Tests Work

**Data Definition:**
Corpus data files define test cases as plists or alists:

```elisp
(defvar jf/bash-corpus-parse-command-substitution
  '((:input "echo $(cat file.txt)"
     :description "Basic command substitution"
     :expected-structure (...))))
```

**Test Generation:**
Corpus runners load the data and generate tests dynamically:

```elisp
(dolist (test-case jf/bash-corpus-parse-command-substitution)
  (let ((input (plist-get test-case :input))
        (expected (plist-get test-case :expected-structure)))
    ;; Generate and run test
    ))
```

**Benefits:**
1. Add 10 test cases by adding 10 plists (not 10 functions)
2. Test structure stays consistent
3. Easy to categorize and filter tests
4. Clear separation of data from logic

## When to Add Corpus Tests

Add corpus tests when:
- You have many similar test scenarios (parsing patterns, file operations)
- Test structure is consistent (same inputs/outputs across cases)
- You want data-driven coverage

**Don't add corpus tests for:**
- Complex test setup/teardown → Use explicit tests
- Unique test scenarios → Use behavioral/unit/integration tests
- Tests requiring custom validation logic → Use explicit tests

## Relationship to Other Test Categories

- **vs Unit/Integration**: Corpus provides breadth; explicit tests provide depth
- **vs Behavioral**: Corpus provides data coverage; behavioral tests validate user scenarios
- **vs Construct**: Corpus provides pattern coverage; construct tests validate syntax handling

Corpus tests complement explicit tests - they ensure comprehensive coverage of common patterns while explicit tests validate edge cases and complex scenarios.

## Corpus Index

The `corpus-index.el` file maintains a registry of all corpus data files and provides discovery functions. This ensures:
- Corpus runners can find all data files
- New corpus files are automatically discovered
- Corpus metadata is centralized
