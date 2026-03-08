# Integration Tests

Multi-component interactions (51 tests).

## Philosophy

Integration tests validate how multiple components work together. Unlike unit tests (which test single functions) and behavioral tests (which test user-facing scenarios), integration tests focus on **component interactions and feature combinations**.

## Test Files

- `test-command-substitution.el` (28 tests) - Command substitution
- `test-pattern-flow.el` (10 tests) - Pattern flow
- `test-expect-file-ops-validation.el` (3 tests) - File ops validation
- `test-treesitter-workarounds.el` (7 tests) - Tree-sitter integration
- `test-find-exec-patterns.el` (3 tests) - Find with exec patterns

Total: 51 tests (9.3% of suite)

## What These Tests Cover

**Command Substitution Integration:**
- How command substitution interacts with variable resolution
- How nested substitutions are parsed and extracted
- How substitutions interact with pipelines, heredocs, and conditionals
- Edge cases: empty substitutions, deeply nested, mixed quotes

**Pattern Flow:**
- How file operations flow through pipelines
- How operations are tracked through logical operators (&&, ||)
- How operations are isolated in subshells and background jobs
- How command groups and nested subshells affect operation extraction

**File Operations Validation:**
- Integration between extraction and validation systems
- How extracted operations are validated for correctness
- Validation of complex multi-operation scenarios

**Tree-sitter Integration:**
- How bash-parser handles tree-sitter parse errors
- Workarounds for malformed syntax that tree-sitter can't parse
- Graceful degradation for partial/incomplete commands

**Find -exec Patterns:**
- Complex find command patterns with -exec blocks
- Multiple -exec blocks in single find command
- Integration with workspace validation scenarios

## Running Tests

```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/integration
```

## When to Add Integration Tests

Add integration tests when testing:
- How two or more components interact
- Feature combinations (e.g., variables + substitution)
- Cross-layer interactions (e.g., semantics + analysis)
- Complex scenarios that involve multiple subsystems

**Don't add integration tests for:**
- Single function behavior → Use unit tests
- User-facing scenarios → Use behavioral tests
- Language constructs → Use construct tests

## Relationship to Other Test Categories

- **vs Unit**: Unit tests validate individual functions; integration tests validate component interactions
- **vs Behavioral**: Behavioral tests validate user scenarios; integration tests validate internal feature combinations
- **vs Construct**: Construct tests validate bash syntax handling; integration tests validate feature interactions within constructs
