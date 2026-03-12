# Behavioral Tests

User-facing behavior and security validation (129 tests).

## Philosophy

Tests WHAT the system does from a user perspective. Maps directly to OpenSpec specifications.

These tests validate the public API and end-to-end behavior that users (including LLMs) depend on. When these tests fail, it indicates a user-visible regression or security issue.

## Test Files

- `test-file-operations.el` (49 tests) - File operation extraction
- `test-security-validator.el` (60 tests) - Security validation
- `test-command-injection.el` (7 tests) - Command injection detection
- `test-backward-compatibility.el` (13 tests) - Backward compatibility

Total: 129 tests (23.5% of suite)

## Running Tests

```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/behavioral
```

## What These Tests Cover

**File Operations Extraction:**
- End-to-end extraction from complex bash commands
- Extraction from all command patterns (pipelines, chains, substitutions)
- Glob pattern handling, variable resolution, confidence scoring
- Metadata completeness and result format validation

**Security Validation:**
- Dangerous operations (rm -rf /, dd to devices, fork bombs)
- Command injection patterns (semicolons, pipes, backticks)
- Path traversal detection (../../, symlinks, devices)
- Network operations (curl, wget, ssh, nc)
- Severity classification and metadata structure

**Command Injection:**
- Injection vector detection in user-provided arguments
- Validation of command boundaries and quote handling
- Multi-line command injection patterns

**Backward Compatibility:**
- Compatibility with legacy APIs and data formats
- Feature detection for optional functionality
- Graceful degradation when features unavailable

## Relationship to Other Test Categories

- **vs Unit Tests**: Behavioral tests validate end-to-end behavior; unit tests validate individual functions
- **vs Integration Tests**: Behavioral tests focus on user-facing scenarios; integration tests focus on component interactions
- **vs Corpus Tests**: Behavioral tests validate specific scenarios; corpus tests provide data-driven coverage

When behavioral tests fail, check unit tests to isolate the specific component causing the regression.
