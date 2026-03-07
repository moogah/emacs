# Bash Parser Test Suite

Organized test suite for the bash-parser experiment with 683 tests across 5 categories.

## Test Organization

### Behavioral Tests (`behavioral/`)
User-facing behavior and security validation (129 tests, 23.5%)

Tests WHAT the system does from a user perspective. Maps to OpenSpec specifications.

**Run:** `./bin/run-tests.sh -d config/experiments/bash-parser/test/behavioral`

### Unit Tests (`unit/`)
Architecture layer testing (240 tests, 43.8%)

Tests HOW the system works, organized by architectural layer:
- `core/` - Low-level parsing primitives (35 tests)
- `semantic/` - Command understanding and variable resolution (146 tests)
- `analysis/` - Recursive parsing and extensions (59 tests)

**Run:** `./bin/run-tests.sh -d config/experiments/bash-parser/test/unit`

### Integration Tests (`integration/`)
Multi-component interactions (51 tests, 9.3%)

Tests cross-layer interactions and feature combinations.

**Run:** `./bin/run-tests.sh -d config/experiments/bash-parser/test/integration`

### Construct Tests (`construct/`)
Bash language construct-specific (94 tests, 17.2%)

Tests specific bash syntax constructs: loops, conditionals, heredocs, directory context.

**Run:** `./bin/run-tests.sh -d config/experiments/bash-parser/test/construct`

### Corpus Tests (`corpus/`)
Data-driven test generation (34 runner tests, 6.2% + 135 generated)

Corpus data files drive test generation for comprehensive coverage.

**Run:** `./bin/run-tests.sh -d config/experiments/bash-parser/test/corpus`

## Running Tests

**All tests:**
```bash
./bin/run-tests.sh -d config/experiments/bash-parser
```

**Specific category:**
```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/behavioral
```

**With snapshot:**
```bash
./bin/run-tests.sh -d config/experiments/bash-parser --snapshot
```

## Test Statistics

- Total tests: 683 (548 explicit + 135 corpus-generated)
- Pass rate: 100%
- Coverage: Comprehensive (see docs/MIGRATION-COVERAGE-REPORT.md)

## Documentation

- `docs/REDUNDANCY-ANALYSIS.md` - Redundancy analysis and decisions
- `docs/MIGRATION-COVERAGE-REPORT.md` - Complete migration report
- `DEPRECATED-TESTS.md` - Deprecated tests removed during migration (legacy location)

## Test Organization Philosophy

Tests are organized by **purpose and architectural layer**, not by file structure:

- **Behavioral** - User-facing scenarios from OpenSpec specifications (WHAT)
- **Unit** - Module tests organized by architecture layer (HOW)
  - Core: Low-level parsing primitives
  - Semantic: Command understanding and variable resolution
  - Analysis: Recursive parsing and parser extensions
- **Integration** - Multi-component feature interactions
- **Construct** - Bash language construct-specific tests
- **Corpus** - Data-driven test generation for comprehensive coverage

This organization makes it easy to:
1. Find tests by purpose (behavioral scenarios vs implementation details)
2. Navigate by architectural layer (core → semantic → analysis)
3. Understand test scope (unit vs integration vs end-to-end)
4. Run targeted test subsets during development
