# Unit Tests

Architecture layer testing (240 tests).

## Organization by Layer

Unit tests are organized by the bash-parser's architectural layers, from low-level primitives up to analysis:

### Core Layer (`core/`)
Low-level parsing primitives and utilities (35 tests)

- `test-glob-matching.el` (35 tests) - Glob pattern matching

**Focus:** Basic building blocks used by higher layers. Tests individual utility functions in isolation.

### Semantic Layer (`semantic/`)
Command understanding and variable resolution (146 tests)

- `test-command-semantics.el` (68 tests) - Command semantics database
- `test-bash-parser-semantics.el` (27 tests) - Parser semantic integration
- `test-semantics-validation.el` (11 tests) - Semantic validation
- `test-variable-resolution-unit.el` (36 tests) - Variable resolution
- `test-variable-chain-ampersand.el` (4 tests) - Variable chain operators

**Focus:** Understanding what commands do and resolving their arguments. Tests semantic databases and variable handling.

### Analysis Layer (`analysis/`)
Recursive parsing and parser extensions (59 tests)

- `test-bash-parser-recursive.el` (25 tests) - Recursive parser
- `test-parser-extension.el` (26 tests) - Parser extensions
- `test-input-validation.el` (8 tests) - Input validation

**Focus:** Advanced parsing features and extensibility. Tests recursive command parsing and extension mechanisms.

Total: 240 tests (43.8% of suite)

## Running Tests

**All unit tests:**
```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/unit
```

**By layer:**
```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/unit/core
./bin/run-tests.sh -d config/experiments/bash-parser/test/unit/semantic
./bin/run-tests.sh -d config/experiments/bash-parser/test/unit/analysis
```

## Testing Philosophy

Unit tests validate **individual functions and modules** in isolation. Each test:
- Tests one specific function or small module
- Uses minimal dependencies (no full parser pipeline)
- Provides clear diagnostic value when it fails

**When to add unit tests:**
- New utility functions (core layer)
- Command semantic rules (semantic layer)
- Parser extension mechanisms (analysis layer)

**When to use other test categories:**
- End-to-end scenarios → Behavioral tests
- Feature combinations → Integration tests
- Language constructs → Construct tests
- Data-driven coverage → Corpus tests

## Relationship to Behavioral Tests

Unit tests answer "HOW does this function work?"
Behavioral tests answer "WHAT does the system do for this scenario?"

Example:
- Unit: `test-command-semantic-read-cat` - Tests that `cat` command maps to `:read` semantic
- Behavioral: `test-extraction-simple-read-command` - Tests that `cat file.txt` produces `:read` operation with path "file.txt"

Both are necessary - unit tests isolate function behavior, behavioral tests validate end-to-end integration.
