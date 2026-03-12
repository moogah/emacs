## Components

### Semantics Database Extension
**Responsibility**: Store interpreter command semantics (python, node, bash, go, etc.) mapping them to `:execute` operation type.

**Location**: `bash-parser-semantics.el`

**Entries**:
- Simple interpreters: python, python3, node, bash, sh, zsh (`:execute` on index 0)
- Shell built-ins: source, . (`:execute` on index 0)
- Subcommand-based: go (run/test → `:execute`, build → `:read`)

### Self-Execution Detector
**Responsibility**: Identify when command-name itself is a path-based reference to an executable file.

**Location**: `bash-parser-file-ops.el` (new function: `jf/bash--command-executes-self-p`)

**Detection Logic**:
- Check if command-name starts with `./`, `/`, or `../`
- Return confidence `:low` for heuristic detection
- Add `:self-executing t` metadata flag

### Script Arguments Extractor
**Responsibility**: Capture positional arguments following the executed script as `:script-args` metadata.

**Location**: `bash-parser-file-ops.el` (enhancement to extraction logic)

**Behavior**:
- For interpreter commands: capture all positional args after index 0
- For self-executing commands: capture all positional args
- Return empty list `()` when no arguments present

### File Operations Extraction Orchestrator
**Responsibility**: Coordinate extraction from all sources (redirections, positional args, exec blocks, self-execution).

**Location**: `bash-parser-file-ops.el` (extend `jf/bash-extract-file-operations`)

**Integration**:
- After existing extraction sources, check for self-execution
- Add execute operations with appropriate metadata
- Maintain existing deduplication and confidence logic

## Interfaces

### Semantics Database Lookup
**Function**: `jf/bash-lookup-command-semantics`

**Input**: Command name (string)
**Output**: Semantics plist with `:operations`, `:flag-handlers`, `:subcommand-handlers`

**New Behavior**: Return `:execute` operation type for interpreter commands with `:index 0` to extract only first positional arg.

### Self-Execution Detection
**Function**: `jf/bash--command-executes-self-p` (new)

**Input**: Command name (string)
**Output**: Boolean (t if path-based, nil otherwise)

**Contract**: Returns t for paths starting with `./`, `/`, `../`. Returns nil for regular command names.

### File Operations Extraction
**Function**: `jf/bash-extract-file-operations`

**Input**: Parsed command (plist), optional variable context (alist)
**Output**: List of operation plists

**Enhanced Output**: Operations now include:
- `:operation :execute` (new type)
- `:script-args` (list of strings, empty list if no args)
- `:self-executing t` (when detected via command-name)

**Backward Compatibility**: Existing operation types (`:read`, `:write`, `:delete`, etc.) unchanged.

## Boundaries

### In Scope
- Interpreter commands executing first positional argument (python, node, bash, sh, go, source, .)
- Self-executing path-based commands (`./script.sh`, `/usr/bin/tool`)
- Script arguments metadata capture for all execute operations
- Execute operations in pipelines, chains, and nested commands
- Variable resolution for executed script paths
- Confidence levels: `:high` for known interpreters, `:low` for path-based detection

### Out of Scope (Deferred)
- Multiple script files in single command (`python script1.py script2.py script3.py`)
- Complex interpreter flags (`python -m module`, `node --inspect`, `python -u`)
- Compiled languages (`gcc`, `javac`, `rustc`)
- Distinguishing between compiled binaries and scripts in self-execution
- Static analysis of script files to determine their file operations
- Script argument type detection (file vs URL vs other)

### Internal
- Semantics database entries (interpreter definitions)
- Self-execution detection heuristics
- Script arguments extraction logic
- Operation confidence assignment

### External
- Command parsing (provided by `jf/bash-parse`)
- Variable resolution (provided by existing variable system)
- Security validation (consumers of extracted operations)
- Operation deduplication (existing logic in extraction)

## Testing Approach

### Test Framework
**ERT (Emacs Lisp Regression Testing)** - Standard Emacs test framework, consistent with existing bash-parser tests.

### Test Organization
**Location**: `config/experiments/bash-parser/test/test-script-execution.el` (new file)

**Corpus**: `config/experiments/bash-parser/test/script-execution-corpus.el` (new file)

**Structure**:
- Main test file contains ERT test functions
- Corpus file contains comprehensive test data (commands, expected operations)
- Follow existing pattern from `test-file-operations.el` and `file-operations-corpus.el`

### Naming Conventions

**Test File**: `test-script-execution.el`

**Test Functions**: `test-script-execution-<scenario>` format
- Example: `test-script-execution-python-interpreter`
- Example: `test-script-execution-self-executing-relative-path`
- Example: `test-script-execution-script-args-capture`

**Corpus File**: `script-execution-corpus.el`

**Corpus Variable**: `jf/bash-script-execution-test-corpus` (list of test case plists)

### Running Tests

**All tests**:
```bash
make test
./bin/run-tests.sh
```

**Script execution tests only**:
```bash
make test-pattern PATTERN='^test-script-execution-'
./bin/run-tests.sh -p '^test-script-execution-'
```

**With snapshot**:
```bash
./bin/run-tests.sh -p '^test-script-execution-' -s
```

**Interactive (Emacs)**:
```elisp
(ert "test-script-execution-*")  ; Run all script execution tests
(ert 'test-script-execution-python-interpreter)  ; Run specific test
```

### Test Patterns

**Standard Test Structure**:
```elisp
(ert-deftest test-script-execution-<scenario> ()
  "Scenario: script-execution § '<scenario name>'

Test description."
  (let* ((parsed (jf/bash-parse "python script.py"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "script.py"))
    (should (eq (plist-get (car ops) :operation) :execute))
    (should (eq (plist-get (car ops) :confidence) :high))
    (should (equal (plist-get (car ops) :script-args) ()))))
```

**Corpus-Based Test Structure**:
```elisp
(ert-deftest test-script-execution-corpus ()
  "Run all script execution corpus test cases."
  (dolist (test-case jf/bash-script-execution-test-corpus)
    (let* ((command (plist-get test-case :command))
           (expected (plist-get test-case :expected-ops))
           (parsed (jf/bash-parse command))
           (ops (jf/bash-extract-file-operations parsed)))
      (should (equal ops expected)))))
```

**Variable Resolution Tests**:
- Use `var-context` parameter to `jf/bash-extract-file-operations`
- Test resolved and unresolved variable scenarios

**No Mocking Required**:
- Tests use actual parser and extraction functions
- No external dependencies to mock
- Pure function testing with input/output verification

### Scenario Mapping

Each spec scenario maps to one or more test functions:

**1:1 Mapping** (most scenarios):
- Spec: "Python interpreter execution" → Test: `test-script-execution-python-interpreter`
- Spec: "Self-executing relative path" → Test: `test-script-execution-self-executing-relative-path`

**1:Many Mapping** (coverage scenarios):
- Spec: "Interpreter command coverage" → Multiple tests for python, python3, node, bash, sh, zsh, source, .

**Many:1 Mapping** (corpus tests):
- Multiple simple scenarios → Single `test-script-execution-corpus` test with comprehensive test data

**Scenario Reference Format**:
Test docstrings include spec references:
```elisp
"Scenario: script-execution § 'Python interpreter execution'"
```

This enables traceability from specs to tests.

### Test Helpers

**Existing Helpers** (reuse from test-file-operations.el):
- Operation comparison helpers (if needed)
- Plist assertion helpers (if needed)

**New Helpers** (if needed):
- `assert-execute-operation` - Helper to validate execute operation structure
- `make-execute-operation` - Helper to construct expected execute operations

**Corpus Helpers**:
- Corpus data structure follows existing `file-operations-corpus.el` pattern
- Each test case: `:command` (string), `:expected-ops` (list of plists), `:note` (description)

## Dependencies

**Internal Dependencies**:
- `bash-parser-core.el` - Command parsing via `jf/bash-parse`
- `bash-parser-semantics.el` - Command semantics database and lookup
- `bash-parser-variables.el` - Variable resolution system
- `bash-parser-extensions.el` - Indirect operation marking (for nested commands)

**External Dependencies**:
- `cl-lib` - Common Lisp emulation library (already used)
- `ert` - Emacs test framework (already used)

**No New External Dependencies Required**.

## Constraints

### Performance
- Extraction must remain fast (no significant overhead for execute detection)
- Self-execution check is simple string prefix matching (O(1))
- Semantics database lookup is hash table access (O(1))

### Compatibility
- Backward compatible: existing operation types unchanged
- Existing tests continue to pass
- No breaking changes to extraction API signature

### Emacs Version
- Minimum Emacs 27.1 (existing constraint from bash-parser)
- No new Emacs features required

### Scope Constraints
- Only detect execution at command level, not analyze script contents
- Script arguments captured as metadata only, no type inference
- Confidence levels are heuristic, not guarantees
- Path-based execution detection is simple (no filesystem access)

### Security Constraints
- Execute detection must be fail-safe (false positives acceptable, false negatives dangerous)
- Low confidence for uncertain cases (self-execution) to enable conservative policies
- Unresolved variables marked appropriately to prevent security bypasses
