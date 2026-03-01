## Components

### Command Semantics Database
A declarative mapping from command names to their file operation patterns. Defines:
- Operation type(s) performed (`:read`, `:write`, `:delete`, `:modify`, `:create`, `:create-or-modify`, `:append`)
- Source of file paths (`:positional-args`, `:redirections`, `:exec-blocks`)
- Argument indexing rules (single index, ranges, all-but-last)
- Flag-dependent behavior (e.g., `sed -i` changes operation from `:read` to `:modify`)
- Subcommand-specific semantics (e.g., `git add` vs `git checkout`)
- Multi-operation commands (e.g., `cp` has `:read` source + `:write` destination)

**Responsibilities:**
- Provide lookup function: command name → semantics plist
- Support at least 20 core commands (cat, cp, mv, rm, grep, sed, find, etc.)
- Handle unknown commands gracefully (return nil or empty semantics)

**Data Structure:** Hash table or alist mapping command names to semantics plists.

### File Operations Extractor
Analyzes parsed bash commands to extract all file operations with metadata.

**Responsibilities:**
- Extract operations from positional arguments using command semantics database
- Extract operations from redirections (>, >>, <, 2>, etc.) with high confidence
- Extract operations from find -exec blocks (treat exec separately)
- Handle multi-command constructs (pipelines, chains) by processing each command
- Track and resolve variable references ($VAR, ${VAR}) against provided context
- Preserve glob patterns (*, **, ?, []) without expansion
- Deduplicate operations (same file + operation type)
- Assign confidence levels (`:high`, `:medium`, `:low`, `:unknown`)
- Mark indirect operations from nested commands with `:indirect t`
- Return list of operation plists with file path, operation type, confidence, source, and metadata

**Key Functions:**
- `jf/bash-extract-file-operations` - Main entry point accepting parsed command + optional variable context
- Internal helpers for extracting from positional args, redirections, exec blocks
- Variable resolution and tracking for command chains (VAR=value && ...)

### Variable Context Manager
Tracks and resolves variable references in bash commands.

**Responsibilities:**
- Accept optional variable context hash table (var name → value)
- Track simple assignments (VAR=value) in command chains
- Resolve variable references in file paths
- Mark unresolved variables with `:unresolved t` metadata
- Support partial resolution (e.g., "$WORKSPACE/$FILE" with only WORKSPACE defined)

**Integration:** Works with File Operations Extractor and Security Validator.

### Glob Pattern Matcher
Matches file paths against glob patterns without filesystem access.

**Responsibilities:**
- Convert glob patterns to regex (*, **, ?, [])
- Split paths and patterns into segments for matching
- Handle ** (recursive wildcard) consuming 0-to-N segments
- Handle * (single-level wildcard) not crossing directory boundaries
- Handle character classes ([0-9], [abc])
- Escape regex special characters in literal path segments

**Key Function:** `jf/bash-glob-match-p` accepting path and pattern strings.

### Security Validator
Validates bash commands against sandbox rules with operation-specific permissions.

**Responsibilities:**
- Define sandbox rules as list of plists (patterns + allowed operations)
- Match file paths against rules using glob pattern matcher
- Validate each operation type separately (read vs write vs delete)
- Apply first matching rule (order matters)
- Reject `cd` commands with clear guidance
- Reject commands with unresolved variables
- Report violations with details (file, operation, matched rule, reason)
- Report unhandled operations (low confidence, unresolved vars) separately
- Support optional stricter policies for indirect operations
- Return validation result plist with `:allowed`, `:violations`, `:unhandled`

**Key Function:** `jf/bash-sandbox-check` accepting command string, rules, and optional variable context.

### Parser Extension (Existing Component)
The existing bash-parser module with backward-compatible extensions.

**Responsibilities:**
- Maintain existing `jf/bash-parse` API unchanged
- Provide parsed data structures (`:redirections`, `:exec-blocks`, `:positional-args`)
- Detect command injection patterns (bash -c, python -c, sh -c, env -S)
- Mark command injection with `:command-injection t` metadata
- Extract nested command strings for recursive parsing
- Strip outer quotes from nested commands before parsing

**No Breaking Changes:** All existing parser consumers continue to work.

## Interfaces

### Command Semantics Database → File Operations Extractor
```elisp
(jf/bash-lookup-command-semantics "cp")
;; => (:operations ((:type :read :indices (0 . -2))
;;                  (:type :write :indices (-1)))
;;     :source :positional-args)
```

### Parser → File Operations Extractor
Extractor receives parsed command structure from `jf/bash-parse`:
```elisp
(jf/bash-parse "cat file.txt > output.txt")
;; => (:type :simple-command
;;     :command "cat"
;;     :positional-args ("file.txt")
;;     :redirections ((:type :output :target "output.txt")))
```

### File Operations Extractor → Security Validator
```elisp
(jf/bash-extract-file-operations parsed-command :var-context '((WORKSPACE . "/workspace")))
;; => ((:file "/workspace/file.txt" :operation :read :confidence :high :source :positional-arg)
;;     (:file "output.txt" :operation :write :confidence :high :source :redirection))
```

### Security Validator → Glob Pattern Matcher
```elisp
(jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/**/*.el")
;; => t
```

### Variable Context Manager → File Operations Extractor
Variable context passed as optional parameter:
```elisp
:var-context '((WORKSPACE . "/workspace") (PROJECT . "emacs"))
```

Assignments tracked during chain processing:
```elisp
"DIR=/tmp && cat $DIR/file.txt"
;; Extractor builds context: ((DIR . "/tmp"))
```

### Public API (New Functions)
```elisp
;; Main entry point for file operations
(jf/bash-extract-file-operations parsed-command &optional var-context)

;; Security validation
(jf/bash-sandbox-check command-string rules &optional var-context)

;; Glob matching (used internally but may be exposed)
(jf/bash-glob-match-p path pattern)
```

## Boundaries

### In Scope
- File operation extraction from parsed bash commands
- Security validation against glob-pattern allowlists
- Variable resolution with provided context
- Operation-specific permissions (read/write/delete)
- Glob pattern matching (*, **, ?, [])
- Command injection detection and nested command parsing
- Confidence levels for operations
- Violation and unhandled operation reporting

### Out of Scope
- Filesystem access or path expansion (stay abstract)
- Shell variable expansion beyond simple VAR=value assignments (no ${VAR:-default}, no array expansion)
- Command execution or side effects
- Working directory tracking (reject `cd` instead)
- Complex shell features (aliases, functions, brace expansion)
- Real-time monitoring or sandboxing enforcement (library provides validation only)
- Glob expansion to actual filenames (patterns preserved as-is)
- Network operations or non-file resources

### Internal vs External
**Internal:**
- Command semantics data structure
- Glob-to-regex conversion logic
- Path segment matching algorithm
- Variable tracking in chains

**External (Public API):**
- `jf/bash-extract-file-operations` - Main extraction function
- `jf/bash-sandbox-check` - Main validation function
- Potentially: `jf/bash-glob-match-p` if useful for callers

### Integration Points
- Depends on existing `jf/bash-parse` function and its output structure
- Extends `config/experiments/bash-parser/bash-parser.org` literate source
- Called by LLM agent sandbox systems (external consumers)
- May integrate with gptel-agent bash tools for permission checking

## Testing Approach

### Test Framework
**ERT (Emacs Regression Testing)** - Native Emacs test framework already used in codebase. Provides:
- `ert-deftest` for test definitions
- `should` and `should-not` assertions
- `should-error` for error testing
- Integration with literate programming workflow
- Interactive test execution with `M-x ert`

### Test Organization
**Co-located with source:** Tests live in `config/experiments/bash-parser/test/` directory alongside the module.

**File structure:**
```
config/experiments/bash-parser/
├── bash-parser.org           # Literate source (includes all components)
├── bash-parser.el            # Tangled elisp
└── test/
    ├── test-command-semantics.el     # Tests for semantics database
    ├── test-file-operations.el       # Tests for extraction logic
    ├── test-glob-matching.el         # Tests for glob pattern matcher
    ├── test-security-validator.el    # Tests for sandbox validation
    └── test-parser-extension.el      # Tests for backward compatibility
```

**Test file generation:** Tests may be written in separate `.org` files and tangled, or written directly as `.el` files.

### Naming Conventions
**Test files:** `test-<component>.el` (e.g., `test-command-semantics.el`)

**Test functions:** `test-<component>-<scenario-slug>` following spec scenario names.

Examples:
- `test-semantics-lookup-simple-read-command` - Maps to "Lookup simple read command" scenario
- `test-extraction-simple-read-command` - Maps to "Simple read command" scenario
- `test-glob-single-level-wildcard-match` - Maps to "Single-level wildcard match" scenario
- `test-security-allow-command-matching-rules` - Maps to "Allow command matching rules" scenario

### Running Tests
**Run all bash-parser tests:**
```elisp
(ert-run-tests-batch-and-exit "^test-")
```

**Run tests for specific component:**
```elisp
(ert "^test-semantics-")     ;; Command semantics tests
(ert "^test-extraction-")    ;; File operations tests
(ert "^test-glob-")          ;; Glob matching tests
(ert "^test-security-")      ;; Security validation tests
```

**Interactive test runner:**
```elisp
M-x ert RET ^test-semantics- RET
```

**Tangle and run from command line:**
```bash
./bin/tangle-org.sh config/experiments/bash-parser/bash-parser.org
emacs -batch -l config/experiments/bash-parser/bash-parser.el \
              -l config/experiments/bash-parser/test/test-command-semantics.el \
              -f ert-run-tests-batch-and-exit
```

### Test Patterns
**Unit tests for individual functions** - Test components in isolation:

1. **Command Semantics Database Tests:** Test lookup function with various commands
   ```elisp
   (ert-deftest test-semantics-lookup-simple-read-command ()
     "Scenario: bash-command-semantics § 'Lookup simple read command'"
     (let ((result (jf/bash-lookup-command-semantics "cat")))
       (should (eq (plist-get result :operation) :read))
       (should (eq (plist-get result :source) :positional-args))))
   ```

2. **Glob Pattern Matching Tests:** Test path-pattern matching without dependencies
   ```elisp
   (ert-deftest test-glob-single-level-wildcard-match ()
     "Scenario: bash-sandbox-security § 'Single-level wildcard match'"
     (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/*.txt")))
   ```

3. **Variable Resolution Tests:** Test variable tracking and resolution
   ```elisp
   (ert-deftest test-variable-resolve-declared-variable ()
     "Scenario: bash-file-operations § 'Resolve declared variable'"
     (let ((context '((WORKSPACE . "/workspace"))))
       (should (equal (jf/bash-resolve-variable "$WORKSPACE/file.txt" context)
                      "/workspace/file.txt"))))
   ```

4. **Operation Extraction Tests:** Test extraction from different sources (positional args, redirections, exec blocks)
   ```elisp
   (ert-deftest test-extraction-simple-read-command ()
     "Scenario: bash-file-operations § 'Simple read command'"
     (let* ((parsed (jf/bash-parse "cat /workspace/foo.txt"))
            (ops (jf/bash-extract-file-operations parsed)))
       (should (= (length ops) 1))
       (should (equal (plist-get (car ops) :file) "/workspace/foo.txt"))
       (should (eq (plist-get (car ops) :operation) :read))
       (should (eq (plist-get (car ops) :confidence) :high))))
   ```

**Integration tests for full command validation** - Test complete pipeline:

```elisp
(ert-deftest test-security-allow-command-matching-rules ()
  "Scenario: bash-sandbox-security § 'Allow command matching rules'"
  (let ((rules '((:patterns ("/workspace/**") :operations (:read :write)))))
    (let ((result (jf/bash-sandbox-check "cat /workspace/file.txt" rules)))
      (should (plist-get result :allowed))
      (should (null (plist-get result :violations))))))
```

**No property-based or characterization tests selected.**

### Scenario Mapping
**One test per scenario with spec reference in comments** - Each scenario from specs becomes a dedicated test function.

**Comment format:**
```elisp
(ert-deftest test-semantics-lookup-simple-read-command ()
  "Scenario: bash-command-semantics § 'Lookup simple read command'"
  ;; Test body
  )
```

The docstring references the spec capability and scenario title, providing clear traceability from test → spec requirement.

**Grouping:** Tests are grouped by component (semantics, extraction, glob, security) via file organization and function naming prefix.

**Coverage:** Every scenario in the 4 spec files gets a corresponding test:
- `bash-command-semantics/spec.md` scenarios → `test-semantics-*` tests
- `bash-file-operations/spec.md` scenarios → `test-extraction-*` or `test-variable-*` tests
- `bash-sandbox-security/spec.md` scenarios → `test-security-*` or `test-glob-*` tests
- `bash-parser/spec.md` scenarios → `test-parser-extension-*` tests

**Success criteria:** A test passes when the actual behavior matches the THEN conditions in the scenario. Failure means the implementation doesn't meet the spec requirement.

## Dependencies

### Internal Dependencies
- **Existing bash-parser module** (`config/experiments/bash-parser/bash-parser.org`)
  - Provides `jf/bash-parse` function
  - Provides parsed data structures (`:redirections`, `:exec-blocks`, `:positional-args`)
  - Already handles tree-sitter parsing, redirections, exec blocks

### External Dependencies
- **cl-lib** - Common Lisp extensions for Emacs (plists, loops, destructuring)
- **tree-sitter-bash** - Already a dependency of existing bash-parser

### No New External Dependencies
All functionality built on existing parser infrastructure. No additional packages required.

## Constraints

### Performance
- **Glob matching must be fast** - Security checks may be called frequently by LLM agents, so pattern matching needs to be O(n) in path segments, not exponential.
- **No filesystem access** - All matching purely string-based, no calls to `file-exists-p` or directory expansion.
- **Efficient semantics lookup** - Use hash table for command semantics (O(1) lookup) rather than linear search.

### Compatibility
- **Backward compatibility required** - Existing `jf/bash-parse` API must remain unchanged. All new functionality via new functions.
- **No breaking changes to parsed data structures** - Can add new optional fields (`:command-injection`, `:indirect`), but existing fields must preserve their types and semantics.

### Security
- **Fail-safe defaults** - Unknown commands return empty/nil semantics (low confidence). Unresolved variables cause rejection. Unhandled operations block approval.
- **Conservative cd rejection** - Rather than track working directory state, reject all `cd` commands and guide users to use absolute paths.
- **Explicit indirect operation handling** - Mark nested command operations as `:indirect t` so callers can apply stricter policies.

### Scope Limitations
- **No complex shell expansion** - Only handle simple VAR=value assignments. No ${VAR:-default}, no arrays, no brace expansion, no command substitution.
- **No command execution** - Library is purely analytical, never invokes bash or executes commands.
- **Patterns not expanded** - Glob patterns like `*.txt` are preserved and matched against, never expanded to actual filenames.

### Literate Programming
- **All code in bash-parser.org** - New sections added to literate source file, tangled to `bash-parser.el`.
- **Validation required** - Must run `./bin/tangle-org.sh` after each significant addition to catch paren errors early.
- **Test files co-located** - Tests live in `test/` subdirectory alongside module for easy discovery.
