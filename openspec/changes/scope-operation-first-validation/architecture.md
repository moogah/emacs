# Architecture: Operation-First Validation

## Components

### 1. Validation Pipeline Orchestrator
**Location:** `config/gptel/tools/scope-shell-tools.org` - `jf/gptel-scope--validate-command-semantics`

**Responsibilities:**
- Orchestrate multi-stage validation flow with early exit on failure
- Thread parsed command and semantics through validation stages
- Return structured errors with context for LLM guidance

**Key functions:**
- `jf/gptel-scope--validate-command-semantics` - Main pipeline entry point
- Stage coordination with cl-block for early exit

### 2. Deny List Validator
**Location:** `config/gptel/tools/scope-shell-tools.org` - Stage 2 validation

**Responsibilities:**
- Check all pipeline commands against bash_tools.deny list
- Extract command names from pipeline segments
- Return command_denied errors with position information

**Key functions:**
- `jf/gptel-scope--extract-pipeline-commands` - Extract all commands from pipeline
- `jf/gptel-scope--validate-pipeline-commands` - Validate each command against deny list

### 3. No-Op Allowance Checker
**Location:** `config/gptel/tools/scope-shell-tools.org` - New Stage 4

**Responsibilities:**
- Check if extracted semantics contain zero file operations
- Short-circuit validation for no-op commands
- Enable version checks and introspection commands by default

**Key functions:**
- `jf/gptel-scope--check-no-op` - New function to check for empty file operations list
- Returns success if `:filesystem` domain is empty or contains no operations

### 4. File Operation Validator
**Location:** `config/gptel/tools/scope-shell-tools.org` - Stage 5 (renamed from Stage 4)

**Responsibilities:**
- Validate extracted file operations against operation-specific path patterns
- Check :read, :write, :execute, :modify, :delete operations
- Return path_out_of_scope errors with operation detail

**Key functions:**
- `jf/gptel-scope--validate-file-operations` - Iterate operations and validate
- `jf/gptel-scope--validate-operation` - Check single operation against paths config
- `jf/gptel-scope--path-matches-any-pattern-p` - Glob pattern matching

### 5. Scope Profile Loader
**Location:** `config/gptel/scope-profiles.org`

**Responsibilities:**
- Load and parse scope profiles from YAML (no categories section)
- Expand variables (${project_root})
- Write session-specific scope.yml

**Key functions:**
- `jf/gptel-scope-profile--load` - Parse YAML profile
- `jf/gptel-scope-profile--write-scope-yml` - Write to session directory
- No longer handles categories section (BREAKING)

### 6. Bash-Parser Integration
**Location:** `config/bash-parser/` (dependency)

**Responsibilities:**
- Parse bash commands into AST with tokens
- Extract semantic operations via plugin system
- Provide parse completeness flag

**Key interfaces:**
- `jf/bash-parse` - Main parsing function
- `jf/bash-extract-semantics` - Semantic extraction with plugins

## Interfaces

### Pipeline → Deny List Validator
**Input:** Parsed command with `:all-commands` list
**Output:** `nil` (success) or error plist with `:error "command_denied"`
**Contract:** Deny list checked before semantics extraction

### Pipeline → No-Op Checker
**Input:** Semantics plist with `:domains` containing `:filesystem` operations
**Output:** `nil` (success, command allowed) or continue to next stage
**Contract:** Returns success immediately if no file operations, otherwise passes through

### Pipeline → File Operation Validator
**Input:** File operations list from `:filesystem` domain, paths config
**Output:** `nil` (success) or error plist with `:error "path_out_of_scope"`
**Contract:** Each operation validated independently against operation-specific patterns

### Scope Profile Loader → Session Scope
**Input:** Profile name or inline scope defaults
**Output:** scope.yml written to session directory
**Contract:** Categories section not written (removed from schema)

### Bash-Parser → Pipeline
**Input:** Command string
**Output:** Parsed command plist with `:parse-complete`, `:all-commands`, semantics with `:domains`
**Contract:** Parser provides reliable operation extraction for no-op detection

## Boundaries

### In Scope
- Validation pipeline refactoring (remove category validation, add no-op check)
- Scope profile schema changes (remove categories)
- Deny list validation (simplified from category-based)
- No-op command allowance (new capability)
- File operation validation (existing, unchanged)
- Test updates (remove category tests, add no-op tests)

### Out of Scope
- Cloud authentication validation (unchanged, remains Stage 6)
- Parse completeness enforcement (unchanged, remains Stage 1)
- Coverage threshold warnings (unchanged, remains Stage 7)
- Bash-parser semantic extraction improvements (dependency, not changed)
- Scope expansion UI (unchanged)
- Tool registration and preset system (unchanged)

### Internal (within this change)
- Pipeline stage reordering (category validation → no-op check)
- Deny list validation logic
- No-op detection logic
- Scope profile schema validation

### External Dependencies
- Bash-parser (`config/bash-parser/`) - Semantic extraction with plugins
- YAML parsing (`yaml.el`) - Scope profile loading
- Scope expansion UI (`jf/gptel-scope-prompt-expansion`) - User approval flow
- Preset registration (`config/gptel/preset-registration.org`) - Scope defaults storage

## Testing Approach

### Test Framework
**Buttercup** - BDD-style testing framework for Emacs Lisp.

**Why Buttercup:**
- `describe`/`it`/`expect` syntax maps naturally to spec scenarios
- Built-in setup/teardown (`before-each`, `after-each`) for test fixtures
- Spy system for mocking bash-parser calls and verifying interactions
- Better support for behavioral tests with nested contexts
- Preferred for new test suites (per CLAUDE.md testing guidelines)

**Framework setup:**
- Already installed via `straight.el` in `config/core/testing.el`
- Test discovery via `jf/test-load-all-buttercup-test-files`
- Integration with Makefile and `./bin/run-tests.sh`

### Test Organization
**Location:** `config/gptel/tools/test/`

**Structure:**
```
config/gptel/tools/test/
├── unit/
│   ├── no-op-allowance-spec.el          # New: No-op command allowance tests
│   ├── pipeline-validation-spec.el       # Modified: Remove category tests
│   └── validators-spec.el                # Existing: File operation validation
├── integration/
│   ├── test-file-paths.el               # Existing: Operation-specific path validation (ERT)
│   ├── test-pipelines.el                # Existing: Pipeline command extraction (ERT)
│   └── test-schema.el                   # Existing: Scope schema loading (ERT)
└── behavioral/
    └── operation-first-workflow-spec.el  # New: End-to-end validation workflow
```

**Naming conventions:**
- Buttercup tests: `*-spec.el` files
- ERT tests: `test-*.el` files (legacy, maintain as-is)
- New tests use Buttercup, existing ERT tests remain unchanged

### Naming Conventions

**Test files:**
- Buttercup: `<component>-spec.el` (e.g., `no-op-allowance-spec.el`)
- ERT: `test-<component>.el` (e.g., `test-file-paths.el`)

**Test functions:**
- Buttercup: `(describe "Component" (it "does something" ...))`
  - Top-level: Component or capability name
  - Nested: Specific behavior or scenario
  - Example: `(describe "No-op allowance" (it "allows python3 --version" ...))`
- ERT: `(ert-deftest test-component-behavior () ...)`

**Scenario mapping:**
- Scenario name from spec becomes `it` description
- Example: Spec scenario "Version check command allowed" → `(it "allows version check commands" ...)`

### Running Tests

**All tests:**
```bash
make test-buttercup                      # All Buttercup tests
./bin/run-tests.sh -f buttercup          # Via CLI wrapper
```

**Directory-scoped:**
```bash
make test-buttercup-directory DIR=config/gptel/tools/test/unit
./bin/run-tests.sh -d config/gptel/tools/test -f buttercup
```

**Interactive:**
```elisp
C-c t    ; Open test transient menu
         ; Select Buttercup option → choose scope
```

**Maintain existing ERT tests:**
```bash
make test-directory DIR=config/gptel/tools/test  # Runs ERT tests
./bin/run-tests.sh -d config/gptel/tools/test -f ert
```

### Test Patterns

**Setup/Teardown:**
```elisp
(describe "Validation pipeline"
  (before-each
    (setq test-scope-config (make-test-scope-config))
    (spy-on 'jf/bash-parse :and-return-value test-parsed-command))

  (after-each
    (setq test-scope-config nil))

  (it "allows no-op commands"
    (expect (validate-command "python3 --version" "/tmp" test-scope-config)
            :to-be nil)))  ; nil = success
```

**Mocking bash-parser:**
```elisp
;; Mock parse result
(spy-on 'jf/bash-parse :and-return-value
  '(:parse-complete t
    :command-name "python3"
    :all-commands ((:command-name "python3"))))

;; Mock semantics extraction
(spy-on 'jf/bash-extract-semantics :and-return-value
  '(:domains (:filesystem [])))  ; Empty = no-op
```

**Assertions:**
```elisp
;; Success (nil return)
(expect result :to-be nil)

;; Error structure
(expect result :to-have-key :error)
(expect (plist-get result :error) :to-equal "command_denied")

;; List membership
(expect commands :to-contain "python3")
```

**Test helpers:**
```elisp
;; Location: config/gptel/tools/test/test-helpers.el
(defun make-test-scope-config (&rest overrides)
  "Create test scope config with defaults."
  (append '(:paths (:read ["/**"] :write [] :execute [] :deny [])
            :bash-tools (:deny ["sudo" "rm"]))
          overrides))

(defun make-no-op-semantics ()
  "Create semantics with no file operations."
  '(:parse-complete t
    :domains (:filesystem [])))

(defun make-execute-semantics (path)
  "Create semantics with execute operation."
  `(:parse-complete t
    :domains (:filesystem ((:operation :execute :path ,path)))))
```

### Scenario Mapping

**Strategy:** One test per spec scenario, with direct traceability.

**Mapping pattern:**
```
Spec scenario:
  #### Scenario: Version check command allowed
  - WHEN command is `python3 --version`
  - AND bash-parser extracts zero file operations
  - THEN validation pipeline allows execution

Becomes test:
  (it "allows version check commands"
    "Scenario: specs/no-op-command-allowance/spec.md § 'Version check command allowed'"
    (spy-on 'jf/bash-extract-semantics :and-return-value (make-no-op-semantics))
    (let ((result (jf/gptel-scope--validate-command-semantics
                   "python3 --version" "/tmp" test-scope-config)))
      (expect result :to-be nil)))
```

**Traceability:**
- First line of test: docstring with spec reference
- Format: `"Scenario: specs/<capability>/spec.md § 'Scenario name'"`
- Enables automated mapping from spec to test

**Edge cases:**
- Complex scenarios may need multiple tests
- Error conditions get separate tests
- Each `it` block tests one logical assertion

## Dependencies

### Internal Dependencies
1. **bash-parser** (`config/bash-parser/`)
   - Tree-sitter based parsing
   - Semantic extraction via plugin system
   - File operations plugin
   - Parse completeness detection

2. **yaml.el** (via straight.el)
   - YAML parsing for scope profiles
   - Used by `jf/gptel-scope-profile--load`

3. **Core testing framework** (`config/core/testing.el`)
   - Buttercup test discovery
   - Test runner functions
   - Makefile integration

### External Dependencies
None (all dependencies are internal to the Emacs configuration)

## Constraints

### Technical Constraints
1. **Backward compatibility:** BREAKING change - scope profiles with categories will fail to load
   - Migration required: Remove categories section from all scope.yml files
   - Old presets referencing categories need updates

2. **Parsing completeness:** Rely on bash-parser for accurate no-op detection
   - If parser fails to extract operations, command may be incorrectly allowed
   - Mitigated by parse completeness enforcement (Stage 1)

3. **Deny list maintenance:** Deny list must be manually curated
   - No automatic population of dangerous commands
   - Risk: Unknown dangerous commands could bypass deny list

### Performance Constraints
1. **Parsing overhead:** Every command parses twice (once for validation, once for execution)
   - Could optimize by caching parsed result
   - Not implemented in initial version (premature optimization)

2. **Semantic extraction:** Plugin system adds overhead to every command
   - Acceptable for interactive use (LLM-driven commands)
   - Not suitable for high-frequency batch operations

### Compatibility Constraints
1. **Emacs version:** Requires Emacs 27.1+ (for cl-lib, pcase, let*)
2. **Buttercup version:** Requires buttercup.el (installed via straight.el)
3. **YAML parsing:** Requires yaml.el package

### Testing Constraints
1. **Mock dependency:** Tests mock bash-parser results
   - Assumes bash-parser behavior is correct
   - Integration tests needed to verify end-to-end flow

2. **Fixture management:** Scope config fixtures need maintenance
   - Test helpers centralize fixture creation
   - Changes to schema require fixture updates
