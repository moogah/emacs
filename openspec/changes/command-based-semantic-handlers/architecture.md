# Architecture

## Components

### Command Handler Files (`bash-parser/commands/*.el`)

Individual command files that encapsulate all semantic extraction logic for a specific command. Each file:
- Defines one or more handler functions
- Registers handlers to semantic domains via `jf/bash-register-command-handler`
- Is self-contained and independently testable

Examples:
- `cat.el` - Simple single-domain handler (filesystem only)
- `aws.el` - Complex multi-domain handler (filesystem, authentication, network)
- `git.el` - Multi-domain handler with subcommand logic

### Command Index (`bash-parser/commands/index.el`)

Auto-discovery and loading mechanism that:
- Scans `commands/` directory for all `.el` files (except itself)
- Loads each file in sequence
- Handles load errors gracefully with logging
- Triggers on `(require 'bash-commands-index)`

### Handler Registry (`bash-parser/semantics/bash-parser-semantics.el`)

Central registry replacing the monolithic database. Provides:
- `jf/bash-command-handlers` - Hash table mapping commands to domain handlers
- `jf/bash-register-command-handler` - Registration API
- `jf/bash-lookup-command-handlers` - Lookup API
- `jf/bash-extract-command-semantics` - Execution and result aggregation

Data structure:
```
{command-name => {domain => [handler-fn, handler-fn, ...]}}
```

### Plugin Orchestrator (`bash-parser/analysis/bash-parser-plugins.el`)

Existing plugin infrastructure (preserved and enhanced):
- Executes universal plugins (redirections, variables)
- Now also integrates command handler results
- Merges all results by domain
- Provides `jf/bash-extract-semantics` public API

## Interfaces

### Handler Registration Interface

```elisp
(jf/bash-register-command-handler
  :command "command-name"    ; String: command name (e.g., "aws", "cat")
  :domain :domain-keyword    ; Keyword: semantic domain (e.g., :filesystem)
  :handler #'handler-fn)     ; Function: handler implementation
```

**Contract:**
- MUST provide all three parameters
- Command name is case-sensitive string
- Domain is a keyword identifying semantic category
- Handler is a function symbol

### Handler Function Interface

```elisp
(defun handler-fn (parsed-command)
  "Extract semantics from PARSED-COMMAND.

  Returns plist:
    (:domain DOMAIN-KEYWORD
     :operations [operation-plist ...]
     :claimed-token-ids [token-id ...]
     :metadata {...})"
  ...)
```

**Input:** `parsed-command` plist from `jf/bash-parse`
- `:tokens` - List of token plists
- `:command-name` - Command string
- `:positional-args` - List of positional arguments
- `:flags` - List of flags
- Other parsing metadata

**Output:** Result plist or `nil`
- `:domain` - MUST match registered domain
- `:operations` - List of domain-specific operation plists
- `:claimed-token-ids` - List of token IDs this handler processed
- `:metadata` - Optional additional context

### Public API Interface (Preserved)

```elisp
(jf/bash-extract-semantics parsed-command)
  => (:domains ((domain . ops) ...)
      :coverage {...}
      :parse-complete t/nil
      :plugin-results [...])
```

**Critical for compatibility:** This interface MUST remain unchanged for gptel scope validation.

### Domain-Specific Operation Schemas

**Filesystem operations:**
```elisp
(:file "/path/to/file"
 :operation :read/:write/:delete/:modify/:execute
 :confidence :high/:medium/:low
 :command "command-name")
```

**Authentication operations:**
```elisp
(:provider :aws/:gcloud/:azure
 :context ((:profile . "prod") (:region . "us-east-1"))
 :command "command-name")
```

**Network operations:**
```elisp
(:protocol :https/:ssh/:git
 :endpoint "example.com"
 :command "command-name")
```

## Boundaries

### In Scope

- Command handler architecture (files, registry, auto-discovery)
- Handler registration and execution API
- Multi-domain support for commands
- Interface preservation for `jf/bash-extract-semantics`
- Migration of existing command semantics to handler files

### Out of Scope

- Changes to bash parsing logic (`jf/bash-parse`)
- Changes to gptel scope validation system
- Changes to token coverage calculation
- Backwards compatibility with old database format
- Migration tools or compatibility shims

### Internal vs External

**Internal:**
- Handler registry data structure
- Handler execution order
- Token claiming implementation
- Result aggregation logic

**External (Public API):**
- `jf/bash-extract-semantics` signature and return structure
- `jf/bash-parse` output format
- Coverage calculation interface
- Plugin registration interface

## Testing Approach

### Test Framework

**Buttercup (BDD-style framework)**

We will use Buttercup for all command handler tests because:
- Modern `describe`/`it`/`expect` syntax is readable and expressive
- Built-in `before-each`/`after-each` for test setup/teardown
- Spy system for mocking functions (e.g., mocking parser output)
- Hierarchical test organization with nested `describe` blocks
- Better for testing behavioral scenarios from specs

### Test Organization

**Location:** `config/bash-parser/commands/test/`

Structure:
```
bash-parser/
├── commands/
│   ├── index.el
│   ├── cat.el
│   ├── aws.el
│   └── test/              # Test directory
│       ├── cat-spec.el    # Tests for cat.el
│       ├── aws-spec.el    # Tests for aws.el
│       └── ...
```

**Rationale:**
- Dedicated test directory keeps implementation clean
- Clear separation between production code and tests
- Easy to find all command handler tests in one place
- Follows existing bash-parser test organization pattern

### Naming Conventions

**Test files:** `*-spec.el` suffix for Buttercup tests
- `cat-spec.el` - Tests for `cat.el`
- `aws-spec.el` - Tests for `aws.el`
- `registry-spec.el` - Tests for registry functions

**Test structure:**
```elisp
(describe "cat command handler"
  (describe "filesystem extraction"
    (it "extracts read operations from positional args"
      (let ((parsed-command '(:command-name "cat"
                              :positional-args ("file1.txt" "file2.txt"))))
        (expect (jf/bash-command-cat--filesystem-handler parsed-command)
                :to-equal expected-result)))))
```

**Naming pattern:**
- Top-level `describe`: Command name or component
- Nested `describe`: Specific capability or domain
- `it`: Specific behavior or scenario

### Running Tests

**All command handler tests:**
```bash
make test-buttercup-directory DIR=config/bash-parser/commands/test
```

**Specific test file:**
```bash
./bin/run-tests.sh -d config/bash-parser/commands/test -p '^cat-'
```

**All Buttercup tests (including command handlers):**
```bash
make test-buttercup
```

**Interactive (via transient menu):**
```
C-c t   # Open test menu
# Select Buttercup → Directory → enter path
```

### Test Patterns

**Test data:** Inline in test files

Each test defines its own `parsed-command` input inline:
```elisp
(it "handles aws s3 cp with local source"
  (let ((parsed-command '(:command-name "aws"
                          :positional-args ("s3" "cp" "local.txt" "s3://bucket/file.txt")
                          :flags ("--profile")
                          :tokens [...])))
    (expect (jf/bash-command-aws--filesystem-handler parsed-command)
            :to-have-operations-matching ...)))
```

**Rationale:**
- Simple cases don't need shared fixtures
- Each test is self-contained and readable
- Easy to understand test data in context
- Avoids indirection for straightforward cases

**Common assertions:**
```elisp
;; Check result structure
(expect result :to-have-key :domain)
(expect (plist-get result :domain) :to-be :filesystem)

;; Check operations list
(let ((ops (plist-get result :operations)))
  (expect (length ops) :to-equal 2)
  (expect (plist-get (car ops) :operation) :to-be :read))

;; Check nil handling
(expect result :to-be nil)
```

**Error handling tests:**
```elisp
(it "returns nil when command doesn't match"
  (let ((parsed-command '(:command-name "not-aws")))
    (expect (jf/bash-command-aws--filesystem-handler parsed-command)
            :to-be nil)))
```

### Scenario Mapping

**From spec scenarios to test cases:**

1. **Each spec scenario becomes at least one test:**
   - Spec: "AWS command multi-domain extraction"
   - Test: `(it "extracts filesystem, auth, and network for aws s3 cp" ...)`

2. **Given/When/Then → Buttercup structure:**
   - GIVEN → `before-each` or `let` binding setup
   - WHEN → Execute handler function
   - THEN → `expect` assertions

3. **Example mapping:**

   **Spec scenario:**
   ```
   Scenario: Register handler with required parameters
   - WHEN calling jf/bash-register-command-handler with :command "cat", :domain :filesystem, :handler #'func
   - THEN the handler SHALL be registered in the handler registry
   - AND the handler SHALL be callable during semantic extraction
   ```

   **Buttercup test:**
   ```elisp
   (describe "handler registration"
     (it "registers handler with all required parameters"
       (let ((registry (make-hash-table :test 'equal)))
         ;; Setup
         (setq jf/bash-command-handlers registry)

         ;; Execute
         (jf/bash-register-command-handler
           :command "cat"
           :domain :filesystem
           :handler #'my-test-handler)

         ;; Verify
         (expect (gethash "cat" registry) :not :to-be nil)
         (let ((domains (gethash "cat" registry)))
           (expect (gethash :filesystem domains) :to-contain #'my-test-handler)))))
   ```

4. **Edge cases get separate tests:**
   - Happy path from spec
   - Error conditions (missing params, nil inputs)
   - Boundary cases (empty operations, no tokens)

## Dependencies

### Existing Bash Parser Components

- `bash-parser-core` - Parsing logic (`jf/bash-parse`)
- `bash-parser-protocol` - Shared protocols and forward declarations
- `bash-parser-plugins` - Plugin infrastructure
- `bash-parser-coverage` - Token coverage calculation
- `bash-parser-variables` - Variable resolution

### Elisp Standard Libraries

- `cl-lib` - Common Lisp extensions (hash tables, loops)
- Built-in hash table functions

### No New External Dependencies

This change uses only existing dependencies. No new packages required.

## Constraints

### Interface Compatibility

**CRITICAL:** The `jf/bash-extract-semantics` return structure MUST NOT change. The gptel scope validation system depends on this exact structure:

```elisp
(:domains ((domain . operations) ...)
 :coverage {:claimed-count ... :total-count ... :percentage ...}
 :parse-complete t/nil
 :plugin-results [...])
```

Any changes to this structure will break scope validation.

### Performance

- Handler registration: One-time cost at initialization
- Handler lookup: O(1) hash table lookup by command name
- Handler execution: Linear in number of registered handlers per command
- No significant performance degradation expected vs current database approach

### Backwards Compatibility

**No backwards compatibility.** The old `jf/bash-command-file-semantics` database will be removed with no fallback mechanism. This is a clean architectural break.

### Testing Coverage

All existing scope validation tests MUST pass after refactoring:
- File path validation tests
- Cloud authentication tests
- Pipeline validation tests
- Coverage enforcement tests

No test modifications are allowed - only implementation changes.

### Directory Structure

The `bash-parser/commands/` directory MUST be at the top level of bash-parser, not nested deeper. This ensures clear organization and easy discovery.
