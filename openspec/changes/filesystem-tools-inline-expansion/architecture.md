## Components

### Metadata Gathering Component
**Module**: `config/gptel/scope/scope-metadata.el`
**Responsibility**: Collects file contextual metadata (git status, existence, type) before validation

Functions:
- `jf/gptel-scope--gather-file-metadata` - Main entry point, returns metadata plist
- `jf/gptel--file-is-git-tracked-p` - Checks if file is tracked by git (exists in scope-filesystem-tools.el, will be reused)

### Scoped Tool Macro Component
**Module**: `config/gptel/scope/scope-core.el`
**Responsibility**: Tool wrapping with automatic validation and inline expansion support

Changes:
- `gptel-make-scoped-tool` macro enhanced to gather metadata for path-based tools
- Metadata passed to validation pipeline

### Validation Pipeline Component
**Module**: `config/gptel/scope/scope-core.el`
**Responsibility**: Core validation logic with metadata propagation

Functions modified (breaking changes):
- `jf/gptel-scope--check-tool-permission (config tool-name args metadata)` - Main dispatcher
- `jf/gptel-scope--validate-path-tool (tool-name args category config metadata)` - Path validator
- `jf/gptel-scope--validate-pattern-tool (tool-name args config metadata)` - Pattern validator
- `jf/gptel-scope--validate-bash-tool (tool-name args config metadata)` - Bash validator

### Filesystem Tools Component
**Module**: `config/gptel/scope/scope-filesystem-tools.el`
**Responsibility**: Scoped filesystem tools with inline expansion

Changes:
- Add `:async` keyword to all three tools (read_file, write_file_in_scope, edit_file_in_scope)
- Remove git-tracked check from edit_file_in_scope tool body (lines 148-152)
- Rely on metadata gathered before validation

### Expansion UI Component
**Module**: `config/gptel/scope/scope-expansion.el`
**Responsibility**: Interactive scope violation handling

Changes:
- Display file metadata (git status, existence) in expansion UI
- No changes to core expansion workflow (already supports path-based tools)

## Interfaces

### Metadata → Validation
```elisp
;; Metadata plist structure (flat)
(:path "/absolute/path"
 :real-path "/symlink/resolved/path"
 :exists t
 :git-tracked t
 :git-repo "/repo/root"
 :type file)  ; file, directory, or other

;; Validation function signature (BREAKING)
(jf/gptel-scope--check-tool-permission config tool-name args metadata)
(jf/gptel-scope--validate-path-tool tool-name args category config metadata)
```

### Tool Wrapper → Metadata Gathering
```elisp
;; For path-based tools, macro calls:
(jf/gptel-scope--gather-file-metadata filepath)
;; Returns metadata plist

;; For non-path tools:
metadata → nil
```

### Validation → Expansion UI
```elisp
;; Existing violation-info structure extended:
(:tool "read_file"
 :resource "/tmp/file.txt"
 :operation :read
 :validation-type 'path
 :reason "path_out_of_scope"
 :allowed-patterns ("/workspace/**")
 :metadata (:path "/tmp/file.txt" :git-tracked nil :exists t))  ; NEW
```

### Tool → Allow-Once List
```elisp
;; Allow-once format for filesystem tools
("read_file" . "/absolute/filepath")
("write_file_in_scope" . "/absolute/filepath")
("edit_file_in_scope" . "/absolute/filepath")

;; Compared to bash format:
("run_bash_command" . "command:directory")
```

## Boundaries

### In Scope
- Inline expansion for three scoped filesystem tools
- Metadata gathering for file context (git, existence, type)
- Breaking changes to validation function signatures
- Comprehensive behavioral test coverage
- Metadata display in expansion UI

### Out of Scope
- Git-based scope policies (metadata gathering enables this, but policies are future work)
- Changes to run_bash_command (already has inline expansion)
- Changes to org-roam tools (different validation strategy)
- Changes to unscoped filesystem tools in `config/gptel/tools/filesystem-tools.el` (legacy, unscoped)
- Caching of metadata (gathered fresh each time)

### Internal vs External
- **Internal**: Validation pipeline, metadata gathering, tool wrappers
- **External**: gptel-agent package API, scope.yml file format, expansion UI (transient)
- **Boundary**: Tool definitions use public gptel-make-scoped-tool macro

## Testing Approach

### Test Framework
**Buttercup** (BDD framework)
- Preferred framework for new test suites
- `describe`/`it`/`expect` syntax matches run_bash_command tests
- Built-in setup/teardown with `before-each`/`after-each`
- Spy system for mocking (`spy-on`)

**Why Buttercup:**
- Behavioral tests map naturally to spec scenarios
- Consistent with existing run_bash_command scope expansion tests
- No unit tests needed - behavioral tests provide sufficient coverage

### Test Organization
**Location**: `config/gptel/tools/test/behavioral/filesystem-tools-scope-expansion-spec.el`

**Structure**:
```elisp
(describe "Filesystem tools: Scope expansion workflows"
  (before-each ...)
  (after-each ...)

  (describe "read_file tool"
    (it "validates read in read scope" ...)
    (it "validates read in write scope (hierarchical)" ...)
    (it "triggers expansion when out of scope" ...)
    (it "approves with add-to-scope" ...)
    (it "approves with allow-once" ...)
    (it "denies expansion" ...))

  (describe "write_file_in_scope tool"
    (it "validates write in write scope" ...)
    (it "fails write in read-only scope" ...)
    (it "creates parent directories" ...)
    ...)

  (describe "edit_file_in_scope tool"
    (it "validates edit in write scope" ...)
    (it "includes git metadata" ...)
    (it "handles string not found" ...)
    ...))
```

**Single file for all three tools** because:
- Shared test infrastructure (setup, helpers, mocks)
- Similar validation logic (path-based)
- Simpler than 3 separate files
- ~30-40 test scenarios total across all tools

### Naming Conventions
**Test files**: `*-spec.el` suffix (Buttercup convention)
**Test structure**:
- Outer describe: Tool name or category
- Inner describe: Feature or workflow
- It blocks: Specific scenario in active voice

**Example**:
```elisp
(describe "read_file tool"
  (describe "Path validation"
    (it "allows read in read scope"
      ...)))
```

### Running Tests
**Via Make** (direct):
```bash
make test-buttercup                                       # All Buttercup tests
make test-buttercup-directory DIR=config/gptel/tools/test # Scoped to tools tests
```

**Via CLI** (user-friendly):
```bash
./bin/run-tests.sh -f buttercup                          # All Buttercup
./bin/run-tests.sh -d config/gptel/tools/test/behavioral # Directory scoped
./bin/run-tests.sh -d config/gptel/tools/test/behavioral -s  # With snapshot
```

**Interactive**:
```
C-c t    # Open transient test menu
# Select Buttercup options
```

### Test Patterns

**Setup/Teardown**:
```elisp
(before-each
  (helpers-spec-setup-session)       ; Mock gptel session
  (setq jf/gptel-scope--allow-once-list nil))  ; Clear allow-once

(after-each
  (helpers-spec-teardown-session))   ; Cleanup
```

**Mocking**:
```elisp
;; Mock expansion UI
(spy-on 'jf/gptel-scope-prompt-expansion
        :and-call-fake
        (lambda (violation callback patterns tool-name)
          ;; Simulate user approval
          (funcall callback (json-serialize (list :success t)))))

;; Mock file operations
(spy-on 'file-exists-p :and-return-value t)
(spy-on 'jf/gptel--file-is-git-tracked-p :and-return-value t)
```

**Assertions**:
```elisp
;; Validation results
(expect result :to-be nil)  ; nil = validation success
(expect result :not :to-be nil)  ; non-nil = validation error

;; Error structure
(expect (plist-get result :error) :to-equal "path_out_of_scope")
(expect (plist-get result :resource) :to-equal "/tmp/file.txt")

;; Allow-once list
(expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
(expect (car (car jf/gptel-scope--allow-once-list)) :to-equal "read_file")
```

**Reuse existing helpers** from `config/gptel/tools/test/helpers-spec.el`:
- `helpers-spec-setup-session` - Create mock gptel session
- `helpers-spec-teardown-session` - Cleanup mock session
- `helpers-spec-make-minimal-scope` - Create test scope.yml
- `helpers-spec-load-scope-config` - Parse scope config
- Custom matchers: `:to-be-validation-success`, `:to-be-validation-error`

**Add new helpers** for filesystem tools:
- `helpers-spec-make-temp-file` - Create temporary test file
- `helpers-spec-setup-git-repo` - Initialize git repo for testing
- `helpers-spec-mock-file-metadata` - Mock metadata gathering
- `helpers-spec-make-scope-with-paths` - Create scope with read/write paths

### Scenario Mapping

Each spec scenario maps to at least one test case:

**From specs → tests**:
1. `filesystem-inline-expansion/spec.md` scenarios → behavioral test `it` blocks
2. `filesystem-metadata/spec.md` scenarios → behavioral test `it` blocks with metadata assertions
3. `filesystem-behavioral-tests/spec.md` scenarios → direct 1:1 mapping to test cases

**Example mapping**:
```
Spec: "Read in read scope succeeds"
  → (it "validates read operation in read scope"
       ;; Setup scope with paths.read
       ;; Call read_file with in-scope path
       ;; Assert validation passes
       ;; Assert file contents returned)

Spec: "Read out of scope triggers expansion"
  → (it "triggers expansion UI when path out of scope"
       ;; Setup scope with restricted paths
       ;; Spy on expansion UI
       ;; Call read_file with out-of-scope path
       ;; Assert expansion UI called
       ;; Assert violation details correct)
```

**Test organization principles**:
- One `describe` block per tool
- Nested `describe` for feature categories (validation, expansion, errors)
- Each `it` block tests one scenario end-to-end
- Mock only stateful operations (file I/O, git, expansion UI)
- Test real validation logic (no mocking of validators)

## Dependencies

**Internal** (existing modules):
- `config/gptel/scope/scope-core.el` - Core validation system
- `config/gptel/scope/scope-expansion.el` - Expansion UI
- `config/gptel/scope/scope-filesystem-tools.el` - Filesystem tools
- `config/gptel/tools/test/helpers-spec.el` - Test infrastructure

**External** (packages):
- `buttercup` - Test framework (already installed)
- `gptel-agent` - Tool definition system (already integrated)
- `yaml` - scope.yml parsing (already used)
- `transient` - Expansion UI (already used)

**No new external dependencies required.**

## Constraints

**Breaking Changes**:
- Validation function signatures change (all callers must be updated)
- Alpha feature - breaking changes expected and accepted
- No backward compatibility layer

**Performance**:
- Metadata gathering adds minimal overhead (git check is fast)
- No caching of metadata (stateless, always fresh)
- Allow-once list is buffer-local (minimal memory impact)

**Compatibility**:
- Emacs 27+ (existing constraint from straight.el)
- Git required for git-tracked detection (optional feature)
- Works with or without git (gracefully degrades)

**Testing**:
- Tests must run in isolation (no shared state)
- Mock external operations (file I/O, git commands)
- Tests should complete in <5 seconds total

**Scope**:
- Only three filesystem tools (read_file, write_file_in_scope, edit_file_in_scope)
- Does NOT change unscoped legacy tools in `config/gptel/tools/filesystem-tools.el`
- Does NOT implement git-based policies (future work)
