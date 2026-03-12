## Context

### Current State
Scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope) use `gptel-make-scoped-tool` macro without the `:async` keyword, resulting in synchronous validation behavior. When validation fails:
1. Tool returns error immediately
2. Error message says "Use request_scope_expansion tool"
3. LLM must manually call request_scope_expansion (separate tool call)
4. User approves/denies in that separate interaction
5. LLM retries original tool (third tool call)

This differs from run_bash_command, which uses `:async` to trigger inline expansion automatically within a single tool call.

Additionally, `edit_file_in_scope` checks git-tracked status AFTER scope validation (lines 148-152 of scope-filesystem-tools.el), preventing this metadata from being used in scope policies or displayed in the expansion UI.

### Problem
- Inconsistent UX between filesystem tools and run_bash_command
- Requires 3 LLM turns for scope approval workflow
- No behavioral test coverage for filesystem tools
- Git metadata unavailable for validation decisions

### Constraints
- Alpha feature: breaking changes acceptable
- Must maintain consistency with run_bash_command patterns
- Git detection should be fast (no complex git operations)
- Tests must be isolated and fast (<5 seconds total)

## Goals / Non-Goals

**Goals:**
- Enable inline expansion for all three scoped filesystem tools
- Gather file metadata before validation
- Pass metadata through validation pipeline
- Create comprehensive behavioral test coverage
- Display git status in expansion UI
- Maintain consistent error format with run_bash_command

**Non-Goals:**
- Implement git-based scope policies (future work - metadata enables this)
- Change unscoped legacy tools in `config/gptel/tools/filesystem-tools.el`
- Add inline expansion to org-roam or other tool types
- Cache metadata (always gather fresh)
- Support non-git VCS systems (git-only for now)

## Decisions

### Decision 1: Single metadata gathering function vs per-tool functions

**Chosen**: Single function `jf/gptel-scope--gather-file-metadata`

**Alternatives considered**:
- Per-tool metadata functions (read-specific, write-specific)
- Metadata gathering in validation functions

**Rationale**:
- All three tools need same metadata (git status, existence, type)
- Single function reduces duplication
- Easier to extend with new metadata fields
- Separation of concerns: gathering is distinct from validation

**Implementation**:
```elisp
(defun jf/gptel-scope--gather-file-metadata (filepath)
  "Gather contextual metadata for FILEPATH."
  (let ((full-path (expand-file-name filepath)))
    (list :path full-path
          :real-path (file-truename full-path)
          :exists (file-exists-p full-path)
          :git-tracked (jf/gptel--file-is-git-tracked-p full-path)
          :git-repo (locate-dominating-file full-path ".git")
          :type (cond ((file-directory-p full-path) 'directory)
                      ((file-regular-p full-path) 'file)
                      (t 'other)))))
```

### Decision 2: Metadata structure - flat plist vs nested

**Chosen**: Flat plist

**Alternatives considered**:
- Nested structure: `(:path "..." :git (:tracked t :repo "..."))`
- Vector with fixed positions
- Struct/defstruct

**Rationale**:
- Flat plist is simpler to access: `(plist-get metadata :git-tracked)`
- Consistent with Emacs patterns (plists common in Emacs Lisp)
- Easy to extend (add new keys without breaking existing code)
- No namespace conflicts in foreseeable future

**Structure**:
```elisp
(:path "/absolute/path"
 :real-path "/symlink/resolved"
 :exists t
 :git-tracked t
 :git-repo "/repo/root"
 :type file)
```

### Decision 3: Breaking change strategy - gradual vs immediate

**Chosen**: Immediate breaking change to all validation functions

**Alternatives considered**:
- Optional metadata parameter with default nil
- Gradual migration with compatibility shim
- Separate metadata-aware validation functions

**Rationale**:
- Alpha feature status accepts breaking changes
- Clean implementation preferred over backward compatibility
- Fewer validation function variants to maintain
- Forces intentional handling of metadata
- Only ~4 call sites need updating (scope is small)

**Migration approach**:
1. Update validation function signatures (add metadata parameter)
2. Update all callers in gptel-make-scoped-tool macro
3. Update bash validation (pass nil for metadata)
4. Update pattern validation (pass nil for metadata)

### Decision 4: Where to gather metadata - wrapper vs validation

**Chosen**: Gather in gptel-make-scoped-tool macro wrapper (before validation)

**Alternatives considered**:
- Gather in validation functions
- Gather in check-tool-permission dispatcher
- Lazy gathering (only if validation needs it)

**Rationale**:
- Metadata gathering in wrapper keeps validation functions pure
- Enables future use cases (logging, auditing, error messages)
- Consistent with bash-parser integration (semantics extracted before validation)
- Clear separation: wrapper handles I/O, validation handles logic

**Implementation location**:
```elisp
;; In gptel-make-scoped-tool macro, ASYNC branch:
(let* ((normalized-args (list ,@arg-names))
       (config (jf/gptel-scope--load-config))
       (metadata (when (eq validation-type 'path)  ; Only for path tools
                   (jf/gptel-scope--gather-file-metadata (car normalized-args)))))
  ;; Pass metadata to validation
  (jf/gptel-scope--check-tool-permission config ,name normalized-args metadata))
```

### Decision 5: Git check removal from edit_file_in_scope

**Chosen**: Remove git check from tool body, rely on metadata

**Alternatives considered**:
- Keep git check in tool body as safety net
- Move git check to validation function
- Make git check a validation policy (configurable)

**Rationale**:
- Separation of concerns: validation happens before execution
- Metadata makes git status available to validation
- Reduces duplication (check once in metadata, not twice)
- Enables future git-based policies in scope.yml
- Tool body should only execute business logic

**Migration**:
- Delete lines 148-152 from edit_file_in_scope (git check)
- Metadata `:git-tracked` available for future policy enforcement
- Current behavior: no explicit git-tracked requirement (future work)

### Decision 6: Test organization - one file vs multiple

**Chosen**: Single behavioral spec file for all three tools

**Alternatives considered**:
- Separate file per tool (filesystem-tools-scope-expansion-spec.el split into 3)
- Mimic run_bash_command structure (directory with multiple files)
- Mixed approach (one file for common scenarios, separate for tool-specific)

**Rationale**:
- All three tools share same validation logic (path-based)
- Shared test infrastructure (setup, helpers, mocks)
- ~30-40 scenarios total (manageable in one file)
- Simpler than 3 separate files with duplicated setup
- Nested describe blocks provide clear organization

**File**: `config/gptel/tools/test/behavioral/filesystem-tools-scope-expansion-spec.el`

### Decision 7: Metadata in expansion UI - show vs hide

**Chosen**: Display git status and file existence in expansion UI

**Alternatives considered**:
- Hide metadata (keep UI minimal)
- Show all metadata fields (verbose)
- Make metadata display configurable

**Rationale**:
- Git status is relevant to user's decision
- Helps user understand what they're approving
- Minimal UI impact (one line indicator)
- Consistent with displaying operation type and resource

**UI format**:
```
Scope Violation: Access Denied
  Tool: edit_file_in_scope
  Resource: /tmp/config.json
  Git Status: NOT TRACKED  ← NEW
  Reason: Path not in write scope
```

### Decision 8: Allow-once resource format for filesystem tools

**Chosen**: `"tool_name:filepath"` format (just two parts)

**Alternatives considered**:
- Same as bash: `"tool_name:command:directory"` (three parts)
- Just filepath: `"filepath"` (no tool name)
- Include metadata: `"tool_name:filepath:git-tracked"`

**Rationale**:
- Filepath alone uniquely identifies the resource
- Tool name prevents cross-tool permission leakage
- Simpler than bash's three-part format (no directory context needed)
- Expanded absolute path ensures uniqueness

**Format**:
```elisp
("read_file" . "/tmp/file.txt")
("write_file_in_scope" . "/workspace/output.txt")
```

Compare to bash:
```elisp
("run_bash_command" . "ls /tmp:/workspace")  ; command:directory
```

## Risks / Trade-offs

### Risk: Metadata gathering performance overhead
**Impact**: Git checks on every file operation could slow down tools

**Mitigation**:
- Git check is fast (single `git ls-files --error-unmatch` call)
- Only happens for path-based tools (not bash, pattern, meta)
- No caching needed (modern git is very fast)
- Future: could add metadata caching if needed

### Risk: Breaking changes impact other tools
**Impact**: Validation signature changes affect all validation call sites

**Mitigation**:
- Small scope: only ~4 call sites in gptel-make-scoped-tool macro
- Tests will catch any missed call sites
- Alpha feature status accepts breaking changes
- Clear migration path documented in design

### Risk: Git not installed or not in PATH
**Impact**: Git-tracked detection fails in non-git environments

**Mitigation**:
- Graceful degradation: `:git-tracked nil` when git not available
- Error handling in `jf/gptel--file-is-git-tracked-p`
- File operations work regardless of git status
- Git metadata is informational, not required

### Risk: Tests don't match actual inline expansion behavior
**Impact**: Tests pass but actual UI workflow broken

**Mitigation**:
- Reuse helpers-spec.el infrastructure (proven with run_bash_command)
- Mock only stateful operations (file I/O, git)
- Test real validation logic (no mocking of validators)
- Spy on expansion UI to verify it's called correctly
- ~40 test scenarios provide comprehensive coverage

### Trade-off: Single file vs multiple test files
**Chosen**: Single file for simplicity
**Trade-off**: Longer file (~500-600 lines) vs easier navigation

**Rationale**: Benefits of shared setup outweigh file length concerns. Nested describe blocks provide structure.

### Trade-off: Metadata always gathered vs lazy gathering
**Chosen**: Always gather for path-based tools
**Trade-off**: Small overhead on every call vs conditional complexity

**Rationale**: Consistency and simplicity outweigh minimal performance cost. Enables future use cases.

### Trade-off: Git check removed from tool body
**Chosen**: Rely on metadata for git status
**Trade-off**: No explicit git-tracked enforcement vs cleaner separation

**Rationale**: Metadata enables future git-based policies without hardcoding behavior. Current change doesn't enforce git-tracked requirement (future work).

## Implementation Plan

### Phase 1: Metadata gathering (foundation)
1. Create `config/gptel/scope/scope-metadata.el`
2. Implement `jf/gptel-scope--gather-file-metadata`
3. Write unit-level behavioral tests for metadata structure

### Phase 2: Validation signature updates (breaking changes)
1. Update `jf/gptel-scope--check-tool-permission` signature
2. Update `jf/gptel-scope--validate-path-tool` signature
3. Update `jf/gptel-scope--validate-pattern-tool` signature (pass nil)
4. Update `jf/gptel-scope--validate-bash-tool` signature (pass nil)
5. Update gptel-make-scoped-tool macro to gather metadata

### Phase 3: Filesystem tools conversion
1. Add `:async` keyword to read_file
2. Add `:async` keyword to write_file_in_scope
3. Add `:async` keyword to edit_file_in_scope
4. Remove git check from edit_file_in_scope (lines 148-152)

### Phase 4: Expansion UI enhancement
1. Update scope-expansion.el to display metadata
2. Format git status indicator
3. Format file existence indicator

### Phase 5: Behavioral tests
1. Create `filesystem-tools-scope-expansion-spec.el`
2. Implement read_file test scenarios (~10 tests)
3. Implement write_file_in_scope test scenarios (~12 tests)
4. Implement edit_file_in_scope test scenarios (~12 tests)
5. Implement transient action handler tests (~5 tests)
6. Implement allow-once lifecycle tests (~3 tests)

### Phase 6: Validation and cleanup
1. Run all tests (make test-buttercup)
2. Verify inline expansion workflow manually
3. Update any documentation
4. Commit with breaking change notice

## Open Questions

None - all design decisions resolved during exploration phase.
