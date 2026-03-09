# Design: Operation-First Validation

## Context

The current bash_tools validation system uses command categories (read_only, safe_write, dangerous) to control which commands are allowed. This approach requires maintaining explicit allowlists of "safe" commands and cannot distinguish between `python3 --version` (safe introspection) and `python3 script.py` (execution requiring validation).

With the bash-parser now providing reliable semantic extraction, we can replace category-based validation with operation-first validation. Commands are allowed by default if they have no file system operations (no-op allowance), and commands with operations are validated against operation-specific path patterns.

**Current implementation:**
- `config/gptel/tools/scope-shell-tools.org` - 7-stage validation pipeline with category checking
- `config/gptel/scope-profiles/*.yml` - Profiles with bash_tools.categories sections
- `config/gptel/tools/test/` - Tests validating category membership

**Constraints:**
- Must maintain security guarantees (deny-by-default for dangerous operations)
- Must preserve existing file operation validation (paths.read/write/execute/modify)
- Breaking change acceptable (categories removal requires migration)
- Bash-parser reliability assumed (parse completeness enforcement mitigates risk)

## Goals / Non-Goals

**Goals:**
1. Replace category-based validation with operation-first validation
2. Enable no-op commands (version checks, help flags) by default without allowlisting
3. Simplify scope profiles by removing categories section
4. Reduce deny list to edge cases only (sudo, commands with parser limitations)
5. Maintain security through operation-specific path validation
6. Provide clear migration path for existing scope profiles

**Non-Goals:**
1. Modify bash-parser semantic extraction (separate concern)
2. Change cloud authentication validation logic
3. Alter scope expansion UI or approval flow
4. Modify parse completeness enforcement
5. Add new file operation types beyond existing (read/write/execute/modify/delete)
6. Support backward compatibility with categories (breaking change)

## Decisions

### Decision 1: No-Op Allowance at Stage 4 (Between Deny List and File Validation)

**Choice:** Insert no-op check after deny list validation but before file operation validation.

**Alternatives considered:**
- **Before deny list:** Would allow `sudo --version` even though sudo is in deny list. Rejected for security.
- **After file validation:** Would require file validation logic to handle empty operations list. Less clean separation.
- **Merged into file validation:** Would complicate file validation logic with special-case handling.

**Rationale:**
- Deny list catches dangerous commands regardless of operations (security first)
- No-op check short-circuits validation for safe commands (performance)
- File validation only sees commands with operations (cleaner logic)
- Clear stage progression: deny → parse → no-op → operations

**Implementation:**
```elisp
(defun jf/gptel-scope--validate-command-semantics (command directory scope-config)
  "Seven-stage validation pipeline."
  (cl-block validate
    (let* ((parsed (jf/bash-parse command))
           (semantics (jf/bash-extract-semantics parsed)))

      ;; Stage 1: Parse completeness (unchanged)
      (when-let ((error (validate-parse-completeness parsed)))
        (cl-return-from validate error))

      ;; Stage 2: Deny list check (modified - no categories)
      (when-let ((error (validate-deny-list parsed scope-config)))
        (cl-return-from validate error))

      ;; Stage 3: Extract semantics (moved before no-op check)
      ;; (semantics already extracted above)

      ;; Stage 4: No-op allowance (NEW)
      (let ((file-ops (plist-get (plist-get semantics :domains) :filesystem)))
        (when (null file-ops)
          (cl-return-from validate nil)))  ; Success - no operations

      ;; Stage 5: File operations validation (renamed from Stage 4)
      (when-let ((error (validate-file-operations file-ops directory scope-config)))
        (cl-return-from validate error))

      ;; Stages 6-7: Cloud auth, coverage (unchanged)
      ...)))
```

### Decision 2: Remove Categories Entirely from Schema

**Choice:** Remove bash_tools.categories section completely, keep only bash_tools.deny list.

**Alternatives considered:**
- **Keep categories as optional:** Would require maintaining two validation paths (categories + operations). Complexity without benefit.
- **Deprecate gradually:** Would require version detection and migration code. Adds complexity for temporary benefit.
- **Convert categories to deny list:** read_only/safe_write → no-op, dangerous → deny. But no-op allowance makes this unnecessary.

**Rationale:**
- Operation-first validation makes categories obsolete
- Simpler schema with only deny list (minimal configuration)
- Clear migration path (remove categories section)
- No need to maintain allowlists of "safe" commands

**Migration impact:**
- All scope profiles need categories section removed
- Presets with inline `:bash-tools` need updates
- User error if categories section present (clear error message)

### Decision 3: Deny List Contains Only Edge Cases

**Choice:** Minimize deny list to commands that are always dangerous or have parser limitations.

**Example deny list:**
```yaml
bash_tools:
  deny:
    - sudo       # Privilege escalation
    - dd         # Raw disk operations
    - chmod      # Permission changes (can't make safe via paths)
    - chown      # Ownership changes
```

**Alternatives considered:**
- **Include rm in deny list:** rm could be allowed with paths validation. But keeping for extra safety.
- **Include package managers:** npm, pip, cargo could be allowed. But installation side effects hard to validate.
- **Empty deny list:** Trust operation validation entirely. Too risky for commands with non-file side effects.

**Rationale:**
- File operations (rm, mv) could be validated via paths.write, but deny list provides extra safety
- Commands with non-file side effects (sudo, chmod) need deny list
- Commands with parser limitations can't be reliably validated
- Minimal deny list reduces maintenance burden

**Trade-off:** Users must request scope expansion for denied commands they need.

### Decision 4: Buttercup for New Tests, Maintain ERT Tests

**Choice:** Write new tests in Buttercup, keep existing ERT tests unchanged.

**Alternatives considered:**
- **Migrate all tests to Buttercup:** High effort, risk of introducing regressions, minimal benefit.
- **Use only ERT:** Miss Buttercup's better BDD syntax and built-in mocking.
- **Duplicate coverage:** Wasteful, maintenance burden.

**Rationale:**
- Buttercup's describe/it syntax maps naturally to spec scenarios
- Built-in before-each/after-each simplifies fixture management
- Spy system for mocking bash-parser calls
- No need to migrate working tests (YAGNI principle)
- Both frameworks run side-by-side via Makefile

**Implementation:**
- New test files: `*-spec.el` (Buttercup)
- Existing test files: `test-*.el` (ERT, unchanged)
- Test discovery handles both patterns
- Run separately: `make test-buttercup` vs `make test`

### Decision 5: One Test Per Spec Scenario for Traceability

**Choice:** Map each spec scenario directly to one test case with traceability docstring.

**Example:**
```elisp
(it "allows version check commands"
  "Scenario: specs/no-op-command-allowance/spec.md § 'Version check command allowed'"
  (spy-on 'jf/bash-extract-semantics :and-return-value (make-no-op-semantics))
  (expect (validate "python3 --version" "/tmp" config) :to-be nil))
```

**Rationale:**
- Clear mapping from requirement to test
- Enables automated traceability reports
- Simplifies test maintenance (one scenario = one test)
- Makes test failures easier to understand

**Trade-off:** Complex scenarios may need multiple assertions in one test. Acceptable for clarity.

### Decision 6: Extract Semantics Once, Reuse in Pipeline

**Choice:** Extract semantics once at pipeline start, thread through stages.

**Current inefficiency:**
```elisp
;; Stage 2: Parse command
(let ((parsed (jf/bash-parse command)))
  ;; Stage 3: Extract for deny list check
  (let ((commands (extract-pipeline-commands parsed)))
    ;; Stage 4: Extract again for operations
    (let ((semantics (jf/bash-extract-semantics parsed)))
      ...)))
```

**Optimized approach:**
```elisp
(let* ((parsed (jf/bash-parse command))
       (semantics (jf/bash-extract-semantics parsed))  ; Extract once
       (commands (extract-pipeline-commands parsed)))
  ;; All stages use same semantics
  ...)
```

**Rationale:**
- Semantic extraction is expensive (plugin system, AST traversal)
- No need to extract twice for same command
- Simpler to pass semantics through pipeline than re-extract

**Implementation:** Refactor `jf/gptel-scope--validate-command-semantics` to extract semantics once at top.

## Risks / Trade-offs

### Risk 1: Bash-Parser Extraction Errors
**Risk:** If bash-parser fails to extract file operations, command incorrectly allowed via no-op allowance.

**Example:** `python3 script.py` extracts no operations due to parser bug → allowed when should require paths.execute.

**Mitigation:**
- Parse completeness enforcement catches parser failures
- Bash-parser has comprehensive test suite (700+ tests)
- Integration tests validate end-to-end extraction
- Monitor for false positives in production use

**Residual risk:** Subtle extraction bugs could bypass validation. Acceptable given parser reliability and deny list backstop.

---

### Risk 2: Unknown Dangerous Commands Bypass Deny List
**Risk:** New dangerous command not in deny list could be allowed via no-op allowance or operation validation.

**Example:** `dangerous-tool --version` allowed via no-op, `dangerous-tool run` allowed if paths validate.

**Mitigation:**
- Deny list maintained as dangerous commands discovered
- LLM can request scope expansion for unknown commands
- Users can add to deny list via scope expansion
- Parse completeness catches suspicious commands with incomplete parses

**Residual risk:** Zero-day dangerous commands could execute before added to deny list. Acceptable for LLM-driven workflow.

---

### Risk 3: Breaking Change Migration Burden
**Risk:** All scope profiles with categories section will fail to load, breaking existing sessions.

**Impact:**
- 5 default profiles need updates (system-explorer, bash-enabled, coding, research, restricted)
- User custom profiles need manual migration
- Existing sessions with old scope.yml will fail

**Mitigation:**
- Clear migration guide in commit message
- Error message explains categories removal and migration
- Update all default profiles in same commit
- Document migration in CLAUDE.md

**Migration steps:**
1. Remove `categories` subsection from `bash_tools`
2. Keep `deny` list
3. Add minimal deny list: `[sudo, dd, chmod, chown]`

**Residual risk:** User custom profiles may break. Acceptable for architectural improvement.

---

### Risk 4: Performance Regression from Double Parsing
**Risk:** Semantic extraction now happens for every command (even no-ops), adding overhead.

**Current:** Category check is simple list membership (fast).

**New:** Parse + extract semantics for every command (slower).

**Mitigation:**
- Semantic extraction is fast enough for interactive use (< 10ms)
- No-op check short-circuits file validation (saves work)
- Could optimize by caching parse results (deferred until proven necessary)

**Residual risk:** Slower validation for high-frequency commands. Acceptable for LLM-driven workflow.

---

### Risk 5: Test Coverage Gaps During Migration
**Risk:** Removing category tests without equivalent operation tests could reduce coverage.

**Mitigation:**
- Spec scenarios comprehensive (68 scenarios across 4 capabilities)
- One test per scenario ensures coverage
- Keep existing integration tests (ERT) for file operations
- Add new Buttercup tests for no-op allowance and deny list

**Coverage plan:**
- Remove: Category membership tests (~15 tests)
- Keep: File operation validation tests (~40 tests)
- Add: No-op allowance tests (~6 tests)
- Add: Deny list pipeline tests (~4 tests)

**Residual risk:** Edge cases in category logic could be missed. Mitigated by comprehensive spec scenarios.

---

## Implementation Phases

### Phase 1: Core Pipeline Refactoring
**Goal:** Modify validation pipeline to use operation-first validation.

**Changes:**
1. Refactor `jf/gptel-scope--validate-command-semantics`:
   - Extract semantics once at top
   - Move semantics extraction before deny list check (Stage 3 → Stage 2)
   - Add no-op check at Stage 4
   - Rename file validation to Stage 5
2. Modify `jf/gptel-scope--validate-pipeline-commands`:
   - Remove category checking logic
   - Keep only deny list validation
3. Add `jf/gptel-scope--check-no-op`:
   - Check if `:filesystem` domain empty
   - Return nil (success) if no operations

**Tests:**
- Add `no-op-allowance-spec.el` (6 tests)
- Modify `pipeline-validation-spec.el` (remove category tests)

**Deliverable:** Pipeline validates commands via deny list + no-op + operations.

---

### Phase 2: Scope Profile Schema Updates
**Goal:** Remove categories section from scope profile schema and all default profiles.

**Changes:**
1. Update `jf/gptel-scope-profile--load`:
   - Remove categories parsing logic
   - Validate that categories section not present (error if found)
2. Update all default profiles:
   - `system-explorer.yml`: Remove categories, keep deny list
   - `bash-enabled.yml`: Remove categories, keep deny list
   - `coding.yml`: Remove categories, keep deny list
   - `research.yml`: Remove categories
   - `restricted.yml`: Already has no bash_tools section
3. Update schema validation:
   - Error if `bash_tools.categories` present
   - Error message guides migration

**Tests:**
- Update `test-schema.el` (remove category loading tests)
- Add schema validation test for categories rejection

**Deliverable:** All profiles use deny-list-only schema.

---

### Phase 3: Test Suite Updates
**Goal:** Add Buttercup tests for new behavior, remove obsolete category tests.

**Changes:**
1. Create test files:
   - `config/gptel/tools/test/unit/no-op-allowance-spec.el`
   - `config/gptel/tools/test/behavioral/operation-first-workflow-spec.el`
2. Create test helpers:
   - `config/gptel/tools/test/test-helpers.el`
   - `make-test-scope-config`, `make-no-op-semantics`, `make-execute-semantics`
3. Remove obsolete tests:
   - Category membership tests from `pipeline-validation-spec.el`
4. Update existing integration tests (ERT):
   - Verify they still pass with new pipeline
   - No changes needed (file operations unchanged)

**Tests:**
- 10+ new Buttercup tests
- ~15 removed category tests
- ~40 existing integration tests validated

**Deliverable:** Comprehensive test coverage for operation-first validation.

---

### Phase 4: Documentation and Specs Updates
**Goal:** Update specs to reflect new validation model.

**Changes:**
1. Archive delta specs to main specs:
   - `openspec/specs/gptel/scope-validation-pipelines/spec.md`
   - `openspec/specs/gptel/scope-profiles.md`
   - `openspec/specs/gptel/bash-tools.md`
2. Add new spec:
   - `openspec/specs/gptel/no-op-command-allowance/spec.md`
3. Update CLAUDE.md:
   - Document new validation model
   - Add migration guide for categories removal
   - Update scope profile examples

**Deliverable:** Documentation reflects operation-first validation.

---

## Open Questions

### Q1: Should rm stay in deny list or trust paths validation?

**Context:** rm is destructive but could be validated via paths.write. Current deny list includes rm for extra safety.

**Options:**
- Keep in deny list: Extra safety, but users must request expansion
- Remove from deny list: Trust operation validation, simpler deny list

**Recommendation:** Keep in deny list initially. Can remove later if operation validation proves sufficient.

---

### Q2: How to handle commands with mixed operations?

**Context:** Some commands have both :read and :execute operations (e.g., script that reads config then executes).

**Current behavior:** Validate each operation independently. Command allowed if all operations in scope.

**Question:** Is this correct? Or should execute operation require all operations to be in execute scope?

**Recommendation:** Current behavior is correct. Each operation validated against its specific scope.

---

### Q3: Should parse completeness be configurable per command?

**Context:** Some commands (complex pipelines) may have incomplete parses but are safe.

**Current:** Global `security.enforce_parse_complete` flag.

**Question:** Should we allow per-command parse completeness bypass?

**Recommendation:** No. Global flag is sufficient. Users can disable strict parsing if needed.

---

## Migration Guide

### For Default Profiles (Included in Commit)

All default profiles updated to remove categories:

```yaml
# Before
bash_tools:
  categories:
    read_only:
      commands: [ls, cat, grep, git, python3, node]
    safe_write:
      commands: [mkdir, touch, git add]
  deny: [sudo, rm, chmod]

# After
bash_tools:
  deny: [sudo, dd, chmod, chown]
```

### For User Custom Profiles

**Detection:** If scope.yml has `bash_tools.categories`, loading will fail with error:

```
Error: bash_tools.categories section no longer supported.
Migration: Remove categories section, keep only deny list.
See CLAUDE.md for migration guide.
```

**Migration steps:**
1. Open scope.yml in session directory
2. Remove entire `categories:` subsection from `bash_tools`
3. Keep `deny:` list, add edge cases if needed (sudo, dd, chmod, chown)
4. Save and reload session

**Example:**
```yaml
# Remove this:
bash_tools:
  categories:
    read_only:
      commands: [...]
    safe_write:
      commands: [...]
  deny: [sudo, rm]

# Replace with:
bash_tools:
  deny: [sudo, dd, chmod, chown]
```

Commands previously in read_only/safe_write are now allowed via:
- No-op allowance (version checks, help flags)
- Operation validation (file operations validated against paths)
