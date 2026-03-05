# PWD/Directory Context Test Results

## Test Execution Summary

**Date:** 2026-03-05
**Purpose:** Validate PWD/directory parameter integration for scope-shell-tools
**Status:** Partial test coverage - 29 tests created, subset executed

## Tests Created

Total: **29 specification tests** organized by feature area:

1. **Explicit $PWD Variable References** (4 tests)
   - test-pwd-explicit-variable-simple
   - test-pwd-explicit-variable-nested-path
   - test-pwd-explicit-variable-in-redirection
   - test-pwd-without-var-context

2. **Relative Path Resolution - Current Directory (.)** (3 tests)
   - test-relative-path-dot-alone
   - test-relative-path-dot-ls
   - test-relative-path-dot-in-pipeline

3. **Relative Path Resolution - ./ Prefix** (4 tests)
   - test-relative-path-dot-slash-file
   - test-relative-path-dot-slash-nested
   - test-relative-path-dot-slash-script-execution
   - test-relative-path-dot-slash-with-args

4. **Relative Path Resolution - ../ Parent** (4 tests)
   - test-relative-path-parent-simple
   - test-relative-path-parent-multiple-levels
   - test-relative-path-parent-with-subdir
   - test-relative-path-parent-script-execution

5. **Variable Chains with PWD Context** (3 tests)
   - test-variable-chain-pwd-assignment
   - test-variable-chain-relative-path-assignment
   - test-variable-chain-parent-path-assignment

6. **Nested Structures** (3 tests)
   - test-nested-for-loop-relative-pattern
   - test-nested-conditional-relative-test
   - test-nested-find-exec-relative-paths

7. **Command Substitution with pwd** (3 tests)
   - test-pwd-substitution-simple
   - test-pwd-substitution-nested
   - test-pwd-backtick-substitution

8. **Edge Cases** (4 tests)
   - test-pwd-context-multiple-operations
   - test-pwd-context-mixed-absolute-relative
   - test-pwd-context-without-pwd-in-context
   - test-pwd-context-empty-var-context

9. **Execution Parity** (1 test)
   - test-execution-parity-relative-path

## Test Results (Partial Execution)

Tests matching `^test-pwd-*`: **11 tests executed**

### ✅ PASSING (6 tests) - Existing functionality works

These tests verify features that already work with current var-context infrastructure:

1. **test-pwd-explicit-variable-simple** ✓
   - `cat $PWD/file.txt` with PWD=/base/dir → `/base/dir/file.txt`
   - Status: Variable resolution works correctly

2. **test-pwd-explicit-variable-nested-path** ✓
   - `cat $PWD/subdir/file.txt` → `/base/dir/subdir/file.txt`
   - Status: Variable resolution works correctly

3. **test-pwd-explicit-variable-in-redirection** ✓
   - `echo test > $PWD/output.txt` → `/base/dir/output.txt`
   - Status: Variable resolution in redirects works

4. **test-pwd-without-var-context** ✓
   - `cat $PWD/file.txt` without var-context → keeps `$PWD` unresolved
   - Status: Correctly preserves unresolved variables

5. **test-pwd-context-without-pwd-in-context** ✓
   - `cat ./file.txt` without PWD in var-context → keeps `./file.txt` relative
   - Status: Correctly doesn't guess PWD

6. **test-pwd-context-empty-var-context** ✓
   - `cat ../file.txt` with empty var-context → keeps `../file.txt` relative
   - Status: Correctly leaves paths unresolved

### ❌ FAILING (5 tests) - Features need implementation

These tests document required behavior that isn't implemented yet:

1. **test-pwd-substitution-simple** ✗ (expected failure)
   - `cat $(pwd)/file.txt` should resolve to `/base/dir/file.txt`
   - **Missing:** pwd command substitution resolution using PWD from var-context

2. **test-pwd-substitution-nested** ✗ (expected failure)
   - `cat $(basename $(pwd))/file.txt` with PWD=/Users/name/project → `project/file.txt`
   - **Missing:** Nested command substitution analysis

3. **test-pwd-backtick-substitution** ✗ (expected failure)
   - `` cat `pwd`/file.txt `` should resolve to `/base/dir/file.txt`
   - **Missing:** Backtick substitution handling

4. **test-pwd-context-multiple-operations** ✗ (expected failure)
   - `cat ./input.txt | grep pattern > ./output.txt`
   - **Missing:** Relative path resolution in pipelines

5. **test-pwd-context-mixed-absolute-relative** ✗ (expected failure)
   - `cp /tmp/file.txt ./dest.txt` should preserve absolute, resolve relative
   - **Missing:** Relative path resolution

### ⏸️  NOT EXECUTED (18 tests)

These tests were created but not executed due to test pattern matching:

- 3 tests for `.` (current directory) resolution
- 4 tests for `./` prefix resolution
- 4 tests for `../` parent directory resolution
- 3 tests for variable chain resolution
- 3 tests for nested structure resolution
- 1 execution parity validation test

**Reason:** Test pattern `^test-pwd-` only matched tests with that specific prefix.

## Implementation Gaps Identified

### 1. Relative Path Resolution (CRITICAL - Security Requirement)

**Current State:** Parser does NOT resolve relative paths
**Required:** Parser must resolve `.`, `./`, `../` using PWD from var-context

**Impact:** This is the PRIMARY security gap. Without relative path resolution:
- `cat ./file.txt` extracts `./file.txt` (relative)
- Scope validation cannot verify if `./file.txt` is within allowed paths
- Attacker could use relative paths to bypass validation

**Examples needing implementation:**
- `cat ./file.txt` with PWD=/dir → `/dir/file.txt`
- `cat ../file.txt` with PWD=/dir/sub → `/dir/file.txt`
- `find . -name '*.txt'` with PWD=/dir → search scope `/dir`
- `./script.sh` with PWD=/dir → execute `/dir/script.sh`

**Implementation location:** Likely needs new function in `bash-parser-variables.el` or `bash-parser-file-ops.el`

### 2. Command Substitution Analysis (DESIGN QUESTION)

**Current State:** Parser recognizes `$(pwd)` as command substitution but doesn't resolve it
**Question:** Should static analysis resolve pwd substitutions?

**Approaches:**
1. **Simple:** Recognize `$(pwd)` and treat as equivalent to `$PWD` variable
2. **Complex:** Fully analyze command substitutions to predict output
3. **Conservative:** Reject commands with pwd substitutions as unresolvable

**Examples:**
- `cat $(pwd)/file.txt` - Could resolve if we treat `$(pwd)` = `$PWD`
- `cat $(basename $(pwd))/file.txt` - Much harder to analyze statically

**Recommendation:** Start with approach #1 (simple pwd recognition) for common case

### 3. Variable Chain Tracking (FUTURE ENHANCEMENT)

**Current State:** Parser doesn't track variable assignments through command chains
**Impact:** Medium - less common than relative paths

**Examples:**
- `BASE=$PWD; cat $BASE/file.txt` - Need to track BASE=PWD assignment
- `DIR=./sub; cat $DIR/file.txt` - Need to resolve relative path in assignment

**Implementation:** Would require command chain analysis and variable tracking

## Security Model Validation

### Critical Security Requirement (from bead)

> The parser's understanding of what paths will be accessed must EXACTLY match what happens during actual command execution. Any mismatch is a security vulnerability that could allow unauthorized file access.

### Current Status

**BLOCKED:** Relative path resolution gap prevents security model validation

Without relative path resolution:
1. ❌ Parser extracts relative paths (./file.txt)
2. ❌ Cannot validate against scope.yml patterns (need absolute paths)
3. ❌ Security gap: Relative paths bypass validation

With relative path resolution:
1. ✓ Parser extracts absolute paths (/base/dir/file.txt)
2. ✓ Can validate against scope.yml patterns
3. ✓ Security model complete

### Next Steps for Security Validation

1. **Implement relative path resolution** (CRITICAL)
2. Re-run all 29 tests to verify resolution works
3. Create integration tests with actual scope.yml validation
4. Test execution parity (parser paths = actual execution paths)

## Integration Design Summary

### Confirmed Design (from discussion)

```
run_bash_command(command, directory)
  ↓
1. Create var-context = ((PWD . directory))
2. Parse: jf/bash-parse(command)
3. Extract ops: jf/bash-extract-file-operations(parsed, var-context)
4. For each operation (:file, :operation):
   - Validate :file against scope.yml patterns:
     - :read operations → paths.read
     - :write operations → paths.write
     - :execute operations → paths.execute (future)
5. If all operations allowed → execute
6. Otherwise → return validation error
```

### Key Changes from Current Implementation

- ✗ Remove command categorization (read_only, safe_write, dangerous, deny)
- ✓ Add file-operation validation against scope.yml patterns
- ✓ Validate EVERY file operation, not just base command
- ✓ Closes security hole where `ls | xargs rm` was allowed

### Backward Compatibility

**Decision:** No backward compatibility (per user directive)
**Impact:** Complete replacement of naive scope controls

## Recommendations

### Immediate Priority

1. **Implement relative path resolution** in bash-parser
   - Location: `bash-parser-file-ops.el` or new module
   - Function: `jf/bash--resolve-relative-path(path, pwd)`
   - Integration: Call during file operations extraction when PWD in var-context

2. **Run full test suite** (all 29 tests)
   - Verify relative path resolution works
   - Identify remaining edge cases

3. **Document design decisions**
   - How to handle pwd command substitutions
   - Whether to implement variable chain tracking
   - Edge cases and limitations

### Future Work

1. Implement pwd command substitution recognition
2. Add variable chain tracking for assignments
3. Create integration tests with scope.yml validation
4. Test execution parity validation
5. Performance testing with complex commands

## Test File Location

`config/experiments/bash-parser/test/test-pwd-directory-context.el`

All tests are documented with:
- Security implications
- Expected behavior
- Implementation requirements
- `:expected-result :failed` for tests requiring new features
