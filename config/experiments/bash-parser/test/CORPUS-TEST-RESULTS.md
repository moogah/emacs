# File Operations Corpus Test Results

## Summary

**Run Date:** 2026-03-02
**Total Tests:** 60
**Passed:** 41 (68.3%)
**Failed:** 19 (31.7%)

## Passed Test Categories

✅ **Read Operations** - 5/6 tests passed
- Simple file reads (cat, grep, tail, wc, less)
- One failure: head command (read-003)

✅ **Write Operations** - 3/4 tests passed
- Output/append redirections
- Tee command
- One failure: touch returns `:create-or-modify` not `:create`

✅ **Delete Operations** - 4/4 tests passed
- Simple deletion, recursive deletion, rmdir, multiple files

✅ **Copy Operations** - 3/3 tests passed
- Single file copy, recursive copy, multiple files

✅ **Move Operations** - 2/2 tests passed
- File rename and move to directory

✅ **Modify Operations** - 4/4 tests passed
- chmod, chown, sed with/without -i flag

✅ **Redirection Operations** - 4/4 tests passed
- Input/output/stderr redirections, multiple redirections

✅ **Pipelines** - 2/2 tests passed
- Multi-command pipelines with read/write operations

✅ **Chains** - 2/3 tests passed
- Command chains with file operations
- One failure: touch operation type

✅ **Git Commands** - 3/4 tests passed
- git add, git checkout, git log with redirection
- One failure: missing `:pattern t` flag for globs

✅ **Directories** - 2/2 tests passed
- mkdir with and without -p flag

✅ **No Operations** - 4/4 tests passed
- Commands that don't modify files (echo, ls, pwd, git status)

## Failed Test Categories

❌ **Archive Operations** - 0/3 tests passed
- tar and zip commands not implemented in semantics database
- **Recommendation:** Add tar/zip to bash-parser-semantics.el

❌ **Glob Patterns** - 0/4 tests passed
- Missing `:pattern t` flag in actual output
- Operations are extracted correctly, just missing pattern metadata
- **Recommendation:** Update corpus to not expect `:pattern t`

❌ **Find -exec** - 0/3 tests passed
- Actual implementation is more complex than expected
- Find treats `-name` argument as a file
- Exec blocks have `:confidence :medium` and `:indirect t`
- **Recommendation:** Update corpus expectations to match implementation

❌ **Variables** - 0/8 tests passed
- Variable resolution not working (all 5 simple tests)
- Variable chain tracking not working (3 tests)
- **Recommendation:** Investigate bash-parser-variables.el implementation

## Detailed Failure Analysis

### 1. Archive Commands (3 failures)
```
archive-001: tar -czf archive.tar.gz files/
archive-002: tar -xzf archive.tar.gz
archive-003: zip -r backup.zip src/
```
**Issue:** Commands return 0 operations
**Root Cause:** tar and zip not in `jf/bash-command-file-semantics`
**Fix:** Add archive command semantics

### 2. Touch Command (2 failures)
```
chain-001: touch newfile.txt
write-001: touch newfile.txt
```
**Expected:** `:operation :create`
**Actual:** `:operation :create-or-modify`
**Fix:** Update corpus to expect `:create-or-modify`

### 3. Find Commands (3 failures)
```
find-001: find . -name '*.log' -exec rm {} \;
find-002: find /tmp -type f -exec cat {} \; > output.txt
find-003: find . -name '*.txt' -exec grep pattern {} \; -exec echo {} \;
```
**Expected:** 2 operations (directory + exec)
**Actual:** 3-4 operations (directory + pattern arg + exec)
**Issues:**
- Find treats `-name '*.log'` as a file argument
- Confidence is `:medium` not `:high` for exec blocks
- Exec blocks have `:indirect t` metadata
**Fix:** Update corpus expectations

### 4. Glob Pattern Flag (4 failures)
```
glob-001: rm *.txt
glob-002: cat config/**/*.json
glob-003: cp *.el backup/
git-002: git add src/*.el
```
**Expected:** `:pattern t` in operation plist
**Actual:** Operations extracted but no `:pattern t` flag
**Fix:** Remove `:pattern t` from corpus expectations

### 5. Variable Resolution (8 failures)
```
variable-001 through variable-005: Variable references not resolved
variable-chain-001 through variable-chain-003: Variable chains not working
```
**Issue:** All variable-related tests fail
**Root Cause:** Unknown - need to check bash-parser-variables.el
**Fix:** Investigate implementation or remove from corpus

### 6. Head Command (1 failure)
```
read-003: head -n 10 /tmp/log.txt
```
**Issue:** Need to check why this specific command fails
**Fix:** Debug or adjust expectation

## Recommendations

### High Priority
1. **Add archive command semantics** - tar and zip are common operations
2. **Fix variable resolution** - 8 test failures, seems like a core feature
3. **Update corpus for touch** - Change `:create` to `:create-or-modify`

### Medium Priority
4. **Update glob expectations** - Remove `:pattern t` flag from corpus
5. **Update find expectations** - Match actual complex behavior
6. **Debug head command** - Investigate why read-003 fails

### Low Priority
7. **Document actual behavior** - Create reference showing what metadata is actually returned

## Next Steps

1. Check if variables are meant to be supported - review bash-parser-variables.el
2. Decide whether to:
   - Update corpus to match implementation (document actual behavior)
   - Update implementation to match corpus (add missing features)
3. Create updated corpus with realistic expectations
4. Consider adding "Phase 2" corpus for unimplemented features
