# Bash Parser Corpus Updates

**Version**: 1.1
**Date**: 2026-03-04
**Focus**: File impact detection optimization

## Summary

Updated bash parser corpus to **prioritize file impact detection** over parser correctness. Added 19 critical tests for file operations and removed 12 edge cases that don't affect file detection.

**Net change**: 154 → 161 tests (+7 tests, but with much higher file-impact value)

---

## Changes Made

### ✅ Added Critical File-Impact Patterns (19 tests)

#### 1. **Xargs Patterns** (5 tests) → `corpus-parse-combined-patterns.el`
**Why**: Xargs is extremely common in LLM-generated commands for batch file operations

- `combined-xargs-001`: `find . -name '*.tmp' | xargs rm -f`
- `combined-xargs-002`: `find . -name '*.log' -print0 | xargs -0 rm -f` (handles spaces)
- `combined-xargs-003`: `git ls-files -d | xargs git rm`
- `combined-xargs-004`: `ls *.el | xargs -I {} emacs --batch -l {}` (placeholder syntax)
- `combined-xargs-005`: `find . -name '*.txt' | xargs -n 1 cat` (batch size)

**File impact**: Indirect batch operations (delete, read, modify) on multiple files

---

#### 2. **Command Substitution in Redirects** (4 tests) → `corpus-parse-combined-patterns.el`
**Why**: Dynamic filename generation is critical for detecting file writes

- `combined-cmdsub-redirect-001`: `echo 'data' > log-$(date +%Y-%m-%d).txt`
- `combined-cmdsub-redirect-002`: `cat data.txt > backup-$(whoami)-$(date +%s).txt` (multiple subs)
- `combined-cmdsub-redirect-003`: `cat <<'EOF' > config-$(date +%Y-%m-%d).yml` (heredoc + cmdsub redirect)
- `combined-cmdsub-redirect-004`: `grep ERROR log > errors-$(basename $(pwd)).txt` (nested cmdsub in redirect)

**File impact**: Dynamically-named file writes (previously undetected!)

**Parse expectation**: `:target-has-substitution t` flag on redirect

---

#### 3. **Find -Exec Improvements** (5 tests) → `corpus-parse-combined-patterns.el`
**Why**: Clarify that `-exec` is a BLOCK not a FLAG (improves file impact detection)

- `combined-find-exec-001`: `find . -name '*.txt' -exec cp {} backup/ \;`
- `combined-find-exec-002`: `find . -name '*.log' -exec rm {} \;` (delete all matches)
- `combined-find-exec-003`: `find . -type f -exec grep {} \; -exec echo {} \;` (multiple blocks)
- `combined-find-exec-004`: `find . -name '*.txt' -exec sh -c 'cat "$1" > "$1.bak"' _ {} \;` (shell script)
- `combined-find-exec-005`: `find . -name '*.org' -exec cp {} backup/{} \;` (placeholder in target)

**File impact**: Operations on EACH found file (critical for batch detection)

**Parse expectation**: `:exec-blocks` list with `:exec-command`, `:exec-args`, `:placeholder`

---

### ❌ Removed Low File-Impact Tests (12 tests)

#### 1. **Command Substitution Edge Cases** (2 removed)

- ~~`cmdsub-edge-004`~~: `echo $()` - Empty substitution
  - **Reason**: Generates empty string, no file impact

- ~~`cmdsub-edge-005`~~: `echo \$(not-a-substitution)` - Escaped substitution
  - **Reason**: Literal string, not executed

**New total**: 30 → 28 tests

---

#### 2. **Heredoc Edge Cases** (2 removed)

- ~~`heredoc-edge-001`~~: `cat <<''` - Empty delimiter
  - **Reason**: Invalid in practice, no file impact

- ~~`heredoc-edge-003`~~: `cat <<'E!O@F#'` - Special chars in delimiter
  - **Reason**: Delimiter format doesn't affect file detection

**New total**: 25 → 23 tests

---

#### 3. **Conditional Test Operators** (8 simplified to 2)

**Removed** (6 tests):
- ~~`cond-testop-001`~~: `[ -z "$VAR" ]` - String empty
- ~~`cond-testop-002`~~: `[ -n "$VAR" ]` - String not empty
- ~~`cond-testop-003`~~: `[ "$a" = "$b" ]` - String equal
- ~~`cond-testop-004`~~: `[ $n -eq 0 ]` - Numeric equal
- ~~`cond-testop-005`~~: `[ $n -lt 10 ]` - Numeric less than
- ~~`cond-testop-006`~~: `[ $n -gt 5 ]` - Numeric greater than

**Reason**: String/numeric tests don't affect file operations. For file impact detection, we only care about file test operators (`-f`, `-d`, `-e`, `-r`, `-w`, `-x`).

**Kept** (2 tests):
- `cond-testop-007`: `[[ -f file && -r file ]]` - Compound file test (AND)
- `cond-testop-008`: `[[ -d dir || -f file ]]` - Compound file test (OR)

**Removed edge cases** (2 tests):
- ~~`cond-edge-003`~~: `if (( x > 5 ))` - Arithmetic test
  - **Reason**: No file impact

- ~~`cond-edge-004`~~: `if [ -f file ]; then :; fi` - Null command (no-op)
  - **Reason**: No file impact

**New total**: 24 → 16 tests

---

## Final Corpus Statistics

| Corpus File | Original | New | Change | File Impact Priority |
|-------------|----------|-----|--------|---------------------|
| **combined-patterns** | 25 | 44 | **+19** | ⭐⭐⭐⭐⭐ CRITICAL |
| **command-substitution** | 30 | 28 | -2 | ⭐⭐⭐⭐⭐ CRITICAL |
| **for-loop** | 26 | 26 | 0 | ⭐⭐⭐⭐⭐ CRITICAL |
| **heredoc** | 25 | 23 | -2 | ⭐⭐⭐ MODERATE |
| **conditional** | 24 | 16 | -8 | ⭐⭐ LOW |
| **process-substitution** | 17 | 17 | 0 | ⭐ DEFER |
| **llm-scenarios** | ~150 | N/A | N/A | Mixed (not indexed) |
| **TOTAL (indexed)** | **147** | **154** | **+7** | - |

**Real-world examples**: ~35 tests marked "REAL: From research"

---

## Key Improvements for File Impact Detection

### 1. **Xargs Detection**
**Before**: Pipeline to xargs was parsed but file operations were not extracted
**After**: Parse expectations include `:xargs-command` and `:xargs-args` for batch operation detection

**Example**:
```bash
find . -name '*.tmp' | xargs rm -f
```
**Impact**: Deletes ALL `.tmp` files (critical to detect!)

---

### 2. **Dynamic Filename Detection**
**Before**: Command substitution in redirect targets was missed
**After**: Parse expectations include `:target-has-substitution t` flag

**Example**:
```bash
echo 'data' > log-$(date +%Y-%m-%d).txt
```
**Impact**: Writes to dynamically-named file (timestamp-based)

---

### 3. **Find -Exec Block Clarity**
**Before**: `-exec` treated as FLAG, not BLOCK (confused file detection)
**After**: Parse expectations use `:exec-blocks` with `:exec-command`, `:placeholder`

**Example**:
```bash
find . -name '*.txt' -exec cp {} backup/ \;
```
**Impact**: Copies EACH `.txt` file to backup/ (not just the pattern!)

---

## Testing Strategy

### Priority 1: Core File Impact (Must implement first)
1. ✅ Command substitution in file arguments
2. ✅ For-loops with command substitution
3. ✅ Xargs patterns (NEW!)
4. ✅ Command substitution in redirects (NEW!)
5. ✅ Find -exec blocks (improved expectations)

### Priority 2: Integration Tests (Validate interactions)
6. Combined patterns corpus (44 tests)
7. For-loop + conditional + file operations
8. Heredoc in command substitution for git commits

### Priority 3: Nice to Have (Defer if time-constrained)
9. Brace expansion for file patterns
10. Process substitution (only 1 real example!)
11. Heredoc delimiter variations
12. Arithmetic expansion (not file-related)

---

## Cross-Reference with File Operations Tests

These parse corpus tests should enable extraction tested in `test-corpus-file-operations.el`:

| Parse Feature | File Operations Test | Status |
|---------------|---------------------|--------|
| Command substitution | `variable-chain-001` to `-003` | ✅ Covered |
| Xargs | **MISSING** → Add `xargs-001` to `-003` | 🔴 TODO |
| Dynamic redirects | **MISSING** → Add `redirect-cmdsub-001` | 🔴 TODO |
| Find -exec | `find-001` to `-003` (basic) | ⚠️ Need improvements |
| For-loops | `chain-*` tests | ✅ Covered |
| Heredoc + redirect | `redirect-004` | ✅ Covered |

**Action Items**:
1. Add xargs file operations tests to `test-corpus-file-operations.el`
2. Add dynamic redirect filename tests
3. Improve find -exec tests to show placeholder expansion

---

## Documentation Updates

### Updated Files
- ✅ `corpus-parse-combined-patterns.el` - Added 19 tests, updated summary
- ✅ `corpus-parse-command-substitution.el` - Removed 2 edge cases, updated count
- ✅ `corpus-parse-heredoc.el` - Removed 2 edge cases, updated count
- ✅ `corpus-parse-conditional.el` - Removed 8 tests, updated summary
- ✅ `corpus-index.el` - Updated version to 1.1
- ✅ `DEPRECATED-TESTS.md` - Documented removed tests with rationale
- ✅ `CORPUS-UPDATES.md` - This file!

### Pending Updates
- [ ] `test-corpus-file-operations.el` - Add xargs, dynamic redirects, improved find-exec
- [ ] Update GitHub issue/PR with corpus changes
- [ ] Re-run corpus statistics after changes: `(jf/bash-corpus-print-stats)`

---

## Validation Commands

```elisp
;; Load updated corpus
(load-file "corpus-index.el")
(jf/bash-corpus-load-all)

;; Check statistics
(jf/bash-corpus-print-stats)

;; Verify no duplicate IDs
(jf/bash-corpus-check-duplicate-ids) ; => should return nil

;; Validate structure
(jf/bash-corpus-validate-all) ; => should return nil

;; Test query functions
(length (jf/bash-corpus-get-combined-tests)) ; => should be 44
(length (jf/bash-corpus-real-only)) ; => should be ~35
```

---

## Next Steps

### Immediate (High Priority)
1. [ ] Add corresponding file operations tests for new parse patterns
2. [ ] Verify parse expectations support file extraction
3. [ ] Run full test suite to ensure no regressions

### Short-term (This Week)
4. [ ] Implement xargs detection in bash-parser
5. [ ] Implement command-substitution in redirect detection
6. [ ] Improve find -exec parsing

### Medium-term (This Month)
7. [ ] Cross-validate parse corpus against file-operations corpus
8. [ ] Document file impact extraction algorithm
9. [ ] Create integration tests: parse → extract → validate file impacts

---

## Philosophy Shift

**Old approach**: Parse bash perfectly, extract files later
**New approach**: Parse for file detection, defer parser perfection

**Key insight**: An 80% accurate parser that catches 100% of file operations is better than a 100% accurate parser that misses 20% of file impacts due to complexity.

**Mantra**: *"Parse what affects files, not what affects parsing."*

---

## Version History

- **v1.0** (Initial): 147 tests across 6 corpus files
- **v1.1** (2026-03-04): 154 tests (+19 file-impact, -12 edge cases)
  - Added: xargs, dynamic redirects, improved find-exec
  - Removed: empty/escaped substitutions, string/numeric test operators, arithmetic/null tests
  - Focus: File impact detection over parser correctness
