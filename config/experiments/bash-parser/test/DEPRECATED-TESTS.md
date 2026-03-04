# Deprecated Test Cases

This file documents test cases that have been marked for deprecation or removal because they provide minimal value for **file impact detection** (the primary goal of the bash parser).

## Deprecation Criteria

Tests are deprecated if they:
1. Test parser edge cases that don't affect file operations
2. Focus on syntax correctness over file detection
3. Cover extremely rare patterns (< 1% of real usage)
4. Duplicate coverage already provided by simpler tests

## Recommended Actions

- **REMOVE**: Delete from corpus entirely
- **SIMPLIFY**: Collapse multiple similar tests into one
- **DEFER**: Keep but mark as low priority for implementation

---

## Heredoc Corpus - Edge Cases (Low File Impact)

### heredoc-edge-001 - Empty delimiter
**Status**: REMOVE
**Reason**: Empty delimiter is invalid in practice, no file impact
**Command**: `cat <<'' ... `
**File Impact**: None (syntax edge case)

### heredoc-edge-003 - Special characters in delimiter
**Status**: REMOVE
**Reason**: Delimiter format doesn't affect file detection
**Command**: `cat <<'E!O@F#' ... E!O@F#`
**File Impact**: None (still just writes heredoc content)

### heredoc-complex-006 - Escaped special chars in heredoc
**Status**: SIMPLIFY
**Reason**: Escaping affects content but not file operations
**Command**: `cat <<EOF\nLine with backticks: \\`date\\` ...`
**File Impact**: Minimal (content transformation only)

### heredoc-basic-005 - Empty heredoc
**Status**: REMOVE
**Reason**: Edge case, no file content written
**Command**: `cat <<END\nEND`
**File Impact**: None (empty write)

**Recommendation**: Keep only heredoc tests with:
- Redirects to files (heredoc IS the file content)
- Variable expansion affecting filenames
- Real-world patterns (git commits, config generation)

---

## Command Substitution Corpus - Edge Cases

### cmdsub-edge-004 - Empty substitution
**Status**: REMOVE
**Reason**: Edge case, generates empty string
**Command**: `echo $()`
**File Impact**: None

### cmdsub-edge-005 - Escaped substitution
**Status**: REMOVE
**Reason**: Literal string, not a substitution at all
**Command**: `echo \\$(not-a-substitution)`
**File Impact**: None (not executed)

### cmdsub-backtick-001 - Legacy backtick syntax
**Status**: DEFER
**Reason**: Rare in modern bash, LLMs prefer $()
**Command**: `echo \`pwd\``
**File Impact**: Low priority (1% of substitutions use backticks)

**Recommendation**: Focus on:
- Nested substitutions (file path generation)
- Substitutions in for-loops (file list generation)
- Substitutions in redirects (dynamic filenames)

---

## Conditional Corpus - Test Operator Variations

### cond-testop-002 through cond-testop-006 - Numeric/string operators
**Status**: SIMPLIFY (collapse into 2 tests)
**Reason**: All test operators are just file reads for detection purposes
**Commands**:
- `[ -z "$VAR" ]` (string empty)
- `[ -n "$VAR" ]` (string not empty)
- `[ "$a" = "$b" ]` (string equal)
- `[ $n -eq 0 ]` (numeric equal)
- `[ $n -lt 10 ]` (numeric less than)
- `[ $n -gt 5 ]` (numeric greater than)

**File Impact**: None (these test variables, not files)

**Recommendation**: Collapse to:
1. One test for file operators (`-f`, `-d`, `-e`, `-r`, `-w`, `-x`)
2. One test for non-file operators (strings, numbers)

### cond-edge-003 - Arithmetic test
**Status**: REMOVE
**Reason**: No file impact
**Command**: `if (( x > 5 )); then echo "greater"; fi`
**File Impact**: None (arithmetic only)

### cond-edge-004 - Null command in then block
**Status**: REMOVE
**Reason**: No-op command, no file impact
**Command**: `if [ -f file ]; then :; fi`
**File Impact**: None (`:` is no-op)

**Recommendation**: Keep only:
- File test operators (`-f`, `-d`, `-e`)
- Conditionals with file operations in branches (mv, cp, rm)
- Real examples from research

---

## Process Substitution Corpus - Entire Category

### procsub-* ALL except procsub-real-001
**Status**: DEFER entire corpus
**Reason**: Only 1 real example in 68 commands (1.5% usage)
**Implementation Priority**: LOW (after all core features)

**File Impact**: Moderate when present, but extremely rare

**Recommendation**:
- Keep only `procsub-real-001` (the one real example)
- Defer remaining 16 pedagogical tests until core features complete
- Process substitution can be treated as: parse inner command recursively

---

## Summary Statistics

**Total deprecated/deferred tests**: 24 tests across 4 corpus files
- Heredoc edge cases: 4 tests → **REMOVE**
- Command-sub edge cases: 3 tests → **REMOVE**
- Conditional operator variations: 6 tests → **SIMPLIFY** to 2
- Process substitution: 16 tests → **DEFER**
- Arithmetic/null operators: 2 tests → **REMOVE**

**Net result**: Reduce corpus from 166 to ~142 focused tests

**Priority focus** (remaining 142 tests):
1. Combined patterns: 44 tests (now includes xargs, dynamic redirects)
2. For-loops: 26 tests (8 real examples)
3. Command substitution: 27 tests (remove 3 edge cases)
4. Heredoc: 21 tests (remove 4 edge cases)
5. Conditional: 18 tests (simplify 6 tests to 2)
6. Process substitution: 1 test (defer 16)

---

## Migration Plan

### Phase 1: Immediate (High File Impact)
- [x] Add xargs patterns (5 tests) - DONE
- [x] Add command-sub in redirects (4 tests) - DONE
- [x] Improve find -exec expectations (5 tests) - DONE
- [ ] Remove obvious edge cases (heredoc/cmdsub empty/escaped)

### Phase 2: Simplification (Reduce Test Bloat)
- [ ] Collapse conditional test operators (6 → 2 tests)
- [ ] Remove arithmetic/null tests (2 tests)
- [ ] Remove heredoc delimiter edge cases (2 tests)

### Phase 3: Documentation (Defer for Later)
- [ ] Mark process-substitution as deferred (16 tests)
- [ ] Document backtick syntax as legacy (1 test)
- [ ] Update corpus-index.el with new counts

### Phase 4: Validation
- [ ] Verify all remaining tests have clear file impact rationale
- [ ] Cross-reference with test-corpus-file-operations.el
- [ ] Ensure parse expectations support file extraction
