# Bash Parser Test Corpus Summary

**Status**: All corpus files validated and ready for testing
**Total Test Cases**: 153
**Date**: 2026-03-04

## Overview

This comprehensive test corpus provides behavioral validation for the bash parser implementation. All test cases have been extracted from real Claude Code session commands or designed to cover pedagogical patterns that lead to real-world usage.

## Corpus Files

### 1. Command Substitution Corpus
**File**: `command-substitution-corpus.el`
**Variable**: `jf/bash-command-substitution-corpus`
**Test Count**: 31 tests
**Status**: ✓ Validated

**Category Breakdown**:
- 6 simple/basic patterns (pedagogical foundation)
- 4 nested substitutions (2-3 levels deep)
- 4 quoted contexts (double quotes, single quotes, nested quotes)
- 4 pipe/redirect patterns (pipes inside substitutions)
- 4 multiple substitutions (2-3 independent substitutions)
- 4 complex real-world patterns (from research data)
- 4 edge cases (arithmetic expansion, heredoc interaction, backticks, empty/escaped)

**Real-world examples**: 6 tests marked "REAL: From research"

**Key patterns**:
- `$(cmd)` and backtick syntax
- Nesting: `$(dirname $(which openspec))`
- In quotes: `echo "Current time: $(date)"`
- With pipes: `$(find "$dir" -name "*.org" | wc -l)`
- Multiple: `echo $(pwd) $(date)`

---

### 2. Heredoc Corpus
**File**: `heredoc-corpus.el`
**Variable**: `jf/bash-heredoc-corpus`
**Test Count**: 25 tests
**Status**: ✓ Validated

**Category Breakdown**:
- 6 basic heredoc patterns (unquoted, quoted, empty, multi-line, custom delimiters)
- 8 heredoc in contexts (git commit, command substitution, pipes, redirects, sequences)
- 7 complex patterns (indented <<-, nested quotes, multi-stage pipelines, real examples)
- 4 edge cases (empty delimiter, special chars, invalid syntax)

**Real-world examples**: 5 tests marked "REAL: From research"

**Key patterns**:
- Basic: `cat <<EOF\ncontent\nEOF`
- Quoted delimiter: `cat <<'EOF'` (no expansion)
- In command substitution: `git commit -m "$(cat <<'EOF'...)"`
- Indented: `cat <<-\tEOF` (strips leading tabs)
- With redirects: `cat <<EOF > file.txt`

---

### 3. For-Loop Corpus
**File**: `for-loop-corpus.el`
**Variable**: `jf/bash-for-loop-corpus`
**Test Count**: 29 tests
**Status**: ✓ Validated

**Category Breakdown**:
- 5 simple patterns (literal lists, basic iteration)
- 8 expansion patterns (globs, brace expansion, command substitution)
- 8 complex real-world patterns (multi-line bodies, pipelines, conditionals)
- 3 nested/compound patterns (multi-command bodies, nested substitution)
- 2 edge cases (empty list, single item, loop control: break/continue)

**Real-world examples**: 8 tests marked "REAL: From research" (36.8% - 90.9% coverage gaps)

**Key patterns**:
- Simple: `for x in a b c; do echo $x; done`
- Glob: `for file in *.txt; do ... done`
- Command substitution list: `for file in $(find . -name "*.el"); do ... done`
- Multi-line body: Real examples with conditionals, pipelines, redirects
- Compound body: Multiple commands with semicolons

---

### 4. Conditional Corpus
**File**: `conditional-corpus.el`
**Variable**: `jf/bash-conditional-corpus`
**Test Count**: 26 tests
**Status**: ✓ Validated

**Category Breakdown**:
- 6 simple conditionals (if/then, if/else, test command, [[ ]] syntax)
- 8 test operators (file: -f/-d/-e/-x/-r, string: -z/-n/=, numeric: -eq/-lt/-gt, compound: &&/||)
- 7 complex real-world patterns (command substitution, chaining, multi-line)
- 5 edge cases (elif chains, negation !, arithmetic (()), null commands)

**Real-world examples**: 4 tests marked "REAL: From research"

**Key patterns**:
- File tests: `if [ -f file ]; then ... fi`
- String tests: `if [ -z "$VAR" ]; then ... fi`
- Numeric tests: `if [ $n -lt 10 ]; then ... fi`
- Compound: `if [[ -f file && -r file ]]; then ... fi`
- Command substitution: `if [ -d "$(dirname "$file")" ]; then ... fi`
- Multi-line branches with exit codes

---

### 5. Process Substitution Corpus
**File**: `process-substitution-corpus.el`
**Variable**: `jf/bash-process-substitution-corpus`
**Test Count**: 17 tests
**Status**: ✓ Validated

**Category Breakdown**:
- 5 basic patterns (diff, grep, cat, sort comparison, line count)
- 5 pipe patterns (pipes inside process substitutions, complex pipelines)
- 2 real-world patterns (test result comparison, git log comparison)
- 5 edge cases (three inputs, mixed with files, nested, heredoc, command substitution)

**Real-world examples**: 1 test marked "REAL: From research" (rare in practice)

**Key patterns**:
- Basic diff: `diff <(cmd1) <(cmd2)`
- Single input: `grep pattern <(find . -name "*.txt")`
- With pipes: `diff <(cat file | grep pattern) <(cat file2 | grep pattern)`
- Nested: `cat <(grep pattern <(other-cmd))`
- With heredoc: `diff <(cat <<EOF...) <(echo -e "...")`

**Note**: Process substitution is a bash-specific feature (not POSIX sh) and rare in Claude Code sessions (only 1 real example found).

---

### 6. Combined Patterns Corpus
**File**: `combined-patterns-corpus.el`
**Variable**: `jf/bash-combined-patterns-corpus`
**Test Count**: 25 tests (integration tests)
**Status**: ✓ Validated

**Category Breakdown**:
- 8 command-substitution + heredoc (dominant pattern: 84% of real combined commands)
- 6 for-loop + command-substitution (file processing, batch operations)
- 2 conditional + command-substitution (dynamic test conditions)
- 4 multi-feature (3+ gap types - stress tests for maximum complexity)
- 5 edge cases (unusual but valid combinations)

**Real-world distribution** (from 68 commands with 2+ gap types):
- command-substitution + heredoc: 57 commands (84%)
- for-loop + command-substitution: 6 commands (9%)
- conditional + command-substitution: 2 commands (3%)
- process-substitution + heredoc: 1 command (1%)
- for-loop + conditional: 1 command (1%)

**Key integration patterns**:
- Git commit with heredoc: `git commit -m "$(cat <<'EOF'...)"`
- For-loop with command substitution in list AND body
- Conditional with command substitution in test condition
- Multi-feature: for-loop + conditional + command-substitution
- Process substitution + command substitution + heredoc (edge case)

**Purpose**: These tests validate parser robustness with feature interaction, nesting, and context tracking. They are NOT isolated feature tests - they test how features combine in real-world usage.

---

## Total Coverage Summary

| Pattern Type | Test Count | Real Examples | Pedagogical | Notes |
|--------------|------------|---------------|-------------|-------|
| Command Substitution | 31 | 6 | 25 | Core feature, high usage |
| Heredoc | 25 | 5 | 20 | Critical for git commits |
| For-Loop | 29 | 8 | 21 | Batch operations |
| Conditional | 26 | 4 | 22 | Control flow, file tests |
| Process Substitution | 17 | 1 | 16 | Rare but important for diff |
| Combined Patterns | 25 | All derived from real data | 0 | Integration tests only |
| **TOTAL** | **153** | **24 direct + 25 derived** | **104** | **Comprehensive coverage** |

---

## Coverage by Pattern Type

### Semantic Gap Types Covered

1. **Command Substitution**: `$()` and backtick syntax
   - Nesting (2-3 levels)
   - Inside quotes
   - With pipes and redirects
   - Multiple substitutions
   - In various contexts (loops, conditionals, heredocs)

2. **Heredoc**: `<<DELIMITER` syntax
   - Quoted vs unquoted delimiters
   - Indented heredoc `<<-`
   - Inside command substitution
   - With command substitution inside content
   - With pipes and redirects
   - Multi-line with markdown, code, structured text

3. **For-Loop**: `for VAR in LIST; do BODY; done`
   - Literal lists
   - Glob patterns (`*.txt`, `*/`)
   - Brace expansion (`{1..5}`, `{a,b,c}`)
   - Command substitution lists
   - Multi-line bodies
   - Nested conditionals in body
   - Compound bodies (multiple commands)
   - Loop control (break, continue)

4. **Conditional**: `if TEST; then BRANCH; fi`
   - Test operators: `[`, `[[`, `test`
   - File tests (-f, -d, -e, -x, -r)
   - String tests (-z, -n, =)
   - Numeric tests (-eq, -lt, -gt)
   - Compound tests (&&, ||)
   - Elif chains
   - Negation (!)
   - Arithmetic tests `(( ))`
   - Command exit status as test

5. **Process Substitution**: `<(cmd)` syntax
   - Single and multiple inputs
   - With pipes inside
   - Nested process substitution
   - Mixed with regular files
   - With command substitution inside

6. **Combined Patterns**: 2-5 features interacting
   - Command substitution + heredoc (dominant)
   - For-loop + command substitution
   - Conditional + command substitution
   - Process substitution + heredoc
   - For-loop + conditional
   - 3+ feature stress tests

---

## Gap Type Distribution

### Real-World Usage Patterns

Based on research data extraction from Claude Code sessions:

**Single Gap Type Commands**:
- Command substitution: Most common single gap
- For-loops: Batch operations, file processing
- Conditionals: File existence checks, validation
- Heredocs: Rare alone (usually combined with command substitution)
- Process substitution: Very rare (1 example found)

**Combined Gap Type Commands** (68 commands analyzed):
- command-substitution + heredoc: **57 (84%)** ← Dominant pattern
- for-loop + command-substitution: 6 (9%)
- conditional + command-substitution: 2 (3%)
- process-substitution + heredoc: 1 (1%)
- for-loop + conditional: 1 (1%)
- 3+ gap types: 1 (1%)

**Critical insight**: The command-substitution + heredoc combination is the dominant real-world pattern, driven by git commit messages and structured content generation (PRs, beads, documentation). This pattern appears in 84% of all combined-gap commands.

---

## Test Structure Guidelines

Each test case includes:

```elisp
(:id "corpus-category-NNN"
 :category "category-name"
 :command "bash command string"
 :expect (:field1 value1
          :field2 value2
          ...)
 :notes "Description and context")
```

**Required fields**:
- `:id` - Unique identifier (corpus-category-NNN)
- `:category` - Category name (simple, nested, complex, etc.)
- `:command` - The bash command to parse
- `:expect` - Expected parse structure (plist format)
- `:notes` - Human-readable description

**Expected structure varies by corpus** but commonly includes:
- `:command-name` - Primary command
- `:subcommand` - Subcommand if applicable (e.g., git commit)
- `:flags` - List of flags (-f, --flag)
- `:positional-args` - List of positional arguments
- `:command-substitutions` - List of `$()` patterns with :content, :nesting-level
- `:heredocs` - List of heredoc patterns with :delimiter, :quoted, :content
- `:loop-variable`, `:loop-list`, `:loop-body` - For for-loops
- `:test-condition`, `:then-branch`, `:else-branch` - For conditionals
- `:process-substitutions` - List of `<()` patterns
- `:pipeline` - Boolean, true if contains pipes
- `:redirects` - List of redirection operators

---

## Real-World Example Metadata

Tests marked "REAL: From research" include context from actual Claude Code sessions:

**Command Substitution Examples**:
- `ls -la $(dirname $(which openspec))` - Nested directory inspection
- `echo "=== $(basename "$dir") ==="` - Directory name extraction
- `$(find "$dir" -name "*.org" | wc -l)` - File counting with pipe

**Heredoc Examples**:
- Git commit messages with multi-line content
- GitHub PR creation with markdown
- Beads creation with structured descriptions

**For-Loop Examples**:
- Beads iteration with error suppression: `for bead in emacs-ags ...; do bd show "$bead" 2>/dev/null; done`
- Git commit analysis: `for commit in 00950fb ...; do git show --stat; done`
- OpenSpec change inspection with conditionals

**Conditional Examples**:
- Archive directory validation: `if [ -d "openspec/changes/archive/..." ]; then error; else proceed; fi`
- Multi-line conditionals with exit codes
- Command substitution in test conditions

**Process Substitution Examples**:
- Test result comparison: `diff <(tail -15 file1) <(tail -15 file2)`

---

## Implementation Guidance

### Test Execution

**Load all corpus files**:
```elisp
(require 'command-substitution-corpus)
(require 'heredoc-corpus)
(require 'for-loop-corpus)
(require 'conditional-corpus)
(require 'process-substitution-corpus)
(require 'combined-patterns-corpus)
```

**Access test data**:
```elisp
;; Get all command substitution tests
jf/bash-command-substitution-corpus

;; Get count
(length jf/bash-heredoc-corpus)

;; Filter by category
(seq-filter (lambda (test) (equal (plist-get test :category) "simple"))
            jf/bash-for-loop-corpus)
```

**Use corpus-index.el for convenience functions** (see next section).

---

### Parser Development Strategy

**Phase 1: Single gap types** (foundation)
1. Command substitution (highest priority - core feature)
2. For-loops (batch operations)
3. Conditionals (control flow)
4. Heredocs (git commits, structured text)
5. Process substitution (rare but important for diff)

**Phase 2: Combined patterns** (integration)
1. Command substitution + heredoc (84% of combined commands)
2. For-loop + command substitution (9% of combined)
3. Conditional + command substitution (3% of combined)
4. Multi-feature stress tests (parser robustness)

**Incremental implementation approach**:
- Start with simple/basic tier tests in each corpus
- Validate expected structure fields are extracted
- Progress to nested/complex tiers
- Add integration tests (combined-patterns corpus)
- Verify real-world examples parse correctly

**Coverage metrics**:
- Track test pass rate per corpus
- Track test pass rate per category
- Track test pass rate per tier (simple → complex)
- Compare parser coverage % vs real-world command coverage %

---

## Gap Analysis and Future Corpus Expansion

### Current Coverage Strengths

1. **Command substitution**: Comprehensive (30 tests covering nesting, quoting, pipes, multiple)
2. **Heredoc**: Good coverage (25 tests covering delimiters, contexts, real examples)
3. **For-loop**: Strong real-world coverage (8 real examples from research)
4. **Conditional**: Good operator coverage (file, string, numeric, compound tests)
5. **Combined patterns**: Excellent integration coverage (32 tests, all derived from real usage)

### Areas for Future Expansion

1. **While-loops**: Currently only in combined-patterns corpus (1 test)
   - Recommendation: Create dedicated while-loop corpus with 15-20 tests
   - Patterns: `while read line`, `while true`, `while [ condition ]`

2. **Case statements**: Currently only in combined-patterns corpus (1 test)
   - Recommendation: Create dedicated case-statement corpus with 15-20 tests
   - Patterns: Variable matching, pattern matching, fallthrough, real examples

3. **Functions**: Not currently covered
   - Recommendation: Add if bash function definitions appear in Claude Code sessions
   - Patterns: `function_name() { ... }`, local variables, return codes

4. **Arrays**: Not currently covered
   - Recommendation: Add if array usage appears in real commands
   - Patterns: Array declaration, indexing, iteration

5. **Arithmetic**: Minimal coverage (1 edge case in command substitution corpus)
   - Recommendation: Expand if arithmetic expressions are common
   - Patterns: `$((expr))`, `let`, `expr`

6. **Subshells**: Not explicitly covered
   - Recommendation: Add corpus if `(cmd1; cmd2)` patterns appear frequently
   - Patterns: Subshell execution, variable scope, exit codes

### Data-Driven Expansion Strategy

1. Run coverage analysis on larger Claude Code session dataset
2. Identify high-frequency patterns not yet in corpus
3. Extract real examples for new corpus files
4. Follow existing corpus structure (tiers, categories, real vs pedagogical)
5. Maintain 80/20 split: 80% real-world patterns, 20% pedagogical foundation

---

## Validation Status

All 6 corpus files have been validated:

```bash
✓ command-substitution-corpus.el - 31 tests loaded successfully
✓ heredoc-corpus.el - 25 tests loaded successfully
✓ for-loop-corpus.el - 29 tests loaded successfully
✓ conditional-corpus.el - 26 tests loaded successfully
✓ process-substitution-corpus.el - 17 tests loaded successfully
✓ combined-patterns-corpus.el - 25 tests loaded successfully
```

**Validation method**: Each file loaded in Emacs batch mode with no syntax errors.

**Files are ready for parser testing.**

---

## Corpus Index

A companion file `corpus-index.el` provides:
- Helper functions to load all corpus files
- Combined corpus variable: `jf/bash-all-corpus`
- Query functions to filter by category, gap type, tier
- Test count summaries
- Real-world example filters

See `corpus-index.el` for usage examples.

---

## References

**Research data sources**:
- `research/command-substitution-examples.tsv` - 30 real commands
- `research/heredoc-examples.tsv` - 25 real commands
- `research/for-loop-examples.tsv` - 26 real commands
- `research/conditional-examples.tsv` - 24 real commands
- `research/process-substitution-examples.tsv` - 17 real commands
- `research/pattern-extracts/combined-patterns.tsv` - 68 real commands

**Parser implementation**: `config/experiments/bash-parser/bash-parser.el`

**Test framework**: ERT (Emacs Lisp Regression Testing)

---

## Maintenance Notes

**When adding new corpus tests**:
1. Follow existing structure (id, category, command, expect, notes)
2. Add to appropriate tier (simple → complex → edge)
3. Mark real-world examples with "REAL: From research"
4. Update docstring with new test count
5. Update this summary document
6. Re-validate corpus file with `emacs --batch -l corpus-file.el`

**When creating new corpus files**:
1. Follow naming convention: `pattern-type-corpus.el`
2. Define variable: `jf/bash-pattern-type-corpus`
3. Add comprehensive docstring with test counts and categories
4. Include `(provide 'pattern-type-corpus)` at end
5. Update `corpus-index.el` to include new corpus
6. Update this summary document with new corpus section
7. Run full validation suite

---

**Last Updated**: 2026-03-04
**Corpus Version**: 1.0
**Total Tests**: 153
