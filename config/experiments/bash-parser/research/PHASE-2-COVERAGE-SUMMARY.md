# Phase 2: Parse Coverage Analysis - Summary

**Date:** 2026-03-04
**Commands Analyzed:** 1,552
**Metric:** Word-level coverage (percentage of input tokens captured in parser output)

## Overview

Parse coverage analysis measures how much of the input bash command is represented in the parser's structured output. This identifies commands where the parser may be missing significant portions of input, indicating potential parsing gaps.

## Key Findings

### Coverage Distribution

| Coverage Range | Count | Percentage |
|----------------|-------|------------|
| **Perfect (100%)** | 1,418 | 91.4% ✅ |
| **High (90-99%)** | 4 | 0.3% |
| **Medium (70-89%)** | 24 | 1.5% |
| **Low (<70%)** | 106 | 6.8% ⚠️ |

- **Mean coverage:** 93.5%
- **Median coverage:** 100.0%

### Overall Assessment

✅ **Parser handles vast majority of commands well** - 91.4% perfect coverage
⚠️ **134 commands (8.6%) have gaps** - need investigation
🎯 **Focus area:** 106 commands with <70% coverage show significant parsing gaps

## Identified Gap Patterns

### 1. For Loop Variables (36-59% coverage)

**Pattern:** Variables in `for ... in ...` lists not captured

**Example:**
```bash
for bead in emacs-ags emacs-1n7 emacs-blx; do
  echo "=== $bead ==="
  bd show "$bead"
done
```

**Coverage:** 36.8% (12 tokens missing)
**Missing:** `emacs-ags`, `emacs-1n7`, `emacs-blx`, etc.

**Impact:** Loop iteration lists invisible to parser, can't detect file operations on those values

**Frequency:** ~15 commands in corpus

---

### 2. Heredoc Content (1-40% coverage)

**Pattern:** Large heredoc/multiline string content not extracted

**Example:**
```bash
bd create --description "Long description...
with YAML content...
spanning many lines..."
```

**Coverage:** 1.6-40.9% (dozens to hundreds of tokens missing)
**Missing:** Entire heredoc body content

**Impact:**
- Can't detect file paths/commands mentioned in descriptions
- Heredoc content completely invisible to file operations extraction

**Frequency:** ~20 commands (mostly `bd create` with long descriptions)

---

### 3. Command Substitution (66-80% coverage)

**Pattern:** Embedded `$(...)` command content partially captured

**Example:**
```bash
ls -la $(dirname $(which openspec))
```

**Coverage:** 66.7%
**Missing:** `$(dirname $(which openspec))`

**Example:**
```bash
--eval '(setq user-emacs-directory "'$(pwd)'/runtime/")'
```

**Coverage:** 70-80%
**Missing:** Parts of embedded `$(pwd)` substitution

**Impact:** Nested commands not fully parsed, indirect file operations missed

**Frequency:** ~25 commands

---

### 4. Conditional Blocks (42-60% coverage)

**Pattern:** `if`/`else`/`fi` block content partially lost

**Example:**
```bash
if [ -d "path" ]; then
  echo "EXISTS"
else
  echo "AVAILABLE"
fi
```

**Coverage:** 42.9-60%
**Missing:** Some literal strings in branches, exit commands

**Impact:** Conditional file operations may be invisible

**Frequency:** ~10 commands

---

### 5. Complex Quoting (70-80% coverage)

**Pattern:** Nested quotes with command substitution

**Example:**
```bash
--eval '(add-to-list '"'"'treesit-extra-load-path "'$(pwd)'")'
```

**Coverage:** 70-80%
**Missing:** Parts of complex quoting structures

**Impact:** May miss file paths in quoted elisp expressions

**Frequency:** ~15 commands

---

## Parser Strengths (100% Coverage Cases)

✅ **Simple commands:** `ls -la /tmp`
✅ **Pipelines:** `cat file.txt | grep pattern | sort`
✅ **Chains:** `mkdir dir && cd dir && ls`
✅ **Redirections:** `cat file.txt > output.txt`
✅ **Basic find -exec:** `find . -name '*.el' -exec grep 'pattern' {} \;`
✅ **Variable assignments:** `FILE=test.txt`
✅ **Standard flags and args:** All common patterns captured perfectly

## Recommendations

### Priority 1: For Loop Variables (High Impact)

**Action:** Enhance parser to extract loop variable lists from `for ... in ...` constructs

**Rationale:**
- Clear pattern: 15+ commands affected
- Moderate frequency in real usage
- Loop values often represent files to process
- Missing these means missing potential file operations

**Implementation:**
- Add tree-sitter node handling for `for_statement`
- Extract words from `in` clause
- Add to positional-args or new `:loop-values` field

### Priority 2: Command Substitution (Medium Impact)

**Action:** Recursively parse `$(...)` and backtick command substitutions

**Rationale:**
- 25+ commands affected
- Nested commands can perform file operations
- Already have recursion pattern from bash -c handling

**Implementation:**
- Detect `command_substitution` nodes in tree-sitter AST
- Recursively call parser on substitution content
- Mark operations as `:indirect t` (like bash -c)

### Priority 3: Heredoc Content (Low Priority for Security)

**Action:** Extract heredoc body text for completeness

**Rationale:**
- 20 commands affected
- Mostly `bd create` descriptions (not executed)
- Low security risk (description text doesn't run)
- Affects coverage metric more than security

**Implementation:**
- Extract `heredoc_body` node content
- Add to `:heredoc-content` field
- Consider truncation for very large heredocs

### Priority 4: Conditional Blocks (Low Priority)

**Action:** Improve `if`/`then`/`else`/`fi` content extraction

**Rationale:**
- 10 commands affected
- Low frequency
- Most conditional content is captured (just missing some literals)

**Implementation:**
- Review `if_statement` node handling
- Ensure all branch bodies are recursively parsed

## Phase 2 Deliverables

✅ **Coverage computation module** - `compute-parse-coverage.el`
✅ **Corpus analysis script** - `analyze-with-coverage.el`
✅ **Coverage data** - `coverage-analysis.tsv` (1,552 commands)
✅ **Gap pattern analysis** - This document

## Next Steps

1. **Review gap patterns** with user - validate priorities
2. **Create test corpus** for each gap pattern
3. **Implement fixes** starting with Priority 1 (for loops)
4. **Re-run coverage analysis** to measure improvement
5. **Update PHASE-1-SUMMARY.md** with coverage insights

## Files Generated

- `config/experiments/bash-parser/research/compute-parse-coverage.el` - Coverage computation
- `config/experiments/bash-parser/research/analyze-with-coverage.el` - Corpus analyzer
- `config/experiments/bash-parser/research/run-coverage-analysis.sh` - Analysis runner
- `config/experiments/bash-parser/research/coverage-analysis.tsv` - Full results
- `config/experiments/bash-parser/research/PHASE-2-COVERAGE-SUMMARY.md` - This document
