# Semantic Gap Analysis - Results

**Date:** 2026-03-04
**Corpus:** 1,552 commands from bash session history
**Analysis:** Two-layer coverage (text + semantic)

## Executive Summary

**Key Finding:** 8.1% of commands (125) have **critical security gaps** that are invisible to text-based coverage analysis.

- 28 commands (1.8%) have **perfect text coverage (100%)** but critical semantic gaps
- These are the most dangerous - they appear fully parsed but miss security-critical nested commands

## Full Results

### Text Coverage (Layer 1)

| Coverage Range | Count | Percentage |
|----------------|-------|------------|
| **Perfect (100%)** | 1,418 | 91.4% ✅ |
| **High (90-99%)** | 4 | 0.3% |
| **Medium (70-89%)** | 24 | 1.5% |
| **Low (<70%)** | 106 | 6.8% ⚠️ |

**Statistics:**
- Mean coverage: 93.5%
- Median coverage: 100.0%

**Assessment:** Parser captures text very well overall.

### Semantic Gap Analysis (Layer 2)

| Metric | Count | Percentage |
|--------|-------|------------|
| **Commands with semantic gaps** | 146 | 9.4% |
| **Commands with CRITICAL gaps** | 125 | 8.1% ⚠️ |
| **Total semantic gaps detected** | 267 | - |
| **Total CRITICAL gaps detected** | 125 | - |

**Gap Type Distribution:**

| Gap Type | Count | Severity | Notes |
|----------|-------|----------|-------|
| **Command substitution** `$(...)` | 64 | Critical | Nested commands invisible to security validation |
| **Heredoc** `<<EOF...EOF` | 63 | Low* | Content not extracted, but not executed (*except in critical combo) |
| **For loops** | 14 | Medium | Loop iteration values invisible |
| **Conditionals** `if/then/else` | 7 | Medium | Branch content partially lost |
| **Process substitution** `<(...)` | 1 | Critical | Process substitution command invisible |

**Total gap instances:** 149 (some commands have multiple gaps)

### The "Invisible Gap" Problem

**28 commands (1.8%) have PERFECT text coverage but critical semantic gaps:**

These are the most dangerous because:
1. Text coverage shows 100% - appears fully parsed ✅
2. But contains command substitution that's treated as literal string ❌
3. Security validation sees outer command only, misses nested operations ❌

**Common pattern:** Git commits with heredocs and command substitution

```bash
git commit -m "$(cat <<'EOF'
Multi-line commit message
with embedded content
EOF
)" && git status
```

**What parser sees:**
- Outer: `git commit -m "..."` ✅
- Text coverage: 100% (all tokens captured) ✅
- **MISSES:** Nested `cat` command ❌
- **MISSES:** Heredoc content ❌

**Security implication:** If the heredoc or command substitution contained dangerous operations, they'd be invisible.

### Breakdown by Coverage + Semantic Gaps

| Text Coverage | Semantic Gap Count | Command Count | Assessment |
|---------------|-------------------|---------------|------------|
| **100%** | 0 gaps | 1,390 | ✅ Perfect (89.5%) |
| **100%** | 1+ gaps | 28 | ⚠️ Invisible gaps (1.8%) |
| **<100%** | 0 gaps | 16 | ⚠️ Text parsing issues only |
| **<100%** | 1+ gaps | 118 | 🔴 Both text AND semantic gaps (7.6%) |

**Key insight:** Only 89.5% of commands are truly fully parsed (both text and semantics).

## Examples by Gap Type

### 1. Command Substitution (Critical, 64 instances)

**Simple:**
```bash
ls -la $(dirname $(which openspec))
```
- Text coverage: 66.7%
- Critical gap: `$(dirname $(which openspec))` not extracted
- Security risk: Nested commands invisible

**In git commits (28 cases):**
```bash
git commit -m "$(cat <<'EOF'
Commit message
EOF
)"
```
- Text coverage: 100% ✅
- Critical gap: `cat` command not extracted ❌
- Heredoc gap: EOF content not extracted ❌

### 2. For Loop Variables (Medium, 14 instances)

```bash
for bead in emacs-ags emacs-1n7 emacs-blx; do
  echo "=== $bead ==="
  bd show "$bead"
done
```
- Text coverage: 36.8%
- Gap: Loop iteration list `emacs-ags emacs-1n7 emacs-blx` not captured
- Risk: Can't detect file operations on loop values

### 3. Heredoc Content (Low severity, 63 instances)

```bash
bd create --description "Long description...
spanning many lines..."
```
- Text coverage: varies (1-40%)
- Gap: Heredoc body content not extracted
- Risk: Low (heredoc is literal text, not executed)
- **Exception:** When combined with command substitution in git commits

### 4. Process Substitution (Critical, 1 instance)

```bash
diff <(ls dir1) <(ls dir2)
```
- Gap: Process substitution commands not extracted
- Risk: Nested `ls` commands invisible

## Validation of Previous Analysis

### Confirms COVERAGE-VS-SUBSTITUTION-ANALYSIS.md Findings

**Original hypothesis (2026-03-03):**
> "Coverage analysis reveals parser is capturing text but missing semantic understanding for command substitution patterns."

**Semantic gap detection CONFIRMS:**
- 64 command substitution gaps detected programmatically ✅
- Many have high text coverage but semantic gaps ✅
- 28 have perfect (100%) text coverage ✅

**Original priority assignment:**
| Priority | Pattern | Previous Estimate | Actual Count |
|----------|---------|------------------|--------------|
| **P1** | Command substitution | ~194 | 64 critical + 63 heredoc |
| **P2** | For loop variables | ~15 | 14 ✅ |
| **P3** | Conditional blocks | ~10 | 7 ✅ |
| **P4** | Heredoc content | ~20 | 63 |

**Note:** Original analysis overcounted command substitution (194 vs 64 actual) because it detected the pattern in heredoc content where it's not evaluated. Semantic gap detection correctly distinguishes evaluated vs literal command substitution.

## Priority Recommendations

### P1: Command Substitution (CRITICAL)

**Impact:** 64 commands, security-critical
**Why critical:**
- Nested commands can execute dangerous operations
- 28 cases have 100% text coverage - invisible to current analysis
- Clear security gap identified in COVERAGE-VS-SUBSTITUTION-ANALYSIS.md

**Implementation approach:**
- 30-test suite already exists (COMMAND-SUBSTITUTION-TEST-STATUS.md)
- Spec documented in bash-parser/spec.md
- Use TDD approach with existing tests

### P2: For Loop Variables (MEDIUM)

**Impact:** 14 commands, data completeness issue
**Why medium:**
- Loop values often represent files to process
- Missing these means missing potential file operations
- Not security-critical, but affects parser completeness

### P3: Heredocs (LOW)

**Impact:** 63 commands, low security risk
**Why low:**
- Heredoc content is literal text, not executed
- Affects coverage metric more than security
- Only becomes critical when combined with command substitution (already tracked in P1)

### P4: Conditionals and Process Substitution (LOW FREQUENCY)

**Impact:** 7 conditionals, 1 process substitution
**Why deprioritized:**
- Low frequency in corpus
- Most conditional content already captured
- Single process substitution case

## Query Examples

Useful queries for the `coverage-analysis.tsv` file:

### Find invisible gaps (100% coverage + critical)
```bash
awk -F'\t' 'NR>1 && $3 == 100.0 && $10 == 1' coverage-analysis.tsv
```

### Find all critical security gaps
```bash
awk -F'\t' 'NR>1 && $10 == 1' coverage-analysis.tsv | wc -l
```

### Show gap type distribution
```bash
cat coverage-analysis.tsv | awk -F'\t' 'NR>1 && length($11) > 0 {print $11}' | \
  sed 's/, /\n/g' | sort | uniq -c | sort -rn
```

### Find commands with both text AND semantic gaps
```bash
awk -F'\t' 'NR>1 && $3 < 100.0 && $10 == 1' coverage-analysis.tsv | wc -l
```

## Conclusions

1. **Two-layer analysis is essential**
   - Text coverage alone is misleading (91.4% appear perfect)
   - Semantic gap detection reveals 8.1% have critical security gaps
   - 28 commands (1.8%) are "false positives" - appear perfect but aren't

2. **Command substitution is the primary gap**
   - 64 instances, all critical severity
   - 28 have perfect text coverage (invisible to previous analysis)
   - Confirmed priority from previous manual analysis

3. **Data-driven prioritization works**
   - Can now programmatically identify security gaps
   - Validates manual analysis findings
   - Enables tracking progress as parser improves

4. **Next step: Implement P1**
   - Use existing 30-test suite as TDD driver
   - Extract command substitution patterns recursively
   - Re-run analysis to verify semantic gaps decrease

## Files

**Analysis outputs:**
- `coverage-analysis.tsv` - Full corpus analysis (1,552 commands × 11 columns)
- `SEMANTIC-GAP-ANALYSIS-RESULTS.md` - This document

**Implementation:**
- `semantic-gap-detection.el` - Gap detection system
- `analyze-with-coverage.el` - Integrated corpus analyzer
- `run-coverage-analysis.sh` - Runner script

**Previous analyses:**
- `COVERAGE-VS-SUBSTITUTION-ANALYSIS.md` - Manual gap discovery (validated ✅)
- `PHASE-2-COVERAGE-SUMMARY.md` - Text coverage findings
- `COMMAND-SUBSTITUTION-TEST-STATUS.md` - P1 test suite (30 tests ready)

**Test suite:**
- `config/experiments/bash-parser/test/` - ERT tests including command substitution suite
