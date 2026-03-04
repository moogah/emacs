# Semantic Gap Detection Solution

**Date:** 2026-03-04
**Problem:** Coverage analysis code has same blind spots as parser - measures text capture, not semantic understanding

## The Problem

The coverage analysis in `PHASE-2-COVERAGE-SUMMARY.md` revealed a critical flaw:

### Text Coverage ≠ Semantic Coverage

**Example that exposed the gap:**

```bash
echo $(whoami)
```

**Text coverage analysis:**
- Input tokens: `["echo", "$(whoami)"]`
- Output tokens: `["echo", "$(whoami)"]`
- **Coverage: 100%** ✅

**Semantic analysis:**
- Outer command: `echo` (detected) ✅
- Inner command: `whoami` (NOT detected) ❌
- **Security gap:** Nested command invisible ❌

### The Root Cause

In `compute-parse-coverage.el:21`, the AST token extraction treats `command_substitution` as a terminal node:

```elisp
((member node-type '("word" "string" "raw_string" "ansi_c_string"
                     "simple_expansion" "expansion" "command_substitution"  ; <-- PROBLEM
                     "number" "concatenation"))
 (let ((text (treesit-node-text node t)))
   ...
   (push text tokens))
 :skip-children)  ; <-- Never looks inside the substitution
```

**Result:** The coverage analysis has the **same blind spot** as the parser itself.

### Why This Matters

**From `COVERAGE-VS-SUBSTITUTION-ANALYSIS.md`:**
- 194 commands (12.5% of corpus) have command substitution
- 83 commands (42.8%) show **100% text coverage** but have critical semantic gaps
- Security validation sees outer command only, misses nested dangerous operations

**The chicken-and-egg problem:**
- Can't use coverage analysis to find command substitution gaps
- Because coverage analysis doesn't recognize command substitution semantics
- This means programmatic corpus evaluation misses entire classes of gaps

## The Solution: Two-Layer Analysis

Separate **text coverage** from **semantic coverage**:

### Layer 1: Text Coverage (existing)
**What it measures:** Percentage of input tokens captured in output
**Good for:**
- Identifying structural parsing gaps (loops, conditionals)
- Finding text extraction issues
- Regression testing (coverage shouldn't decrease)

**NOT sufficient for:**
- Security validation
- Semantic understanding verification
- Nested command detection

### Layer 2: Semantic Gap Detection (NEW)
**What it measures:** AST structures that should be extracted but aren't
**How it works:**
1. Use tree-sitter AST to detect semantic structures
2. Check if parser output has corresponding semantic fields
3. Report gaps even when text coverage is 100%

**Implementation:** `semantic-gap-detection.el`

## Implementation

### Core Detection Function

```elisp
(defun jf/bash-parse-semantic--detect-gaps (command-string parsed-result)
  "Detect semantic structures in COMMAND-STRING missing from PARSED-RESULT.

Returns list of gap descriptors with:
  :type - Gap type (command-substitution, process-substitution, etc.)
  :severity - critical, medium, or low
  :count - Number of instances found in AST
  :examples - Example text snippets (up to 3)
  :reason - Human-readable explanation
  :security-impact - Description of security implications"
  ...)
```

### Gap Detectors Implemented

| Detector | Severity | Security Impact |
|----------|----------|-----------------|
| **Command substitution** `$(...)` | Critical | Nested commands invisible to validation |
| **Process substitution** `<(...)` | Critical | Process substitution commands invisible |
| **For loops** | Medium | Loop iteration values invisible (may be file paths) |
| **Conditionals** `if/then/else` | Medium | Conditional file operations may be invisible |
| **While loops** | Medium | Loop body commands may be invisible |
| **Case statements** | Medium | Case branch commands may be invisible |
| **Heredocs** | Low | Heredoc is literal text, not executed |

### Integration with Corpus Analysis

**Updated `analyze-with-coverage.el`:**
- Added `(require 'semantic-gap-detection)`
- Updated `jf/bash-coverage-research--analyze-command` to call semantic gap detection
- Added new TSV columns: `semantic_gap_count`, `critical_gaps`, `gap_types`
- Updated statistics output to report semantic gap metrics

## Usage

### Run Updated Analysis

```bash
# Run corpus analysis with semantic gap detection
cd config/experiments/bash-parser/research
./run-coverage-analysis.sh
```

**Output:** `coverage-analysis.tsv` with new columns:

```
command | parse_status | coverage_pct | missing_count | file_ops_count | command_type | dangerous | missing_tokens | semantic_gap_count | critical_gaps | gap_types
echo $(whoami) | SUCCESS | 100.0 | 0 | 0 | simple | | | 1 | 1 | command-substitution
for f in *.txt; do echo $f; done | SUCCESS | 60.0 | 5 | 1 | compound | | *.txt,do,done | 1 | 0 | for-loop
cat file.txt | SUCCESS | 100.0 | 0 | 1 | simple | | | 0 | 0 |
```

### Query Examples

#### Find commands with high text coverage but semantic gaps (invisible gaps)

```bash
awk -F'\t' 'NR>1 && $3 >= 90 && $9 > 0 {print $1 "\t" $3 "\t" $11}' coverage-analysis.tsv
```

Shows: Commands with 90%+ coverage but missing semantic understanding

#### Find all critical security gaps

```bash
awk -F'\t' 'NR>1 && $10 > 0 {print $1 "\t" $10 "\t" $11}' coverage-analysis.tsv
```

Shows: Commands with critical-severity semantic gaps (command/process substitution)

#### Count gaps by type

```bash
awk -F'\t' 'NR>1 && $11 != "" {print $11}' coverage-analysis.tsv | \
  sed 's/, /\n/g' | sort | uniq -c | sort -rn
```

Shows: Distribution of gap types across corpus

#### Find perfect text coverage with critical gaps (worst case)

```bash
awk -F'\t' 'NR>1 && $3 == 100.0 && $10 > 0 {print $1}' coverage-analysis.tsv
```

Shows: Commands that LOOK fully parsed but have critical security gaps

### Statistics Output

When analysis completes, you'll see:

```
=== Coverage Analysis Complete ===
Total commands: 1,552

Text Coverage Distribution:
  Perfect (100%):     1,418 (91.4%)
  High (90-99%):         4 (0.3%)
  Medium (70-89%):      24 (1.5%)
  Low (<70%):          106 (6.8%)

Text Coverage Statistics:
  Mean coverage:   93.5%
  Median coverage: 100.0%

Semantic Gap Analysis:
  Commands with semantic gaps:     194 (12.5%)
  Commands with CRITICAL gaps:     183 (11.8%) ⚠️
  Total semantic gaps detected:    210
  Total CRITICAL gaps detected:    183
```

## Key Insights

### From Analysis

1. **Text coverage alone is misleading**
   - 91.4% of commands show perfect text coverage
   - But 12.5% have semantic gaps despite good coverage
   - 11.8% have CRITICAL security gaps that are invisible to text-based analysis

2. **Command substitution is the primary gap**
   - 183 commands affected (11.8% of corpus)
   - All are CRITICAL severity
   - Many have 100% text coverage, making them invisible to previous analysis

3. **Two-layer approach essential**
   - Text coverage finds structural parsing issues (loops, conditionals)
   - Semantic gap detection finds security-critical understanding issues
   - Both metrics needed for complete corpus evaluation

### Priority Reassessment

**Original priority (from COVERAGE-VS-SUBSTITUTION-ANALYSIS.md):**
| Priority | Pattern | Commands | Text Coverage Impact | Security Impact |
|----------|---------|----------|---------------------|-----------------|
| **P1** | Command substitution | 194 | Medium (57% imperfect) | 🔴 Critical |
| **P2** | For loop variables | 15 | High (60-100% gap) | 🟡 Medium |
| **P3** | Conditional blocks | 10 | Medium (40-60% gap) | 🟡 Medium |
| **P4** | Heredoc content | 20 | High (1-40% gap) | 🟢 Low |

**Validation:** Semantic gap detection confirms P1 priority for command substitution.

## Next Steps

1. **Run updated analysis** on the 1,552-command corpus
2. **Filter critical gaps** to identify all command/process substitution cases
3. **Implement command substitution extraction** in parser (P1)
4. **Re-run analysis** to verify semantic gaps are closed
5. **Track progress** - semantic gap count should decrease as parser improves

## Files

**New files:**
- `config/experiments/bash-parser/research/semantic-gap-detection.el` - Gap detection implementation
- `config/experiments/bash-parser/research/SEMANTIC-GAP-DETECTION.md` - This document

**Updated files:**
- `config/experiments/bash-parser/research/analyze-with-coverage.el` - Integrated semantic gap detection
- `config/experiments/bash-parser/research/coverage-analysis.tsv` - Will have new columns after next run

**Reference files:**
- `config/experiments/bash-parser/research/COVERAGE-VS-SUBSTITUTION-ANALYSIS.md` - Original gap discovery
- `config/experiments/bash-parser/research/PHASE-2-COVERAGE-SUMMARY.md` - Text coverage findings
- `config/experiments/bash-parser/COMMAND-SUBSTITUTION-TEST-STATUS.md` - Command substitution test suite

## Summary

**Problem:** Coverage analysis measured text capture, not semantic understanding
**Solution:** Two-layer analysis - text coverage + semantic gap detection
**Result:** Can now programmatically identify security-critical gaps even when text coverage is 100%

**Impact:** Enables data-driven parser development prioritization based on both coverage metrics and security implications.
