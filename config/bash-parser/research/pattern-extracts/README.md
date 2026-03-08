# Pattern-Specific Examples

This directory contains pattern-specific extracts from the coverage analysis for targeted investigation of semantic gaps in the bash parser.

## Files

### Individual Pattern Types

- **heredoc-examples.tsv** (9 commands)
  - Commands containing heredoc patterns
  - Repetitive git commit patterns filtered out
  - Coverage: 80.0% - 100.0% (avg: 97.8%)

- **for-loop-examples.tsv** (8 commands)
  - Commands with for-loop constructs
  - Coverage: 36.8% - 90.9% (avg: 55.6%)

- **conditional-examples.tsv** (4 commands)
  - Commands with if/test conditionals
  - Coverage: 42.9% - 87.5% (avg: 65.1%)

- **process-substitution-examples.tsv** (1 command)
  - Commands using process substitution `<()`
  - Coverage: 80.0% - 80.0% (avg: 80.0%)

- **command-substitution-examples.tsv** (132 commands)
  - Commands using command substitution `$()` or backticks
  - Coverage: 0.0% - 100.0% (avg: 30.9%)
  - Largest category - significant parsing challenges

### Combined Patterns

- **combined-patterns.tsv** (68 commands)
  - Commands with 2+ semantic gap types
  - Complex patterns requiring multiple parser enhancements
  - Coverage: 0.0% - 100.0% (avg: 56.2%)
  - Average gap count: 2.0

- **critical-gaps.tsv** (71 commands)
  - Commands with critical semantic gaps (critical_gaps > 0)
  - High-impact failures requiring attention
  - Coverage: 0.0% - 100.0% (avg: 57.8%)

## Usage

All files are sorted by `coverage_pct` (ascending) to prioritize interesting gaps. Lower coverage percentages indicate more tokens that the parser failed to extract.

### File Format

Each TSV file contains:
1. Header comments with pattern stats (lines starting with `#`)
2. Column headers (from coverage-analysis.tsv)
3. Data rows matching the pattern criteria

### Example Analysis Workflow

```bash
# View lowest coverage heredoc examples
head -20 pattern-extracts/heredoc-examples.tsv

# Find all for-loops with coverage < 50%
awk -F'\t' '$3 < 50 && NR > 9' pattern-extracts/for-loop-examples.tsv

# Count critical gaps by gap type
grep -v '^#' pattern-extracts/critical-gaps.tsv | cut -f11 | sort | uniq -c

# Extract specific gap type from combined patterns
awk -F'\t' '/for-loop.*conditional/ {print $1}' pattern-extracts/combined-patterns.tsv
```

## Regeneration

To regenerate these extracts after updating coverage-analysis.tsv:

```bash
cd /Users/jefffarr/emacs/config/experiments/bash-parser/research
./extract-pattern-examples.sh
```

## Notes

- **Heredoc filtering**: Git commit heredocs are excluded as they're repetitive (same pattern repeated across many commits). The header shows both total count and filtered count.

- **Command substitution dominance**: 132 commands use command substitution, making it the most common semantic gap pattern. This suggests command substitution should be a priority for parser improvements.

- **Coverage interpretation**:
  - 0% coverage = parser extracted nothing (complete failure)
  - 50% coverage = half the tokens extracted (partial parsing)
  - 100% coverage = all tokens extracted (pattern not causing issues)

## Column Reference

From coverage-analysis.tsv:

1. **command** - The bash command string
2. **parse_status** - SUCCESS/ERROR
3. **coverage_pct** - Percentage of tokens extracted (0-100)
4. **missing_count** - Number of tokens not extracted
5. **file_ops_count** - Number of file operations detected
6. **command_type** - :simple/:pipeline/:chain
7. **dangerous** - Flag if command is dangerous
8. **missing_tokens** - Comma-separated list of missing tokens
9. **semantic_gap_count** - Number of semantic gap types present
10. **critical_gaps** - Count of critical gaps
11. **gap_types** - Comma-separated list of gap types
