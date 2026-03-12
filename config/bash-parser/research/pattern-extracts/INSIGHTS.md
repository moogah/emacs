# Pattern Analysis Insights

Quick insights from the extracted pattern data.

## Pattern Distribution

| Pattern Type | Count | Avg Coverage | Notes |
|--------------|-------|--------------|-------|
| Command Substitution | 132 | 30.9% | **Largest category** - significant parsing gaps |
| Combined (2+ gaps) | 68 | 56.2% | Complex multi-gap patterns |
| Heredoc | 9 | 97.8% | Well-handled, high coverage |
| For-loop | 8 | 55.6% | Moderate coverage issues |
| Conditional | 4 | 65.1% | Relatively well-handled |
| Process Substitution | 1 | 80.0% | Rare pattern |
| **Critical Gaps** | **71** | **57.8%** | High-impact failures |

## Key Findings

### 1. Command Substitution is the Dominant Gap

With 132 commands (nearly 50% more than any other category) and only 30.9% average coverage, command substitution represents the largest parsing challenge.

**Examples of low coverage:**
- Commands with nested `$(cat <<'EOF' ...)` - 0% coverage
- Multiple layers of command substitution
- Command substitution combined with heredocs

**Recommendation:** Prioritize command substitution handling in parser improvements.

### 2. Heredocs are Well-Supported

Average coverage of 97.8% indicates heredocs are mostly working correctly. The few low-coverage examples (80%) typically involve heredocs combined with other patterns.

### 3. Critical Gaps Require Attention

71 commands have critical semantic gaps (critical_gaps > 0), averaging 57.8% coverage. These represent high-impact failures that could cause security issues or incorrect behavior.

### 4. Pattern Combinations Create Complexity

68 commands have 2+ semantic gap types. Average gap count is 2.0, with coverage at 56.2%. Common combinations:
- `command-substitution + heredoc` (most common)
- `for-loop + conditional`
- `for-loop + command-substitution`

## Coverage Distribution

```
  0-20%: Severe parsing failures
 20-40%: Major gaps
 40-60%: Moderate coverage
 60-80%: Good coverage with minor gaps
 80-100%: Excellent coverage
```

### By Pattern (lowest coverage commands):

- **Command Substitution:** Many at 0% (complete failures)
- **For-loop:** Lowest at 36.8%
- **Conditional:** Lowest at 42.9%
- **Combined:** Many at 0% (complex nested patterns)

## Actionable Priorities

Based on impact and frequency:

1. **High Priority:** Command substitution handling
   - 132 commands affected
   - 30.9% avg coverage (worst)
   - Many complete failures (0% coverage)

2. **Medium Priority:** For-loop parsing
   - 8 commands affected
   - 55.6% avg coverage
   - Room for improvement

3. **Low Priority:** Process substitution
   - Only 1 command affected
   - 80% coverage already

4. **Critical Priority:** Address critical gaps
   - 71 commands with critical_gaps > 0
   - Security implications
   - Review critical-gaps.tsv for specific issues

## Next Steps

1. Review `command-substitution-examples.tsv` for patterns
2. Focus on 0% coverage cases first (complete failures)
3. Identify common command substitution patterns to support
4. Review `critical-gaps.tsv` for security-sensitive failures
5. Test parser improvements against these specific examples
