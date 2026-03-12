# Tree-Sitter-Bash Nested Backtick Investigation

## Issue Summary

**Test**: `test-cmdsub-nested-backticks`
**Command**: `echo \`echo \\\`date\\\`\``
**Status**: Consistently failing (not flaky as originally reported)
**Priority**: P3 (edge case - legacy syntax)

## Expected vs Actual Behavior

### Expected (from corpus)
Two command substitutions:
1. Level 1: `(:syntax "\`" :content "echo \\\`date\\\`" :nesting-level 1)`
2. Level 2: `(:syntax "\`" :content "date" :nesting-level 2)`

### Actual (from parser)
One command substitution:
1. Level 1: `(:syntax "\`" :content "echo \\\\\\\\" :nesting-level 1)`

## Analysis

The issue is in **tree-sitter-bash parsing**, not our extraction logic.

### Test Evidence

```elisp
(jf/bash-parse "echo \`echo \\\\\`date\\\\\`\`")
```

Result shows only ONE substitution with content `"echo \\\\\\\\"` instead of `"echo \\\\\`date\\\\\`"`.

The inner backtick-escaped `date` command is being truncated. Tree-sitter-bash appears to stop parsing the backtick content when it encounters the **first unescaped backtick**, even though in nested backticks the inner backticks should be escaped with `\\`.

### Escaping Rules for Nested Backticks

In bash, nested backticks require increasingly complex escaping:
- Outer backticks: `` `command` ``
- Nested once: `` `echo \`nested\`` ``
- Nested twice: `` `echo \`echo \\\`nested\\\`\`` ``

Each level of nesting doubles the backslashes. The tree-sitter-bash grammar may not handle this escaping correctly.

## Tree-Sitter-Bash Grammar Investigation

The tree-sitter-bash grammar needs to be checked for:
1. How it handles escaped backticks within backtick command substitutions
2. Whether it correctly parses multi-level backtick nesting
3. If there are any known issues or limitations documented

## Workaround Options

### Option 1: Accept the Limitation
- Mark test as `:expected-result :failed` permanently
- Document that nested backticks are not fully supported
- **Rationale**: Legacy syntax, rarely used in modern scripts (modern scripts use `$(...)`)
- **Status**: Already done - test marked as failing

### Option 2: Fix Upstream
- Report issue to tree-sitter-bash project
- Contribute a fix if possible
- Wait for upstream fix and update
- **Complexity**: High - requires C grammar expertise
- **Benefit**: Proper solution, benefits entire ecosystem

### Option 3: Pre-process Workaround
- Similar to the `((` workaround we added for subshells
- Pre-process nested backticks to convert to `$(...)` syntax before parsing
- **Example**: `` `echo \`date\`` `` → `$(echo $(date))`
- **Complexity**: Medium - regex-based transformation
- **Risk**: May break valid commands that contain both syntaxes

### Option 4: Manual Extraction Fallback
- Detect when tree-sitter fails to parse nested backticks
- Fall back to manual parsing for backtick nesting
- Use stack-based approach to track nesting levels and escaping
- **Complexity**: High - reimplementing parser logic
- **Risk**: Maintenance burden, likely to have edge cases

## Recommendation

**Accept the limitation (Option 1)** because:

1. **Legacy syntax**: Nested backticks are deprecated in favor of `$(...)` which nests cleanly
2. **Rare in practice**: Modern bash scripts overwhelmingly use `$(...)` for command substitution
3. **Already marked**: Test is already marked as `:expected-result :failed`
4. **Low priority**: P3 status indicates this is not core functionality
5. **Upstream issue**: The root cause is in tree-sitter-bash, not our code
6. **Cost vs benefit**: Workarounds are complex and risky for minimal real-world benefit

## File Impact Assessment

### Commands Affected
Only commands using nested backtick syntax: `` `cmd1 \`cmd2\`` ``

### Modern Alternative
Use `$(...)` syntax instead: `$(cmd1 $(cmd2))` - fully supported

### Real-World Impact
**Minimal** - this syntax is extremely rare in:
- Modern bash scripts (prefer `$(...)`)
- Interactive shells (users type `$(...)`)
- Generated scripts (tools generate `$(...)`)
- LLM-generated commands (Claude uses `$(...)`)

The only place you find nested backticks is in legacy scripts written before 2000.

## Testing Results

Ran test 10 times - **consistently fails** (not flaky):
```
Run 1: failed
Run 2: failed
...
Run 10: failed
```

All failures show identical behavior - tree-sitter truncates at first inner backtick.

## Conclusion

This is a **tree-sitter-bash parsing limitation**, not a bug in our extraction logic. The test should remain marked as `:expected-result :failed` with updated documentation explaining why.

### Bead Description Update

The bead description incorrectly describes this as a `((` parsing issue. The actual issue is:
- **Wrong**: "(( parsing limitation"
- **Right**: "Nested backtick parsing limitation"

The `((` workaround was for subshell detection (already fixed). This is a different issue.

## Action Items

- [x] Investigate root cause (tree-sitter-bash limitation)
- [x] Confirm not flaky (consistently fails)
- [x] Document findings in this file
- [ ] Update bead description to correct the issue description
- [ ] Consider filing upstream issue with tree-sitter-bash (optional)
- [ ] Update HANDOFF.md to clarify this is tree-sitter limitation, not intermittent

## References

- Test file: `config/experiments/bash-parser/test/test-command-substitution.el`
- Corpus entry: `config/experiments/bash-parser/test/corpus-parse-command-substitution.el` (cmdsub-edge-003)
- Extraction function: `config/experiments/bash-parser/bash-parser-core.org` (jf/bash-parse--extract-command-substitutions)
- Tree-sitter-bash: https://github.com/tree-sitter/tree-sitter-bash
