# Bash Parser Core Review - Beads Summary

**Review Date:** 2026-03-06
**Total Beads Created:** 7 (from this core architecture review)
**Labels:** bash-parser, 26-03-06-review

---

## Beads Created from Core Architecture Review

### Priority 1 (Critical/High)

1. **emacs-er8n** - Add missing bash-parser-security dependency to bash-parser-core
   - Type: bug
   - Labels: bash-parser, 26-03-06-review, dependencies
   - Issue: Core module references variables from security module without declaring dependency
   - Impact: Can cause load-order bugs and undefined variable errors

2. **emacs-95io** - Replace global depth counter with parameter passing in bash-parser-core
   - Type: task
   - Labels: bash-parser, 26-03-06-review, thread-safety
   - Issue: Global mutable state for recursion depth tracking is not thread-safe
   - Impact: Breaks with async code, non-reentrant, hard to debug

3. **emacs-64np** - Document AST node lifetime limitations in bash-parser-core
   - Type: task
   - Labels: bash-parser, 26-03-06-review, documentation
   - Issue: AST nodes in :ast field become invalid after parsing completes
   - Impact: Can cause crashes if users try to use AST after parse returns

4. **emacs-votj** - Implement command injection detection per bash-parser spec
   - Type: task
   - Labels: bash-parser, 26-03-06-review, spec-compliance
   - Issue: Spec requires bash -c, python -c, sh -c detection but not implemented
   - Impact: Missing critical security feature for analyzing nested commands

### Priority 2 (Should Address)

5. **emacs-lmn5** - Eliminate code duplication in bash-parser handler functions
   - Type: task
   - Labels: bash-parser, 26-03-06-review, refactoring
   - Issue: 200+ lines of duplicated code between handler and -node function pairs
   - Impact: Maintenance burden, potential for divergence

6. **emacs-8x1a** - Add input validation to jf/bash-parse entry point
   - Type: task
   - Labels: bash-parser, 26-03-06-review, validation
   - Issue: No validation that command-string is actually a string
   - Impact: Confusing error messages for invalid input

### Priority 3 (Nice to Have)

7. **emacs-mu54** - Document tree-sitter (( workaround and track upstream fix
   - Type: task
   - Labels: bash-parser, 26-03-06-review, documentation, upstream
   - Issue: Workaround for tree-sitter (( parsing is underdocumented
   - Impact: Future maintainers won't understand when it can be removed

---

## Additional Context

This review was conducted on the bash-parser-core module only. Additional Beads exist from other review sessions covering:
- bash-parser-security
- bash-parser-file-operations
- bash-parser-recursive-analyzer
- Integration and testing infrastructure

See full review document at:
`config/experiments/bash-parser/review/core-architecture-review.md`

---

## Implementation Order Recommendation

1. **First:** emacs-er8n (dependencies) - Prerequisite for correct module loading
2. **Second:** emacs-95io (thread-safety) - Foundational correctness issue
3. **Third:** emacs-64np (AST documentation) - Quick win, prevents user confusion
4. **Fourth:** emacs-8x1a (validation) - Improves error handling
5. **Fifth:** emacs-lmn5 (deduplication) - Improves maintainability
6. **Sixth:** emacs-votj (command injection) - Major feature, requires design work
7. **Seventh:** emacs-mu54 (documentation) - Long-term tracking

---

## Query Commands

List all core review Beads:
```bash
bd list --label 26-03-06-review
```

Show P1 issues only:
```bash
bd list --label 26-03-06-review --priority 1
```

Show ready-to-implement issues:
```bash
bd ready --label 26-03-06-review
```
