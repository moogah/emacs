# Bash Parser Core Review - Beads Summary

**Review Date:** 2026-03-06
**Total Beads Created:** 7 (from this core architecture review)
**Labels:** bash-parser, 26-03-06-review

---

## Beads Created from Core Architecture Review

### Priority 1 (Critical/High)

**✅ Completed (Batch 4):**
1. **✅ emacs-er8n** - Add missing bash-parser-security dependency - COMPLETE (Batch 1)
2. **✅ emacs-95io** - Replace global depth counter with parameter passing - COMPLETE (Batch 4)
3. **✅ emacs-8x1a** - Add input validation to jf/bash-parse entry point - COMPLETE (Batch 4)

**Remaining:**
4. **emacs-64np** - Document AST node lifetime limitations in bash-parser-core
   - Type: task
   - Labels: bash-parser, 26-03-06-review, documentation
   - Issue: AST nodes in :ast field become invalid after parsing completes
   - Impact: Can cause crashes if users try to use AST after parse returns

5. **emacs-votj** - Implement command injection detection per bash-parser spec
   - Type: task
   - Labels: bash-parser, 26-03-06-review, spec-compliance
   - Issue: Spec requires bash -c, python -c, sh -c detection but not implemented
   - Impact: Missing critical security feature for analyzing nested commands

### Priority 2 (Should Address)

**✅ Completed (Batch 5):**
6. **✅ emacs-lmn5** - Eliminate code duplication - COMPLETE (Batch 5)
7. **✅ emacs-h634** - Consolidate duplicate handler logic - COMPLETE (Batch 5)

**Remaining:**
(No P2 beads from core architecture review remaining - all moved to other categories)

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

**✅ Completed:**
1. **✅ First:** emacs-er8n (dependencies) - COMPLETE (Batch 1)
2. **✅ Second:** emacs-95io (thread-safety) - COMPLETE (Batch 4)
3. **✅ Fourth:** emacs-8x1a (validation) - COMPLETE (Batch 4)
4. **✅ Fifth:** emacs-lmn5 (deduplication) - COMPLETE (Batch 5)

**Remaining:**
5. **Third:** emacs-64np (AST documentation) - Quick win, prevents user confusion
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
