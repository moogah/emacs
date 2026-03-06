# Bash Parser Directory Context - Progress Handoff

## Session Summary

Successfully implemented **18 tests** worth of bash parser directory context features using sequential agents. Reduced expected test failures from **20 → 2** with **zero regressions**.

**Latest Session:** Completed nested subshells fix - 1 additional test fixed.

## Current Status

**Test Results:** 536 total tests, 2 expected failures, 0 unexpected results
**Branch:** `gptel-scoped-bash-tools`
**Baseline:** `config/experiments/bash-parser/test-results.txt` (updated after each agent)

## Completed Beads (Closed ✓)

1. **emacs-zu4t** - HOME expansion for cd (`cd ~`, `cd`)
   - Fixed: test-cd-no-args, test-cd-with-tilde
   - Files: bash-parser-variables.org/el

2. **emacs-wpa0** - PWD assignments in variable chains
   - Fixed: test-variable-chain-pwd-assignment, test-variable-chain-relative-path-assignment, test-variable-chain-parent-path-assignment, test-pwd-assignment-simple
   - Files: bash-parser-core.org/el

3. **emacs-6ysw** - PWD inline assignment semantics
   - Fixed: test-pwd-assignment-inline
   - Files: bash-parser-core.org/el, bash-parser-file-ops.org/el

4. **emacs-fegi** - Directory context in conditionals and loops
   - Fixed: test-cd-in-if-statement, test-nested-conditional-relative-test, test-cd-conditional-both-branches (collateral)
   - Files: bash-parser-recursive.org/el
   - Created 2 new beads for parser bugs (see below)

5. **emacs-n1jk** - Command substitution in cd targets
   - Fixed: test-cd-with-command-substitution
   - Files: bash-parser-variables.org/el

6. **emacs-90p4** - OLDPWD tracking for cd -
   - Fixed: test-cd-with-dash
   - Files: bash-parser-recursive.org/el, bash-parser-variables.org/el

7. **emacs-6xjh** - Directory stack tracking (pushd/popd)
   - Fixed: test-pushd-absolute-path, test-popd-returns-to-previous
   - Files: bash-parser-variables.org/el, bash-parser-recursive.org/el

8. **emacs-o20p** - Edge cases (partial completion)
   - Fixed: test-relative-path-dot-ls
   - Remaining: test-pwd-substitution-nested (requires extensive file path resolution changes)
   - Files: bash-parser-semantics.org/el, bash-parser-variables.org/el

9. **emacs-254g** - Semicolon after || operator (parser bug)
   - Fixed: test-cd-with-or-fallback
   - Root cause: Tree-sitter parses `cd /dir || exit; cat file.txt` as [list(cd || exit), semicolon, command(cat)]
   - Solution: Enhanced list handler to detect root with both list and command children
   - Files: bash-parser-core.org/el

10. **emacs-pkh3** - For loop flattening in chains (parser bug)
   - Fixed: test-cd-for-loop-files
   - Root cause: Parser descended into for-loop bodies instead of treating for_statement as top-level command
   - Solution: Updated command node collection to preserve for_statement and if_statement structures
   - Files: bash-parser-core.org/el

11. **emacs-eg5t** - Subshell detection (completed)
   - Fixed: test-subshell-cd-isolation, test-subshell-pwd-assignment-isolation, test-nested-subshells (3 of 3 tests)
   - Root cause: Previous implementation didn't distinguish between `(...)` subshells, `$(...)` command subs, `$((...))`  arithmetic
   - Solution: Updated node detection to skip arithmetic_expansion and command_substitution, collect true subshell nodes
   - Additional fix: Pre-process `((` to `( (` to work around tree-sitter-bash parsing limitation
   - Files: bash-parser-core.org/el

## Remaining Open Beads (2 expected failures)

### Directory Context Tests (1 test)

**test-pwd-substitution-nested** (Priority: P3)
- Command: `cat $(basename $(pwd))/file.txt` with PWD=/Users/name/project → project/file.txt
- Requires: Extensive command substitution resolution in file path handling
- Status: Infrastructure partially added in emacs-o20p but needs more work
- Files: bash-parser-variables.org, bash-parser-file-ops.org

### Tree-Sitter Limitation (1 test)

**test-cmdsub-nested-backticks** (Priority: P3)
- Command: `echo \`echo \\\`date\\\`\``
- Issue: Tree-sitter-bash does not correctly parse nested backticks (consistently fails, not flaky)
- Root cause: Parser truncates content at first inner backtick, extracting only `echo \\` instead of `echo \\\`date\\\``
- Status: Legacy syntax (modern scripts use `$(...)` which nests correctly), marked as `:expected-result :failed`
- Investigation: See `config/experiments/bash-parser/docs/tree-sitter-bash-investigation.md`
- Files: bash-parser-core.org (jf/bash-parse--extract-command-substitutions)
- Recommendation: Accept limitation - nested backticks are deprecated, extremely rare in practice

## Recommended Next Steps

### Option 1: Fix Nested $(pwd) Substitution (More Complex)
Work on test-pwd-substitution-nested to implement nested command substitution in file paths. This is P3 and requires extensive changes to command substitution resolution.

### Option 2: Investigate Tree-Sitter Backtick Limitation (Completed - emacs-nrhw)
Investigated test-cmdsub-nested-backticks - confirmed as tree-sitter-bash parsing limitation. Test is consistently failing (not flaky). Root cause documented in `docs/tree-sitter-bash-investigation.md`. Recommendation: Accept limitation as nested backticks are legacy syntax.

**Status:** Only 2 tests remain out of 20 original directory-context tests! Both are P3 priority and represent edge cases rather than core functionality.

## Agent Usage Pattern

All implementations used this pattern:
```bash
# Launch agent with bead-implementation skill
/Task subagent_type=general-purpose
  - Use /bead-implementation skill
  - Run full test suite after changes
  - Compare against baseline in test-results.txt
  - Check for regressions
  - DO NOT run: git pull, git push, bd sync (handled by main session)
  - Provide skills: writing-elisp, emacs-literate-programming, bead-implementation
```

## Key Files

**Source (edit these):**
- `config/experiments/bash-parser/bash-parser-core.org` - Parser, node detection
- `config/experiments/bash-parser/bash-parser-variables.org` - Variable/cd extraction
- `config/experiments/bash-parser/bash-parser-recursive.org` - Chain/context tracking
- `config/experiments/bash-parser/bash-parser-file-ops.org` - File operations extraction

**Tests:**
- `config/experiments/bash-parser/test/test-directory-changing-commands.el`
- `config/experiments/bash-parser/test/test-pwd-directory-context.el`

**Baseline:**
- `config/experiments/bash-parser/test-results.txt` - Updated after each successful agent

## Important Lessons Learned

1. **Revert carefully:** The subshell commit (e492ae1) included both good (cd tracking in chains) and bad (incorrect subshell detection) changes. Had to revert and cherry-pick.

2. **Check file conflicts:** Only run agents in parallel if they work on different primary files. Sequential for same-file modifications.

3. **Trust but verify:** Always run tests after each agent and check for regressions before proceeding.

4. **Parser bugs vs semantic bugs:** Some test failures are tree-sitter parsing issues (emacs-254g, emacs-pkh3), not semantic analysis bugs.

## Git State

**Current commit:** Latest commit includes all completed implementations
**All changes pushed to:** `origin/gptel-scoped-bash-tools`
**Working tree:** Clean

## To Continue

1. List remaining beads: `bd list --label bash-parser --status open`
2. Review this handoff document
3. Choose next bead(s) to work on
4. Launch agents with same pattern as above
5. Update baseline after each successful agent
6. Close beads as they complete

**Progress:** 18 tests fixed, 2 remaining → 90% complete for directory-context feature set

### Nested Subshells Session Summary (Latest)
Completed nested subshells fix - final subshell test passing:
- test-nested-subshells: Multi-level context isolation ✓

**Results:** 3 → 2 expected failures, 1 test fixed, zero regressions

**Key insight:** Tree-sitter-bash doesn't correctly parse `((` at start of command (confuses with arithmetic expansion). Solution: Pre-process to normalize `((` → `( (` with space. This allows proper parsing of nested subshells like `((cd /a && cat a.txt) && cd /b && cat b.txt)`. Added structure detection and handler for subshell nodes. The Step 8 handler in bash-parser-recursive.org already had correct context isolation logic - no changes needed there.

### Subshell Detection Session Summary
Completed Option 2 - fixed subshell detection (emacs-eg5t):
- emacs-eg5t: Subshell detection (2 of 3 tests) ✓ partial

**Results:** 5 → 3 expected failures, 2 tests fixed, zero regressions

**Key insight:** The fix properly distinguishes between `(...)` subshells, `$(...)` command substitutions, and `$((...))`  arithmetic expansions. Node detection in bash-parser-core.org now correctly skips command_substitution and arithmetic_expansion nodes while collecting true subshell nodes. Test-nested-subshells remains as it requires multi-level context handling in bash-parser-recursive.org.

### Parser Bugs Session Summary
Completed Option 3 - fixed both parser bugs sequentially:
- emacs-254g: Semicolon after || operator (1 test) ✓
- emacs-pkh3: For loop flattening in chains (1 test) ✓

**Results:** 8 → 5 expected failures, 2 tests fixed, zero regressions

**Key insight:** Both bugs were in `jf/bash-parse--get-all-command-nodes` - the parser was either missing commands (254g) or descending into structures it should treat as atomic (pkh3). The fixes improved the parser's ability to preserve command structure.

### Phase 5 Session Summary
Completed 3 beads sequentially to avoid file conflicts:
- emacs-90p4: OLDPWD tracking (1 test) ✓
- emacs-6xjh: pushd/popd stack (2 tests) ✓
- emacs-o20p: Edge cases (1 of 2 tests) ⚠️ partial

**Results:** 11 → 7 expected failures, 4 tests fixed, zero regressions
