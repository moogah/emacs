# Directory Context Tracking - Test Coverage and Implementation Guide

## Overview

This document describes test coverage for **stateful directory context tracking** - the ability to track how `cd`, PWD reassignment, and other commands change what directory subsequent file operations resolve against.

## Test Files

- **test-pwd-directory-context.el** - Existing tests for PWD resolution (static context)
  - Explicit `$PWD` variable references
  - Relative path resolution (`.`, `./`, `../`)
  - Command substitutions with `pwd`
  - Variable chains with PWD context

- **test-directory-changing-commands.el** - NEW tests for dynamic directory changes
  - `cd` command tracking
  - PWD variable reassignment
  - `pushd`/`popd` directory stack
  - Subshell isolation
  - Conditional and error handling

## Test Categories in test-directory-changing-commands.el

### 1. cd Command - Absolute Paths (3 tests)
**Priority: HIGH** - Most common and straightforward case

- `cd /path && cat file.txt` → file.txt resolves in /path
- `cd /path && cat ./file.txt` → ./file.txt resolves against /path
- Multiple operations after cd all use new context

**Implementation:** Track cd command, extract target directory (resolve variables), update directory context for subsequent commands in the chain.

### 2. cd Command - Relative Paths (4 tests)
**Priority: HIGH** - Essential for real-world usage

- `cd subdir && cat file.txt` → resolves subdir against current PWD first
- `cd ./subdir` → same as above
- `cd ../other` → navigate up from current PWD, then to sibling
- `cd .` → no-op, stays in current directory

**Implementation:** Resolve cd's target path against current PWD using existing `jf/bash-resolve-relative-path`, then update context.

### 3. cd Command - Variable Expansion (3 tests)
**Priority: MEDIUM** - Common in scripts

- `cd $DIR && cat file.txt` → resolve $DIR first, then update context
- `cd $SUBDIR` where SUBDIR is relative → resolve against current PWD
- `cd $PWD/sub` → resolve $PWD then navigate

**Implementation:** Use existing variable resolution on cd's target, then update context.

### 4. cd Special Forms (3 tests)
**Priority: LOW** - Less common, more complex

- `cd -` → uses OLDPWD (requires tracking previous directory)
- `cd ~` → uses $HOME
- `cd` (no args) → uses $HOME

**Implementation:**
- `~` and no args: straightforward, use HOME from var-context
- `cd -`: requires tracking OLDPWD through cd sequences (complex)

### 5. Multiple cd Commands (4 tests)
**Priority: HIGH** - Critical for correctness

- `cd /first && cd /second && cat file.txt` → last cd wins
- `cd /base && cd subdir` → second cd resolves against first
- Operations between cds use context at their position
- Semicolon separator `cd /dir; cat file.txt`

**Implementation:** Process commands sequentially, maintaining directory context state through the chain. Each cd updates context for subsequent commands.

### 6. PWD Variable Reassignment (4 tests)
**Priority: MEDIUM** - Less common than cd, but possible

- `PWD=/new/path cat file.txt` → inline assignment affects single command
- `export PWD=/new/path && cat file.txt` → affects subsequent commands
- `PWD=/new/path; cat file.txt` → simple assignment
- `PWD=$VAR && cat file.txt` → assignment with variable expansion

**Implementation:** Similar to cd tracking, but watch for PWD assignments in variable assignments. Update context for subsequent commands.

**Design question:** Should inline `PWD=/path cmd` update context for later commands in chain, or only for that command?

### 7. pushd/popd Directory Stack (2 tests)
**Priority: LOW** - Uncommon in security-sensitive contexts

- `pushd /dir && cat file.txt` → changes directory like cd
- `popd && cat file.txt` → returns to previous directory in stack

**Implementation:** Would require full directory stack tracking. Lower priority than cd.

### 8. Subshell Isolation (3 tests)
**Priority: MEDIUM-HIGH** - Important for correctness

- `(cd /sub && cat sub.txt) && cat main.txt` → cd in subshell doesn't affect parent
- `(PWD=/sub && cat sub.txt) && cat main.txt` → PWD assignment isolated
- Nested subshells each have isolated context

**Implementation:** When entering subshell, create copy of directory context. Changes in subshell don't propagate to parent. Requires tracking subshell boundaries.

### 9. cd with Conditionals (3 tests)
**Priority: MEDIUM** - Important for real scripts

- `cd /dir || exit; cat file.txt` → assume cd succeeds for static analysis
- `if cd /dir; then cat file.txt; fi` → cd updates context for then block
- `if [ -d /dir ]; then cd /dir && cat a.txt; else cd /other && cat b.txt; fi` → different contexts per branch

**Implementation:** For conservative security analysis, assume cd succeeds. Track different contexts for then/else branches (may report both possible paths).

### 10. Edge Cases (3 tests)
**Priority: LOW** - Unusual patterns

- `cd ''` → empty string, typically fails
- `cd file.txt` → cd to non-directory (would fail)
- `cd /a*/b*` → cd with glob patterns
- `cd $(dirname $FILE)` → cd with command substitution

**Implementation:** Low priority, complex runtime behavior.

### 11. Integration Tests (3 tests)
**Priority: MEDIUM** - Real-world validation

- `cd /project && ./run-tests.sh` → script execution in new directory
- `cd /logs && for f in *.log; do cat $f; done` → loop with directory context
- `cd /deploy && ./build.sh && cd /target && ./install.sh` → deployment pattern

**Implementation:** These validate that cd tracking works with other features (script execution, loops).

## Implementation Strategy

### Phase 1: Basic cd Tracking (HIGH priority)
**Goal:** Track simple cd commands in linear command chains

1. Detect `cd` commands in chains/pipelines
2. Extract cd's target directory (absolute or relative)
3. Resolve target using existing variable resolution and relative path resolution
4. Update directory context (PWD) for subsequent commands
5. Pass updated context to file operation extraction

**Files to modify:**
- `bash-parser-recursive.el` or `bash-parser-core.el` - cd detection and context updating
- Integrate with existing `jf/bash-analyze-file-operations-recursive`

**Tests that should pass:**
- All "cd Command - Absolute Paths" tests
- All "cd Command - Relative Paths" tests
- Most "Multiple cd Commands" tests
- "cd Command - Variable Expansion" tests (if variable resolution already works)

### Phase 2: PWD Reassignment (MEDIUM priority)
**Goal:** Track explicit PWD variable assignments

1. Detect PWD assignments (simple, export, inline)
2. Resolve assignment values
3. Update directory context like cd does
4. Handle inline vs persistent assignments

**Tests that should pass:**
- All "PWD Variable Reassignment" tests

### Phase 3: Subshell Isolation (MEDIUM-HIGH priority)
**Goal:** Prevent subshell cd from affecting parent context

1. Detect subshell boundaries in AST
2. Create context copies for subshells
3. Isolate context changes within subshells
4. Restore parent context after subshell

**Tests that should pass:**
- All "Subshell Isolation" tests

### Phase 4: Conditionals (MEDIUM priority)
**Goal:** Track different contexts in then/else branches

1. Track directory context through conditional branches
2. Possibly report operations from both branches with context markers
3. Conservative analysis: assume success path

**Tests that should pass:**
- All "cd with Conditionals" tests

### Phase 5: Special Forms and Edge Cases (LOW priority)
**Goal:** Handle less common patterns

- `cd ~`, `cd` (no args) - straightforward with HOME
- `cd -` - requires OLDPWD tracking
- `pushd`/`popd` - requires directory stack
- Edge cases - as needed

## Key Design Decisions

### 1. Context Propagation Model

**Question:** How should directory context flow through commands?

**Proposal:**
- Context is passed forward through sequential commands (&&, ;, \n)
- Context is isolated in subshells (parentheses)
- Context diverges in conditionals (if/then/else)
- Pipeline commands share the same initial context (no propagation within pipeline)

### 2. Inline PWD Assignment Scope

**Question:** Does `PWD=/path cmd1 && cmd2` affect cmd2?

Bash behavior:
- `PWD=/path cmd1` - only affects cmd1's environment
- `PWD=/path; cmd2` - does NOT affect cmd2 (PWD is special, shell maintains it)
- `export PWD=/path; cmd2` - affects cmd2 (but shell may override)

**Proposal:** For security analysis, be conservative:
- Inline assignments affect only that command
- Explicit assignments (`PWD=...;` or `export PWD=...`) update context for subsequent commands
- Document that this may not match exact bash behavior, but is conservative for security

### 3. Error Handling in cd

**Question:** Should `cd /nonexistent && cat file.txt` assume cd fails or succeeds?

**Proposal:** Conservative security analysis assumes success path:
- Assume cd succeeds unless provably impossible
- Report file operations based on successful cd
- This prevents false negatives (missing file accesses that could happen)

### 4. Command Substitution in cd

**Question:** How to handle `cd $(dirname $FILE)`?

**Proposal:**
- If command substitution can be statically evaluated (e.g., dirname, basename with known args), do so
- Otherwise, mark as unresolved and leave path as-is
- Low priority - complex to implement fully

## Testing Workflow

### Running Tests

```bash
# Run all directory context tests
make test-directory DIR=config/experiments/bash-parser/test

# Run specific test file
make test-pattern PATTERN='test-directory-changing-commands'

# Run with snapshot (to track progress)
./bin/run-tests.sh -d config/experiments/bash-parser/test -s
```

### Incremental Implementation

1. **Start with all tests failing** - these are specification tests
2. **Implement Phase 1** - basic cd tracking
3. **Run tests** - watch Phase 1 tests start passing
4. **Remove `:expected-result :failed`** from passing tests
5. **Repeat** for each phase

### Test Snapshot Strategy

Use snapshot testing to track implementation progress:

```bash
# Capture baseline (all failing)
./bin/run-tests.sh -p 'test-directory-changing' -s

# After implementing cd tracking
./bin/run-tests.sh -p 'test-directory-changing' -s

# Compare progress
git diff config/experiments/bash-parser/test/test-results.txt
```

## Integration with Existing Code

### Current Variable Resolution

Already implemented:
- `jf/bash-resolve-relative-path` - resolves `.`, `./`, `../` against PWD
- `jf/bash--resolve-path-variables` - resolves $VAR in paths
- Variable chain tracking - tracks variable assignments

To leverage:
- Use these functions to resolve cd's target directory
- Update var-context with new PWD value
- Pass updated context to file operation extraction

### Recursive Analysis Architecture

The recursive analyzer (`jf/bash-analyze-file-operations-recursive`) already:
- Traverses command chains, pipelines, loops, conditionals
- Maintains context through recursion
- Passes var-context down through structure

To extend:
- Add directory context tracking alongside var-context
- Update context when encountering cd/PWD assignments
- Handle context isolation for subshells

### Security Implications

**Critical:** Directory context tracking is security-critical for scope validation:

1. **Security goal:** Extract exact paths that execution would access
2. **Failure mode:** Parser extracts `/original/file.txt` but execution accesses `/after-cd/file.txt`
3. **Consequence:** Scope validation passes, but command accesses unauthorized files

**Conservative analysis principle:**
- When in doubt, assume cd succeeds (report both possible paths if needed)
- Never fail to report a file access that could happen at runtime
- Better to report false positives than false negatives

## Summary

**Total test coverage:** 39 new tests for directory context tracking

**Implementation phases:**
1. Basic cd tracking (HIGH) - 14 tests
2. PWD reassignment (MEDIUM) - 4 tests
3. Subshell isolation (MEDIUM-HIGH) - 3 tests
4. Conditionals (MEDIUM) - 3 tests
5. Special forms and edge cases (LOW) - 9 tests
6. Integration validation (MEDIUM) - 3 tests

**Start here:** Phase 1 - Basic cd tracking with absolute and relative paths. This provides the most value with the clearest implementation path.
