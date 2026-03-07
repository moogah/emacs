# Construct Tests

Bash language construct-specific tests (94 tests).

## Philosophy

Construct tests validate how bash-parser handles specific **bash language constructs** like loops, conditionals, heredocs, and directory-changing commands. These tests ensure the parser correctly understands bash syntax and maintains state across construct boundaries.

## Test Files

- `test-loop-context.el` (9 tests) - Loop context (for/while)
- `test-conditional-context.el` (10 tests) - Conditionals (if/case)
- `test-heredoc-context.el` (10 tests) - Heredoc handling
- `test-directory-changing-commands.el` (36 tests) - Directory changes
- `test-pwd-directory-context.el` (29 tests) - Working directory tracking

Total: 94 tests (17.2% of suite)

## What These Tests Cover

**Loop Context:**
- For loops (basic, list iteration, range iteration)
- While loops, until loops
- Nested loops
- File operations within loops
- Break/continue handling

**Conditional Context:**
- If statements (basic, if-else, if-elif-else)
- Nested conditionals
- Test commands and bracket variations ([, [[, test)
- Case statements
- File operations within conditionals

**Heredoc Handling:**
- Basic heredocs
- Heredocs with variables and command substitution
- Quoted delimiters
- Indented heredocs (<<-)
- Multiple heredocs
- Empty heredocs and nested heredocs

**Directory Context:**
- cd commands (absolute, relative, tilde, hyphen, .., .)
- pushd/popd
- Directory changes in subshells vs main shell
- Directory context in pipelines, chains, and logical operators
- Directory changes within functions, loops, and conditionals
- Error handling for invalid paths
- Paths with spaces and special characters

**Working Directory Tracking:**
- pwd command tracking
- pwd after cd, pushd, popd
- pwd in subshells and command substitutions
- Logical vs physical pwd (-L vs -P)
- pwd with symbolic links
- pwd integration with file operations

## Running Tests

```bash
./bin/run-tests.sh -d config/experiments/bash-parser/test/construct
```

## When to Add Construct Tests

Add construct tests when testing:
- Bash-specific syntax handling (loops, conditionals, heredocs)
- State tracking across construct boundaries (directory context)
- Construct-specific parsing edge cases

**Don't add construct tests for:**
- General parsing → Use unit tests
- User scenarios → Use behavioral tests
- Feature interactions → Use integration tests

## Relationship to Other Test Categories

- **vs Unit**: Unit tests validate functions; construct tests validate bash syntax handling
- **vs Integration**: Integration tests validate feature interactions; construct tests validate construct-specific behavior
- **vs Behavioral**: Behavioral tests validate user scenarios; construct tests validate language construct handling

Construct tests ensure the parser understands bash's unique syntax patterns, while other test categories validate the parser's logic and user-facing behavior.
