# LLM Scenario Testing for Bash Parser

## Overview

This test corpus extends the baseline bash parser tests with **55+ complex scenarios** that an LLM agent would likely generate when assisting with software development, testing, and repository management.

## What's New

### Files Created

1. **test-corpus-llm-scenarios.el** - Test data with 55+ complex commands
2. **test-llm-scenarios.el** - Interactive test runner
3. **LLM-SCENARIOS.md** - This documentation

### Test Categories

The new test corpus covers these LLM-common scenarios:

1. **Complex Flag Combinations** (4 tests)
   - Long flag chains: `tar -czf backup.tar.gz --exclude=.git --exclude=node_modules`
   - Multiple filter flags: `grep -rn --include='*.el' --exclude-dir=.git`

2. **Nested Command Chains** (4 tests)
   - Pipeline + chain: `git diff HEAD | grep -v '^-' | wc -l && echo 'Lines added'`
   - Long pipelines: `cat file | grep | sort | uniq -c | sort -rn`

3. **Xargs Patterns** (4 tests)
   - Null-separated: `find . -name '*.log' -print0 | xargs -0 rm -f`
   - Placeholder syntax: `ls *.el | xargs -I {} emacs --batch -l {}`

4. **Hidden Dangers** (6 tests)
   - `git clean -fdx` (more dangerous than -fd)
   - `chmod -R 777 .` (overly permissive)
   - `dd if=/dev/zero` (disk filling)
   - `docker system prune -af --volumes` (extremely destructive)
   - `:(){:|:&};:` (fork bomb)

5. **Test Framework Commands** (4 tests)
   - `npm run test -- --coverage --verbose`
   - `pytest -v -s --cov=src --cov-report=html`
   - `cargo test --release -- --nocapture`

6. **Docker Operations** (4 tests)
   - Volume mounts with command substitution
   - docker-compose chains
   - Interactive containers

7. **Environment Variables** (4 tests)
   - PATH modification: `export PATH=$PATH:/usr/local/bin`
   - Per-command env: `NODE_ENV=production npm run build`
   - Clean environments: `env -i HOME=$HOME command`

8. **Sudo/Privilege Escalation** (4 tests)
   - `sudo rm -rf /tmp/test`
   - `sudo -u www-data php script.php`
   - `sudo apt-get install -y package`

9. **Advanced Redirections** (4 tests)
   - Separate streams: `command > output.txt 2> error.txt`
   - Bash shorthand: `command &> combined.txt`
   - Here-strings: `cat <<< 'inline text'`

10. **Complex Find Operations** (3 tests)
    - `-delete` flag: `find . -type f -size +100M -delete`
    - OR conditions with exec
    - Pipeline to xargs

11. **Git Advanced Operations** (4 tests)
    - Interactive rebase: `git rebase -i HEAD~5`
    - History rewriting: `git filter-branch --force`
    - Reflog expiry: `git reflog expire --expire=now --all`

12. **Build Systems** (3 tests)
    - CMake workflows
    - Ninja build chains
    - Make with parallelism

13. **Compression/Archiving** (3 tests)
    - Zip with exclusions
    - Tar extraction with path manipulation
    - Unzip with options

14. **Process Management** (3 tests)
    - Force kill with pgrep: `kill -9 $(pgrep -f 'pattern')`
    - pkill with fallback
    - nohup with redirection

15. **Network Operations** (3 tests)
    - Download and execute: `wget -qO- url | bash` (dangerous!)
    - Complex curl API calls
    - netcat simple servers

16. **Text Processing Chains** (3 tests)
    - Multi-stage pipelines: 7 commands chained
    - AWK with bc for calculations
    - jq for JSON processing

## Usage

### Running All Tests

```elisp
;; Load the files
(load-file "config/experiments/bash-parser/bash-parser.el")
(load-file "config/experiments/bash-parser/test-corpus-llm-scenarios.el")
(load-file "config/experiments/bash-parser/test-llm-scenarios.el")

;; Generate full report
M-x jf/bash-parser-llm-scenarios-report
```

This creates a detailed markdown report showing:
- Summary statistics
- Exploratory tests that parsed successfully (with actual output)
- Exploratory tests that failed (with error messages)
- Tests with expectations (comparing expected vs actual)

### View by Category

```elisp
M-x jf/bash-parser-llm-scenarios-by-category
```

Shows results organized by category (flags, xargs, docker, etc.) with success/failure status.

### Test Single Scenario

```elisp
M-x jf/bash-parser-test-single-llm-scenario
```

Prompts for a test ID (with completion) and shows detailed results for that specific test.

### Programmatic Access

```elisp
;; Run all tests and get data structure
(let ((results (jf/bash-parser-test-all-llm-scenarios)))
  (message "Total: %d, Success: %d"
           (plist-get (plist-get results :stats) :total)
           (plist-get (plist-get results :stats) :parse-success)))

;; Test specific command
(jf/bash-parse "docker run --rm -v $(pwd):/app node:16 npm install")
```

## What We're Testing For

### Safety Detection Goals

1. **Dangerous commands must be flagged**
   - Even in complex contexts (pipelines, chains)
   - Even with "safe-looking" wrappers
   - Including new dangerous patterns not yet in database

2. **Complex structures must parse correctly**
   - Multi-stage pipelines
   - Nested chains
   - Mixed operators

3. **Edge cases must not break parser**
   - Command substitution
   - Process substitution
   - Complex quoting
   - Environment variable syntax

4. **Real-world LLM patterns must work**
   - Long flag chains
   - Test framework invocations
   - Docker workflows
   - Build system commands

### Expected Findings

Some tests have `:expect nil` because:
- Command substitution not yet implemented
- Process substitution not supported
- Complex features intentionally deferred
- Invalid syntax (like fork bomb)

These are **exploratory tests** - we run them to see what happens and document the behavior.

## Next Steps After Running Tests

1. **Review successful exploratory tests**
   - Are they parsed correctly?
   - Should they have explicit expectations?
   - Any surprising results?

2. **Analyze failed tests**
   - Expected failures (command substitution, etc.)?
   - Or unexpected bugs?
   - Should they be fixed or documented as unsupported?

3. **Identify new dangerous patterns**
   - `chmod -R 777` - overly permissive
   - `dd` - can fill disk
   - `docker system prune -af` - destroys resources
   - `sudo` - should always require approval
   - `wget | bash` - download and execute

4. **Update dangerous pattern database**
   - Add new commands
   - Add new subcommands
   - Add new flag patterns

5. **Document unsupported features**
   - Update FINDINGS.md
   - Mark limitations clearly
   - Provide workarounds if available

## Integration Points

These tests help validate the parser for:

1. **gptel bash tools** - AI-generated command validation
2. **Safety checking** - Pre-execution approval workflows
3. **Scope validation** - Ensuring commands stay within allowed paths
4. **Audit logging** - Recording what commands were executed

## Contributing New Scenarios

To add new test cases:

1. Edit `test-corpus-llm-scenarios.el`
2. Add to appropriate category or create new one
3. Follow the plist format:
   ```elisp
   (:id "llm-category-NNN"
    :command "the bash command"
    :note "description of what this tests"
    :expect (:command-name "cmd"
             :flags (...)
             ...))
   ```
4. Use `:expect nil` for exploratory tests
5. Run `M-x jf/bash-parser-llm-scenarios-report` to verify

## Test ID Convention

Format: `llm-<category>-<number>`

Examples:
- `llm-flags-001` - Complex flag combinations
- `llm-xargs-001` - Xargs patterns
- `llm-docker-001` - Docker operations
- `llm-danger-001` - Hidden dangerous commands

This makes it easy to find related tests and organize by category.
