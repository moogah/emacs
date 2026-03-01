# Bash Parser - LLM Scenario Testing Guide

## What We Built

A comprehensive test suite for validating bash command parsing with **55+ complex scenarios** that LLMs commonly generate.

## Files Created

```
config/experiments/bash-parser/
├── test-corpus-llm-scenarios.el    # 55+ test cases for LLM commands
├── test-llm-scenarios.el           # Interactive test runner
├── LLM-SCENARIOS.md                # Detailed documentation
├── LLM-FINDINGS.md                 # Critical security gaps discovered
└── TESTING-GUIDE.md                # This file
```

## Quick Start

### 1. Load Everything

```elisp
;; In Emacs
(load-file "config/experiments/bash-parser/bash-parser.el")
(load-file "config/experiments/bash-parser/test-corpus-llm-scenarios.el")
(load-file "config/experiments/bash-parser/test-llm-scenarios.el")
```

### 2. Run Full Report

```elisp
M-x jf/bash-parser-llm-scenarios-report
```

This generates a detailed markdown report showing:
- Summary statistics (total tests, success/failure rates)
- Exploratory tests with actual parser output
- Tests with expectations for validation

### 3. View by Category

```elisp
M-x jf/bash-parser-llm-scenarios-by-category
```

Shows results organized by:
- Complex flags (tar, rsync, grep)
- Nested chains (pipeline + &&/||)
- Xargs patterns
- Hidden dangers (sudo, chmod, dd)
- Test frameworks (npm, pytest, cargo)
- Docker operations
- Environment variables
- And 9 more categories...

### 4. Test Single Scenario

```elisp
M-x jf/bash-parser-test-single-llm-scenario
```

Prompts for test ID with completion and shows detailed results.

## Test Categories Overview

The test suite covers **16 categories** with **55+ test cases**:

| Category | Tests | Key Scenarios |
|----------|-------|---------------|
| Complex Flags | 4 | tar, rsync, curl with multiple long flags |
| Nested Chains | 4 | Pipeline followed by && or \|\| operators |
| Xargs Patterns | 4 | find \| xargs, null-separated, placeholder syntax |
| Hidden Dangers | 6 | sudo, chmod 777, dd, fork bomb, docker prune |
| Test Frameworks | 4 | npm, pytest, cargo, jest with complex flags |
| Docker | 4 | Volume mounts, docker-compose, exec, build |
| Environment | 4 | export PATH, FOO=bar command, env -i |
| Sudo/Privilege | 4 | sudo rm, sudo -u, sudo apt-get, sudo -E |
| Advanced Redirections | 4 | Multiple streams, here-strings, tee |
| Complex Find | 3 | -delete flag, OR conditions, xargs chains |
| Git Advanced | 4 | rebase -i, filter-branch, reflog expire |
| Build Systems | 3 | cmake, ninja, make with parallelism |
| Compression | 3 | zip/tar/unzip with complex options |
| Process Mgmt | 3 | kill, pkill, nohup with redirection |
| Network | 3 | wget \| bash, curl POST, netcat |
| Text Processing | 3 | Multi-stage pipelines, awk, jq |

## Critical Findings

### 🔴 Security Gaps Discovered

1. **Sudo commands NOT flagged as dangerous** (CRITICAL)
   - `sudo rm -rf /tmp/test` → dangerous-p: nil
   - Parser sees sudo as command, rm as argument
   - Nested dangerous command not detected

2. **Git clean flag variations missed** (HIGH)
   - `git clean -fdx` → dangerous-p: nil
   - Database has `-fd` but not `-fdx`
   - Flag combinations not handled

3. **Missing dangerous commands** (HIGH)
   - `chmod -R 777` - overly permissive
   - `dd if=/dev/zero` - can fill disk
   - `docker system prune -af` - destroys resources

4. **No recursive parsing** (MEDIUM)
   - Sudo, env, nice, nohup need nested parsing
   - Wrapper commands hide dangerous operations

### ✅ What Works Great

1. **Complex flag parsing** - Multiple long flags handled correctly
2. **Pipeline detection** - Multi-stage pipelines fully parsed
3. **Chain operators** - &&, ||, ; all work correctly
4. **Nested structures** - Pipeline + chain combinations work

## Recommended Actions

### Phase 1: Fix Critical Gaps (Do First)

1. **Add sudo to dangerous patterns**
   ```elisp
   (sudo . ((:any t)))  ; Always dangerous
   (doas . ((:any t)))
   (su . ((:any t)))
   ```

2. **Fix git clean pattern matching**
   ```elisp
   ;; Instead of exact flags:
   (:subcommand "clean" :flags ("-f" "-fd" "-df"))

   ;; Use component matching:
   (:subcommand "clean" :any-flag-contains ("f" "d"))
   ```

3. **Add chmod dangerous pattern**
   ```elisp
   (chmod . ((:flags ("-R" "-r"))
             (:any-flag-contains ("R" "r"))))
   ```

4. **Add dd dangerous pattern**
   ```elisp
   (dd . ((:any t)))  ; Always dangerous
   ```

### Phase 2: Enhance Pattern Matching

1. **Implement flag component matching**
   ```elisp
   (defun jf/bash-parser--flag-contains-chars (flag chars)
     "Check if FLAG contains all CHARS."
     (cl-every (lambda (c) (string-match-p (regexp-quote c) flag))
               chars))
   ```

2. **Add danger levels** (critical/high/medium/low)
   ```elisp
   (sudo . ((:any t :level critical)))
   (rm . ((:any-flag-contains ("r" "f") :level high)))
   ```

3. **Implement recursive parsing for sudo**
   - Parse sudo's positional args as nested command
   - Propagate nested danger up to parent

### Phase 3: Expand Test Coverage

1. **Run full test suite** and document results
2. **Add new dangerous patterns** discovered from tests
3. **Create integration tests** for gptel bash tools
4. **Document unsupported patterns** clearly

## How to Extend the Test Suite

### Adding New Test Cases

Edit `test-corpus-llm-scenarios.el`:

```elisp
(:id "llm-category-NNN"
 :command "the bash command"
 :note "description of what this tests"
 :expect (:command-name "cmd"
          :subcommand "subcmd"
          :flags ("-flag")
          :positional-args ("arg1")
          :dangerous-p t))
```

For exploratory tests (don't know expected output):
```elisp
(:id "llm-explore-001"
 :command "some complex command"
 :note "testing edge case"
 :expect nil)  ; Will show actual parser output
```

### Test ID Convention

Format: `llm-<category>-<number>`

Examples:
- `llm-flags-001` - Complex flag combinations
- `llm-xargs-001` - Xargs patterns
- `llm-danger-001` - Hidden dangerous commands

### Recommended New Categories

Consider adding tests for:
- **Kubernetes** - kubectl commands with complex flags
- **Cloud CLIs** - aws, gcloud, az commands
- **Database** - psql, mysql with -c flag
- **Scripting** - perl -e, ruby -e, node -e
- **Package managers** - pip, gem, cargo, go get
- **Version control** - hg, svn, p4 commands
- **System admin** - systemctl, journalctl, sysctl

## Integration with gptel

The ultimate goal is safe LLM command execution:

```elisp
;; Validate before execution
(defun gptel-bash-tool--validate-command (command)
  "Check if COMMAND is safe to execute."
  (let ((result (jf/bash-parse command)))
    (cond
     ((not (plist-get result :success))
      (user-error "Command failed to parse: %s"
                  (plist-get result :error)))

     ((plist-get result :dangerous-p)
      ;; Require explicit user approval
      (gptel-bash-tool--request-approval command result))

     (t
      ;; Safe to execute
      result))))
```

## Success Metrics

After implementing fixes:

- [ ] 100% detection of sudo commands
- [ ] Detection of git clean flag variations
- [ ] Detection of chmod overly permissive modes
- [ ] Detection of dd commands
- [ ] Detection of download-and-execute patterns
- [ ] All 55+ tests documented with expected behavior
- [ ] Full integration with gptel bash tools
- [ ] Comprehensive danger level system

## Resources

- **Main documentation**: `README.md` - Parser overview
- **Original findings**: `FINDINGS.md` - First development cycle
- **LLM scenarios**: `LLM-SCENARIOS.md` - Test suite documentation
- **Security gaps**: `LLM-FINDINGS.md` - Critical issues discovered
- **Test corpus**: `test-corpus.el` - Original 60 baseline tests
- **LLM corpus**: `test-corpus-llm-scenarios.el` - 55+ new tests

## Example Session

```elisp
;; 1. Load everything
(load-file "config/experiments/bash-parser/bash-parser.el")
(load-file "config/experiments/bash-parser/test-corpus-llm-scenarios.el")
(load-file "config/experiments/bash-parser/test-llm-scenarios.el")

;; 2. Run specific test
(jf/bash-parse "sudo rm -rf /tmp/test")
;; => Shows sudo parsing issue

;; 3. Test by category
M-x jf/bash-parser-llm-scenarios-by-category
;; => View all tests organized by type

;; 4. Full report
M-x jf/bash-parser-llm-scenarios-report
;; => Comprehensive markdown report

;; 5. Single test deep dive
M-x jf/bash-parser-test-single-llm-scenario
;; => Pick "llm-danger-001" to see sudo issue
```

## Next Steps

1. **Review this guide** and the findings in `LLM-FINDINGS.md`
2. **Run the test suite** to see current parser behavior
3. **Prioritize fixes** starting with critical security gaps
4. **Expand test coverage** for your specific LLM use cases
5. **Integrate with gptel** bash tools for safe command execution

## Questions?

- Parser crashes? → Check FINDINGS.md for known issues
- Adding new patterns? → See bash-parser.el dangerous patterns
- New test categories? → Follow format in test-corpus-llm-scenarios.el
- Integration help? → See gptel bash tool examples

---

**Bottom Line:** The parser structure is solid. The dangerous pattern database needs expansion to handle real-world LLM commands safely. This test suite gives you a comprehensive foundation for validating and improving the parser.
