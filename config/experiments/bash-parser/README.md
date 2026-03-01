# Bash Command Parser

A standalone bash command parser using tree-sitter for semantic analysis.

## What We Built

A literate programming module (`bash-parser.org`) that parses bash command strings and extracts:

- **Command names**: Base executable (e.g., `git`, `ls`, `python`)
- **Subcommands**: Secondary command for multi-command tools (e.g., `log` in `git log`)
- **Flags**: Arguments starting with `-` or `--`
- **Positional arguments**: Non-flag arguments
- **Dangerous pattern detection**: Matches commands against a database of risky operations

## Files

```
config/experiments/bash-parser/
├── bash-parser.org          # Main literate source (edit this!)
├── bash-parser.el           # Generated from .org (do not edit)
├── test-corpus.el           # Test data with expected results
├── bash-parser-test.el      # ERT test suite
├── debug-ast.el             # AST debugging utilities
└── README.md                # This file
```

## Key Learnings

### 1. Tree-sitter Node Types Are Strings

**Critical discovery:** `treesit-node-type` returns **strings**, not symbols!

```elisp
;; WRONG - compares against symbol
(eq (treesit-node-type node) 'command)  ; Always nil!

;; CORRECT - compares against string
(string= (treesit-node-type node) "command")  ; Works!
```

### 2. Bash AST Structure

Tree-sitter bash grammar produces this structure:

```
program (root)
  └── command
      ├── command_name
      │   └── word: "git"
      ├── word: "log"          ← subcommand
      ├── word: "--oneline"    ← flag
      └── number: "-10"        ← number nodes for numeric flags!
```

**Important:** Numeric flags like `-10` are `number` nodes, not `word` nodes!

### 3. Node Types We Handle

The parser extracts text from these node types:
- `word` - Most arguments and commands
- `string` - Quoted strings (we strip quotes)
- `raw_string` - Raw strings
- `concatenation` - String concatenations
- `number` - Numeric arguments (like `-10`)

### 4. What Works

**Simple commands:**
```bash
ls -la /tmp
# command-name: "ls"
# flags: ("-la")
# positional-args: ("/tmp")
```

**Git subcommands:**
```bash
git log --oneline -10
# command-name: "git"
# subcommand: "log"
# flags: ("--oneline" "-10")
```

**Quoted arguments:**
```bash
git commit -m 'test message'
# flags: ("-m")
# positional-args: ("test message")  # quotes stripped
```

**Dangerous pattern detection:**
```bash
rm -rf /tmp/test
# dangerous-p: t

git push --force
# dangerous-p: t
```

## Running Tests

```bash
# Run all tests
./bin/emacs-isolated.sh --batch \
  -l config/experiments/bash-parser/bash-parser.el \
  -l config/experiments/bash-parser/test-corpus.el \
  -l config/experiments/bash-parser/bash-parser-test.el \
  -f ert-run-tests-batch-and-exit

# Interactive test runner (from Emacs)
M-x load-file RET config/experiments/bash-parser/bash-parser-test.el RET
M-x jf/bash-parser-run-all-tests

# Test a specific command interactively
M-x jf/bash-parser-test-command RET git log --oneline RET
```

## Usage Examples

```elisp
;; Parse a command
(jf/bash-parse "git log --oneline -10")
; => (:success t
;     :command-name "git"
;     :subcommand "log"
;     :flags ("--oneline" "-10")
;     :positional-args nil
;     :dangerous-p nil
;     :ast #<treesit-node>)

;; Check if a command is dangerous
(jf/bash-parse "rm -rf /tmp/test")
; => (:success t
;     :command-name "rm"
;     :subcommand nil
;     :flags ("-rf")
;     :positional-args ("/tmp/test")
;     :dangerous-p t
;     :ast #<treesit-node>)

;; Handle parse errors gracefully
(jf/bash-parse "invalid ||| syntax")
; => (:success nil :error "...")
```

## Dangerous Pattern Database

Located in `jf/bash-parser-dangerous-patterns`. Currently detects:

- **rm**: `-rf`, `-r`, `-f` flags
- **git**:
  - `push --force`, `push -f`
  - `reset --hard`
  - `clean -f`, `clean -fd`
- **docker**: `rm -f`, `rm --force`
- **python/bash/sh**: `-c` flag (arbitrary code execution)

Pattern format:
```elisp
(command . ((:subcommand "name" :flags (list))
            (:flags (list))
            (:any-flag-contains (list))))
```

## What We Haven't Tackled Yet

These were in our exploration but not implemented yet:

1. **Pipelines**: `ls | grep foo` - Multiple commands
2. **Command chains**: `git add . && git commit -m 'test'`
3. **Redirections**: `echo foo > file.txt`
4. **Command substitution**: `echo $(date)`
5. **Path target validation**: Check if paths are within allowed scope
6. **Script execution detection**: Special handling for `python script.py`

## Next Steps

Based on what we've learned, here are potential directions:

### Easy Additions
- Add more commands to dangerous patterns database
- Support more node types (as we discover them)
- Better error messages for parse failures

### Medium Complexity
- **Pipeline support**: Detect and parse multi-command pipelines
- **Redirection detection**: Extract file targets from redirections
- **Path target extraction**: Identify which arguments are file/directory paths

### Hard Problems
- **Command substitution**: Recursively parse `$(...)` and backticks
- **Scope validation integration**: Check paths against glob patterns
- **Shell variable expansion**: Handle `$VAR` references
- **Heredocs**: Parse `<< EOF` blocks

## Performance

Current implementation is fast enough for interactive use:
- Average parse time: ~0.002 seconds per command
- No noticeable latency for command validation

## Maintenance

**Always edit the `.org` file**, not the `.el` file!

```bash
# After editing bash-parser.org:
./bin/tangle-org.sh config/experiments/bash-parser/bash-parser.org

# This generates bash-parser.el and validates it
```

## Integration Points

This module is intentionally standalone and generic. Potential consumers:

1. **gptel bash tools** - Validate AI-generated commands before execution
2. **Shell script analyzer** - Detect dangerous patterns in scripts
3. **Command builder UI** - Help construct safe commands interactively
4. **Documentation generator** - Extract command examples from code

## References

- Tree-sitter bash grammar: https://github.com/tree-sitter/tree-sitter-bash
- Emacs treesit API: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html
- Research: `openspec/research/bash-command-sandboxing-2026.md`
