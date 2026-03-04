# Bash Command Parser

A standalone bash command parser using tree-sitter for semantic analysis with recursive file operations extraction.

## What We Built

A literate programming module that parses bash command strings and extracts:

- **Command names**: Base executable (e.g., `git`, `ls`, `python`)
- **Subcommands**: Secondary command for multi-command tools (e.g., `log` in `git log`)
- **Flags**: Arguments starting with `-` or `--`
- **Positional arguments**: Non-flag arguments
- **File operations**: Recursive extraction from all nesting levels
- **Dangerous pattern detection**: Matches commands against a database of risky operations

## Features

### Recursive Analysis
Extracts file operations from nested command substitutions, loops, and conditionals:
- **Command substitutions**: `cat $(find . -name '*.log')`
- **For-loops**: `for f in *.txt; do rm $f; done`
- **Conditionals**: `if [ -f file.txt ]; then cat file.txt; fi`
- **Nested execution**: `bash -c "cat file.txt"`

### Pattern Flow Tracking
Links pattern producers (find, ls) to pattern consumers (cat, rm):
```bash
cat $(find . -name '*.log')
# => file operations include:
#    - find reads directory "."
#    - find matches pattern "*.log"
#    - cat reads pattern "*.log" (linked to find)
```

### Loop Context
Resolves loop variables to understand batch operations:
```bash
for f in *.txt; do rm $f; done
# => rm operates on "*.txt" pattern (loop variable resolved)
```

### Conditional Context
Marks operations in conditional branches and test expressions:
```bash
if [ -f file.txt ]; then cat file.txt; fi
# => both test metadata read and conditional read marked
```

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

### Basic Parsing

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
```

### File Operations Extraction

```elisp
;; Simple command
(jf/bash-extract-file-operations (jf/bash-parse "cat file.txt"))
; => ((:file "file.txt" :operation :read :confidence :high
;      :source :positional-arg :command "cat"))

;; Nested substitution with pattern flow
(jf/bash-extract-file-operations (jf/bash-parse "cat $(find . -name '*.log')"))
; => ((:file "." :operation :read-directory :from-substitution t :command "find")
;     (:file "*.log" :operation :match-pattern :pattern t :from-substitution t :command "find")
;     (:file "*.log" :operation :read :pattern t :pattern-source (:command "find" ...) :command "cat"))

;; Loop with variable resolution
(jf/bash-extract-file-operations (jf/bash-parse "for f in *.txt; do rm $f; done"))
; => ((:file "*.txt" :operation :delete :loop-context t :pattern t :command "rm"))

;; Conditional with test
(jf/bash-extract-file-operations (jf/bash-parse "if [ -f file.txt ]; then cat file.txt; fi"))
; => ((:file "file.txt" :operation :read-metadata :test-condition t ...)
;     (:file "file.txt" :operation :read :conditional t :branch :then ...))
```

### Feature Detection

```elisp
;; Check if recursive analysis is available
(jf/bash-parser-has-feature-p :recursive-analysis)
; => t

;; Check if pattern flow tracking is available
(jf/bash-parser-has-feature-p :pattern-flow)
; => t

;; Check for unknown feature
(jf/bash-parser-has-feature-p :nonexistent)
; => nil
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

## Operation Types

The parser extracts these operation types:

- **:read** - Read file contents
- **:write** - Create or overwrite file
- **:delete** - Remove file
- **:modify** - Modify file in-place
- **:append** - Append to file
- **:create-or-modify** - Create new file or update timestamp
- **:match-pattern** - Search for files matching pattern
- **:read-directory** - Read directory to find files
- **:read-metadata** - Read file metadata (test operators)
- **:execute** - Execute script file

## Context Flags

Operations include optional context flags:

- **:from-substitution** - Operation from command substitution
- **:loop-context** - Operation in loop body
- **:loop-variable** - Loop variable name if in loop
- **:conditional** - Operation in conditional branch
- **:branch** - :then or :else if in conditional
- **:test-condition** - Operation in test expression
- **:pattern** - File path is a glob pattern
- **:pattern-source** - Link to pattern producer command
- **:indirect** - From nested/indirect execution

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
