# Pipeline Parsing Root Cause Analysis

**Date:** 2026-03-01
**Focus:** Why pipelines only show the first command

## TL;DR - Root Cause

The parser uses `jf/bash-parse--find-first-command` which performs a depth-first search for the first "command" node and **immediately returns** when found. For pipelines like `ls -la | grep test`, tree-sitter produces a "pipeline" node containing multiple command nodes, but the parser returns after finding only the first one.

## Tree-sitter Bash Grammar Structure

### Grammar Definition (from grammar.js)

```javascript
pipeline: $ => prec.right(seq(
  $._statement_not_pipeline,      // First command
  repeat1(seq(
    choice('|', '|&'),             // Pipe operator
    $._statement_not_pipeline,     // Subsequent commands
  )),
)),
```

The grammar creates a "pipeline" node containing:
1. First statement (command)
2. One or more pipe operators (`|` or `|&`)
3. Additional statements (commands)

### Node Structure (from node-types.json)

```json
{
  "type": "pipeline",
  "named": true,
  "fields": {},
  "children": {
    "multiple": true,    // ← KEY: Multiple children allowed!
    "required": true,
    "types": [
      {
        "type": "_statement",
        "named": true
      }
    ]
  }
}
```

**Critical insight:** The "pipeline" node has `"multiple": true` for children, meaning it can contain multiple `_statement` nodes.

## AST Structure Example

For the command `ls -la | grep test`, tree-sitter produces:

```
program
  └── pipeline
      ├── _statement_not_pipeline [child 0]
      │   └── command
      │       ├── command_name
      │       │   └── word: "ls"
      │       └── argument
      │           └── word: "-la"
      │
      ├── | [child 1] (pipe operator)
      │
      └── _statement_not_pipeline [child 2]
          └── command
              ├── command_name
              │   └── word: "grep"
              └── argument
                  └── word: "test"
```

## Current Parser Behavior

### The Problematic Function (bash-parser.el:70-89)

```elisp
(defun jf/bash-parse--find-first-command (node)
  "Find the first command node in tree starting from NODE."
  (if (null node)
      nil
    (let ((node-type (treesit-node-type node)))
      (cond
       ;; If this node is a command, return it
       ((string= node-type "command")
        node)                              ; ← RETURNS IMMEDIATELY!

       ;; Otherwise search children recursively
       (t
        (let ((child-count (treesit-node-child-count node))
              (result nil))
          (dotimes (i child-count)
            (when (null result)            ; ← Stops after first result
              (let ((child (treesit-node-child node i)))
                (when child
                  (setq result (jf/bash-parse--find-first-command child))))))
          result))))))
```

### Execution Trace for `ls -la | grep test`

1. **Start:** `jf/bash-parse--find-first-command` called with root "program" node
2. **Step 1:** Node type is "program" (not "command"), so search children
3. **Step 2:** First child is "pipeline" node
4. **Step 3:** Node type is "pipeline" (not "command"), so search children
5. **Step 4:** First child of pipeline is "_statement_not_pipeline"
6. **Step 5:** Node type is "_statement_not_pipeline" (not "command"), search children
7. **Step 6:** First child is "command" node
8. **Step 7:** Node type is "command" → **MATCH! Return immediately**
9. **Never reached:** Second _statement_not_pipeline child (containing `grep test`)

### Why It Happens

The function has two early-exit mechanisms:

1. **Immediate return on first match:**
   ```elisp
   ((string= node-type "command")
    node)  ; Returns immediately, never checks siblings
   ```

2. **Single result variable:**
   ```elisp
   (when (null result)  ; Only assigns once
     (setq result ...))
   ```

Both prevent finding subsequent commands in the pipeline.

## Verification with Debug Output

Using the `debug-pipelines.el` script (included in this directory):

```elisp
(jf/bash-explore-pipelines)
```

Expected output for `ls -la | grep test`:

```
[program] (1 children) text="ls -la | grep test"
  [pipeline] (2 children) text="ls -la | grep test"
    [_statement_not_pipeline] (1 children) text="ls -la"
      [command] (2 children) text="ls -la"
        [command_name] (1 children) text="ls"
          [word] (0 children) text="ls"
        [word] (0 children) text="-la"
    [|] (0 children) text="|"
    [_statement_not_pipeline] (1 children) text="grep test"
      [command] (2 children) text="grep test"
        [command_name] (1 children) text="grep"
          [word] (0 children) text="grep"
        [word] (0 children) text="test"
```

**Notice:** Both command nodes exist in the tree, but only the first is extracted!

## Why Command Chains Have the Same Problem

The same issue affects command chains (`&&`, `||`, `;`). The grammar defines:

```javascript
// List (semicolon-separated commands)
list: $ => prec.left(-1, seq(
  $._statement,
  repeat1(seq(
    choice(';', '&', ';;'),
    optional($._statement),
  )),
)),

// Compound lists (AND/OR chains)
// Similar structure with && and || operators
```

For `git add . && git commit -m 'test'`:

```
program
  └── list
      ├── _statement [git add .]
      ├── && operator
      └── _statement [git commit -m 'test']
```

Same problem: first command found, second ignored.

## Security Implications

This behavior creates **dangerous blind spots**:

```bash
# Appears safe at first glance
echo "hello" | rm -rf /tmp/dangerous

# Parser sees:
#   command: "echo"
#   args: ["hello"]
#   dangerous-p: nil  ← WRONG!

# Parser MISSES:
#   Second command: rm -rf /tmp/dangerous
#   dangerous-p should be: t
```

**Attack vector:** An adversary could hide dangerous commands in later pipeline stages:

```bash
ls /safe/directory | git push --force
# Parser approves (sees only "ls")
# Actually executes force push!
```

## Solution Options

### Option 1: Detect and Reject Pipelines (Recommended for Security)

**Approach:** Pre-flight check before parsing

```elisp
(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING, rejecting complex multi-command constructs."
  (condition-case err
      (progn
        ;; Pre-flight: reject pipelines and chains
        (when (string-match-p "\\(|\\|&&\\|||\\|;\\)" command-string)
          (list :success nil
                :error "Multi-command constructs not supported"
                :reason "Contains pipeline or chain operators"))

        ;; Continue with normal parsing
        (jf/bash-parse--internal command-string))
    (error (list :success nil
                 :error (error-message-string err)))))
```

**Pros:**
- Simple to implement
- Explicit about limitations
- Secure (prevents evasion)
- Matches FINDINGS.md recommendations

**Cons:**
- Rejects legitimate use cases
- Doesn't extract what we *can* parse

### Option 2: Extract All Commands (Breaking Change)

**Approach:** Return list of commands instead of single command

```elisp
(defun jf/bash-parse--find-all-commands (node)
  "Find ALL command nodes in tree starting from NODE."
  (let ((commands '()))
    (jf/bash-parse--visit-node
     node
     (lambda (n)
       (when (string= (treesit-node-type n) "command")
         (push n commands))))
    (nreverse commands)))

(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING returning all commands found."
  (let* ((root-node ...)
         (all-commands (jf/bash-parse--find-all-commands root-node))
         (parsed-commands (mapcar #'jf/bash-parse--parse-single-command
                                   all-commands)))
    (list :success t
          :commands parsed-commands        ; ← List, not single command
          :pipeline-detected (> (length all-commands) 1))))
```

**Pros:**
- Complete solution
- Handles all pipeline stages
- Enables full validation

**Cons:**
- Breaking API change (`:command-name` → `:commands`)
- More complex to use
- May still miss dangerous interactions between commands

### Option 3: Return First + Warning (Pragmatic)

**Approach:** Current behavior + metadata about what's missing

```elisp
(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING, warning if only first command extracted."
  (let* ((root-node ...)
         (all-commands (jf/bash-parse--find-all-commands root-node))
         (first-command (car all-commands))
         (result (jf/bash-parse--parse-single-command first-command)))
    (append result
            (list :total-commands-found (length all-commands)
                  :pipeline-or-chain-detected (> (length all-commands) 1)
                  :security-warning (when (> (length all-commands) 1)
                                      "Only first command validated!")))))
```

**Pros:**
- Backward compatible
- Warns users about limitations
- Enables informed decision-making

**Cons:**
- Doesn't solve the security problem
- Users might ignore warnings
- Still incomplete parsing

### Option 4: Pipeline-Aware Parsing (Comprehensive)

**Approach:** Explicitly handle pipeline nodes differently

```elisp
(defun jf/bash-parse--internal (command-string)
  "Internal parser with pipeline awareness."
  (let* ((root-node ...)
         (pipeline-node (jf/bash-parse--find-node-by-type root-node "pipeline")))

    (if pipeline-node
        ;; Handle pipeline specially
        (let* ((statements (jf/bash-parse--get-pipeline-statements pipeline-node))
               (commands (mapcar #'jf/bash-parse--parse-statement statements)))
          (list :success t
                :type :pipeline
                :commands commands
                :dangerous-p (seq-some (lambda (cmd)
                                        (plist-get cmd :dangerous-p))
                                      commands)))

      ;; Handle single command normally
      (jf/bash-parse--parse-single-command
       (jf/bash-parse--find-first-command root-node)))))
```

**Pros:**
- Correctly models bash semantics
- Complete security validation
- Clean API distinction (`:type :pipeline` vs `:type :simple`)

**Cons:**
- Most complex implementation
- Requires understanding pipeline semantics
- Caller needs to handle different result shapes

## Recommended Path Forward

Based on the original use case (gptel bash tools validation), I recommend:

### Phase 1: Reject Multi-Command Constructs (Option 1)

Implement pre-flight validation rejecting any command with:
- Pipeline operators: `|`, `|&`
- Chain operators: `&&`, `||`, `;`
- Command substitution: `$(...)`, `` `...` ``

This aligns with FINDINGS.md recommendations (lines 193-202):
> 1. **Pre-flight check**: Reject commands containing:
>    - Pipeline operators (`|`)
>    - Chain operators (`&&`, `||`, `;`)
>    - Command substitution (`$()`, backticks)

### Phase 2 (Optional): Pipeline-Aware Parsing (Option 4)

If full pipeline support is needed later, implement Option 4 with:
- Explicit pipeline detection
- All commands extracted and validated
- Clear API contract for different command types

## Testing Strategy

To verify the fix, add these test cases:

```elisp
;; Should reject
(:id "reject-pipeline-001"
 :command "ls -la | grep test"
 :expect (:success nil
          :error "Multi-command constructs not supported"))

(:id "reject-chain-001"
 :command "git add . && git commit -m 'test'"
 :expect (:success nil
          :error "Multi-command constructs not supported"))

;; Security: dangerous command hidden in pipeline
(:id "security-pipeline-001"
 :command "echo safe | rm -rf /tmp/test"
 :expect (:success nil))  ; Must reject, not approve!

;; Should still accept single commands
(:id "accept-simple-001"
 :command "git log --oneline"
 :expect (:success t
          :command-name "git"
          :subcommand "log"))
```

## References

- Tree-sitter Bash Grammar: https://github.com/tree-sitter/tree-sitter-bash
- Node Types Documentation: https://tree-sitter.github.io/tree-sitter/using-parsers/6-static-node-types.html
- Grammar Source: https://github.com/tree-sitter/tree-sitter-bash/blob/master/grammar.js
- Node Types JSON: https://github.com/tree-sitter/tree-sitter-bash/blob/master/src/node-types.json

## Appendix: Complete Node Type Hierarchy

```
program
  ├── pipeline (multiple _statement children)
  ├── list (multiple _statement children with ; separator)
  ├── _statement
  │   ├── command
  │   ├── if_statement
  │   ├── while_statement
  │   ├── for_statement
  │   └── ...
  └── command
      ├── command_name
      ├── argument (multiple)
      └── redirect (optional)
```

The key insight: `pipeline` and `list` are **container nodes** with multiple `_statement` children, each potentially containing a `command` node. The current parser doesn't recognize these containers and only extracts the first command it encounters.
