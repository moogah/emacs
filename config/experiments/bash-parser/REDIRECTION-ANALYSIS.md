# Deep Dive: Bash Redirection Parsing

## Problem Statement

The current parser strips redirection operators and targets:

```bash
echo 'hello' > output.txt
# Sees: command="echo", args=["hello"]
# Missing: "> output.txt" completely ignored
```

## Root Cause Analysis

### 1. Tree-sitter Grammar Structure

According to the [tree-sitter-bash grammar](https://github.com/tree-sitter/tree-sitter-bash), redirections are represented as separate nodes, not as part of the command words:

**Relevant Node Types:**

- **`redirected_statement`** - Wrapper node when a statement has redirections
  - Fields: `body` (the actual command), `redirect` (the redirection nodes)

- **`file_redirect`** - A single file redirection
  - Fields:
    - `descriptor` (optional) - file descriptor like `2` in `2>&1`
    - `destination` (optional) - target file path
  - Operators: `<`, `>`, `>>`, `&>`, `&>>`, `<&`, `>&`, `>|`, `<&-`, `>&-`

- **`herestring_redirect`** - Here-string syntax `<<<`
  - Fields: `descriptor` (optional)

- **`heredoc_redirect`** - Multi-line here-document
  - Fields: `descriptor`, `operator`, and heredoc body

**Grammar Rule for `file_redirect`:**

```javascript
file_redirect: $ => prec.left(seq(
  field('descriptor', optional($.file_descriptor)),
  choice(
    seq(
      choice('<', '>', '>>', '&>', '&>>', '<&', '>&', '>|'),
      field('destination', repeat1($._literal)),
    ),
    seq(
      choice('<&-', '>&-'),
      optional(field('destination', $._literal)),
    ),
  ),
))
```

### 2. AST Structure for Redirection Commands

For `echo 'hello' > output.txt`, tree-sitter produces:

```
program
  redirected_statement
    body: command
      command_name: word "echo"
      string "'hello'"
    redirect: file_redirect
      operator: ">"
      destination: word "output.txt"
```

The redirection is **not a child of the command node** - it's a sibling in the `redirected_statement`.

### 3. Current Parser Behavior

**Location of the bug:** `jf/bash-parse--extract-words` in `bash-parser.org:242-271`

```elisp
(defun jf/bash-parse--extract-words (command-node)
  "Extract all word nodes from COMMAND-NODE as strings."
  (let ((words '()))
    (jf/bash-parse--visit-node
     command-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (when (or (string= node-type "word")
                   (string= node-type "string")
                   (string= node-type "raw_string")
                   (string= node-type "concatenation")
                   (string= node-type "number"))
           ;; Extract text...
           ))))
    (nreverse words)))
```

**Problems:**

1. **Only searches within `command-node`** - doesn't look at parent `redirected_statement`
2. **Only extracts word-like nodes** - ignores `file_redirect`, `herestring_redirect`, etc.
3. **No operator extraction** - redirection operators like `>`, `>>` are never captured

The current architecture assumes all relevant information is in the `command` node's word children. This is true for simple commands, but **redirections live outside the command node**.

### 4. Why Redirections Are Invisible

The parser flow:

1. `jf/bash-parse--handle-simple-command` finds the first `command` node
2. Passes it to `jf/bash-parse--parse-single-command-node`
3. Which calls `jf/bash-parse--extract-words` with **only the command node**
4. Extract-words only visits children of command node
5. **Redirect nodes are siblings, not children** - never visited

## Solution Design

### Option 1: Extract Redirections at Statement Level (Recommended)

**Approach:** Check for `redirected_statement` wrapper and extract both command and redirections.

**Changes needed:**

```elisp
;; New function to detect redirected statements
(defun jf/bash-parse--is-redirected-statement (node)
  "Check if NODE is a redirected_statement."
  (string= (treesit-node-type node) "redirected_statement"))

;; Extract redirections from statement node
(defun jf/bash-parse--extract-redirections (statement-node)
  "Extract all redirections from STATEMENT-NODE.
Returns list of plists with :operator, :descriptor, :destination."
  (let ((redirects '()))
    (jf/bash-parse--visit-node
     statement-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (cond
          ;; file_redirect: >, >>, <, 2>&1, etc.
          ((string= node-type "file_redirect")
           (push (jf/bash-parse--parse-file-redirect node) redirects))

          ;; herestring_redirect: <<<
          ((string= node-type "herestring_redirect")
           (push (jf/bash-parse--parse-herestring-redirect node) redirects))

          ;; heredoc_redirect: <<, <<-
          ((string= node-type "heredoc_redirect")
           (push (jf/bash-parse--parse-heredoc-redirect node) redirects))))))
    (nreverse redirects)))

;; Parse individual file_redirect node
(defun jf/bash-parse--parse-file-redirect (redirect-node)
  "Parse a file_redirect NODE into plist.
Returns: (:type :file :operator \">\", :descriptor nil :destination \"output.txt\")"
  (let ((descriptor nil)
        (operator nil)
        (destination nil))

    ;; Extract descriptor (optional field)
    (when-let ((desc-node (treesit-node-child-by-field-name redirect-node "descriptor")))
      (setq descriptor (treesit-node-text desc-node t)))

    ;; Extract destination (optional field)
    (when-let ((dest-node (treesit-node-child-by-field-name redirect-node "destination")))
      (setq destination (treesit-node-text dest-node t)))

    ;; Extract operator by finding operator child
    ;; Operator is typically the first non-field child
    (jf/bash-parse--visit-node
     redirect-node
     (lambda (node)
       (let ((text (treesit-node-text node t)))
         (when (and (null operator)
                   (member text '(">" ">>" "<" "&>" "&>>"
                                 "<&" ">&" ">|" "<&-" ">&-"
                                 "2>&1" "2>")))
           (setq operator text)))))

    (list :type :file
          :operator operator
          :descriptor descriptor
          :destination destination)))

;; Modify parse-single-command-node to check for redirected_statement wrapper
(defun jf/bash-parse--parse-single-command-node (command-or-statement-node)
  "Parse COMMAND-OR-STATEMENT-NODE which may be a command or redirected_statement."
  (let ((command-node nil)
        (redirections nil))

    ;; Check if this is a redirected_statement
    (if (string= (treesit-node-type command-or-statement-node) "redirected_statement")
        (progn
          ;; Extract the actual command from the body field
          (setq command-node
                (treesit-node-child-by-field-name command-or-statement-node "body"))
          ;; Extract redirections
          (setq redirections
                (jf/bash-parse--extract-redirections command-or-statement-node)))
      ;; Not a redirected statement, use node as-is
      (setq command-node command-or-statement-node))

    ;; Parse command as before
    (let* ((words (jf/bash-parse--extract-words command-node))
           (command-name (car words))
           (remaining-words (cdr words))
           (subcommand (jf/bash-parse--detect-subcommand command-name remaining-words))
           (args-start (if subcommand (cdr remaining-words) remaining-words))
           (flags (jf/bash-parse--extract-flags args-start))
           (positional-args (jf/bash-parse--extract-positional-args args-start))
           (dangerous-p (jf/bash-parse--is-dangerous command-name subcommand flags)))

      ;; Return with redirections added
      (list :command-name command-name
            :subcommand subcommand
            :flags flags
            :positional-args positional-args
            :dangerous-p dangerous-p
            :redirections redirections))))
```

**Result format:**

```elisp
(:success t
 :type :simple
 :command-name "echo"
 :flags ()
 :positional-args ("hello")
 :dangerous-p nil
 :redirections ((:type :file
                 :operator ">"
                 :descriptor nil
                 :destination "output.txt")))
```

### Option 2: Extract Redirections as Separate Pass

**Approach:** Keep command parsing unchanged, add separate redirection extraction pass.

**Pros:**
- Less invasive
- Backward compatible

**Cons:**
- Requires searching AST twice
- More complex logic to associate redirections with commands in pipelines

### Option 3: Add Redirection Info to Command Node Search

**Approach:** When finding command nodes, also search for sibling redirect nodes.

**Cons:**
- Violates AST structure (redirects aren't siblings of commands in all cases)
- Won't work for complex cases like `if` statements with redirections

## Implementation Complexity

### Node Field Access in Tree-sitter

Tree-sitter provides field access via `treesit-node-child-by-field-name`:

```elisp
;; Get the body field from redirected_statement
(treesit-node-child-by-field-name statement-node "body")

;; Get the descriptor field from file_redirect
(treesit-node-child-by-field-name redirect-node "descriptor")

;; Get the destination field from file_redirect
(treesit-node-child-by-field-name redirect-node "destination")
```

### Operator Extraction Challenge

The operator itself (`>`, `>>`, etc.) is **not a named field** - it's an anonymous node in the grammar.

**Solution:** Visit all children and match against known operator strings:

```elisp
(defun jf/bash-parse--extract-operator (redirect-node)
  "Extract redirection operator from REDIRECT-NODE."
  (let ((operator nil))
    (dotimes (i (treesit-node-child-count redirect-node))
      (when-let ((child (treesit-node-child redirect-node i)))
        (let ((text (treesit-node-text child t)))
          (when (member text '(">" ">>" "<" "&>" "&>>"
                              "<&" ">&" ">|" "<&-" ">&-"))
            (setq operator text)))))
    operator))
```

**Alternative:** Pattern match the full redirect-node text:

```elisp
(let ((full-text (treesit-node-text redirect-node t)))
  (when (string-match "^\\([0-9]*\\)\\([<>|&-]+\\)" full-text)
    (list :descriptor (match-string 1 full-text)
          :operator (match-string 2 full-text))))
```

## Test Cases

### Simple Output Redirection

```bash
echo 'hello' > output.txt
```

**Expected:**

```elisp
(:redirections ((:type :file :operator ">" :descriptor nil :destination "output.txt")))
```

### Append Redirection

```bash
cat input.txt >> output.txt
```

**Expected:**

```elisp
(:redirections ((:type :file :operator ">>" :descriptor nil :destination "output.txt")))
```

### Input Redirection

```bash
grep error < logfile.txt
```

**Expected:**

```elisp
(:redirections ((:type :file :operator "<" :descriptor nil :destination "logfile.txt")))
```

### Stderr Redirection

```bash
command 2>&1
```

**Expected:**

```elisp
(:redirections ((:type :file :operator ">&" :descriptor "2" :destination "1")))
```

### Multiple Redirections

```bash
git log > /dev/null 2>&1
```

**Expected:**

```elisp
(:redirections ((:type :file :operator ">" :descriptor nil :destination "/dev/null")
                (:type :file :operator ">&" :descriptor "2" :destination "1")))
```

### Separate Stderr Redirection

```bash
echo 'test' 2> error.log
```

**Expected:**

```elisp
(:redirections ((:type :file :operator ">" :descriptor "2" :destination "error.log")))
```

## Implementation Priority

### Phase 1: Basic File Redirect Support
- Detect `redirected_statement` nodes
- Extract `file_redirect` nodes
- Parse operator and destination
- Add to result as `:redirections` field
- Test with simple `>`, `>>`, `<` cases

### Phase 2: File Descriptor Support
- Extract `descriptor` field
- Handle `2>`, `2>&1`, etc.
- Test with stderr/stdout redirections

### Phase 3: Advanced Redirections
- Implement `herestring_redirect` (`<<<`)
- Implement `heredoc_redirect` (`<<`, `<<-`)
- Handle edge cases (`>&-`, `<&-`)

### Phase 4: Pipeline Integration
- Ensure redirections work with pipelines
- Handle per-command redirections in chains
- Test: `cmd1 > file.txt | cmd2`

## Security Implications

### Current State
Redirections are invisible to the parser, but this is **not a major security issue** for the current use case (single-command validation) because:
- Dangerous flags are still detected correctly
- Command and subcommand analysis works
- The *command itself* is validated, even if output is redirected

### Post-Implementation
With redirection support:
- **File target validation** becomes possible
- Can detect suspicious redirection targets (e.g., `/etc/passwd`, system files)
- Can warn on destructive patterns: `> /important/file` with `rm -rf`
- Better understanding of command intent

## Backward Compatibility

### Proposed Approach
Add `:redirections` field to result:

```elisp
;; Simple command (no redirections)
(:success t :command-name "ls" :redirections nil)

;; Command with redirections
(:success t :command-name "echo" :redirections ((...)))
```

**Impact:** Backward compatible - existing code ignores `:redirections` field.

## Recommendations

1. **Implement Option 1** (statement-level extraction) - cleanest architecture
2. **Start with Phase 1** - basic file redirects only
3. **Add comprehensive tests** - all operator types, multiple redirections
4. **Update FINDINGS.md** - change from ⚠️ Limitation to ✅ Supported
5. **Consider security rules** - add dangerous patterns for file targets

## References

- [tree-sitter-bash Grammar](https://github.com/tree-sitter/tree-sitter-bash)
- [tree-sitter-bash node-types.json](https://github.com/tree-sitter/tree-sitter-bash/blob/master/src/node-types.json)
- [Tree-sitter Grammar DSL](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)
