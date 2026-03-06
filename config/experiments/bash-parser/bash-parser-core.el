;;; bash-parser-core.el --- Core bash parsing functions -*- lexical-binding: t; -*-

(require 'treesit)
(require 'bash-parser-security)

(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING using tree-sitter with full pipeline/chain support.

COMMAND-STRING must be a non-empty string. Signals wrong-type-argument if
COMMAND-STRING is not a string. Returns error plist if string is empty.

Returns plist with:
  :success - t if parsing succeeded
  :type - :simple, :pipeline, or :chain
  :all-commands - list of all parsed commands (always present)
  :command-count - number of commands found

For simple commands, also includes (for backward compatibility):
  :command-name - base command
  :subcommand - subcommand if detected
  :flags - list of flags
  :positional-args - list of non-flag arguments
  :args - original argument list (preserves order, includes flags)
  :dangerous-p - t if command matches dangerous patterns

For pipelines/chains:
  Each element in :all-commands has the same structure as simple commands

Additional fields:
  :ast - tree-sitter root node (for debugging)
         WARNING: AST nodes are only valid during parsing. Do not use
         this field after jf/bash-parse returns, as the underlying
         buffer is destroyed. The AST is provided for debugging only.
         All necessary information is extracted into other fields.
  :error - error message if parsing failed

Recursion depth is limited by jf/bash--max-parse-depth to prevent
infinite loops in pathological cases.

KNOWN LIMITATIONS:
  Commands starting with (( are normalized to ( ( to work around a
  tree-sitter-bash parser bug. This may cause position offsets to be
  off by 1 in error reporting but does not affect functionality.
  See Known Limitations section in module documentation."
  ;; Validate argument type
  (unless (stringp command-string)
    (signal 'wrong-type-argument
            (list 'stringp command-string (type-of command-string))))
  ;; Validate argument is not empty
  (if (string-empty-p command-string)
      (list :success nil
            :error "Empty command string"
            :type :empty)
    ;; Proceed with normal parsing
    (jf/bash-parse--with-depth command-string 0)))

(defun jf/bash-parse--with-depth (command-string depth)
  "Internal parser with explicit DEPTH parameter for recursion control.
Wraps parsing with depth checking and error handling."
  (when (> depth jf/bash--max-parse-depth)
    (error "Max parse depth exceeded: possible recursion cycle"))
  (condition-case err
      (jf/bash-parse--internal command-string depth)
    (error (list :success nil
                 :error (error-message-string err)))))

(defun jf/bash-parse--internal (command-string depth)
  "Internal parser implementation for COMMAND-STRING with pipeline support.
DEPTH parameter tracks current recursion depth."
  ;; Parse in temp buffer. Note: AST nodes returned in :ast field
  ;; become invalid after this function returns. All needed data
  ;; is extracted into the result plist.
  (condition-case err
      (with-temp-buffer
        ;; WORKAROUND: Fix tree-sitter-bash bug with (( at command start
        ;; Bug: tree-sitter-bash incorrectly parses (( as arithmetic expansion
        ;;      instead of nested subshells ( (...)
        ;; Fix: Normalize "((cmd))" → "( (cmd))" by inserting space
        ;; Impact: Position offsets in errors may be off by 1
        ;; TODO: Remove when upstream fixes (( parsing
        ;;       See: Known Limitations section in module introduction
        (let ((normalized-string
               (if (string-prefix-p "((" command-string)
                   (concat "( " (substring command-string 1))
                 command-string)))
          (insert normalized-string))
        (let* ((parser (treesit-parser-create 'bash))
               (root-node (treesit-parser-root-node parser))
               (struct-type (jf/bash-parse--detect-structure-type root-node)))

          (pcase struct-type
            (:pipeline
             (jf/bash-parse--handle-pipeline root-node depth))
            (:list
             (jf/bash-parse--handle-list root-node depth))
            (:conditional
             (jf/bash-parse--handle-conditional root-node))
            (:for-loop
             (jf/bash-parse--handle-for-loop root-node))
            (:subshell
             (jf/bash-parse--handle-subshell root-node depth))
            (:simple
             (jf/bash-parse--handle-simple-command root-node depth))
            (_
             (list :success nil
                   :error "Unknown command structure")))))
    (error (list :success nil
                 :error (format "Parse error: %s" (error-message-string err))))))

(defun jf/bash-parse--detect-structure-type (root-node)
  "Detect the bash command structure type from ROOT-NODE tree-sitter parse tree.

Analyzes the tree-sitter parse tree to determine which type of bash structure
is present, enabling the parser to route to the appropriate handler function.

ROOT-NODE is the root node of the tree-sitter parse tree.

Returns a keyword indicating the structure type:
  :pipeline    - Commands connected with pipes (|)
  :list        - Commands connected with operators (&&, ||, ;)
  :conditional - If/then/else statements
  :for-loop    - For loop statements
  :subshell    - Subshell expression (command)
  :simple      - Single command with no special structure

Detection strategy:
  - Only checks direct children to avoid detecting nested structures
  - Prevents false positives from command substitutions like $(pipeline)
  - Checks in priority order: pipeline, list, conditionals, loops, subshells
  - Handles special case of redirected statements wrapping complex structures

Examples:
  (jf/bash-parse--detect-structure-type root-node-for-\"ls | grep foo\")
    => :pipeline

  (jf/bash-parse--detect-structure-type root-node-for-\"cd /tmp && ls\")
    => :list

  (jf/bash-parse--detect-structure-type root-node-for-\"if [ -f file ]; then cat file; fi\")
    => :conditional

  (jf/bash-parse--detect-structure-type root-node-for-\"ls file.txt\")
    => :simple

Internal helper for jf/bash-parse--internal."
  (let ((pipeline-node (jf/bash-parse--find-direct-child-by-type root-node "pipeline"))
        (list-node (jf/bash-parse--find-direct-child-by-type root-node "list"))
        (if-node (jf/bash-parse--find-direct-child-by-type root-node "if_statement"))
        (for-node (jf/bash-parse--find-direct-child-by-type root-node "for_statement"))
        (subshell-node (jf/bash-parse--find-direct-child-by-type root-node "subshell"))
        (redirected-stmt (jf/bash-parse--find-direct-child-by-type root-node "redirected_statement")))
    (cond
     (pipeline-node :pipeline)
     (list-node :list)
     (if-node :conditional)
     (for-node :for-loop)
     (subshell-node :subshell)
     ;; Check for semicolon-separated commands BEFORE redirected_statement
     ;; This handles chains like "cat a.txt > b.txt; cat c.txt >> b.txt"
     ((> (jf/bash-parse--count-command-children root-node) 1) :list)
     ;; Check if redirected_statement wraps a list or pipeline
     ;; This handles chains/pipelines with redirections: "A && B > file"
     (redirected-stmt
      (when-let ((body-node (treesit-node-child-by-field-name redirected-stmt "body")))
        (let ((body-type (treesit-node-type body-node)))
          (cond
           ((string= body-type "pipeline") :pipeline)
           ((string= body-type "list") :list)
           (t :simple)))))
     (t :simple))))

(defun jf/bash-parse--count-command-children (node)
  "Count the number of command-like children in NODE.

Counts direct children that represent executable commands, including regular
commands, redirected statements, and variable assignments. Used to detect
command chains where the root has multiple command children.

NODE is a tree-sitter parse tree node to examine.

Returns integer count of command-like children found.

Counts these node types:
  - command - Regular bash commands
  - redirected_statement - Commands with I/O redirections
  - variable_assignment - Variable assignment statements

Examples:
  (jf/bash-parse--count-command-children root-node-for-\"cat a.txt; cat b.txt\")
    => 2

  (jf/bash-parse--count-command-children root-node-for-\"ls file.txt\")
    => 1

  (jf/bash-parse--count-command-children root-node-for-\"A=1 && B=2\")
    => 2

Internal helper for jf/bash-parse--detect-structure-type."
  (let ((count 0))
    (dotimes (i (treesit-node-child-count node))
      (when-let ((child (treesit-node-child node i)))
        (let ((child-type (treesit-node-type child)))
          (when (or (string= child-type "command")
                    (string= child-type "redirected_statement")
                    (string= child-type "variable_assignment"))
            (setq count (1+ count))))))
    count))

(defun jf/bash-parse--find-direct-child-by-type (node target-type)
  "Find the first direct child of NODE matching TARGET-TYPE.

Searches only the immediate children of NODE without recursing into
descendants. This prevents false matches from nested structures like
command substitutions or subshells.

NODE is a tree-sitter parse tree node to search within.
TARGET-TYPE is a string naming the node type to find (e.g., \"pipeline\", \"command\").

Returns the first matching child node, or nil if no match found.

Search behavior:
  - Only examines immediate children (no recursion)
  - Returns first match found
  - Returns nil if NODE is nil or has no matching children

Examples:
  (jf/bash-parse--find-direct-child-by-type root-node \"pipeline\")
    => <pipeline-node> or nil

  (jf/bash-parse--find-direct-child-by-type root-node \"command\")
    => <command-node> or nil

  (jf/bash-parse--find-direct-child-by-type command-node \"command_name\")
    => <command-name-node> or nil

Internal helper for jf/bash-parse--detect-structure-type and
jf/bash-parse--handle-simple-command."
  (when node
    (let ((child-count (treesit-node-child-count node))
          (result nil))
      (dotimes (i child-count)
        (when (null result)
          (when-let ((child (treesit-node-child node i)))
            (when (string= (treesit-node-type child) target-type)
              (setq result child)))))
      result)))

(defun jf/bash-parse--find-node-by-type (node target-type)
  "Find the first node of TARGET-TYPE anywhere in NODE's subtree.

Recursively searches NODE and all its descendants to find a node matching
TARGET-TYPE. Unlike jf/bash-parse--find-direct-child-by-type, this function
searches the entire subtree, not just immediate children.

NODE is a tree-sitter parse tree node to search within.
TARGET-TYPE is a string naming the node type to find (e.g., \"pipeline\", \"list\").

Returns the first matching node found in depth-first order, or nil if no match.

Search behavior:
  - Recursively searches entire subtree
  - Depth-first traversal order
  - Returns first match found
  - Returns nil if NODE is nil or no match exists

Examples:
  (jf/bash-parse--find-node-by-type root-node \"pipeline\")
    => <pipeline-node> (even if deeply nested)

  (jf/bash-parse--find-node-by-type root-node \"command_substitution\")
    => <command-substitution-node> or nil

  (jf/bash-parse--find-node-by-type statement-node \"file_redirect\")
    => <file-redirect-node> or nil

Internal helper for jf/bash-parse--handle-pipeline, jf/bash-parse--handle-list,
and other structure handlers."
  (when node
    (if (string= (treesit-node-type node) target-type)
        node
      (let ((child-count (treesit-node-child-count node))
            (result nil))
        (dotimes (i child-count)
          (when (null result)
            (when-let ((child (treesit-node-child node i)))
              (setq result (jf/bash-parse--find-node-by-type child target-type)))))
        result))))

(defun jf/bash-parse--handle-pipeline (root-node depth)
  "Handle pipeline command from ROOT-NODE.
DEPTH parameter tracks current recursion depth."
  (let* ((pipeline-node (jf/bash-parse--find-node-by-type root-node "pipeline"))
         (command-nodes (jf/bash-parse--get-all-command-nodes pipeline-node))
         (parsed-commands (mapcar (lambda (node)
                                    (jf/bash-parse--parse-single-command-node node depth))
                                   command-nodes))
         (any-dangerous (seq-some (lambda (cmd) (plist-get cmd :dangerous-p))
                                  parsed-commands)))

    ;; Check if pipeline is wrapped in redirected_statement
    (when-let* ((redirected-stmt (jf/bash-parse--find-node-by-type root-node "redirected_statement"))
                ;; Verify the redirected_statement's body is indeed the pipeline
                (body-node (treesit-node-child-by-field-name redirected-stmt "body"))
                ((string= (treesit-node-type body-node) "pipeline"))
                ;; Extract redirections from the statement
                (redirections (jf/bash-parse--extract-redirections redirected-stmt))
                (redirections))
      ;; Attach redirections to the last command in the pipeline
      ;; (since that's whose output is being redirected)
      (when (and parsed-commands (> (length parsed-commands) 0))
        (let* ((last-cmd-index (1- (length parsed-commands)))
               (last-cmd (nth last-cmd-index parsed-commands))
               ;; Merge with any existing redirections on the last command
               (existing-redirections (plist-get last-cmd :redirections))
               (merged-redirections (append existing-redirections redirections)))
          ;; Update the last command with merged redirections
          (setf (nth last-cmd-index parsed-commands)
                (plist-put last-cmd :redirections merged-redirections)))))

    (list :success t
          :type :pipeline
          :all-commands parsed-commands
          :command-count (length parsed-commands)
          :dangerous-p any-dangerous
          :ast root-node)))

(defun jf/bash-parse--root-has-list-plus-commands-p (root-node)
  "Check if ROOT-NODE has a direct list child PLUS other command children.
This detects the pattern 'A || B; C' where root has [list, ;, command].
Returns nil for 'A && B' (only list) or 'A && B > file' (redirected_statement wrapping list)."
  (let ((found-list nil)
        (found-command nil))
    (dotimes (i (treesit-node-child-count root-node))
      (let* ((child (treesit-node-child root-node i))
             (child-type (treesit-node-type child)))
        (cond
         ((string= child-type "list")
          (setq found-list t))
         ((or (string= child-type "command")
              (string= child-type "redirected_statement")
              (string= child-type "variable_assignment"))
          (setq found-command t)))))
    (and found-list found-command)))

(defun jf/bash-parse--handle-list (root-node depth)
  "Handle command list/chain from ROOT-NODE.
DEPTH parameter tracks current recursion depth."
  (let* ((list-node (jf/bash-parse--find-node-by-type root-node "list"))
         ;; Determine container based on structure:
         ;; - If root has BOTH a direct list AND other command children, use root
         ;;   Example: "A || B; C" -> root has [list, ;, command]
         ;; - Otherwise use list-node (or root if no list found)
         ;;   Examples: "A && B" -> root has [list]
         ;;             "A && B > file" -> root has [redirected_statement[list]]
         (container-node (if (jf/bash-parse--root-has-list-plus-commands-p root-node)
                             root-node
                           (or list-node root-node)))
         (command-nodes (jf/bash-parse--get-all-command-nodes container-node))
         (parsed-commands (mapcar (lambda (node)
                                    (jf/bash-parse--parse-single-command-node node depth))
                                   command-nodes))
         (any-dangerous (seq-some (lambda (cmd) (plist-get cmd :dangerous-p))
                                  parsed-commands)))

    ;; Check if list is wrapped in redirected_statement
    (when-let* ((redirected-stmt (jf/bash-parse--find-node-by-type root-node "redirected_statement"))
                ;; Verify the redirected_statement's body is indeed the list
                (body-node (treesit-node-child-by-field-name redirected-stmt "body"))
                ((string= (treesit-node-type body-node) "list"))
                ;; Extract redirections from the statement
                (redirections (jf/bash-parse--extract-redirections redirected-stmt))
                (redirections))
      ;; Attach redirections to the last command in the list
      ;; (since that's whose output is being redirected)
      (when (and parsed-commands (> (length parsed-commands) 0))
        (let* ((last-cmd-index (1- (length parsed-commands)))
               (last-cmd (nth last-cmd-index parsed-commands))
               ;; Merge with any existing redirections on the last command
               (existing-redirections (plist-get last-cmd :redirections))
               (merged-redirections (append existing-redirections redirections)))
          ;; Update the last command with merged redirections
          (setf (nth last-cmd-index parsed-commands)
                (plist-put last-cmd :redirections merged-redirections)))))

    ;; Return flattened structure for backward compatibility
    ;; Flatten first command's fields to top level (similar to simple-command handler)
    (if (null parsed-commands)
        ;; Empty chain - shouldn't happen but handle defensively
        (list :success t
              :type :chain
              :all-commands nil
              :command-count 0
              :dangerous-p nil
              :ast root-node)
      ;; Normal case: flatten first command's fields to top level
      (append (list :success t
                    :type :chain
                    :all-commands parsed-commands
                    :command-count (length parsed-commands)
                    :dangerous-p any-dangerous
                    :ast root-node)
              ;; Flatten first command's fields to top level
              (car parsed-commands)))))

(defun jf/bash-parse--handle-simple-command (root-node depth)
  "Handle simple (single) command from ROOT-NODE.
DEPTH parameter tracks current recursion depth."
  (let* ((redirected-stmt (jf/bash-parse--find-node-by-type root-node "redirected_statement"))
         ;; Use direct child search to avoid finding commands inside substitutions
         (command-node (jf/bash-parse--find-direct-child-by-type root-node "command"))
         (var-assign-node (jf/bash-parse--find-node-by-type root-node "variable_assignment"))
         ;; Prioritize command over variable_assignment ONLY if it has a direct command_name child
         ;; This handles inline env vars (PWD=/path cmd) while preserving standalone assignments (VAR=$(cmd))
         (command-or-redir (or redirected-stmt
                               (when (and command-node
                                         (jf/bash-parse--find-direct-child-by-type command-node "command_name"))
                                 command-node)
                               var-assign-node
                               command-node)))

    (if (null command-or-redir)
        (list :success nil
              :error "No command found in input")

      (let ((parsed-cmd (jf/bash-parse--parse-single-command-node command-or-redir depth)))
        ;; Return flattened structure for backward compatibility
        (append (list :success t
                      :type :simple
                      :all-commands (list parsed-cmd)
                      :command-count 1
                      :ast root-node)
                ;; Flatten first command's fields to top level
                parsed-cmd)))))

(defun jf/bash--extract-conditional-structure (if-node)
  "Extract conditional structure from IF-NODE.

IF-NODE is an if_statement tree-sitter node.

Returns plist with:
  :success t
  :type :conditional
  :command-name \"if\"
  :condition-text - test condition as text
  :then-text - then branch as text
  :else-text - else branch as text (optional)
  :ast - tree-sitter node

This shared logic is used by both jf/bash-parse--handle-conditional (root-node wrapper)
and jf/bash-parse--handle-conditional-node (direct node handler)."
  (let* ((condition-node (treesit-node-child-by-field-name if-node "condition"))
         (condition-text (when condition-node
                          (treesit-node-text condition-node t)))
         (then-text nil)
         (else-text nil))

    ;; Extract then and else branches
    (let ((child-count (treesit-node-child-count if-node))
          (collecting-then nil)
          (then-nodes '()))
      ;; Collect then-branch nodes (between 'then' and 'else_clause'/'elif_clause'/'fi')
      (dotimes (i child-count)
        (let* ((child (treesit-node-child if-node i))
               (child-type (treesit-node-type child)))
          (cond
           ;; Start collecting then branch after 'then' keyword
           ((string= child-type "then")
            (setq collecting-then t))
           ;; Stop at else_clause, elif_clause, or fi
           ((or (string= child-type "else_clause")
                (string= child-type "elif_clause")
                (string= child-type "fi"))
            (setq collecting-then nil)
            ;; Extract else branch text from else_clause node
            (when (string= child-type "else_clause")
              ;; else_clause contains 'else' keyword + commands
              ;; Extract text after the 'else' keyword
              (let ((else-clause-text (treesit-node-text child t)))
                ;; Strip leading 'else' keyword and whitespace
                (when (string-match "^else\\s-*\\(.*\\)$" else-clause-text)
                  (setq else-text (string-trim (match-string 1 else-clause-text)))
                  ;; Remove trailing semicolon if present
                  (when (string-suffix-p ";" else-text)
                    (setq else-text (substring else-text 0 -1)))))))
           ;; Collect then-branch command nodes (skip semicolons and keywords)
           (collecting-then
            (unless (member child-type '(";" "if" "then"))
              (push child then-nodes))))))

      ;; Convert then-branch node list to text
      (when then-nodes
        (setq then-text (mapconcat (lambda (node) (treesit-node-text node t))
                                  (nreverse then-nodes)
                                  "; "))))

    (list :success t
          :type :conditional
          :command-name "if"
          :condition-text condition-text
          :then-text then-text
          :else-text else-text
          :ast if-node)))

(defun jf/bash-parse--handle-conditional (root-node)
  "Handle conditional (if/then/else) from ROOT-NODE.

Extracts test condition, then branch, and else branch as text strings
while the buffer is still alive. Returns structured conditional data.

Bash tree-sitter structure for if_statement:
  [0] 'if' keyword
  [1] condition (with field name 'condition')
  [2] ';' or newline
  [3] 'then' keyword
  [4..n] then-branch commands
  [...] optional 'elif' or 'else' keywords with branches
  [last] 'fi' keyword"
  (let ((if-node (jf/bash-parse--find-node-by-type root-node "if_statement")))
    (if if-node
        (jf/bash--extract-conditional-structure if-node)
      (list :success nil
            :error "No if_statement found"))))

(defun jf/bash--extract-for-loop-structure (for-node depth)
  "Extract for-loop structure from FOR-NODE.

FOR-NODE is a for_statement tree-sitter node.
DEPTH parameter tracks current recursion depth for command substitution parsing.

Returns plist with:
  :success t
  :type :for-loop
  :command-name \"for\"
  :variable - loop variable name
  :iteration-values - list of iteration value strings
  :body-text - loop body as text
  :loop-variable - alias for :variable
  :loop-body - alias for :body-text
  :loop-list - first iteration value
  :loop-source-type - :substitution, :glob, :literal, or nil
  :command-substitutions - list of substitution structures (optional)
  :ast - tree-sitter node

This shared logic is used by both jf/bash-parse--handle-for-loop (root-node wrapper)
and jf/bash-parse--handle-for-loop-node (direct node handler)."
  (let* ((variable-node (treesit-node-child-by-field-name for-node "variable"))
         (variable-name (when variable-node
                         (treesit-node-text variable-node t)))
         (body-text nil)
         (iteration-values nil)
         (command-substitutions nil))

    ;; Extract command substitutions from the entire for-loop node
    (setq command-substitutions
          (jf/bash-parse--extract-command-substitutions for-node depth))

    ;; Extract iteration values and body
    (let ((child-count (treesit-node-child-count for-node))
          (collecting-values nil)
          (collecting-body nil)
          (value-nodes '())
          (body-nodes '()))
      ;; Collect value nodes and body nodes
      (dotimes (i child-count)
        (let* ((child (treesit-node-child for-node i))
               (child-type (treesit-node-type child)))
          (cond
           ;; Start collecting values after 'in' keyword
           ((string= child-type "in")
            (setq collecting-values t))
           ;; Start collecting body at do_group
           ((string= child-type "do_group")
            (setq collecting-values nil)
            (setq collecting-body t)
            ;; Extract body text from do_group node
            ;; do_group contains 'do' keyword + commands + 'done'
            (let ((do-group-text (treesit-node-text child t)))
              ;; Strip 'do' and 'done' keywords
              (when (string-match "^do\\s-+\\(.*\\)\\s-+done$" do-group-text)
                (setq body-text (string-trim (match-string 1 do-group-text))))))
           ;; Stop at 'done' keyword
           ((string= child-type "done")
            (setq collecting-body nil))
           ;; Collect value nodes (between 'in' and 'do_group')
           (collecting-values
            (unless (member child-type '(";" "in"))
              (push child value-nodes))))))

      ;; Convert value nodes to text list
      (when value-nodes
        (setq iteration-values
              (mapcar (lambda (node) (treesit-node-text node t))
                     (nreverse value-nodes)))))

    ;; Determine loop source type
    (let ((loop-source-type
           (cond
            ;; If there are command substitutions in the iteration values, it's from substitution
            (command-substitutions :substitution)
            ;; If the first iteration value contains glob characters, it's a glob
            ((and iteration-values
                  (string-match-p "[*?]\\|\\[.*\\]" (car iteration-values)))
             :glob)
            ;; Otherwise, it's a literal list
            (iteration-values :literal)
            (t nil))))

      ;; Build result with both old field names (for compatibility) and new field names (for recursive analysis)
      (let ((result (list :success t
                         :type :for-loop
                         :command-name "for"
                         ;; Original field names
                         :variable variable-name
                         :iteration-values iteration-values
                         :body-text body-text
                         ;; Alias field names for recursive analysis
                         :loop-variable variable-name
                         :loop-body body-text
                         :loop-list (car iteration-values)
                         :loop-source-type loop-source-type
                         :ast for-node)))
        ;; Only add command-substitutions if present
        (when command-substitutions
          (setq result (plist-put result :command-substitutions command-substitutions)))
        result))))

(defun jf/bash-parse--handle-for-loop (root-node)
  "Handle for-loop from ROOT-NODE.

Extracts loop variable, iteration values, body, and command substitutions.
Returns structured for-loop data.

Bash tree-sitter structure for for_statement:
  [0] 'for' keyword
  [1] variable (with field name 'variable')
  [2] 'in' keyword (optional - may use default $@)
  [3] value nodes (with field name 'value')
  [4] do_group with 'do' keyword and body commands
  [last] 'done' keyword"
  (let ((for-node (jf/bash-parse--find-node-by-type root-node "for_statement")))
    (if for-node
        ;; For-loop is a top-level handler, so depth is 0
        (jf/bash--extract-for-loop-structure for-node 0)
      (list :success nil
            :error "No for_statement found"))))

(defun jf/bash-parse--handle-subshell (root-node depth)
  "Handle subshell from ROOT-NODE.
DEPTH parameter tracks current recursion depth.
Returns structure with :type :subshell and :subshell-body containing parsed body."
  (let* ((subshell-node (jf/bash-parse--find-direct-child-by-type root-node "subshell")))
    (if (not subshell-node)
        (list :success nil
              :error "No subshell node found in root")
      ;; Extract content between parentheses and recursively parse
      (let* ((subshell-text (treesit-node-text subshell-node t))
             ;; Extract content between parentheses
             (inner-text (when (string-match "^(\\(.*\\))$" subshell-text)
                          (match-string 1 subshell-text)))
             ;; Recursively parse the content with incremented depth
             (parsed-body (when inner-text
                           (jf/bash-parse--with-depth inner-text (1+ depth)))))
        ;; Return as subshell type for recursive handler to process with isolated context
        (list :success t
              :type :subshell
              :subshell-body parsed-body
              :ast root-node)))))

(defun jf/bash-parse--handle-for-loop-node (for-node depth)
  "Handle for-loop from FOR-NODE directly.
FOR-NODE is a for_statement node from tree-sitter.
DEPTH parameter tracks current recursion depth.
Returns same structure as jf/bash-parse--handle-for-loop but works with node directly."
  (jf/bash--extract-for-loop-structure for-node depth))

(defun jf/bash-parse--handle-conditional-node (if-node)
  "Handle conditional (if/then/else) from IF-NODE directly.
IF-NODE is an if_statement node from tree-sitter.
Returns same structure as jf/bash-parse--handle-conditional but works with node directly."
  (jf/bash--extract-conditional-structure if-node))

(defun jf/bash-parse--get-all-command-nodes (container-node)
  "Get all command nodes from CONTAINER-NODE (pipeline or list).
Returns command, redirected_statement, variable_assignment, for_statement, if_statement, or subshell nodes.
Only collects top-level commands - does not descend into command substitutions, arithmetic expansions, or statement bodies."
  (let ((commands '()))
    (jf/bash-parse--visit-node
     container-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (cond
          ;; Skip command substitutions and arithmetic expansions - don't collect nested commands
          ;; command_substitution is $(...) or `...`
          ;; arithmetic_expansion is $((...))
          ((or (string= node-type "command_substitution")
               (string= node-type "arithmetic_expansion"))
           :skip-children)
          ;; Collect control flow statements and skip their children
          ;; (to avoid descending into loop/conditional bodies)
          ((or (string= node-type "for_statement")
               (string= node-type "if_statement"))
           (push node commands)
           :skip-children)
          ;; Collect redirected_statement nodes and skip their children
          ;; (the nested command node would be a duplicate)
          ((string= node-type "redirected_statement")
           (push node commands)
           :skip-children)
          ;; Collect subshell nodes and skip their children
          ;; Subshells are (command) and need isolated context
          ;; NOTE: Must distinguish from command_substitution $(...) and arithmetic_expansion $((...))
          ((string= node-type "subshell")
           (push node commands)
           :skip-children)
          ;; Collect command and variable_assignment nodes
          ((or (string= node-type "command")
               (string= node-type "variable_assignment"))
           (push node commands)
           nil)))))
    (nreverse commands)))

(defun jf/bash-parse--parse-find-with-exec (command-name args redirections)
  "Parse find command with -exec or -execdir blocks.
COMMAND-NAME is 'find', ARGS are all arguments, REDIRECTIONS are any redirections.
Returns a command structure with :exec-blocks field containing parsed exec commands."
  (let ((flags '())
        (positional-args '())
        (exec-blocks '())
        (i 0))

    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ;; Start of exec block
         ((or (string= arg "-exec") (string= arg "-execdir"))
          (let ((exec-type arg)
                (exec-words '())
                (j (1+ i))
                (found-terminator nil))

            ;; Collect words until we find \; or +
            (while (and (< j (length args))
                       (not found-terminator))
              (let ((word (nth j args)))
                (cond
                 ;; Terminators
                 ((or (string= word "\\;") (string= word ";")
                      (string= word "+"))
                  (setq found-terminator word)
                  (setq j (1+ j)))
                 ;; Regular word in exec block
                 (t
                  (push word exec-words)
                  (setq j (1+ j))))))

            ;; Parse the exec block as a command
            (setq exec-words (nreverse exec-words))
            (when exec-words
              (let* ((exec-cmd-name (car exec-words))
                     (exec-args (cdr exec-words))
                     (exec-flags (jf/bash-parse--extract-flags exec-args))
                     (exec-positional (jf/bash-parse--extract-positional-args exec-args))
                     (exec-dangerous (jf/bash-parse--is-dangerous exec-cmd-name nil exec-flags)))

                (push (list :type exec-type
                           :terminator found-terminator
                           :command-name exec-cmd-name
                           :flags exec-flags
                           :positional-args exec-positional
                           :args exec-args
                           :dangerous-p exec-dangerous)
                      exec-blocks)))

            ;; Add the -exec flag itself to find's flags
            (push exec-type flags)

            ;; Move past the exec block
            (setq i j)))

         ;; Regular flag
         ((string-prefix-p "-" arg)
          (push arg flags)
          (setq i (1+ i)))

         ;; Positional argument
         (t
          (push arg positional-args)
          (setq i (1+ i))))))

    ;; Reverse lists to restore original order
    (setq flags (nreverse flags))
    (setq positional-args (nreverse positional-args))
    (setq exec-blocks (nreverse exec-blocks))

    ;; Build result
    (let ((result (list :command-name command-name
                       :subcommand nil
                       :flags flags
                       :positional-args positional-args
                       :args args
                       :exec-blocks exec-blocks
                       :dangerous-p (or (seq-some (lambda (block)
                                                   (plist-get block :dangerous-p))
                                                 exec-blocks)
                                       nil))))
      (when redirections
        (setq result (plist-put result :redirections redirections)))
      result)))

(defun jf/bash-parse--parse-wrapper-command (command-name wrapper-spec remaining-words redirections)
  "Parse wrapper command like sudo, env, time, etc.
COMMAND-NAME is the wrapper command name.
WRAPPER-SPEC is the spec from jf/bash-parser-wrapper-commands.
REMAINING-WORDS are all words after the command name.
REDIRECTIONS are any redirections on the command.
Returns command structure with wrapper's flags separated from wrapped command."
  (let ((flags-with-args (plist-get wrapper-spec :flags-with-args))
        (flags-no-args (plist-get wrapper-spec :flags-no-args))
        (is-dangerous (plist-get wrapper-spec :dangerous))
        (wrapper-flags '())
        (positional-args '())
        (i 0))

    ;; Parse wrapper's flags
    (while (< i (length remaining-words))
      (let ((word (nth i remaining-words)))
        (cond
         ;; Flag that takes an argument
         ((member word flags-with-args)
          (push word wrapper-flags)
          (setq i (1+ i))
          ;; Also consume the argument
          (when (< i (length remaining-words))
            (push (nth i remaining-words) positional-args)
            (setq i (1+ i))))

         ;; Flag that doesn't take an argument
         ((member word flags-no-args)
          (push word wrapper-flags)
          (setq i (1+ i)))

         ;; Start of wrapped command (first non-wrapper-flag word)
         (t
          ;; Everything from here is the wrapped command
          (while (< i (length remaining-words))
            (push (nth i remaining-words) positional-args)
            (setq i (1+ i)))))))

    ;; Reverse lists to restore original order
    (setq wrapper-flags (nreverse wrapper-flags))
    (setq positional-args (nreverse positional-args))

    ;; Build result
    (let ((result (list :command-name command-name
                       :subcommand nil
                       :flags wrapper-flags
                       :positional-args positional-args
                       :args remaining-words
                       :dangerous-p is-dangerous)))
      (when redirections
        (setq result (plist-put result :redirections redirections)))
      result)))

(defvar jf/bash--max-parse-depth 10
  "Maximum parsing depth to prevent infinite recursion.
When exceeded, jf/bash-parse will signal an error.
Depth is tracked via parameter passing for thread safety.")

(defun jf/bash-parse--extract-command-substitutions (command-or-statement-node depth)
  "Extract all command substitutions from COMMAND-OR-STATEMENT-NODE.
DEPTH parameter tracks current recursion depth for nested parsing.
Returns list of plists with :syntax, :content, :nesting-level, :parsed.
Handles both $(...) and backtick syntax, tracks nesting levels, and
recursively parses substitution content."
  (let ((substitutions '()))
    ;; Find all command_substitution nodes in the tree
    (jf/bash-parse--visit-node
     command-or-statement-node
     (lambda (node)
       (when (string= (treesit-node-type node) "command_substitution")
         ;; Extract the full text including delimiters
         (let* ((full-text (treesit-node-text node t))
                (syntax (cond
                        ;; Check for $() syntax
                        ((and (string-prefix-p "$(" full-text)
                              (string-suffix-p ")" full-text))
                         "$()")
                        ;; Check for backtick syntax
                        ((and (string-prefix-p "`" full-text)
                              (string-suffix-p "`" full-text))
                         "`")
                        ;; Default to $() if we can't determine
                        (t "$()")))
                ;; Extract content without delimiters
                (content (cond
                         ((string= syntax "$()")
                          (substring full-text 2 -1))
                         ((string= syntax "`")
                          (substring full-text 1 -1))
                         (t full-text)))
                ;; Calculate nesting level by counting parent command_substitution nodes
                (nesting-level (jf/bash-parse--calculate-nesting-level node))
                ;; Recursively parse the substitution content with incremented depth
                (parsed-subst (when (and content (> (length content) 0))
                               (jf/bash-parse--with-depth content (1+ depth)))))

           ;; Add to substitutions list with parsed field
           (push (list :syntax syntax
                      :content content
                      :nesting-level nesting-level
                      :parsed parsed-subst)
                 substitutions))
         ;; Continue visiting children to find nested substitutions
         nil)))
    ;; Return in order encountered (reverse the accumulated list)
    (nreverse substitutions)))

(defun jf/bash-parse--calculate-nesting-level (node)
  "Calculate nesting level for NODE by counting parent command_substitution nodes.
Returns nesting level starting from 1 for outermost substitution."
  (let ((level 1)
        (parent (treesit-node-parent node)))
    ;; Walk up the tree counting command_substitution ancestors
    (while parent
      (when (string= (treesit-node-type parent) "command_substitution")
        (setq level (1+ level)))
      (setq parent (treesit-node-parent parent)))
    level))

(defun jf/bash-parse--parse-single-command-node (command-or-statement-node depth)
  "Parse COMMAND-OR-STATEMENT-NODE which may be command, redirected_statement, variable_assignment, for_statement, if_statement, or subshell.
DEPTH parameter tracks current recursion depth.
Returns command structure with optional :redirections and :command-substitutions fields."
  (let ((command-node nil)
        (redirections nil)
        (command-substitutions nil)
        (node-type (treesit-node-type command-or-statement-node)))

    ;; Handle control flow statements and subshells by creating appropriate structures
    ;; This allows for_statement, if_statement, and subshell nodes to be parsed correctly when found in chains
    (cond
     ((string= node-type "for_statement")
      ;; Create a wrapper program node and call the for-loop handler
      (let ((result (jf/bash-parse--handle-for-loop-node command-or-statement-node depth)))
        ;; Return the result structure (already complete from handler)
        result))

     ((string= node-type "if_statement")
      ;; Create a wrapper program node and call the conditional handler
      (let ((result (jf/bash-parse--handle-conditional-node command-or-statement-node)))
        ;; Return the result structure (already complete from handler)
        result))

     ((string= node-type "subshell")
      ;; Parse subshell: (command)
      ;; Extract content between parentheses and recursively parse
      ;; NOTE: This is distinct from command_substitution $(...) and arithmetic_expansion $((...))
      (let* ((subshell-text (treesit-node-text command-or-statement-node t))
             ;; Extract content between parentheses
             (inner-text (when (string-match "^(\\(.*\\))$" subshell-text)
                          (match-string 1 subshell-text)))
             ;; Recursively parse the content with incremented depth
             (parsed-body (when inner-text
                           (jf/bash-parse--with-depth inner-text (1+ depth)))))
        ;; Return as subshell type for recursive handler to process with isolated context
        (list :success t
              :type :subshell
              :subshell-body parsed-body)))

     ;; Normal command/statement processing
     (t
      ;; Extract command substitutions from the entire node tree
      (setq command-substitutions
            (jf/bash-parse--extract-command-substitutions command-or-statement-node depth))

      ;; Check if this is a redirected_statement wrapper
      (if (string= node-type "redirected_statement")
          (progn
            ;; Extract the actual command from the body field
            (setq command-node
                  (treesit-node-child-by-field-name command-or-statement-node "body"))
            ;; Extract redirections
            (setq redirections
                  (jf/bash-parse--extract-redirections command-or-statement-node)))
        ;; Not a redirected statement, use node as-is
        (setq command-node command-or-statement-node))

      ;; Handle variable_assignment nodes specially
      (if (string= (treesit-node-type command-node) "variable_assignment")
          (let* ((name-node (treesit-node-child-by-field-name command-node "name"))
                 (value-node (treesit-node-child-by-field-name command-node "value"))
                 (var-name (treesit-node-text name-node))
                 (var-value (treesit-node-text value-node)))
            ;; Return with variable name as command-name and value as positional arg
            (let ((result (list :command-name var-name
                               :subcommand nil
                               :flags nil
                               :positional-args (list var-value)
                               :args (list var-value)
                               :dangerous-p nil)))
              ;; Add command-substitutions if present
              (when command-substitutions
                (setq result (plist-put result :command-substitutions command-substitutions)))
              result))

        ;; Parse regular command
        (let* ((env-vars (jf/bash-parse--extract-env-vars command-node))
               (words (jf/bash-parse--extract-words command-node))
               (command-name (car words))
               (remaining-words (cdr words))
               (subcommand (jf/bash-parse--detect-subcommand command-name remaining-words))
               (injection-info (jf/bash-parse--detect-command-injection command-name remaining-words))
               (args-start (if subcommand (cdr remaining-words) remaining-words)))

          ;; Check for wrapper commands (sudo, env, time, etc.)
          (if-let ((wrapper-spec (and (not (string-match-p "=" command-name))
                                      (alist-get (intern command-name)
                                                 jf/bash-parser-wrapper-commands))))
              (let ((result (jf/bash-parse--parse-wrapper-command command-name wrapper-spec remaining-words redirections)))
                (when command-substitutions
                  (setq result (plist-put result :command-substitutions command-substitutions)))
                (when env-vars
                  (setq result (plist-put result :env-vars env-vars)))
                result)

            ;; Check for find command with -exec blocks
            (if (and (string= command-name "find")
                     (or (member "-exec" args-start)
                         (member "-execdir" args-start)))
                (let ((result (jf/bash-parse--parse-find-with-exec command-name args-start redirections)))
                  (when command-substitutions
                    (setq result (plist-put result :command-substitutions command-substitutions)))
                  (when env-vars
                    (setq result (plist-put result :env-vars env-vars)))
                  result)

              ;; Normal command processing
              (let* ((flags (jf/bash-parse--extract-flags args-start))
                     (positional-args (jf/bash-parse--extract-positional-args args-start))
                     (dangerous-p (jf/bash-parse--is-dangerous command-name subcommand flags)))

                ;; Build result, only include redirections if present
                (let ((result (list :command-name command-name
                                   :subcommand subcommand
                                   :flags flags
                                   :positional-args positional-args
                                   :args args-start
                                   :dangerous-p dangerous-p)))
                  (when redirections
                    (setq result (plist-put result :redirections redirections)))
                  (when command-substitutions
                    (setq result (plist-put result :command-substitutions command-substitutions)))
                  (when env-vars
                    (setq result (plist-put result :env-vars env-vars)))

                  ;; Handle command injection if detected
                  (when injection-info
                    (let ((injection-type (plist-get injection-info :injection-type))
                          (nested-cmd (plist-get injection-info :nested-command)))
                      (setq result (plist-put result :command-injection t))
                      (setq result (plist-put result :injection-type injection-type))
                      ;; Recursively parse the nested command with incremented depth
                      (when nested-cmd
                        (let ((parsed-nested (jf/bash-parse--with-depth nested-cmd (1+ depth))))
                          (setq result (plist-put result :nested-command parsed-nested))))))

                  result))))))))))

(defun jf/bash-parse--extract-env-vars (command-node)
  "Extract inline environment variable assignments from COMMAND-NODE.
Returns alist of (VAR-SYMBOL . VALUE-STRING) for inline assignments like PWD=/path.
These assignments only affect the command they prefix, not subsequent commands."
  (let ((env-vars '()))
    ;; Only process command nodes (not variable_assignment nodes)
    (when (string= (treesit-node-type command-node) "command")
      ;; Check direct children for variable_assignment nodes
      (dotimes (i (treesit-node-child-count command-node))
        (when-let ((child (treesit-node-child command-node i)))
          (when (string= (treesit-node-type child) "variable_assignment")
            (let* ((name-node (treesit-node-child-by-field-name child "name"))
                   (value-node (treesit-node-child-by-field-name child "value"))
                   (var-name (treesit-node-text name-node t))
                   (var-value (treesit-node-text value-node t)))
              (push (cons (intern var-name) var-value) env-vars))))))
    (nreverse env-vars)))

(defun jf/bash-parse--extract-words (command-node)
  "Extract all word nodes from COMMAND-NODE as strings.
Returns list of strings representing all words in the command.
Skips variable_assignment nodes (inline env vars like PWD=/path cmd)."
  (let ((words '()))
    (jf/bash-parse--visit-node
     command-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (cond
          ;; Skip variable_assignment nodes (inline env vars)
          ;; These are handled separately by jf/bash-parse--extract-env-vars
          ((string= node-type "variable_assignment")
           :skip-children)

          ;; Terminal nodes: extract text and skip children to prevent duplication
          ((or (string= node-type "concatenation")
               (string= node-type "string")
               (string= node-type "raw_string")
               (string= node-type "ansi_c_string")
               (string= node-type "simple_expansion")
               (string= node-type "expansion")
               (string= node-type "command_substitution")
               (string= node-type "arithmetic_expansion"))
           (let ((text (treesit-node-text node t)))
             (when (and text (> (length text) 0))
               (setq text (string-trim text))
               ;; Remove surrounding quotes for string types (not concatenation/expansion/substitution)
               (when (and (> (length text) 1)
                         (not (member node-type '("concatenation" "simple_expansion" "expansion" "command_substitution" "arithmetic_expansion")))
                         (or (and (string-prefix-p "\"" text)
                                  (string-suffix-p "\"" text))
                             (and (string-prefix-p "'" text)
                                  (string-suffix-p "'" text))))
                 (setq text (substring text 1 -1)))
               (unless (string-empty-p text)
                 (push text words))))
           ;; Return :skip-children to prevent recursing into components
           :skip-children)

          ;; Leaf nodes: extract text and continue visiting siblings
          ((or (string= node-type "word")
               (string= node-type "number"))
           (let ((text (treesit-node-text node t)))
             (when (and text (> (length text) 0))
               (setq text (string-trim text))
               (unless (string-empty-p text)
                 (push text words))))
           nil)

          ;; Other nodes: just continue visiting children
          (t nil)))))
    (nreverse words)))

(defun jf/bash-parse--extract-redirections (statement-node)
  "Extract all redirections from STATEMENT-NODE.
Returns list of plists with :type, :operator, :descriptor, :destination."
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

(defun jf/bash-parse--parse-file-redirect (redirect-node)
  "Parse a file_redirect REDIRECT-NODE into plist.
Returns: (:type :file :operator \">\" :descriptor nil :destination \"output.txt\")"
  (let ((descriptor nil)
        (operator nil)
        (destination nil))

    ;; Extract descriptor (optional field)
    (when-let ((desc-node (treesit-node-child-by-field-name redirect-node "descriptor")))
      (setq descriptor (treesit-node-text desc-node t)))

    ;; Extract destination (optional field)
    (when-let ((dest-node (treesit-node-child-by-field-name redirect-node "destination")))
      (setq destination (treesit-node-text dest-node t)))

    ;; Extract operator by visiting children and matching known operators
    (dotimes (i (treesit-node-child-count redirect-node))
      (when-let ((child (treesit-node-child redirect-node i)))
        (let ((text (treesit-node-text child t)))
          (when (and (null operator)
                    (member text '(">" ">>" "<" "&>" "&>>"
                                  "<&" ">&" ">|" "<&-" ">&-")))
            (setq operator text)))))

    (list :type :file
          :operator operator
          :descriptor descriptor
          :destination destination)))

(defun jf/bash-parse--parse-herestring-redirect (redirect-node)
  "Parse a herestring_redirect REDIRECT-NODE into plist.
Returns: (:type :herestring :operator \"<<<\" :descriptor nil :value \"...\")"
  (let ((descriptor nil)
        (value nil))

    ;; Extract descriptor (optional field)
    (when-let ((desc-node (treesit-node-child-by-field-name redirect-node "descriptor")))
      (setq descriptor (treesit-node-text desc-node t)))

    ;; Extract value (the content after <<<)
    ;; Visit children to find the literal value
    (dotimes (i (treesit-node-child-count redirect-node))
      (when-let ((child (treesit-node-child redirect-node i)))
        (let ((child-type (treesit-node-type child)))
          (when (member child-type '("word" "string" "raw_string"))
            (setq value (treesit-node-text child t))))))

    (list :type :herestring
          :operator "<<<"
          :descriptor descriptor
          :value value)))

(defun jf/bash-parse--parse-heredoc-redirect (redirect-node)
  "Parse a heredoc_redirect REDIRECT-NODE into plist.
Returns: (:type :heredoc :operator \"<<\" :descriptor nil :delimiter \"...\")"
  (let ((descriptor nil)
        (operator nil)
        (delimiter nil))

    ;; Extract descriptor (optional field)
    (when-let ((desc-node (treesit-node-child-by-field-name redirect-node "descriptor")))
      (setq descriptor (treesit-node-text desc-node t)))

    ;; Extract operator (<< or <<-)
    (dotimes (i (treesit-node-child-count redirect-node))
      (when-let ((child (treesit-node-child redirect-node i)))
        (let ((text (treesit-node-text child t)))
          (when (and (null operator)
                    (member text '("<<" "<<-")))
            (setq operator text)))))

    ;; Extract delimiter (heredoc_start)
    (dotimes (i (treesit-node-child-count redirect-node))
      (when-let ((child (treesit-node-child redirect-node i)))
        (when (string= (treesit-node-type child) "heredoc_start")
          (setq delimiter (treesit-node-text child t)))))

    (list :type :heredoc
          :operator operator
          :descriptor descriptor
          :delimiter delimiter)))

(defun jf/bash-parse--visit-node (node visitor-fn)
  "Recursively visit NODE and all children, calling VISITOR-FN on each node.

Implements a depth-first tree traversal that applies VISITOR-FN to every node
in NODE's subtree. The visitor function controls recursion by returning
:skip-children when it wants to stop descending into a node's children.

NODE is a tree-sitter parse tree node to traverse.
VISITOR-FN is a function called on each node with signature: (lambda (node) ...).

VISITOR-FN return values:
  :skip-children - Stop recursing into this node's children
  nil            - Continue normal recursion (any non-:skip-children value works)

Traversal order: depth-first, parent before children

Used for:
  - Collecting nodes matching certain criteria
  - Extracting information from parse trees
  - Avoiding recursion into command substitutions or nested structures

Examples:
  ;; Collect all \"word\" nodes
  (let ((words '()))
    (jf/bash-parse--visit-node root-node
      (lambda (node)
        (when (string= (treesit-node-type node) \"word\")
          (push node words))))
    (nreverse words))

  ;; Skip command substitutions to avoid nested commands
  (jf/bash-parse--visit-node root-node
    (lambda (node)
      (if (string= (treesit-node-type node) \"command_substitution\")
          :skip-children  ; Don't recurse into substitution
        nil)))           ; Continue normally

Internal helper for jf/bash-parse--get-all-command-nodes,
jf/bash-parse--extract-words, and jf/bash-parse--extract-redirections."
  (when node
    (let ((visitor-result (funcall visitor-fn node)))
      ;; Only recurse if visitor didn't return :skip-children
      (unless (eq visitor-result :skip-children)
        (let ((child-count (treesit-node-child-count node)))
          (dotimes (i child-count)
            (when-let ((child (treesit-node-child node i)))
              (jf/bash-parse--visit-node child visitor-fn))))))))

(defun jf/bash-parse--detect-subcommand (command-name remaining-words)
  "Detect if COMMAND-NAME has a subcommand in REMAINING-WORDS.
Returns subcommand string or nil."
  (when (and remaining-words
             (member command-name '("git" "docker" "npm" "cargo" "kubectl")))
    ;; First non-flag word after command is the subcommand
    (let ((first-word (car remaining-words)))
      (when (and first-word
                 (not (string-prefix-p "-" first-word)))
        first-word))))

(defvar jf/bash-parser-injection-commands
  '((bash . ("-c"))
    (sh . ("-c")))
  "Alist of commands and their injection flags.
Each entry is (COMMAND . (FLAGS...)) where FLAGS indicate command injection.
Note: env is handled as a wrapper command, not an injection command.")

(defun jf/bash-parse--strip-outer-quotes (str)
  "Remove outer quotes from STR if present.
Handles both single and double quotes."
  (if (and (stringp str)
           (>= (length str) 2)
           (or (and (string-prefix-p "\"" str) (string-suffix-p "\"" str))
               (and (string-prefix-p "'" str) (string-suffix-p "'" str))))
      (substring str 1 -1)
    str))

(defun jf/bash-parse--extract-injection-arg (remaining-words injection-flag)
  "Extract the argument following INJECTION-FLAG in REMAINING-WORDS.
Returns the unquoted command string or nil if not found."
  (let ((pos (seq-position remaining-words injection-flag)))
    (when (and pos (< (1+ pos) (length remaining-words)))
      (jf/bash-parse--strip-outer-quotes (nth (1+ pos) remaining-words)))))

(defun jf/bash-parse--detect-command-injection (command-name remaining-words)
  "Detect command injection pattern in COMMAND-NAME with REMAINING-WORDS.
Returns plist with :injection-type and :nested-command if detected, nil otherwise."
  (when-let* ((injection-flags (alist-get (intern command-name) jf/bash-parser-injection-commands))
              (found-flag (seq-find (lambda (flag) (member flag remaining-words))
                                    injection-flags)))
    (when-let ((nested-cmd (jf/bash-parse--extract-injection-arg remaining-words found-flag)))
      (list :injection-type found-flag
            :nested-command nested-cmd))))

(defun jf/bash-parse--extract-flags (words)
  "Extract flag arguments from WORDS.
Returns list of strings that start with -."
  (seq-filter (lambda (word)
                (string-prefix-p "-" word))
              words))

(defun jf/bash-parse--extract-positional-args (words)
  "Extract non-flag arguments from WORDS.
Returns list of strings that don't start with -."
  (seq-remove (lambda (word)
                (string-prefix-p "-" word))
              words))

(defun jf/bash-parse--is-dangerous (command-name subcommand flags)
  "Check if command matches dangerous patterns.
COMMAND-NAME is the base command.
SUBCOMMAND is the subcommand (or nil).
FLAGS is list of flag strings.
Returns t if dangerous, nil otherwise."
  (when-let ((patterns (and (not (string-match-p "=" command-name))
                            (alist-get (intern command-name)
                                       jf/bash-parser-dangerous-patterns))))
    (catch 'dangerous
      (dolist (pattern patterns)
        (let ((pattern-subcommand (plist-get pattern :subcommand))
              (pattern-flags (plist-get pattern :flags))
              (pattern-any-contains (plist-get pattern :any-flag-contains)))

          ;; Check if subcommand matches (or pattern has no subcommand requirement)
          (when (or (null pattern-subcommand)
                    (equal pattern-subcommand subcommand))

            ;; Check if any dangerous flag is present
            (when pattern-flags
              (dolist (dangerous-flag pattern-flags)
                (when (member dangerous-flag flags)
                  (throw 'dangerous t))))

            ;; Check if any flag contains dangerous substring
            (when pattern-any-contains
              (dolist (flag flags)
                (dolist (dangerous-substr pattern-any-contains)
                  (when (string-match-p dangerous-substr flag)
                    (throw 'dangerous t))))))))
      nil)))

(provide 'bash-parser-core)
;;; bash-parser-core.el ends here
