;;; bash-parser-recursive.el --- Recursive semantic analysis -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, recursion, semantics
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Recursive tree traversal for semantic analysis.
;; Extracts file operations from all nesting levels including:
;; - Command substitutions
;; - Pipeline commands
;; - Chain commands (with variable tracking)
;; - Loop bodies (with variable binding)
;; - Conditional branches (with context markers)

;;; Code:

(require 'cl-lib)

(defvar jf/bash-recursive-max-depth 10
  "Maximum recursion depth for semantic analysis.
Prevents infinite recursion in pathological cases.")

(defun jf/bash--process-command-substitutions (parsed-command var-context depth)
  "Process command substitutions recursively from PARSED-COMMAND.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.

Returns cons cell (OPERATIONS . SUBSTITUTION-PATTERNS) where:
- OPERATIONS is list of operations from substitutions (marked :from-substitution t)
- SUBSTITUTION-PATTERNS is list of pattern metadata for pattern flow tracking"
  (let ((operations nil)
        (substitution-patterns nil))
    (when-let ((substs (plist-get parsed-command :command-substitutions)))
      (dolist (subst substs)
        (when-let ((parsed-subst (plist-get subst :parsed)))
          (when (plist-get parsed-subst :success)  ; Only if parse succeeded
            (let ((subst-ops (jf/bash-analyze-file-operations-recursive
                             parsed-subst var-context (1+ depth))))
              ;; Check if substitution produces a file pattern
              (let ((pattern-op (seq-find
                                (lambda (op)
                                  (eq (plist-get op :operation) :match-pattern))
                                subst-ops)))
                (when pattern-op
                  ;; Track this substitution's pattern output
                  (push (list :substitution-content (plist-get subst :content)
                             :pattern (plist-get pattern-op :file)
                             :search-scope (plist-get pattern-op :search-scope)
                             :command (plist-get pattern-op :command)
                             :from-substitution t)
                        substitution-patterns)))

              ;; Mark all nested operations as from-substitution
              ;; Build correctly with append instead of copy-tree for O(n) performance
              (let ((marked-ops
                     (mapcar (lambda (op)
                              (append op (list :from-substitution t)))
                            subst-ops)))
                (setq operations (append operations marked-ops))))))))
    (cons operations substitution-patterns)))

(defun jf/bash--process-chain-with-context (parsed-command var-context depth)
  "Process command chain with variable and directory context tracking.

PARSED-COMMAND is the parsed chain command.
VAR-CONTEXT is initial variable bindings alist.
DEPTH is current recursion depth.

Returns list of operations from all chain commands with proper context.

Tracks:
- Variable assignments across chain (FOO=bar && use $FOO)
- Directory changes (cd /tmp && rm file)
- pushd/popd directory stack
- PWD and OLDPWD updates"
  (let ((operations nil)
        (chain-context var-context)
        (current-pwd (or (alist-get 'PWD var-context) "/"))
        (dir-stack nil))  ; Initialize directory stack for pushd/popd
    (dolist (cmd (plist-get parsed-command :all-commands))
      ;; Extract variable assignments from this command with resolution
      ;; Pass current chain-context so assignments can resolve using earlier assignments
      (when (fboundp 'jf/bash--extract-assignments-from-command)
        (let ((assignments (jf/bash--extract-assignments-from-command cmd chain-context)))
          (when assignments
            ;; Assignments are already fully resolved (variables, pwd, relative paths)
            (setq chain-context (append assignments chain-context))
            ;; Check if PWD was assigned - this updates directory context
            (when-let ((new-pwd (alist-get 'PWD assignments)))
              (setq current-pwd new-pwd)))))

      ;; Check for cd command and update directory context
      (when (and (fboundp 'jf/bash--is-cd-command-p)
                 (jf/bash--is-cd-command-p cmd))
        (when-let ((new-pwd (jf/bash--extract-cd-target cmd chain-context current-pwd)))
          (unless (eq new-pwd :unresolved)
            ;; Save current PWD to OLDPWD before updating
            (let ((old-pwd current-pwd))
              (setq current-pwd new-pwd)
              ;; Update both PWD and OLDPWD in context for subsequent commands
              (setq chain-context (cons (cons 'PWD current-pwd)
                                       (assq-delete-all 'PWD chain-context)))
              (setq chain-context (cons (cons 'OLDPWD old-pwd)
                                       (assq-delete-all 'OLDPWD chain-context)))))))

      ;; Check for pushd command and update directory context + stack
      (when (and (fboundp 'jf/bash--is-pushd-command-p)
                 (jf/bash--is-pushd-command-p cmd))
        (when-let ((new-pwd (jf/bash--extract-pushd-target cmd chain-context current-pwd)))
          (unless (eq new-pwd :unresolved)
            ;; Push current directory onto stack
            (push current-pwd dir-stack)
            ;; Update current directory
            (setq current-pwd new-pwd)
            ;; Update PWD in context for subsequent commands
            (setq chain-context (cons (cons 'PWD current-pwd)
                                     (assq-delete-all 'PWD chain-context))))))

      ;; Check for popd command and restore from directory stack
      (when (and (fboundp 'jf/bash--is-popd-command-p)
                 (jf/bash--is-popd-command-p cmd))
        ;; Only pop if stack is not empty
        (when dir-stack
          (let ((popped-pwd (pop dir-stack)))
            (setq current-pwd popped-pwd)
            ;; Update PWD in context for subsequent commands
            (setq chain-context (cons (cons 'PWD current-pwd)
                                     (assq-delete-all 'PWD chain-context))))))

      ;; Extract operations with updated context (PWD now reflects current-pwd)
      (let ((cmd-ops (jf/bash-analyze-file-operations-recursive
                     cmd chain-context (1+ depth))))
        (setq operations (append operations cmd-ops))))
    operations))

(defun jf/bash--process-loop-with-binding (parsed-command var-context depth operations)
  "Process for-loop with variable binding in body.

PARSED-COMMAND is the parsed for-loop command.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.
OPERATIONS is current operations list (may contain pattern from substitution).

Returns list of operations from loop body with loop metadata.

The function:
1. Determines loop variable value from source type (glob, substitution, literal)
2. Creates pattern operation for glob sources
3. Parses and analyzes loop body with variable in context
4. Marks all loop operations with :loop-context metadata"
  (let* ((loop-ops nil)
         (loop-var (plist-get parsed-command :loop-variable))
         (loop-source-type (plist-get parsed-command :loop-source-type))
         (loop-body (plist-get parsed-command :loop-body))
         (loop-var-value nil))

    ;; Determine what the loop variable represents
    (cond
     ;; Loop source is substitution - bind to pattern from substitution
     ((eq loop-source-type :substitution)
      ;; Find the pattern produced by the substitution
      (let ((pattern-op (seq-find
                        (lambda (op)
                          (and (eq (plist-get op :operation) :match-pattern)
                               (plist-get op :from-substitution)))
                        operations)))
        (when pattern-op
          (setq loop-var-value (plist-get pattern-op :file)))))

     ;; Loop source is glob - bind to glob pattern and create pattern operation
     ((eq loop-source-type :glob)
      (let* ((glob-pattern (plist-get parsed-command :loop-list))
             (current-pwd (or (alist-get 'PWD var-context) "/"))
             ;; Resolve glob pattern against current directory
             (resolved-pattern (if (string-prefix-p "/" glob-pattern)
                                  glob-pattern
                                (expand-file-name glob-pattern current-pwd))))
        (setq loop-var-value glob-pattern)
        ;; Create a pattern operation for the glob expansion
        (when (and glob-pattern (fboundp 'jf/bash--has-glob-pattern-p))
          (when (jf/bash--has-glob-pattern-p glob-pattern)
            (push (list :file resolved-pattern
                       :operation :match-pattern
                       :confidence :high
                       :source :loop-glob
                       :pattern t
                       :loop-context t
                       :loop-variable loop-var)
                  loop-ops)))))

     ;; Loop source is literal - bind to literal values
     ((eq loop-source-type :literal)
      (setq loop-var-value :literal-list)))

    ;; Parse and analyze loop body with loop variable in context
    (when loop-body
      (let* ((loop-body-parsed-raw (jf/bash-parse loop-body))
             ;; Remove :ast field to prevent accessing killed buffer nodes
             (loop-body-parsed (when (plist-get loop-body-parsed-raw :success)
                                (let ((clean-parsed (copy-sequence loop-body-parsed-raw)))
                                  (setq clean-parsed (plist-put clean-parsed :ast nil))
                                  clean-parsed)))
             ;; Add loop variable to context only if it has a resolvable value
             ;; Skip keyword markers like :literal-list which can't be used in resolution
             (loop-context (if (and loop-var-value
                                   (not (keywordp loop-var-value)))
                              (append (list (cons (intern loop-var) loop-var-value))
                                      var-context)
                            var-context))
             (body-ops (when loop-body-parsed
                        (jf/bash-analyze-file-operations-recursive
                         loop-body-parsed loop-context (1+ depth)))))
        ;; Mark all loop operations with :loop-context metadata
        (when body-ops
          (dolist (op body-ops)
            (plist-put op :loop-context t)
            (plist-put op :loop-variable loop-var))
          (setq loop-ops (append loop-ops body-ops)))))

    loop-ops))

(defun jf/bash--normalize-dynamic-paths (operations)
  "Normalize dynamic file paths in OPERATIONS.

Handles heredoc + variables and command substitutions in loops.
Returns updated operations list with normalized paths.

Normalization rules:
- Heredoc + variable: \\$var -> {dynamic}
- Command substitution in loop: \\$(cmd) -> {dynamic}

Dynamic operations are marked with :dynamic t."
  (mapcar (lambda (op)
            (let ((file (plist-get op :file))
                  (has-heredoc (plist-get op :heredoc-content))
                  (in-loop (plist-get op :loop-context)))

              ;; Check if file path contains dynamic content
              (when file
                (let ((has-variables (string-match-p "\\$[a-zA-Z_][a-zA-Z0-9_]*\\|\\${[^}]+}" file))
                      (has-cmd-subst (string-match-p "\\$([^)]+)\\|`[^`]+`" file)))

                  ;; Normalize heredoc + variable paths
                  (when (and has-heredoc has-variables)
                    (plist-put op :loop-context t)
                    (plist-put op :dynamic t)
                    (let ((normalized (replace-regexp-in-string
                                      "\\$[a-zA-Z_][a-zA-Z0-9_]*\\|\\${[^}]+}"
                                      "{dynamic}"
                                      file)))
                      (plist-put op :file normalized)))

                  ;; Normalize command substitutions in loop context
                  (when (and in-loop has-cmd-subst)
                    (plist-put op :dynamic t)
                    (let ((normalized (replace-regexp-in-string
                                      "\\$([^)]+)\\|`[^`]+`"
                                      "{dynamic}"
                                      file)))
                      (plist-put op :file normalized))))))
            op)
          operations))

(defun jf/bash-analyze-file-operations-recursive (parsed-command var-context &optional depth)
  "Recursively extract file operations from PARSED-COMMAND at all nesting levels.

PARSED-COMMAND is the output from `jf/bash-parse'.
VAR-CONTEXT is an alist mapping variable names (symbols) to values (strings).
DEPTH prevents infinite recursion (default 0, max controlled by
jf/bash-recursive-max-depth). When max depth exceeded, returns error
plist with :success nil and :error describing the limit.

Returns flat list of all operations including nested operations from:
- Command substitutions (:command-substitutions with :parsed)
- Loop bodies (:loop-body-parsed if implemented)
- Conditional branches (:then-branch-parsed, :else-branch-parsed if implemented)
- Pipeline commands (:all-commands)
- Chain commands (:all-commands with variable tracking)

Operations from command substitutions are marked with :from-substitution t.

Pattern flow tracking: When a substitution produces a pattern (via find, ls, grep -l),
and the outer command operates on that pattern, a :pattern-source field is added
to link the pattern producer to the pattern consumer.

This is the core recursive engine that powers the semantic analysis system.
It coordinates with jf/bash--extract-from-single-command to extract operations
at each level, then recursively descends into nested structures.

Example:
  ;; cat $(find . -name '*.log')
  (let* ((parsed (jf/bash-parse \"cat $(find . -name '*.log')\"))
         (ops (jf/bash-analyze-file-operations-recursive parsed nil 0)))
    ops)
  => (;; Operations from find (marked :from-substitution t)
      (:file \".\" :operation :read-directory :from-substitution t ...)
      (:file \"*.log\" :operation :match-pattern :from-substitution t ...)
      ;; Operation from cat (with pattern flow)
      (:file \"*.log\" :operation :read :pattern-source (:command \"find\" ...) ...))"
  (condition-case err
      (catch 'depth-exceeded
        (let ((depth (or depth 0))
              (operations nil))

          ;; Depth check - return error plist instead of throwing for graceful degradation
          (when (>= depth jf/bash-recursive-max-depth)
            (message "Warning: Max recursion depth %d exceeded, returning error plist"
                     jf/bash-recursive-max-depth)
            (setq operations (list :error (format "Maximum recursion depth exceeded (limit: %d)"
                                                 jf/bash-recursive-max-depth)
                                  :success nil))
            ;; Return immediately with error plist
            (throw 'depth-exceeded operations))

          ;; 1. Extract operations from this command level
        (when (fboundp 'jf/bash--extract-from-single-command)
          (let ((this-level-ops (jf/bash--extract-from-single-command
                                 parsed-command var-context)))
            (setq operations (append operations this-level-ops))))

        ;; 2. Detect and recursively process nested commands (bash -c, sh -c, eval)
        (when (and (fboundp 'jf/bash-detect-command-injection)
                   (fboundp 'jf/bash--parse-nested-command)
                   (eq (plist-get parsed-command :type) :simple))
          (when-let ((injection-info (jf/bash-detect-command-injection parsed-command)))
            (let ((nested-cmd-string (plist-get injection-info :nested-command-string)))
              (when nested-cmd-string
                (let* ((nested-parsed (jf/bash--parse-nested-command
                                      nested-cmd-string (1+ depth) parsed-command))
                       (nested-ops (when (plist-get nested-parsed :success)
                                    (jf/bash-analyze-file-operations-recursive
                                     nested-parsed var-context (1+ depth)))))
                  ;; Mark all nested operations as indirect with nesting depth
                  (dolist (op nested-ops)
                    (plist-put op :indirect t)
                    (plist-put op :nesting-depth (1+ depth)))
                  (setq operations (append operations nested-ops)))))))

        ;; 3. Process command substitutions using helper
        (let* ((subst-result (jf/bash--process-command-substitutions
                             parsed-command var-context depth))
               (subst-ops (car subst-result))
               (substitution-patterns (cdr subst-result)))
          (setq operations (append operations subst-ops))

          ;; Add operations for outer command operating on substitution results
          (when substitution-patterns
            (when (fboundp 'jf/bash--extract-pattern-flow-operations)
              (let ((pattern-flow-ops (jf/bash--extract-pattern-flow-operations
                                      parsed-command substitution-patterns var-context)))
                (setq operations (append operations pattern-flow-ops))))))

        ;; 4. Process pipeline commands
        (when (eq (plist-get parsed-command :type) :pipeline)
          (dolist (cmd (plist-get parsed-command :all-commands))
            (let ((cmd-ops (jf/bash-analyze-file-operations-recursive
                           cmd var-context (1+ depth))))
              (setq operations (append operations cmd-ops)))))

        ;; 5. Process chain commands using helper
        (when (eq (plist-get parsed-command :type) :chain)
          (let ((chain-ops (jf/bash--process-chain-with-context
                           parsed-command var-context depth)))
            (setq operations (append operations chain-ops))))

        ;; 6. Process for-loops using helper
        (when (string= (plist-get parsed-command :command-name) "for")
          (let ((loop-ops (jf/bash--process-loop-with-binding
                          parsed-command var-context depth operations)))
            (setq operations (append operations loop-ops))))

        ;; 7. Process conditional branches
        (when (eq (plist-get parsed-command :type) :conditional)
          (let ((conditional-ops (jf/bash--extract-conditional-context-operations
                                 parsed-command var-context (1+ depth))))
            (setq operations (append operations conditional-ops))))

        ;; 8. Process subshells with isolated context
        (when (eq (plist-get parsed-command :type) :subshell)
          (when-let ((subshell-body (plist-get parsed-command :subshell-body)))
            (when (plist-get subshell-body :success)
              (let* ((isolated-context (copy-alist var-context))
                     (subshell-ops (jf/bash-analyze-file-operations-recursive
                                   subshell-body isolated-context (1+ depth))))
                ;; Mark all subshell operations for tracing
                (dolist (op subshell-ops)
                  (plist-put op :subshell-context t))
                (setq operations (append operations subshell-ops))))))

          ;; 9. Post-process: Normalize dynamic file paths using helper
          (setq operations (jf/bash--normalize-dynamic-paths operations))

          operations))
    (error (list :error (format "Recursive analysis error: %s" (error-message-string err))
                 :success nil))))

(defun jf/bash--extract-conditional-context-operations (parsed-conditional var-context depth)
  "Extract file operations from PARSED-CONDITIONAL with context tracking.

Processes if/then/else structures, marking operations:
- Test condition operations: :test-condition t
- Then branch operations: :conditional t :branch :then
- Else branch operations: :conditional t :branch :else

PARSED-CONDITIONAL is a parsed command with :type :conditional.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.

Returns list of operations with conditional context markers."
  (let ((operations nil)
        (condition-text (plist-get parsed-conditional :condition-text))
        (then-text (plist-get parsed-conditional :then-text))
        (else-text (plist-get parsed-conditional :else-text))
        (branch-context var-context))  ; Track context changes from test condition

    ;; Extract test condition operations and check for directory changes
    (when condition-text
      (let ((test-ops (jf/bash--extract-test-condition-operations-from-text
                      condition-text var-context depth)))
        (setq operations (append operations test-ops))

        ;; Check if test condition is a cd command - if so, update context for branches
        ;; Conservative approach: assume cd succeeds
        (when (string-match "^\\s-*cd\\s-+" condition-text)
          (when-let* ((parsed-test (jf/bash-parse condition-text))
                      (cd-parsed (when (plist-get parsed-test :success)
                                  parsed-test))
                      (current-pwd (or (alist-get 'PWD var-context) "/"))
                      (new-pwd (when (and (fboundp 'jf/bash--is-cd-command-p)
                                         (fboundp 'jf/bash--extract-cd-target)
                                         (jf/bash--is-cd-command-p cd-parsed))
                                (jf/bash--extract-cd-target cd-parsed var-context current-pwd))))
            (unless (eq new-pwd :unresolved)
              ;; Update context for then/else branches with new PWD
              (setq branch-context (cons (cons 'PWD new-pwd)
                                        (assq-delete-all 'PWD (copy-alist var-context)))))))))

    ;; Extract then branch operations with updated context
    (when then-text
      (let ((then-ops (jf/bash--extract-then-branch-operations-from-text
                      then-text branch-context depth)))
        (setq operations (append operations then-ops))))

    ;; Extract else branch operations with updated context
    (when else-text
      (let ((else-ops (jf/bash--extract-else-branch-operations-from-text
                      else-text branch-context depth)))
        (setq operations (append operations else-ops))))

    operations))

(defun jf/bash--extract-test-condition-operations-from-text (condition-text var-context depth)
  "Extract operations from test CONDITION-TEXT.

Handles two types of test conditions:
1. File test operators: [ -f file ], [[ -d dir ]]
2. Command-based tests: grep -q pattern file

All extracted operations are marked with :test-condition t.

CONDITION-TEXT is the condition text from the conditional.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.

Returns list of operations marked with :test-condition t."
  (let ((operations nil))
    ;; Check if it's a file test operator
    (if (string-match "\\[\\[?\\s-*\\(-[fderswx]\\)\\s-+\\([^]]+\\)\\s-*\\]\\]?" condition-text)
        ;; Extract file test operation
        (let* ((test-operator (match-string 1 condition-text))
               (raw-file-path (string-trim (match-string 2 condition-text)))
               ;; Strip surrounding quotes (both single and double)
               (file-path (replace-regexp-in-string
                          "^['\"]\\|['\"]$" "" raw-file-path))
               (resolved-path (if var-context
                                 (jf/bash-resolve-variables file-path var-context)
                               file-path))
               (var-resolved-path (if (stringp resolved-path)
                                     resolved-path
                                   (plist-get resolved-path :path)))
               ;; Resolve relative paths against PWD from var-context (only if PWD is set)
               (current-pwd (alist-get 'PWD var-context))
               (final-path (if (and current-pwd
                                   var-resolved-path
                                   (not (string-prefix-p "/" var-resolved-path)))
                              ;; Relative path with explicit PWD - resolve against it
                              (expand-file-name var-resolved-path current-pwd)
                            var-resolved-path))
               ;; Check for glob patterns in the unquoted path
               (has-pattern (jf/bash--has-glob-pattern-p final-path)))
          (push (append (list :file final-path
                             :operation :read-metadata
                             :confidence :high
                             :source :test-expression
                             :test-operator test-operator
                             :test-condition t)
                       (when has-pattern (list :pattern t)))
                operations))
      ;; Otherwise, parse as command and mark operations
      (when (fboundp 'jf/bash-parse)
        (let* ((parsed-test (jf/bash-parse condition-text))
               ;; Clean :ast to avoid killed buffer issues
               (clean-parsed (when (plist-get parsed-test :success)
                              (let ((cleaned (copy-sequence parsed-test)))
                                (plist-put cleaned :ast nil)
                                cleaned)))
               (test-ops (when (and clean-parsed (fboundp 'jf/bash-analyze-file-operations-recursive))
                          (jf/bash-analyze-file-operations-recursive
                           clean-parsed var-context depth))))
          ;; Mark all test command operations with :test-condition
          (dolist (op test-ops)
            (push (append op (list :test-condition t)) operations)))))
    (nreverse operations)))

(defun jf/bash--extract-then-branch-operations-from-text (then-text var-context depth)
  "Extract operations from THEN-TEXT.

All extracted operations are marked with :conditional t and :branch :then.

THEN-TEXT is the then branch text from the conditional.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.

Returns list of operations marked with :conditional t :branch :then."
  (let ((operations nil))
    (when (fboundp 'jf/bash-parse)
      (let* ((parsed-then (jf/bash-parse then-text))
             ;; Clean :ast to avoid killed buffer issues
             (clean-parsed (when (plist-get parsed-then :success)
                            (let ((cleaned (copy-sequence parsed-then)))
                              (plist-put cleaned :ast nil)
                              cleaned)))
             (then-ops (when (and clean-parsed (fboundp 'jf/bash-analyze-file-operations-recursive))
                        (jf/bash-analyze-file-operations-recursive
                         clean-parsed var-context depth))))
        ;; Mark all then branch operations
        (dolist (op then-ops)
          (push (append op (list :conditional t :branch :then)) operations))))
    (nreverse operations)))

(defun jf/bash--extract-else-branch-operations-from-text (else-text var-context depth)
  "Extract operations from ELSE-TEXT if present.

All extracted operations are marked with :conditional t and :branch :else.

ELSE-TEXT is the else branch text from the conditional.
VAR-CONTEXT is variable bindings alist.
DEPTH is current recursion depth.

Returns list of operations marked with :conditional t :branch :else."
  (let ((operations nil))
    (when (fboundp 'jf/bash-parse)
      (let* ((parsed-else (jf/bash-parse else-text))
             ;; Clean :ast to avoid killed buffer issues
             (clean-parsed (when (plist-get parsed-else :success)
                            (let ((cleaned (copy-sequence parsed-else)))
                              (plist-put cleaned :ast nil)
                              cleaned)))
             (else-ops (when (and clean-parsed (fboundp 'jf/bash-analyze-file-operations-recursive))
                        (jf/bash-analyze-file-operations-recursive
                         clean-parsed var-context depth))))
        ;; Mark all else branch operations
        (dolist (op else-ops)
          (push (append op (list :conditional t :branch :else)) operations))))
    (nreverse operations)))

(defun jf/bash--extract-pattern-flow-operations (parsed-command substitution-patterns var-context)
  "Create operations for outer command consuming pattern from substitution.

PARSED-COMMAND: Top-level parsed command containing substitution
SUBSTITUTION-PATTERNS: List of patterns produced by substitutions
  Each element is a plist with:
    :substitution-content - Original $(command) string
    :pattern - File pattern from command (e.g. '*.log')
    :search-scope - Directory being searched (e.g. '.')
    :command - Command that produced pattern (e.g. 'find')
VAR-CONTEXT: Variable bindings

Returns list of operations for outer command operating on substitution patterns."
  (let ((operations nil)
        (command-name (plist-get parsed-command :command-name))
        (positional-args (plist-get parsed-command :positional-args)))

    ;; For each positional arg containing a substitution
    (dolist (arg positional-args)
      (when (and arg (string-prefix-p "$(" arg))
        ;; Find matching substitution pattern
        (when-let ((pattern-info (seq-find
                                  (lambda (p)
                                    (string-match-p
                                     (regexp-quote (plist-get p :substitution-content))
                                     arg))
                                  substitution-patterns)))
          ;; Create operation using pattern instead of literal
          (let* ((pattern (plist-get pattern-info :pattern))
                 (operation (jf/bash--infer-operation-from-command
                            command-name pattern)))
            ;; Only create operation if command actually operates on files
            (when (and pattern operation)
              (push (list :file pattern
                         :operation operation
                         :confidence :high
                         :source :positional-arg
                         :command command-name
                         :pattern t
                         :from-substitution t
                         :pattern-source pattern-info)
                    operations))))))

    (nreverse operations)))

(defun jf/bash--infer-operation-from-command (command-name file-path)
  "Infer operation type from COMMAND-NAME and FILE-PATH context.

Returns operation type symbol (:read, :write, :delete, :modify) based on
common command semantics. This is used for pattern flow when we need to
determine what operation the outer command performs on the pattern.

Returns nil for commands that don't operate on files (echo, printf, etc).

Examples:
  (jf/bash--infer-operation-from-command \"cat\" \"*.log\")    => :read
  (jf/bash--infer-operation-from-command \"rm\" \"*.tmp\")     => :delete
  (jf/bash--infer-operation-from-command \"chmod\" \"*.sh\")   => :modify
  (jf/bash--infer-operation-from-command \"echo\" \"*.log\")   => nil"
  (when (and command-name
             ;; Don't try to intern assignment strings like "DIR=/tmp"
             (not (string-match-p "=" command-name)))
    (let ((cmd (intern command-name)))
      (cond
       ;; Read operations
       ((memq cmd '(cat head tail less more grep egrep fgrep wc file stat))
        :read)

       ;; Write operations
       ((memq cmd '(tee cp mv))
        :write)

       ;; Delete operations
       ((memq cmd '(rm))
        :delete)

       ;; Modify operations
       ((memq cmd '(chmod chown chgrp touch sed awk))
        :modify)

       ;; Compress/archive operations
       ((memq cmd '(gzip gunzip bzip2 bunzip2 xz unxz tar zip unzip))
        :compress)

       ;; Default - not a file operation
       (t nil)))))

(provide 'bash-parser-recursive)
;;; bash-parser-recursive.el ends here
