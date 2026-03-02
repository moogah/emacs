;;; bash-parser-core.el --- Core bash parsing functions -*- lexical-binding: t; -*-

(require 'treesit)

(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING using tree-sitter with full pipeline/chain support.

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
  :dangerous-p - t if command matches dangerous patterns

For pipelines/chains:
  Each element in :all-commands has the same structure as simple commands

Additional fields:
  :ast - tree-sitter root node (for debugging)
  :error - error message if parsing failed"
  (condition-case err
      (jf/bash-parse--internal command-string)
    (error (list :success nil
                 :error (error-message-string err)))))

(defun jf/bash-parse--internal (command-string)
  "Internal parser implementation for COMMAND-STRING with pipeline support."
  (with-temp-buffer
    (insert command-string)
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser))
           (struct-type (jf/bash-parse--detect-structure-type root-node)))

      (pcase struct-type
        (:pipeline
         (jf/bash-parse--handle-pipeline root-node))
        (:list
         (jf/bash-parse--handle-list root-node))
        (:simple
         (jf/bash-parse--handle-simple-command root-node))
        (_
         (list :success nil
               :error "Unknown command structure"))))))

(defun jf/bash-parse--detect-structure-type (root-node)
  "Detect the structure type from ROOT-NODE.
Returns :pipeline, :list, or :simple."
  (let ((pipeline-node (jf/bash-parse--find-node-by-type root-node "pipeline"))
        (list-node (jf/bash-parse--find-node-by-type root-node "list")))
    (cond
     (pipeline-node :pipeline)
     (list-node :list)
     ;; Check if program has multiple command children (semicolon-separated)
     ((> (jf/bash-parse--count-command-children root-node) 1) :list)
     (t :simple))))

(defun jf/bash-parse--count-command-children (node)
  "Count how many command or redirected_statement children NODE has."
  (let ((count 0))
    (dotimes (i (treesit-node-child-count node))
      (when-let ((child (treesit-node-child node i)))
        (let ((child-type (treesit-node-type child)))
          (when (or (string= child-type "command")
                    (string= child-type "redirected_statement"))
            (setq count (1+ count))))))
    count))

(defun jf/bash-parse--find-node-by-type (node target-type)
  "Find first node of TARGET-TYPE in tree starting from NODE."
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

(defun jf/bash-parse--handle-pipeline (root-node)
  "Handle pipeline command from ROOT-NODE."
  (let* ((pipeline-node (jf/bash-parse--find-node-by-type root-node "pipeline"))
         (command-nodes (jf/bash-parse--get-all-command-nodes pipeline-node))
         (parsed-commands (mapcar #'jf/bash-parse--parse-single-command-node
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

(defun jf/bash-parse--handle-list (root-node)
  "Handle command list/chain from ROOT-NODE."
  (let* ((list-node (jf/bash-parse--find-node-by-type root-node "list"))
         ;; Use list-node if found, otherwise use root-node for semicolon-separated commands
         (container-node (or list-node root-node))
         (command-nodes (jf/bash-parse--get-all-command-nodes container-node))
         (parsed-commands (mapcar #'jf/bash-parse--parse-single-command-node
                                   command-nodes))
         (any-dangerous (seq-some (lambda (cmd) (plist-get cmd :dangerous-p))
                                  parsed-commands)))

    (list :success t
          :type :chain
          :all-commands parsed-commands
          :command-count (length parsed-commands)
          :dangerous-p any-dangerous
          :ast root-node)))

(defun jf/bash-parse--handle-simple-command (root-node)
  "Handle simple (single) command from ROOT-NODE."
  (let* ((command-or-redir (or (jf/bash-parse--find-node-by-type root-node "redirected_statement")
                               (jf/bash-parse--find-node-by-type root-node "command"))))

    (if (null command-or-redir)
        (list :success nil
              :error "No command found in input")

      (let ((parsed-cmd (jf/bash-parse--parse-single-command-node command-or-redir)))
        ;; Return flattened structure for backward compatibility
        (append (list :success t
                      :type :simple
                      :all-commands (list parsed-cmd)
                      :command-count 1
                      :ast root-node)
                ;; Flatten first command's fields to top level
                parsed-cmd)))))

(defun jf/bash-parse--get-all-command-nodes (container-node)
  "Get all command nodes from CONTAINER-NODE (pipeline or list).
Returns command, redirected_statement, or variable_assignment nodes."
  (let ((commands '()))
    (jf/bash-parse--visit-node
     container-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (when (or (string= node-type "command")
                   (string= node-type "redirected_statement")
                   (string= node-type "variable_assignment"))
           (push node commands)))))
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
                       :dangerous-p is-dangerous)))
      (when redirections
        (setq result (plist-put result :redirections redirections)))
      result)))

(defun jf/bash-parse--parse-single-command-node (command-or-statement-node)
  "Parse COMMAND-OR-STATEMENT-NODE which may be command, redirected_statement, or variable_assignment.
Returns command structure with optional :redirections field."
  (let ((command-node nil)
        (redirections nil))

    ;; Check if this is a redirected_statement wrapper
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

    ;; Handle variable_assignment nodes specially
    (if (string= (treesit-node-type command-node) "variable_assignment")
        (let* ((name-node (treesit-node-child-by-field-name command-node "name"))
               (value-node (treesit-node-child-by-field-name command-node "value"))
               (var-name (treesit-node-text name-node))
               (var-value (treesit-node-text value-node))
               (assignment-str (format "%s=%s" var-name var-value)))
          ;; Return as a simple command with assignment in command-name
          (list :command-name assignment-str
                :subcommand nil
                :flags nil
                :positional-args nil
                :dangerous-p nil))

      ;; Parse regular command
      (let* ((words (jf/bash-parse--extract-words command-node))
             (command-name (car words))
             (remaining-words (cdr words))
             (subcommand (jf/bash-parse--detect-subcommand command-name remaining-words))
             (args-start (if subcommand (cdr remaining-words) remaining-words)))

      ;; Check for wrapper commands (sudo, env, time, etc.)
      (if-let ((wrapper-spec (alist-get (intern command-name)
                                        jf/bash-parser-wrapper-commands)))
          (jf/bash-parse--parse-wrapper-command command-name wrapper-spec remaining-words redirections)

        ;; Check for find command with -exec blocks
        (if (and (string= command-name "find")
                 (or (member "-exec" args-start)
                     (member "-execdir" args-start)))
            (jf/bash-parse--parse-find-with-exec command-name args-start redirections)

          ;; Normal command processing
          (let* ((flags (jf/bash-parse--extract-flags args-start))
                 (positional-args (jf/bash-parse--extract-positional-args args-start))
                 (dangerous-p (jf/bash-parse--is-dangerous command-name subcommand flags)))

            ;; Build result, only include redirections if present
            (let ((result (list :command-name command-name
                               :subcommand subcommand
                               :flags flags
                               :positional-args positional-args
                               :dangerous-p dangerous-p)))
              (when redirections
                (setq result (plist-put result :redirections redirections)))
              result))))))))

(defun jf/bash-parse--extract-words (command-node)
  "Extract all word nodes from COMMAND-NODE as strings.
Returns list of strings representing all words in the command."
  (let ((words '()))
    (jf/bash-parse--visit-node
     command-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (cond
          ;; Terminal nodes: extract text and skip children to prevent duplication
          ((or (string= node-type "concatenation")
               (string= node-type "string")
               (string= node-type "raw_string")
               (string= node-type "ansi_c_string")
               (string= node-type "simple_expansion")
               (string= node-type "expansion"))
           (let ((text (treesit-node-text node t)))
             (when (and text (> (length text) 0))
               (setq text (string-trim text))
               ;; Remove surrounding quotes for string types (not concatenation/expansion)
               (when (and (> (length text) 1)
                         (not (member node-type '("concatenation" "simple_expansion" "expansion")))
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
  "Visit NODE and all children, calling VISITOR-FN on each.
VISITOR-FN can return :skip-children to prevent recursion into children."
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
  (when-let ((patterns (alist-get (intern command-name)
                                   jf/bash-parser-dangerous-patterns)))
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
