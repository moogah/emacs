;;; bash-parser.el --- Parse bash commands with tree-sitter -*- lexical-binding: t; -*-

(require 'treesit)

(defvar jf/bash-parser-dangerous-patterns
  '((rm . ((:flags ("-rf" "-fr" "-Rf" "-fR" "-r"))
           (:any-flag-contains ("f" "r"))))
    (git . ((:subcommand "push" :flags ("--force" "-f"))
            (:subcommand "reset" :flags ("--hard"))
            (:subcommand "clean" :flags ("-f" "-fd" "-df"))))
    (docker . ((:subcommand "rm" :flags ("-f" "--force"))))
    (npm . ((:subcommand "uninstall" :flags ("--force"))
            (:subcommand "install" :flags ("--force"))))
    (python . ((:flags ("-c"))
               (:any-flag-contains ("c"))))
    (bash . ((:flags ("-c"))
             (:any-flag-contains ("c"))))
    (sh . ((:flags ("-c"))
           (:any-flag-contains ("c")))))
  "Database of dangerous command patterns.
Format: (command . ((:subcommand \"name\" :flags (list))
                    (:flags (list))
                    (:any-flag-contains (list))))")

(defvar jf/bash-parser-wrapper-commands
  '((sudo . (:flags-with-args ("-u" "-g" "-C" "--close-from" "-D" "-h" "-p" "-R" "-r" "-t" "-T" "-U")
             :flags-no-args ("-A" "-b" "-E" "-e" "-H" "-i" "-K" "-k" "-l" "-n" "-P" "-S" "-s" "-V" "-v")
             :dangerous t))
    (env . (:flags-with-args ("-C" "--chdir" "-S" "--split-string" "-u" "--unset")
            :flags-no-args ("-i" "--ignore-environment" "-0" "--null" "-v" "--debug")
            :dangerous nil))
    (time . (:flags-with-args ("-f" "--format" "-o" "--output")
             :flags-no-args ("-a" "--append" "-v" "--verbose" "-p" "--portability")
             :dangerous nil))
    (nice . (:flags-with-args ("-n" "--adjustment")
             :flags-no-args ()
             :dangerous nil))
    (nohup . (:flags-with-args ()
              :flags-no-args ()
              :dangerous nil)))
  "Database of wrapper commands that execute other commands.
Format: (command . (:flags-with-args (list-of-flags-that-take-arguments)
                    :flags-no-args (list-of-flags-without-arguments)
                    :dangerous t-or-nil))")

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
  "Parse COMMAND-OR-STATEMENT-NODE which may be command or redirected_statement.
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

    ;; Parse command as before
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
              result)))))))

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

(defun jf/bash--glob-segment-to-regex (glob-segment)
  "Convert GLOB-SEGMENT to regex pattern.
Handles *, ?, and [abc] character classes.
Does not handle ** (that's handled at segment level).
Escapes regex special characters."
  (let ((regex "")
        (i 0)
        (len (length glob-segment)))
    (while (< i len)
      (let ((ch (aref glob-segment i)))
        (cond
         ;; Single-char wildcard
         ((eq ch ??)
          (setq regex (concat regex "."))
          (setq i (1+ i)))

         ;; Any-chars wildcard (within segment)
         ((eq ch ?*)
          (setq regex (concat regex "[^/]*"))
          (setq i (1+ i)))

         ;; Character class [abc]
         ((eq ch ?\[)
          (let ((class-start i)
                (class-end nil))
            ;; Find matching ]
            (setq i (1+ i))
            (while (and (< i len) (null class-end))
              (when (eq (aref glob-segment i) ?\])
                (setq class-end i))
              (setq i (1+ i)))
            (if class-end
                ;; Valid character class found
                (setq regex (concat regex
                                   (substring glob-segment class-start (1+ class-end))))
              ;; No closing ], treat [ as literal
              (setq regex (concat regex "\\["))
              (setq i (1+ class-start)))))

         ;; Escape regex special characters
         ((memq ch '(?\\ ?. ?+ ?^ ?$ ?\( ?\) ?{ ?} ?|))
          (setq regex (concat regex "\\" (char-to-string ch)))
          (setq i (1+ i)))

         ;; Regular character
         (t
          (setq regex (concat regex (char-to-string ch)))
          (setq i (1+ i))))))
    regex))

(defun jf/bash--match-segments (path-segments pattern-segments)
  "Match PATH-SEGMENTS against PATTERN-SEGMENTS recursively.
Handles ** consuming 0-to-N segments. Returns t if match, nil otherwise."
  (cond
   ;; Both empty - successful match
   ((and (null path-segments) (null pattern-segments))
    t)

   ;; Pattern empty but path has segments - no match
   ((null pattern-segments)
    nil)

   ;; Path empty but pattern has non-** segments - no match
   ((null path-segments)
    ;; Only match if remaining patterns are all **
    (seq-every-p (lambda (seg) (string= seg "**")) pattern-segments))

   ;; Handle ** pattern (matches 0-to-N segments)
   ((string= (car pattern-segments) "**")
    (let ((rest-pattern (cdr pattern-segments)))
      (or
       ;; Try matching ** as zero segments (skip it)
       (jf/bash--match-segments path-segments rest-pattern)
       ;; Try matching ** as one or more segments (consume one path segment)
       (jf/bash--match-segments (cdr path-segments) pattern-segments))))

   ;; Handle regular segment matching
   (t
    (let* ((path-seg (car path-segments))
           (pattern-seg (car pattern-segments))
           (regex-pattern (concat "\\`" (jf/bash--glob-segment-to-regex pattern-seg) "\\'"))
           (segment-matches (string-match-p regex-pattern path-seg)))
      ;; Current segment must match, then recurse on rest
      (and segment-matches
           (jf/bash--match-segments (cdr path-segments) (cdr pattern-segments)))))))

(defun jf/bash-glob-match-p (path pattern)
  "Test if PATH matches PATTERN using glob semantics.
No filesystem access - purely string-based matching.

Supported patterns:
  *     - matches any characters within a segment (doesn't cross /)
  **    - matches zero or more complete segments (recursive)
  ?     - matches exactly one character
  [abc] - matches any character in the set

Examples:
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/*.txt\")
    => t
  (jf/bash-glob-match-p \"/workspace/src/foo.el\" \"/workspace/**/*.el\")
    => t
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/**/file.txt\")
    => t (** matches zero segments)
  (jf/bash-glob-match-p \"/tmp/foo.el\" \"/workspace/**/*.el\")
    => nil

Returns t if PATH matches PATTERN, nil otherwise."
  (let ((path-segments (split-string path "/" t))
        (pattern-segments (split-string pattern "/" t)))
    (jf/bash--match-segments path-segments pattern-segments)))

(defun jf/bash-match-rule (file-path rules)
  "Find first rule in RULES matching FILE-PATH.
Returns the matched rule plist or nil if no rules match.

Uses first-match semantics: rules are evaluated in order,
the first rule with any matching pattern is returned.

FILE-PATH is matched against each pattern in each rule using
glob pattern matching (see `jf/bash-glob-match-p').

Example:
  (defvar my-rules
    \\='((:patterns (\"/workspace/temp/**\")
        :operations (:read :write :delete))
      (:patterns (\"/workspace/**\")
        :operations (:read))))

  (jf/bash-match-rule \"/workspace/temp/file.txt\" my-rules)
    => (:patterns (\"/workspace/temp/**\")
        :operations (:read :write :delete))

  (jf/bash-match-rule \"/workspace/src/foo.el\" my-rules)
    => (:patterns (\"/workspace/**\")
        :operations (:read))

  (jf/bash-match-rule \"/etc/passwd\" my-rules)
    => nil"
  (cl-loop for rule in rules
           when (cl-loop for pattern in (plist-get rule :patterns)
                         thereis (jf/bash-glob-match-p file-path pattern))
           return rule))

(defun jf/bash-check-operation-permission (operation matched-rule)
  "Check if OPERATION is allowed by MATCHED-RULE.
Returns nil if allowed, or a violation plist if denied.

OPERATION is a plist with :file and :operation keys.
MATCHED-RULE is a rule plist from `jf/bash-match-rule', or nil.

Denial conditions:
  1. No rule matched (path not in allowlist)
  2. Rule's :operations is not :all AND operation type not in list

Violation plist contains:
  :file - The file path
  :operation - The operation type (:read, :write, :delete, etc.)
  :matched-rule - The rule that was evaluated (or nil)
  :reason - Human-readable explanation

Examples:
  ;; Allowed operation
  (jf/bash-check-operation-permission
    \\='(:file \"/workspace/file.txt\" :operation :read)
    \\='(:patterns (\"/workspace/**\") :operations (:read :write)))
  => nil

  ;; Allowed: :all permits any operation
  (jf/bash-check-operation-permission
    \\='(:file \"/tmp/file.txt\" :operation :delete)
    \\='(:patterns (\"/tmp/**\") :operations :all))
  => nil

  ;; Denied: operation not in rule
  (jf/bash-check-operation-permission
    \\='(:file \"/workspace/file.txt\" :operation :delete)
    \\='(:patterns (\"/workspace/**\") :operations (:read :write)))
  => (:file \"/workspace/file.txt\"
      :operation :delete
      :matched-rule (:patterns (\"/workspace/**\") :operations (:read :write))
      :reason \"Operation delete not allowed (rule permits: (:read :write))\")

  ;; Denied: no rule matched
  (jf/bash-check-operation-permission
    \\='(:file \"/etc/passwd\" :operation :read)
    nil)
  => (:file \"/etc/passwd\"
      :operation :read
      :matched-rule nil
      :reason \"No allowlist rule matches this file path\")"
  (let ((file (plist-get operation :file))
        (op-type (plist-get operation :operation))
        (allowed-ops (plist-get matched-rule :operations)))
    (cond
     ;; No rule matched = violation (fail-safe: deny by default)
     ((null matched-rule)
      (list :file file
            :operation op-type
            :matched-rule nil
            :reason "No allowlist rule matches this file path"))

     ;; Rule allows all operations
     ((eq allowed-ops :all)
      nil)  ; Allowed

     ;; Operation not in allowed list = violation
     ((not (memq op-type allowed-ops))
      (list :file file
            :operation op-type
            :matched-rule matched-rule
            :reason (format "Operation %s not allowed (rule permits: %s)"
                           op-type allowed-ops)))

     ;; Allowed
     (t nil))))

(defun jf/bash-contains-cd-command-p (command-string)
  "Return t if COMMAND-STRING contains a cd command.

Detects cd in various contexts:
  - Simple command: \"cd /tmp\"
  - With flags: \"cd -P /tmp\"
  - In pipelines: \"cd /tmp && ls\"
  - In chains: \"cd /tmp; ls\"
  - Builtin invocation: \"builtin cd /tmp\"

Does NOT match:
  - cd as part of another word: \"cdrom\", \"abcd\"
  - cd in strings (would be caught by parser)

Returns t if cd command found, nil otherwise."
  (let ((case-fold-search nil))  ; Case-sensitive matching
    (string-match-p "\\(?:^\\|[;&|]\\|\\s-\\)\\(?:builtin\\s-+\\)?cd\\(?:\\s-\\|$\\)"
                    command-string)))

(defun jf/bash-operation-has-unresolved-vars-p (operation)
  "Return t if OPERATION contains unresolved variable references.

An operation has unresolved variables if:
  - The :file path contains shell variable syntax ($VAR, ${VAR})
  - The :unresolved flag is set to t

Operations with unresolved variables cannot be reliably validated
and should be reported as unhandled."
  (or (plist-get operation :unresolved)
      (let ((file (plist-get operation :file)))
        (and (stringp file)
             (string-match-p "\\$\\(?:{[^}]+}\\|[A-Za-z_][A-Za-z0-9_]*\\)" file)))))

(defun jf/bash-sandbox-check (command-string rules &optional var-context indirect-policy)
  "Validate COMMAND-STRING against sandbox RULES.

This is the main entry point for security validation. It coordinates
the complete validation pipeline and returns a comprehensive result.

Arguments:
  COMMAND-STRING - The bash command to validate
  RULES - List of security rule plists (from `jf/bash-match-rule')
  VAR-CONTEXT - Optional variable resolution context (future use)
  INDIRECT-POLICY - How to handle indirect operations (:strict, :warn, :permissive)
                    :strict - Reject all indirect operations as violations
                    :warn - Flag indirect operations as unhandled
                    :permissive (default) - Validate indirect operations normally

Returns validation result plist:
  :allowed - t if command is safe, nil if denied
  :command - Original command string (for reference)
  :operations - All extracted file operations (for debugging)
  :violations - List of security violations (non-empty => denied)
  :unhandled - Operations that couldn't be validated
  :cd-detected - t if cd command was detected

Validation logic:
  1. Reject if cd command detected
  2. Parse command to structured form
  3. Extract file operations from parsed command
  4. For each operation:
     - Check indirect policy (if operation has :indirect t)
     - Check for unresolved variables → unhandled
     - Match against security rules
     - Validate operation permission
     - Collect violations or mark as unhandled
  5. Allow only if no violations AND no unhandled operations

Examples:
  ;; Safe command
  (jf/bash-sandbox-check
    \"cat /workspace/file.txt\"
    \\='((:patterns (\"/workspace/**\") :operations (:read :write))))
  => (:allowed t
      :command \"cat /workspace/file.txt\"
      :operations ((:file \"/workspace/file.txt\" :operation :read ...))
      :violations nil
      :unhandled nil
      :cd-detected nil)

  ;; Denied: operation not allowed
  (jf/bash-sandbox-check
    \"rm /workspace/file.txt\"
    \\='((:patterns (\"/workspace/**\") :operations (:read))))
  => (:allowed nil
      :command \"rm /workspace/file.txt\"
      :operations ((:file \"/workspace/file.txt\" :operation :delete ...))
      :violations ((:file \"/workspace/file.txt\"
                    :operation :delete
                    :reason \"Operation delete not allowed...\"))
      :unhandled nil
      :cd-detected nil)

  ;; Denied: unresolved variable
  (jf/bash-sandbox-check
    \"cat $FILE\"
    \\='((:patterns (\"/workspace/**\") :operations (:read))))
  => (:allowed nil
      :command \"cat $FILE\"
      :operations ((:file \"$FILE\" :operation :read ...))
      :violations nil
      :unhandled ((:file \"$FILE\"
                   :operation :read
                   :reason \"Unresolved variable reference\"))
      :cd-detected nil)

  ;; Denied: cd command
  (jf/bash-sandbox-check
    \"cd /tmp && cat file.txt\"
    \\='((:patterns (\"/**\") :operations (:read))))
  => (:allowed nil
      :command \"cd /tmp && cat file.txt\"
      :operations nil
      :violations ((:reason \"cd command not allowed in sandbox\"))
      :unhandled nil
      :cd-detected t)

  ;; Denied: indirect operation with strict policy
  (jf/bash-sandbox-check
    \"bash -c 'rm /workspace/file.txt'\"
    \\='((:patterns (\"/workspace/**\") :operations (:read :write :delete)))
    nil :strict)
  => (:allowed nil
      :command \"bash -c 'rm /workspace/file.txt'\"
      :operations ((:file \"/workspace/file.txt\" :operation :delete :indirect t ...))
      :violations ((:file \"/workspace/file.txt\"
                    :operation :delete
                    :reason \"Indirect operation not allowed (strict policy)\"))
      :unhandled nil
      :cd-detected nil)

  ;; Warned: indirect operation with warn policy
  (jf/bash-sandbox-check
    \"bash -c 'cat /workspace/file.txt'\"
    \\='((:patterns (\"/workspace/**\") :operations (:read :write)))
    nil :warn)
  => (:allowed nil
      :command \"bash -c 'cat /workspace/file.txt'\"
      :operations ((:file \"/workspace/file.txt\" :operation :read :indirect t ...))
      :violations nil
      :unhandled ((:file \"/workspace/file.txt\"
                   :operation :read
                   :reason \"Indirect operation (flagged by warn policy)\"))
      :cd-detected nil)"
  (let ((violations nil)
        (unhandled nil)
        (operations nil)
        (cd-detected nil))

    ;; Check for cd command (immediate rejection)
    (if (jf/bash-contains-cd-command-p command-string)
        (setq cd-detected t
              violations (list (list :reason "cd command not allowed in sandbox - use absolute paths instead"
                                    :command command-string)))

      ;; Normal validation pipeline
      (let ((parsed (jf/bash-parse command-string)))
        (when (plist-get parsed :success)
          (setq operations (jf/bash-extract-file-operations parsed var-context))

          ;; Validate each operation
          (dolist (op operations)
            (cond
             ;; Indirect operation policy handling (check before other validations)
             ((and (plist-get op :indirect)
                   (eq indirect-policy :strict))
              ;; Strict policy: reject all indirect operations
              (push (list :file (plist-get op :file)
                         :operation (plist-get op :operation)
                         :reason "Indirect operation not allowed (strict policy)")
                    violations))

             ((and (plist-get op :indirect)
                   (eq indirect-policy :warn))
              ;; Warn policy: flag indirect operations as unhandled
              (push (plist-put (copy-sequence op)
                              :reason "Indirect operation (flagged by warn policy)")
                    unhandled))

             ;; Unresolved variables → unhandled
             ((jf/bash-operation-has-unresolved-vars-p op)
              (push (plist-put (copy-sequence op)
                              :reason "Unresolved variable reference")
                    unhandled))

             ;; Low confidence → unhandled
             ((eq (plist-get op :confidence) :low)
              (push (plist-put (copy-sequence op)
                              :reason "Low confidence operation detection")
                    unhandled))

             ;; Normal validation: match rule and check permission
             ;; (includes permissive indirect operations - no special handling)
             (t
              (let* ((file (plist-get op :file))
                     (rule (jf/bash-match-rule file rules))
                     (violation (jf/bash-check-operation-permission op rule)))
                (when violation
                  (push violation violations)))))))))

    ;; Return comprehensive result
    (list :allowed (and (null violations) (null unhandled))
          :command command-string
          :operations operations
          :violations (nreverse violations)
          :unhandled (nreverse unhandled)
          :cd-detected cd-detected)))

(defvar jf/bash-command-file-semantics
  '((cat . (:operations ((:source :positional-args :operation :read))))
    (head . (:operations ((:source :positional-args :operation :read))))
    (tail . (:operations ((:source :positional-args :operation :read))))
    (less . (:operations ((:source :positional-args :operation :read))))
    (more . (:operations ((:source :positional-args :operation :read))))
    (wc . (:operations ((:source :positional-args :operation :read))))
    (file . (:operations ((:source :positional-args :operation :read))))
    (stat . (:operations ((:source :positional-args :operation :read))))
    (grep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (egrep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (fgrep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (touch . (:operations ((:source :positional-args :operation :create-or-modify))))
    (mkdir . (:operations ((:source :positional-args :operation :create))))
    (rm . (:operations ((:source :positional-args :operation :delete))))
    (rmdir . (:operations ((:source :positional-args :operation :delete))))
    (chmod . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (chown . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (chgrp . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (ln . (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
                        (:source :positional-args :index -1 :operation :write))))
    (cp . (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
                        (:source :positional-args :index -1 :operation :write))))
    (mv . (:operations ((:source :positional-args :indices (0 . -2) :operation :delete)
                        (:source :positional-args :index -1 :operation :write))))
    (tar . (:operations :flag-dependent
            :flag-handlers ((("-x" "--extract" "--get") . ((:source :positional-args :operation :write)))
                           (("-c" "--create") . ((:source :positional-args :operation :read))))))
    (sed . (:operations :flag-dependent
            :flag-handlers ((("-i" "--in-place") . ((:source :positional-args :operation :modify :skip-indices (0))))
                           (() . ((:source :positional-args :operation :read :skip-indices (0)))))))
    (find . (:operations ((:source :positional-args :operation :read))))
    (tee . (:operations ((:source :positional-args :operation :write))))
    (dd . (:operations ((:source :named-args :names ("if") :operation :read)
                        (:source :named-args :names ("of") :operation :write))))
    (git . (:operations :complex
            :subcommand-handlers ((add . ((:source :positional-args :operation :read)))
                                 (rm . ((:source :positional-args :operation :delete)))
                                 (checkout . ((:source :positional-args :operation :modify)))
                                 (restore . ((:source :positional-args :operation :modify)))
                                 (mv . ((:source :positional-args :indices (0 . -2) :operation :delete)
                                        (:source :positional-args :index -1 :operation :write)))
                                 (diff . ((:source :positional-args :operation :read)))
                                 (show . ((:source :positional-args :operation :read))))))
    (docker . (:operations :complex
               :subcommand-handlers ((cp . ((:source :positional-args :indices (0 . -2) :operation :read)
                                            (:source :positional-args :index -1 :operation :write))))))
    (npm . (:operations :complex
            :subcommand-handlers ()))
    (cargo . (:operations :complex
              :subcommand-handlers ()))
    (kubectl . (:operations :complex
                :subcommand-handlers ())))
  "Database mapping command names to file operation semantics.

Each entry maps a command symbol to a plist describing how the command
interacts with files:

Simple commands:
  (:operations ((operation-spec) ...))

Flag-dependent commands (operation depends on flags):
  (:operations :flag-dependent
   :flag-handlers ((flag-list . operations) ...))

Subcommand-based commands (operation depends on subcommand):
  (:operations :complex
   :subcommand-handlers ((subcommand . operations) ...))

Operation spec format:
  :source - Where file paths come from:
    :positional-args - From positional arguments
    :named-args - From named arguments (like dd if=/path of=/path)
    :redirections - From shell redirections (handled separately)
    :exec-blocks - From find -exec blocks (handled separately)

  :operation - Type of file operation:
    :read - Read file contents
    :write - Create new file or overwrite existing
    :delete - Remove file or directory
    :modify - Change existing file in place
    :create - Create new file (must not exist)
    :create-or-modify - Create or update file

  :index - Single positional argument index (0-based, -1 for last)
  :indices - Range of positional arguments:
    (0 . -2) means all arguments from first to second-to-last
    (0 . -1) means all arguments from first to last
    :all means all positional arguments

  :skip-indices - List of argument indices to skip (e.g., (0) skips first arg)
  :names - List of named argument names (for dd-style commands)")

(defun jf/bash-lookup-command-semantics (command-name)
  "Look up file operation semantics for COMMAND-NAME.

Returns semantics plist from `jf/bash-command-file-semantics', or nil
if command is not in the database.

The returned plist describes how the command interacts with files:
- :operations - Operation specifications or :flag-dependent/:complex
- :flag-handlers - For flag-dependent commands
- :subcommand-handlers - For subcommand-based commands

Example return values:

Simple command:
  (:operations ((:source :positional-args :operation :read)))

Flag-dependent command:
  (:operations :flag-dependent
   :flag-handlers (((\"-i\") . ((:source :positional-args :operation :modify)))))

Subcommand-based command:
  (:operations :complex
   :subcommand-handlers ((add . ((:source :positional-args :operation :read)))))"
  (when command-name
    (alist-get (intern command-name) jf/bash-command-file-semantics)))

(defun jf/bash-detect-variable-references (file-path)
  "Detect variable references in FILE-PATH.

Identifies both simple $VAR and braced ${VAR} syntax without resolving them.
Returns cons cell (HAS-VARS . VAR-NAMES) where:
  HAS-VARS - t if any variables found, nil otherwise
  VAR-NAMES - list of variable names (strings) in order of appearance

Supported patterns:
  $VAR       - Simple variable reference
  ${VAR}     - Braced variable reference
  $A/$B/$C   - Multiple variables in path

Examples:
  (jf/bash-detect-variable-references \"/workspace/file.txt\")
    => (nil)
  (jf/bash-detect-variable-references \"$WORKSPACE/file.txt\")
    => (t . (\"WORKSPACE\"))
  (jf/bash-detect-variable-references \"${TEMP_DIR}/output.txt\")
    => (t . (\"TEMP_DIR\"))
  (jf/bash-detect-variable-references \"$SRC/$FILE.txt\")
    => (t . (\"SRC\" \"FILE\"))

Variable names match pattern: [A-Za-z_][A-Za-z0-9_]*"
  (let ((var-names nil))
    (save-match-data
      ;; Match ${VAR} or $VAR patterns
      ;; Group 1: braced variable (${VAR})
      ;; Group 2: simple variable ($VAR)
      (let ((pos 0))
        (while (string-match "\\${\\([A-Za-z_][A-Za-z0-9_]*\\)}\\|\\$\\([A-Za-z_][A-Za-z0-9_]*\\)"
                            file-path pos)
          (let ((var-name (or (match-string 1 file-path)
                             (match-string 2 file-path))))
            (push var-name var-names))
          (setq pos (match-end 0)))))
    ;; Return cons cell: (has-vars . var-names)
    (if var-names
        (cons t (nreverse var-names))
      (cons nil nil))))

(defun jf/bash-resolve-variables (file-path var-context)
  "Resolve variables in FILE-PATH using VAR-CONTEXT.

VAR-CONTEXT is an alist mapping variable names (as symbols) to string values:
  ((VAR1 . \"value1\") (VAR2 . \"value2\") ...)

Supports both $VAR and ${VAR} syntax. Performs partial resolution - some variables
may be resolved while others remain unresolved.

Return values:
  - String: All variables resolved successfully
  - Plist: Some variables remain unresolved
    (:path \"partially/resolved/$PATH\" :unresolved (\"PATH\"))

Resolution behavior:
  - Resolved variables: Replaced with their values from context
  - Unresolved variables: Preserved in original syntax ($ or ${})
  - No variables: Returns original path unchanged

Examples:
  ;; All resolved - return simple string
  (jf/bash-resolve-variables \"$WORKSPACE/file.txt\"
                             '((WORKSPACE . \"/workspace\")))
    => \"/workspace/file.txt\"

  ;; Partial resolution - return plist with unresolved list
  (jf/bash-resolve-variables \"$WORKSPACE/$FILE\"
                             '((WORKSPACE . \"/workspace\")))
    => (:path \"/workspace/$FILE\" :unresolved (\"FILE\"))

  ;; No context - mark all as unresolved
  (jf/bash-resolve-variables \"$UNKNOWN/file.txt\" nil)
    => (:path \"$UNKNOWN/file.txt\" :unresolved (\"UNKNOWN\"))

  ;; No variables - return unchanged
  (jf/bash-resolve-variables \"/absolute/path.txt\" nil)
    => \"/absolute/path.txt\"

  ;; Both syntax forms
  (jf/bash-resolve-variables \"$A/${B}/file\" '((A . \"/a\") (B . \"b\")))
    => \"/a/b/file\"

Security note: Unresolved variables should be treated as security risks since
their runtime values cannot be validated against scope constraints."
  (let* ((detection-result (jf/bash-detect-variable-references file-path))
         (has-variables (car detection-result))
         (var-names (cdr detection-result)))

    ;; Fast path: no variables found
    (if (not has-variables)
        file-path

      ;; Process each variable reference
      (let ((resolved file-path)
            (unresolved nil))
        (dolist (var-name var-names)
          (let ((value (alist-get (intern var-name) var-context)))
            (if value
                ;; Replace all occurrences of $VAR or ${VAR} with value
                ;; Use word boundary \\b for $VAR to prevent partial matches
                ;; (e.g., matching $VAR when the variable is $VARIABLE)
                (setq resolved
                      (replace-regexp-in-string
                       (format "\\${%s}\\|\\$%s\\b" var-name var-name)
                       value
                       resolved
                       t  ; fixedcase - preserve case
                       t)) ; literal - treat replacement string literally
              ;; Track unresolved variable
              (push var-name unresolved))))

        ;; Return format based on resolution status
        (if unresolved
            ;; Partial or no resolution - return plist with metadata
            (list :path resolved :unresolved (nreverse unresolved))
          ;; Full resolution - return simple string
          resolved)))))

(defun jf/bash-track-assignments (parsed-command &optional initial-context)
  "Track variable assignments from PARSED-COMMAND, merging with INITIAL-CONTEXT.

Extracts simple VAR=value assignments from the parsed command structure and
builds a variable context alist mapping variable names (as symbols) to their
values (as strings).

PARSED-COMMAND is the output of `jf/bash-parse'.
INITIAL-CONTEXT is an optional alist of existing variable bindings.

Returns updated context alist with new assignments merged.

Supported patterns:
- Simple assignment: VAR=value
- Assignment before command: DIR=/tmp cat $DIR/file
- Command chains: A=1 && B=2 && cmd (accumulates left-to-right)

Unsupported (returns unchanged context):
- Complex expansions: ${VAR:-default}
- Arrays: ARR=(a b c)
- Command substitution: VAR=$(cmd)

Examples:
  (jf/bash-track-assignments
    (jf/bash-parse \"DIR=/tmp && cat $DIR/file.txt\")
    nil)
  => ((DIR . \"/tmp\"))

  (jf/bash-track-assignments
    (jf/bash-parse \"A=/foo && B=$A/bar\")
    ((WORKSPACE . \"/workspace\")))
  => ((B . \"$A/bar\") (A . \"/foo\") (WORKSPACE . \"/workspace\"))

Implementation note:
  Assignments are detected by checking if the command name or positional
  arguments match the VAR=value pattern. Tree-sitter may parse these as
  variable_assignment nodes or include them in the word list depending on
  command structure."
  (let ((context (copy-alist initial-context))
        (command-type (plist-get parsed-command :type))
        (all-commands (plist-get parsed-command :all-commands)))

    (cond
     ;; Chain or pipeline: process all commands in order
     ((or (eq command-type :chain) (eq command-type :pipeline))
      (dolist (cmd all-commands)
        (when-let ((assignments (jf/bash--extract-assignments-from-command cmd)))
          ;; Prepend assignments to maintain left-to-right precedence
          (setq context (append assignments context)))))

     ;; Simple command: check for assignments
     ((eq command-type :simple)
      (when-let ((assignments (jf/bash--extract-assignments-from-command parsed-command)))
        (setq context (append assignments context)))))

    context))

(defun jf/bash--extract-assignments-from-command (command)
  "Extract variable assignments from COMMAND structure.

COMMAND is a single parsed command (from :all-commands or a simple command).

Returns alist of (VAR-SYMBOL . VALUE-STRING) for each assignment found,
or nil if no assignments detected.

Detects assignments by checking if the command name matches the pattern
VAR=value. Tree-sitter parses these as variable_assignment nodes, but
they may also appear in the word list.

This function only handles simple assignments (no complex expansions).

Examples:
  ;; Assignment as command
  (jf/bash--extract-assignments-from-command
    (:command-name \"DIR=/tmp\" :subcommand nil ...))
  => ((DIR . \"/tmp\"))

  ;; No assignment
  (jf/bash--extract-assignments-from-command
    (:command-name \"cat\" :positional-args (\"file.txt\") ...))
  => nil

  ;; Assignment in positional args
  (jf/bash--extract-assignments-from-command
    (:command-name \"env\" :positional-args (\"VAR=value\" \"cmd\") ...))
  => ((VAR . \"value\"))"
  (let ((assignments nil)
        (command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))

    ;; Check if command-name itself is an assignment (VAR=value)
    (when (and command-name (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" command-name))
      (let ((var-name (match-string 1 command-name))
            (var-value (match-string 2 command-name)))
        (push (cons (intern var-name) var-value) assignments)))

    ;; Also check positional args for assignment patterns
    ;; (in case tree-sitter includes them there)
    (dolist (arg positional-args)
      (when (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" arg)
        (let ((var-name (match-string 1 arg))
              (var-value (match-string 2 arg)))
          (push (cons (intern var-name) var-value) assignments))))

    (nreverse assignments)))

(defun jf/bash-extract-file-operations (parsed-command &optional var-context)
  "Extract all file operations from PARSED-COMMAND.

This is the main entry point for file operation extraction. It coordinates
extraction from all sources:
- Redirections (>, >>, <, 2>, etc.)
- Positional arguments (based on command semantics)
- Exec blocks (find -exec ... \\;)

PARSED-COMMAND is the output from `jf/bash-parse'.
VAR-CONTEXT is an optional alist mapping variable names (symbols) to values (strings).

Returns a list of operation plists, each containing:
  :file - File path (may contain unresolved variables)
  :operation - Operation type (:read, :write, :delete, :modify, :create, :append)
  :confidence - Confidence level (:high, :medium, :low, :unknown)
  :source - Source of operation (:redirection, :positional-arg, :exec-block)
  :indirect - t if from nested command (optional)
  :unresolved - List of unresolved variable names (optional)

Handles multi-command constructs:
- Pipelines: Extract from each command in the pipeline
- Chains: Extract from each command, tracking variable assignments
- Simple: Extract from the single command

Operations are deduplicated: if the same file appears with the same operation
type multiple times, only one entry is returned.

Examples:
  ;; Simple read
  (jf/bash-extract-file-operations
    (jf/bash-parse \"cat /workspace/file.txt\"))
  => ((:file \"/workspace/file.txt\" :operation :read
       :confidence :high :source :positional-arg))

  ;; Multiple sources
  (jf/bash-extract-file-operations
    (jf/bash-parse \"cat input.txt > output.txt\"))
  => ((:file \"input.txt\" :operation :read :confidence :high :source :positional-arg)
      (:file \"output.txt\" :operation :write :confidence :high :source :redirection))

  ;; Pipeline
  (jf/bash-extract-file-operations
    (jf/bash-parse \"cat file.txt | grep pattern > output.txt\"))
  => ((:file \"file.txt\" :operation :read ...)
      (:file \"output.txt\" :operation :write ...))

  ;; Variable resolution
  (jf/bash-extract-file-operations
    (jf/bash-parse \"cat $WORKSPACE/file.txt\")
    '((WORKSPACE . \"/workspace\")))
  => ((:file \"/workspace/file.txt\" :operation :read ...))"
  (let ((operations nil)
        (context (or var-context nil))
        (command-type (plist-get parsed-command :type))
        (all-commands (plist-get parsed-command :all-commands)))

    (cond
     ;; Chain: process sequentially, tracking variable assignments
     ((eq command-type :chain)
      (dolist (cmd all-commands)
        ;; Track assignments from this command
        (setq context (jf/bash-track-assignments cmd context))
        ;; Extract operations from this command
        (let ((cmd-ops (jf/bash--extract-from-single-command cmd context)))
          (setq operations (append operations cmd-ops)))))

     ;; Pipeline: process each command independently
     ((eq command-type :pipeline)
      (dolist (cmd all-commands)
        (let ((cmd-ops (jf/bash--extract-from-single-command cmd context)))
          (setq operations (append operations cmd-ops)))))

     ;; Simple: extract from single command
     ((eq command-type :simple)
      (setq operations (jf/bash--extract-from-single-command parsed-command context))))

    ;; Deduplicate: same file + operation
    (jf/bash--deduplicate-operations operations)))

(defun jf/bash--extract-from-single-command (command var-context)
  "Extract file operations from a single COMMAND with VAR-CONTEXT.

COMMAND is a single parsed command structure (from :all-commands or top-level).
VAR-CONTEXT is an alist of variable bindings.

Returns list of operation plists from all extraction sources."
  (let ((operations nil))
    ;; Extract from redirections (high confidence)
    (when-let ((redir-ops (jf/bash-extract-operations-from-redirections command var-context)))
      (setq operations (append operations redir-ops)))

    ;; Extract from positional arguments (command semantics)
    (when-let ((pos-ops (jf/bash-extract-operations-from-positional-args command var-context)))
      (setq operations (append operations pos-ops)))

    ;; Extract from exec blocks (find -exec)
    (when-let ((exec-ops (jf/bash-extract-from-exec-blocks command var-context)))
      (setq operations (append operations exec-ops)))

    operations))

(defun jf/bash--deduplicate-operations (operations)
  "Deduplicate OPERATIONS list by file + operation type.

If multiple operations have the same :file and :operation values,
keep only the first occurrence. This handles cases where a file
appears multiple times in a command.

Returns deduplicated list maintaining original order."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (op operations)
      (let* ((file (plist-get op :file))
             (operation (plist-get op :operation))
             (key (cons file operation)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push op result))))
    (nreverse result)))

(defun jf/bash-extract-operations-from-positional-args (parsed-command &optional var-context)
  "Extract file operations from positional arguments in PARSED-COMMAND.

Uses command semantics database to determine which positional arguments
represent file paths and what operations they represent.

PARSED-COMMAND is the output of `jf/bash-parse'.
VAR-CONTEXT is optional alist mapping variable names (symbols) to values (strings).

Returns list of operation plists with:
  :file - File path (string, possibly with variables)
  :operation - Operation type (:read, :write, :delete, :modify, etc.)
  :confidence - Confidence level (:high for known commands, nil for unknown)
  :source - Source of file path (:positional-arg)
  :command - Command name that performs the operation

Returns empty list if:
- Command is not in semantics database
- Command has no positional arguments
- No positional args map to file operations per semantics

Example:
  (jf/bash-extract-operations-from-positional-args
    (jf/bash-parse \"cat /workspace/foo.txt\"))
  => ((:file \"/workspace/foo.txt\" :operation :read
       :confidence :high :source :positional-arg :command \"cat\"))

  (jf/bash-extract-operations-from-positional-args
    (jf/bash-parse \"cp src.txt dst.txt\"))
  => ((:file \"src.txt\" :operation :read :confidence :high ...)
      (:file \"dst.txt\" :operation :write :confidence :high ...))

  (jf/bash-extract-operations-from-positional-args
    (jf/bash-parse \"grep pattern file.txt\"))
  => ((:file \"file.txt\" :operation :read :confidence :high ...))
      ;; Note: pattern arg at index 0 is skipped per semantics"
  (let* ((command-name (plist-get parsed-command :command-name))
         (subcommand (plist-get parsed-command :subcommand))
         (flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (operations nil))

    ;; Only proceed if we have a command and positional args
    (when (and command-name positional-args)
      (let ((semantics (jf/bash-lookup-command-semantics command-name)))
        (when semantics
          (let ((ops-spec (plist-get semantics :operations)))
            (cond
             ;; Complex command (git, docker, etc.) - handle subcommand
             ((eq ops-spec :complex)
              (when subcommand
                (let ((subcommand-handlers (plist-get semantics :subcommand-handlers)))
                  (when-let ((handler-spec (alist-get (intern subcommand) subcommand-handlers)))
                    (setq operations
                          (jf/bash--extract-ops-from-positional-specs
                           handler-spec positional-args command-name var-context))))))

             ;; Flag-dependent command (tar, sed, etc.) - check flags
             ((eq ops-spec :flag-dependent)
              (let* ((flag-handlers (plist-get semantics :flag-handlers))
                     (matched-handler nil))
                ;; Find first matching flag handler
                (dolist (handler flag-handlers)
                  (when (null matched-handler)
                    (let ((trigger-flags (car handler))
                          (handler-spec (cdr handler)))
                      ;; Check if any trigger flag is present
                      (when (or (null trigger-flags)  ; Empty trigger matches always
                                (seq-some (lambda (f) (member f flags)) trigger-flags))
                        (setq matched-handler handler-spec)))))
                (when matched-handler
                  (setq operations
                        (jf/bash--extract-ops-from-positional-specs
                         matched-handler positional-args command-name var-context)))))

             ;; Simple command - direct operation specs
             ((listp ops-spec)
              (setq operations
                    (jf/bash--extract-ops-from-positional-specs
                     ops-spec positional-args command-name var-context))))))))

    operations))

(defun jf/bash--extract-ops-from-positional-specs (op-specs positional-args command-name var-context)
  "Apply OP-SPECS to POSITIONAL-ARGS to extract file operations.

OP-SPECS is a list of operation specification plists.
POSITIONAL-ARGS is list of argument strings.
COMMAND-NAME is the command performing the operations.
VAR-CONTEXT is optional variable resolution context.

Returns list of operation plists.

Operation spec format:
  :source - Must be :positional-args (others handled elsewhere)
  :operation - Operation type (:read, :write, :delete, :modify, etc.)
  :index - Single index (0-based, -1 for last)
  :indices - Range (0 . -2) for first to second-to-last, :all for all args
  :skip-indices - List of indices to skip (e.g., (0) skips first arg)"
  (let ((operations nil))
    (dolist (spec op-specs)
      (when (eq (plist-get spec :source) :positional-args)
        (let* ((operation (plist-get spec :operation))
               (index (plist-get spec :index))
               (indices (plist-get spec :indices))
               (skip-indices (plist-get spec :skip-indices))
               (target-indices nil))

          ;; Determine which positional arg indices to extract
          (cond
           ;; Single index specified
           (index
            (let ((resolved-idx (jf/bash--resolve-index index positional-args)))
              (when resolved-idx
                (setq target-indices (list resolved-idx)))))

           ;; Range of indices specified
           (indices
            (setq target-indices (jf/bash--resolve-index-range indices positional-args)))

           ;; No index specification - all positional args
           (t
            (setq target-indices (number-sequence 0 (1- (length positional-args))))))

          ;; Filter out skip-indices
          (when skip-indices
            (setq target-indices (seq-remove (lambda (i) (member i skip-indices))
                                            target-indices)))

          ;; Extract file paths at target indices
          (dolist (idx target-indices)
            (when (and (>= idx 0) (< idx (length positional-args)))
              (let* ((file-path (nth idx positional-args))
                     (resolved-path (jf/bash--resolve-path-variables file-path var-context))
                     ;; Extract path and unresolved metadata
                     (final-path (if (stringp resolved-path)
                                    resolved-path
                                  (plist-get resolved-path :path)))
                     (unresolved-vars (when (listp resolved-path)
                                       (plist-get resolved-path :unresolved))))
                ;; Create operation plist
                (push (append (list :file final-path
                                   :operation operation
                                   :confidence :high
                                   :source :positional-arg
                                   :command command-name)
                             (when unresolved-vars
                               (list :unresolved unresolved-vars)))
                      operations)))))))

    (nreverse operations)))

(defun jf/bash--resolve-index (index args)
  "Resolve INDEX (possibly negative) to actual position in ARGS.

INDEX can be:
  - Positive integer (0-based): 0 = first, 1 = second, etc.
  - Negative integer: -1 = last, -2 = second-to-last, etc.

ARGS is the list of positional arguments.

Returns actual 0-based index, or nil if out of bounds.

Examples:
  (jf/bash--resolve-index 0 '(\"a\" \"b\" \"c\"))   => 0
  (jf/bash--resolve-index -1 '(\"a\" \"b\" \"c\"))  => 2
  (jf/bash--resolve-index -2 '(\"a\" \"b\" \"c\"))  => 1
  (jf/bash--resolve-index 5 '(\"a\" \"b\" \"c\"))   => nil (out of bounds)"
  (let ((len (length args)))
    (cond
     ;; Negative index - count from end
     ((< index 0)
      (let ((resolved (+ len index)))
        (if (>= resolved 0) resolved nil)))
     ;; Positive index - use as-is if in bounds
     ((< index len)
      index)
     ;; Out of bounds
     (t nil))))

(defun jf/bash--resolve-index-range (range-spec args)
  "Resolve RANGE-SPEC to list of actual indices in ARGS.

RANGE-SPEC can be:
  - Cons cell (START . END): range from START to END (inclusive)
  - Symbol :all: all indices

START and END can be positive or negative integers.

ARGS is the list of positional arguments.

Returns list of actual 0-based indices.

Examples:
  (jf/bash--resolve-index-range '(0 . -2) '(\"a\" \"b\" \"c\"))
    => (0 1)  ;; First to second-to-last

  (jf/bash--resolve-index-range '(0 . -1) '(\"a\" \"b\" \"c\"))
    => (0 1 2)  ;; First to last

  (jf/bash--resolve-index-range :all '(\"a\" \"b\" \"c\"))
    => (0 1 2)  ;; All indices

  (jf/bash--resolve-index-range '(1 . 2) '(\"a\" \"b\" \"c\" \"d\"))
    => (1 2)  ;; Second to third"
  (cond
   ;; :all symbol - return all indices
   ((eq range-spec :all)
    (number-sequence 0 (1- (length args))))

   ;; Cons cell range
   ((consp range-spec)
    (let* ((start-idx (jf/bash--resolve-index (car range-spec) args))
           (end-idx (jf/bash--resolve-index (cdr range-spec) args)))
      (when (and start-idx end-idx (<= start-idx end-idx))
        (number-sequence start-idx end-idx))))

   ;; Unknown spec
   (t nil)))

(defun jf/bash--resolve-path-variables (file-path var-context)
  "Resolve variables in FILE-PATH using VAR-CONTEXT.

This is a wrapper around `jf/bash-resolve-variables' for use in
file operations extraction.

FILE-PATH is the file path string (may contain variables).
VAR-CONTEXT is optional alist mapping variable names to values.

Returns:
  - String: Fully resolved path (if all variables resolved)
  - Plist: Partially resolved path with :unresolved metadata
  - Original path: If no variables or no context

Examples:
  (jf/bash--resolve-path-variables \"/workspace/file.txt\" nil)
    => \"/workspace/file.txt\"

  (jf/bash--resolve-path-variables \"$WORKSPACE/file.txt\"
                                   '((WORKSPACE . \"/workspace\")))
    => \"/workspace/file.txt\"

  (jf/bash--resolve-path-variables \"$WORKSPACE/$FILE\" nil)
    => (:path \"$WORKSPACE/$FILE\" :unresolved (\"WORKSPACE\" \"FILE\"))"
  (jf/bash-resolve-variables file-path var-context))

(defun jf/bash-extract-operations-from-redirections (parsed-command &optional var-context)
  "Extract file operations from :redirections field with high confidence.

PARSED-COMMAND is the output of `jf/bash-parse'.
VAR-CONTEXT is an optional alist mapping variable names (symbols) to values (strings).

Returns list of operation plists:
  (:file \"path\" :operation :read/:write/:append
   :confidence :high :source :redirection
   :metadata (:operator \">\" :descriptor nil ...))

For paths with variable references:
  - Resolved variables: Returns simple path string
  - Unresolved variables: Returns plist with :path and :unresolved list

Redirections are always :high confidence since they are explicit grammar constructs.
File operations are unambiguous - \">\" always writes, \"<\" always reads.

Examples:
  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cat file.txt > output.txt\")
    nil)
  => ((:file \"output.txt\" :operation :write :confidence :high
       :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cmd >> log.txt\")
    nil)
  => ((:file \"log.txt\" :operation :append :confidence :high
       :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cat < input.txt\")
    nil)
  => ((:file \"input.txt\" :operation :read :confidence :high
       :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cmd > $OUTFILE\")
    '((OUTFILE . \"/workspace/output.txt\")))
  => ((:file \"/workspace/output.txt\" :operation :write :confidence :high
       :source :redirection :metadata (...)))"
  (let ((operations nil))
    ;; Handle different command types
    (let ((command-type (plist-get parsed-command :type))
          (all-commands (plist-get parsed-command :all-commands)))

      (cond
       ;; Simple command: check for redirections
       ((eq command-type :simple)
        (when-let ((redirections (plist-get parsed-command :redirections)))
          (setq operations (jf/bash--extract-ops-from-redirect-list redirections var-context))))

       ;; Pipeline or chain: process each command's redirections
       ((or (eq command-type :pipeline) (eq command-type :chain))
        (dolist (cmd all-commands)
          (when-let ((redirections (plist-get cmd :redirections)))
            (setq operations
                  (append operations
                         (jf/bash--extract-ops-from-redirect-list redirections var-context))))))))

    operations))

(defun jf/bash--extract-ops-from-redirect-list (redirections var-context)
  "Extract operations from REDIRECTIONS list with VAR-CONTEXT.

REDIRECTIONS is a list of redirection plists from the parser.
VAR-CONTEXT is optional variable resolution context.

Returns list of operation plists with resolved file paths."
  (let ((operations nil))
    (dolist (redir redirections)
      (let* ((redir-type (plist-get redir :type))
             (operator (plist-get redir :operator))
             (destination (plist-get redir :destination)))

        ;; Only process file redirections (not heredoc/herestring)
        (when (and (eq redir-type :file) destination)
          ;; Map operator to operation type
          (let ((operation-type (jf/bash--map-redirect-operator-to-operation operator)))
            (when operation-type
              ;; Resolve variables in destination path
              (let* ((resolved-path (if var-context
                                       (jf/bash-resolve-variables destination var-context)
                                     destination))
                     ;; Extract path and unresolved metadata
                     (file-path (if (stringp resolved-path)
                                   resolved-path
                                 (plist-get resolved-path :path)))
                     (unresolved-vars (when (listp resolved-path)
                                       (plist-get resolved-path :unresolved))))
                ;; Build operation plist
                (push (append (list :file file-path
                                   :operation operation-type
                                   :confidence :high
                                   :source :redirection
                                   :metadata redir)
                             (when unresolved-vars
                               (list :unresolved unresolved-vars)))
                      operations)))))))
    (nreverse operations)))

(defun jf/bash--map-redirect-operator-to-operation (operator)
  "Map shell redirection OPERATOR to file operation type.

Returns operation keyword or nil for non-file operators:
  \">\"   => :write   (output redirection)
  \">>\"  => :append  (append redirection)
  \"<\"   => :read    (input redirection)
  \"2>\"  => :write   (stderr redirection)
  \"&>\"  => :write   (combined stdout+stderr)
  \"&>>\" => :append  (combined append)

Returns nil for descriptor operators (>&, <&, >&-, <&-) and other
non-file redirection operators."
  (pcase operator
    ;; Output redirections - write operations
    (">" :write)
    ("2>" :write)
    ("&>" :write)
    (">|" :write)  ;; noclobber override

    ;; Append redirections
    (">>" :append)
    ("&>>" :append)

    ;; Input redirections - read operations
    ("<" :read)

    ;; Descriptor manipulation - not file operations
    (">&" nil)
    ("<&" nil)
    (">&-" nil)
    ("<&-" nil)

    ;; Unknown operator
    (_ nil)))

(defun jf/bash-extract-from-exec-blocks (parsed-command var-context)
  "Extract file operations from find -exec blocks in PARSED-COMMAND.

PARSED-COMMAND is the output of `jf/bash-parse' containing parsed exec blocks.
VAR-CONTEXT is an optional alist mapping variable names to values.

Returns list of operation plists with :indirect t metadata, representing file
operations that will be executed by the nested command in each exec block.

Each operation includes:
  :file - File path (may contain {} placeholder or resolved path)
  :operation - Operation type (:read, :write, :delete, :modify, etc.)
  :confidence - Confidence level (:medium for exec blocks)
  :source - Source of the file path (:exec-block)
  :indirect - Always t (marks operation as nested)
  :exec-type - Type of exec block (\"-exec\" or \"-execdir\")
  :command-name - Name of command in exec block

The {} placeholder in exec blocks represents files matched by find. Operations on
{} are extracted but confidence is :medium since the actual files depend on
find's runtime results.

Examples:
  ;; find . -name '*.txt' -exec cat {} \\;
  ;; => ((:file \"{}\" :operation :read :confidence :medium
  ;;      :indirect t :exec-type \"-exec\" :command-name \"cat\"))

  ;; find /tmp -name '*.log' -exec rm {} \\;
  ;; => ((:file \"{}\" :operation :delete :confidence :medium
  ;;      :indirect t :exec-type \"-exec\" :command-name \"rm\"))

  ;; Multiple exec blocks are processed independently
  ;; find . -exec cat {} \\; -exec rm {} \\;
  ;; => Two operations: one :read, one :delete"
  (let ((exec-blocks (plist-get parsed-command :exec-blocks))
        (operations nil))

    (when exec-blocks
      (dolist (exec-block exec-blocks)
        (let* ((exec-type (plist-get exec-block :type))
               (exec-cmd-name (plist-get exec-block :command-name))
               (exec-flags (plist-get exec-block :flags))
               (exec-positional (plist-get exec-block :positional-args)))

          ;; Build operation for each positional arg in exec command
          ;; This uses the simplified inference - full extraction would use semantics database
          (dolist (file-path exec-positional)
            (let ((operation-type (jf/bash--infer-operation-type exec-cmd-name)))

              (when operation-type
                (push (list :file file-path
                           :operation operation-type
                           :confidence :medium  ; indirect operations have medium confidence
                           :source :exec-block
                           :indirect t
                           :exec-type exec-type
                           :command-name exec-cmd-name)
                      operations)))))))

    (nreverse operations)))

(defun jf/bash--infer-operation-type (command-name)
  "Infer operation type from COMMAND-NAME.

Returns operation type symbol (:read, :write, :delete, :modify) or nil if
command is not recognized or doesn't operate on files.

This is a simplified heuristic for exec block extraction. For full operation
extraction, use the command semantics database instead.

Recognized patterns:
  - Read operations: cat, head, tail, less, more, grep, wc, file, stat
  - Write operations: touch, tee, dd (of=)
  - Delete operations: rm, rmdir
  - Modify operations: chmod, chown, chgrp, sed -i

Unknown commands return nil."
  (when command-name
    (let ((cmd (intern command-name)))
      (cond
       ;; Read operations
       ((memq cmd '(cat head tail less more grep egrep fgrep wc file stat))
        :read)

       ;; Write operations
       ((memq cmd '(touch tee))
        :write)

       ;; Delete operations
       ((memq cmd '(rm rmdir))
        :delete)

       ;; Modify operations
       ((memq cmd '(chmod chown chgrp))
        :modify)

       ;; Unknown or non-file-operation command
       (t nil)))))

(defun jf/bash-mark-indirect-operations (operations)
  "Mark OPERATIONS as indirect if they should have stricter security policies.

Operations are marked as indirect (:indirect t) if they come from:
- Exec blocks (find -exec, find -execdir)
- Nested commands (bash -c, sh -c, python -c) [future]
- Command substitution [future]

OPERATIONS is a list of operation plists from extraction functions.

Returns updated operations list with :indirect t added where appropriate.

The :indirect flag is a security signal indicating that the operation
will be executed in a nested context, which may require stricter
validation or explicit approval.

Examples:
  ;; Direct operation - not marked
  (jf/bash-mark-indirect-operations
    '((:file \"file.txt\" :operation :read :source :positional-arg)))
  => ((:file \"file.txt\" :operation :read :source :positional-arg))

  ;; Exec block operation - marked as indirect
  (jf/bash-mark-indirect-operations
    '((:file \"{}\" :operation :delete :source :exec-block)))
  => ((:file \"{}\" :operation :delete :source :exec-block :indirect t))

  ;; Mixed operations - only exec-block marked
  (jf/bash-mark-indirect-operations
    '((:file \"a.txt\" :operation :read :source :positional-arg)
      (:file \"{}\" :operation :delete :source :exec-block)))
  => ((:file \"a.txt\" :operation :read :source :positional-arg)
      (:file \"{}\" :operation :delete :source :exec-block :indirect t))"
  (mapcar
   (lambda (op)
     (let ((source (plist-get op :source)))
       (cond
        ;; Exec blocks are always indirect
        ((eq source :exec-block)
         (plist-put (copy-sequence op) :indirect t))

        ;; Already marked as indirect (from nested command parsing)
        ((plist-get op :indirect)
         op)

        ;; Direct operations remain unmarked
        (t op))))
   operations))

(defvar jf/bash-command-injection-patterns
  '((bash . (:flags ("-c") :arg-after-flag t))
    (sh . (:flags ("-c") :arg-after-flag t))
    (python . (:flags ("-c") :arg-after-flag t))
    (env . (:flags ("-S" "--split-string") :arg-after-flag t))
    (eval . (:no-flag-required t :first-arg t)))
  "Database of command injection patterns.

Each entry maps a command symbol to detection parameters:
  :flags - List of flags that indicate command injection
  :arg-after-flag - If t, nested command is the argument after the flag
  :no-flag-required - If t, command doesn't need a flag (like eval)
  :first-arg - If t, first positional arg is the nested command

Examples:
  bash -c 'rm file.txt'     - Flag: -c, nested: 'rm file.txt'
  python -c 'import os'     - Flag: -c, nested: 'import os'
  env -S 'prog arg'         - Flag: -S, nested: 'prog arg'
  eval 'rm file.txt'        - No flag, nested: 'rm file.txt'")

(defun jf/bash-detect-command-injection (parsed-command)
  "Detect command injection patterns in PARSED-COMMAND.

Returns plist with command injection metadata if pattern detected, nil otherwise:
  :command-injection - Always t when pattern detected
  :nested-command-string - The nested command string to be executed
  :injection-type - Type of injection (:flag-based or :direct)
  :trigger-flag - The flag that triggers injection (for flag-based)

PARSED-COMMAND is the output from `jf/bash-parse' (simple command only).

Detects patterns like:
  bash -c 'command'    - Flag-based injection
  sh -c 'command'      - Flag-based injection
  python -c 'code'     - Flag-based injection
  env -S 'prog args'   - Flag-based injection
  eval 'command'       - Direct injection (no flag)

Handles flags appearing in different positions:
  bash -x -e -c 'cmd'  - Detects -c despite other flags
  bash -c -x 'cmd'     - Detects -c even with trailing flags

The nested command string is extracted from positional arguments based on
the pattern definition. For flag-based injection, it's typically the first
positional arg after all flags.

Examples:
  (jf/bash-detect-command-injection
    (jf/bash-parse \"bash -c 'rm file.txt'\"))
  => (:command-injection t
      :nested-command-string \"rm file.txt\"
      :injection-type :flag-based
      :trigger-flag \"-c\")

  (jf/bash-detect-command-injection
    (jf/bash-parse \"eval 'rm file.txt'\"))
  => (:command-injection t
      :nested-command-string \"rm file.txt\"
      :injection-type :direct)

  (jf/bash-detect-command-injection
    (jf/bash-parse \"bash script.sh\"))
  => nil  ; No injection pattern detected"
  (let* ((cmd-name (plist-get parsed-command :command-name))
         (flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (pattern nil)
         (result nil))

    ;; Look up command in injection patterns database
    (when cmd-name
      (setq pattern (alist-get (intern cmd-name) jf/bash-command-injection-patterns)))

    ;; If pattern found, check if conditions are met
    (when pattern
      (let ((pattern-flags (plist-get pattern :flags))
            (arg-after-flag (plist-get pattern :arg-after-flag))
            (no-flag-required (plist-get pattern :no-flag-required))
            (first-arg (plist-get pattern :first-arg))
            (nested-cmd nil)
            (trigger-flag nil))

        (cond
         ;; No flag required - first positional arg is nested command
         (no-flag-required
          (when (and first-arg positional-args)
            (setq nested-cmd (car positional-args))
            (setq result (list :command-injection t
                              :nested-command-string nested-cmd
                              :injection-type :direct))))

         ;; Flag-based injection - check if trigger flag present
         ((and pattern-flags arg-after-flag)
          ;; Find if any pattern flag is present in command flags
          (dolist (flag pattern-flags)
            (when (and (null trigger-flag)
                      (member flag flags))
              (setq trigger-flag flag)))

          ;; If trigger flag found, extract nested command from positional args
          (when (and trigger-flag positional-args)
            ;; First positional arg (after all flags) is the nested command
            (setq nested-cmd (car positional-args))
            (setq result (list :command-injection t
                              :nested-command-string nested-cmd
                              :injection-type :flag-based
                              :trigger-flag trigger-flag)))))))

    result))

(defun jf/bash--strip-outer-quotes (str)
  "Strip outer single or double quotes from STR.

Preserves inner quotes - only removes matching outermost quote pair.

Handles:
  - Single quotes: 'cmd' -> cmd
  - Double quotes: \"cmd\" -> cmd
  - Preserved inner: 'grep \"pattern\" file' -> grep \"pattern\" file
  - No quotes: Returns str unchanged
  - Escaped quotes: Preserves escaped quotes in result

Examples:
  (jf/bash--strip-outer-quotes \"'rm file.txt'\")
  => \"rm file.txt\"

  (jf/bash--strip-outer-quotes \"\\\"cat file.txt\\\"\")
  => \"cat file.txt\"

  (jf/bash--strip-outer-quotes \"'grep \\\"pattern\\\" file'\")
  => \"grep \\\"pattern\\\" file\"

  (jf/bash--strip-outer-quotes \"no-quotes\")
  => \"no-quotes\"

Limitation: Does not handle escaped outer quotes (\\\\' or \\\\\\\") at the
boundaries. These are rare in practice since bash command strings
typically use different quote types for inner/outer levels."
  (cond
   ;; Single quotes - strip if both start and end match
   ((and (>= (length str) 2)
         (string-prefix-p "'" str)
         (string-suffix-p "'" str))
    (substring str 1 (1- (length str))))

   ;; Double quotes - strip if both start and end match
   ((and (>= (length str) 2)
         (string-prefix-p "\"" str)
         (string-suffix-p "\"" str))
    (substring str 1 (1- (length str))))

   ;; No outer quotes - return unchanged
   (t str)))

(defun jf/bash-parse-nested-command (nested-command-string &optional nesting-level parent-command)
  "Parse nested command string recursively.

NESTED-COMMAND-STRING is the command string to parse (may have outer quotes).
NESTING-LEVEL is optional recursion depth counter (default 1).
PARENT-COMMAND is optional reference to outer command structure.

Returns parsed command structure with metadata:
  :nested-level - Recursion depth (1 = first nested, 2+ = deeper)
  :parent-command - Reference to outer command (optional)
  :success - t if parsing succeeded
  :error - Error message if parsing failed

Recursion termination:
  - Maximum depth: 10 levels (prevents infinite recursion)
  - Detection failure: No more injection patterns found
  - Parse failure: Command string is invalid

Quote handling:
  - Strips outer quotes before parsing: 'cat file' -> cat file
  - Preserves inner quotes: 'grep \"pattern\" file' -> grep \"pattern\" file
  - Handles both single and double quotes

Examples:
  ;; Single nesting
  (jf/bash-parse-nested-command \"'rm file.txt'\")
  => (:command-name \"rm\" :positional-args (\"file.txt\")
      :nested-level 1 :success t ...)

  ;; Multiple nesting
  (jf/bash-parse-nested-command \"'bash -c \\\"rm file\\\"'\")
  => (:command-name \"bash\" :flags (\"-c\")
      :positional-args (\"rm file\")
      :nested-command (:command-name \"rm\" :positional-args (\"file\")
                       :nested-level 2 :success t ...)
      :nested-level 1 :success t ...)

  ;; Maximum depth exceeded
  (jf/bash-parse-nested-command (deeply-nested-command) 11)
  => (:success nil :error \"Maximum nesting depth exceeded\")

Integration:
  This function is called by file operation extraction to recursively
  extract operations from nested commands. Operations from nested commands
  are marked with :indirect t to indicate indirect execution.

Nesting levels:
  - 0: Top-level command (not nested, parsed via jf/bash-parse)
  - 1: First level of nesting (bash -c 'cmd')
  - 2+: Deeper nesting levels (bash -c \"bash -c 'cmd'\")"
  (let ((level (or nesting-level 1)))
    ;; Check recursion depth limit
    (if (> level 10)
        (list :success nil
              :error "Maximum nesting depth exceeded (limit: 10)"
              :nested-level level)

      ;; Strip outer quotes and parse
      (let* ((stripped (jf/bash--strip-outer-quotes nested-command-string))
             (parsed (jf/bash-parse stripped)))

        (when (plist-get parsed :success)
          ;; Add nesting level metadata
          (setq parsed (plist-put parsed :nested-level level))

          ;; Add parent command reference if provided
          (when parent-command
            (setq parsed (plist-put parsed :parent-command parent-command)))

          ;; Check if this parsed command itself contains injection
          (when-let ((injection-info (jf/bash-detect-command-injection parsed)))
            (let ((nested-cmd-string (plist-get injection-info :nested-command-string)))
              ;; Recursively parse the nested injection
              (when nested-cmd-string
                (let ((nested-parsed (jf/bash-parse-nested-command
                                     nested-cmd-string
                                     (1+ level)
                                     parsed)))
                  ;; Add nested command to parsed structure
                  (setq parsed (plist-put parsed :nested-command nested-parsed)))))))

        parsed))))

(provide 'bash-parser)
;;; bash-parser.el ends here
