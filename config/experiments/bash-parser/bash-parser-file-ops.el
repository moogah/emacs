;;; bash-parser-file-ops.el --- File operations extraction -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, file-operations, security
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; File operations extraction subsystem for bash-parser.
;; Extracts file operations from parsed bash command structures.

;;; Code:

(require 'cl-lib)

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
        (when-let ((assignments (jf/bash--extract-assignments-from-command cmd)))
          ;; Resolve variables in assignment values using current context
          (setq assignments
                (mapcar (lambda (assignment)
                          (let* ((var-name (car assignment))
                                 (var-value (cdr assignment))
                                 (resolved-value (if (string-match-p "\\$" var-value)
                                                   (jf/bash-resolve-variables var-value context)
                                                   var-value)))
                            ;; If resolution returns a plist, extract the path
                            (cons var-name
                                  (if (listp resolved-value)
                                      (plist-get resolved-value :path)
                                    resolved-value))))
                        assignments))
          (setq context (append assignments context)))
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
                         (jf/bash--extract-ops-from-redirect-list redirections var-context))))))

       ;; Single command structure (no :type field)
       ;; This happens when called from jf/bash--extract-from-single-command
       ((null command-type)
        (when-let ((redirections (plist-get parsed-command :redirections)))
          (setq operations (jf/bash--extract-ops-from-redirect-list redirections var-context))))))

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

(provide 'bash-parser-file-ops)
;;; bash-parser-file-ops.el ends here
