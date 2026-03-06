;;; bash-parser-file-ops.el --- File operations extraction -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, file-operations, security
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; File operations extraction subsystem for bash-parser.
;; Extracts file operations from parsed bash command structures.

;;; Code:

(require 'cl-lib)

(defconst jf/bash-valid-operation-types
  '(:read :write :delete :modify :create :create-or-modify :append
    :match-pattern :read-directory :read-metadata :execute)
  "Valid operation types for bash file operations.

These operation types correspond to the semantic categories in the
command semantics database:

  :read             - Read file contents
  :write            - Write/create file (overwrites)
  :append           - Append to file
  :delete           - Delete file
  :modify           - Modify file metadata or contents in-place
  :create           - Create new file/directory (mkdir)
  :create-or-modify - Create file or update timestamp (touch)
  :match-pattern    - Match files against pattern (find, grep -l)
  :read-directory   - Read directory contents (ls, find search path)
  :read-metadata    - Read file metadata (which, dirname)
  :execute          - Execute file as script/binary")

(defun jf/bash--has-glob-pattern-p (file-path)
  "Return t if FILE-PATH contains glob pattern metacharacters.

Detects glob patterns:
  * - matches any characters (except /)
  ? - matches single character
  [abc] - character class
  {a,b} - brace expansion
  ** - recursive directory match

Returns t if any glob metacharacters are found, nil otherwise.

Examples:
  (jf/bash--has-glob-pattern-p \"file.txt\")       => nil
  (jf/bash--has-glob-pattern-p \"*.txt\")          => t
  (jf/bash--has-glob-pattern-p \"file?.txt\")      => t
  (jf/bash--has-glob-pattern-p \"file[0-9].txt\")  => t
  (jf/bash--has-glob-pattern-p \"**/*.el\")        => t
  (jf/bash--has-glob-pattern-p \"{a,b}.txt\")      => t"
  (and (stringp file-path)
       (or (string-match-p "\\*" file-path)
           (string-match-p "\\?" file-path)
           (string-match-p "\\[.*\\]" file-path)
           (string-match-p "{.*,.*}" file-path))))

(defun jf/bash--valid-var-context-p (context)
  "Return t if CONTEXT is valid variable context alist.

A valid variable context is either nil or an alist where each entry is a
cons cell (VAR . VALUE) with:
  - VAR (car) is a symbol or string
  - VALUE (cdr) is a string

Returns t if valid, nil otherwise.

Examples:
  (jf/bash--valid-var-context-p nil)                      => t
  (jf/bash--valid-var-context-p '((FILE . \"/path\")))     => t
  (jf/bash--valid-var-context-p '((\"FILE\" . \"/path\"))) => t
  (jf/bash--valid-var-context-p '((A . \"1\") (B . \"2\"))) => t

Invalid examples (return nil):
  (jf/bash--valid-var-context-p \"not-a-list\")           => nil
  (jf/bash--valid-var-context-p '((HOME /path)))         => nil (not cons)
  (jf/bash--valid-var-context-p '((nil . \"value\")))     => nil (nil key)
  (jf/bash--valid-var-context-p '((123 . \"value\")))     => nil (invalid key type)
  (jf/bash--valid-var-context-p '((KEY . 123)))          => nil (invalid value type)"
  (and (listp context)
       (seq-every-p
        (lambda (binding)
          (and (consp binding)
               (or (symbolp (car binding)) (stringp (car binding)))
               (not (null (car binding)))
               (stringp (cdr binding))))
        context)))

(defun jf/bash--normalize-var-context (var-context)
  "Normalize VAR-CONTEXT to use symbol keys.

VAR-CONTEXT can have either string or symbol keys. This function
ensures all keys are symbols for consistent lookup.

VAR-CONTEXT must be an alist where each entry is a cons cell (KEY . VALUE):
  - KEY must be a symbol or string
  - VALUE must be a string

Signals error if var-context is malformed with helpful message showing
the invalid entry and expected format.

Examples:
  ((\"FILE\" . \"/path\"))     => ((FILE . \"/path\"))
  ((FILE . \"/path\"))       => ((FILE . \"/path\"))
  ((\"A\" . \"1\") (B . \"2\")) => ((A . \"1\") (B . \"2\"))

Invalid examples (signal error):
  ((HOME /path))           - Not a cons cell
  (((nil . value)))        - Key is nil
  (((123 . value)))        - Key is not symbol/string
  ((\"KEY\" . 123))         - Value is not string"
  (when var-context
    ;; Validate var-context is a list
    (unless (listp var-context)
      (error "Invalid variable context: expected list, got %S. Variable context must be an alist like ((VAR1 . \"value1\") (VAR2 . \"value2\"))"
             (type-of var-context)))

    ;; Validate each entry
    (let ((index 0))
      (dolist (binding var-context)
        (unless (consp binding)
          (error "Invalid variable context at index %d: %S
Expected cons cell like (VAR . \"value\")
Variable context must be alist: ((VAR1 . \"value1\") (VAR2 . \"value2\"))"
                 index binding))

        (let ((key (car binding))
              (value (cdr binding)))
          ;; Validate key
          (when (null key)
            (error "Invalid variable context at index %d: key is nil in %S
Expected symbol or string key
Variable context must be alist: ((VAR1 . \"value1\") (VAR2 . \"value2\"))"
                   index binding))

          (unless (or (symbolp key) (stringp key))
            (error "Invalid variable context at index %d: key has invalid type %S in %S
Expected symbol or string, got %S
Variable context must be alist: ((VAR1 . \"value1\") (VAR2 . \"value2\"))"
                   index (type-of key) binding key))

          ;; Validate value
          (unless (stringp value)
            (error "Invalid variable context at index %d: value has invalid type %S in %S
Expected string value, got %S
Variable context must be alist: ((VAR1 . \"value1\") (VAR2 . \"value2\"))"
                   index (type-of value) binding value)))

        (setq index (1+ index))))

    ;; Normalize keys to symbols
    (mapcar (lambda (binding)
              (let ((key (car binding))
                    (value (cdr binding)))
                (cons (if (stringp key) (intern key) key)
                      value)))
            var-context)))

(defun jf/bash--flag-present-p (flag flags-list)
  "Return t if FLAG is present in FLAGS-LIST.

Handles both exact matches and combined Unix-style flags.
For example, \"-c\" matches both \"-c\" and \"-czf\".

FLAG should be a short flag like \"-c\" or long flag like \"--create\".
FLAGS-LIST is a list of flag strings from parsed command.

Examples:
  (jf/bash--flag-present-p \"-c\" '(\"-c\"))           => t
  (jf/bash--flag-present-p \"-c\" '(\"-czf\"))        => t
  (jf/bash--flag-present-p \"--create\" '(\"--create\")) => t
  (jf/bash--flag-present-p \"-c\" '(\"-xzf\"))        => nil"
  (seq-some
   (lambda (f)
     (cond
      ;; Exact match (handles long flags and standalone short flags)
      ((equal flag f) t)
      ;; Combined short flags: check if flag char is in the combined string
      ;; Only for short flags starting with single dash
      ((and (string-prefix-p "-" flag)
            (not (string-prefix-p "--" flag))
            (= (length flag) 2)
            (string-prefix-p "-" f)
            (not (string-prefix-p "--" f)))
       ;; Extract flag character (e.g., "c" from "-c")
       (let ((flag-char (substring flag 1 2)))
         ;; Check if character is in combined flags string
         (string-match-p (regexp-quote flag-char) (substring f 1))))
      (t nil)))
   flags-list))

(defun jf/bash-extract-file-operations (parsed-command &optional var-context)
  "Extract all file operations from PARSED-COMMAND.

This is the main entry point for file operation extraction. Now uses
recursive semantic analysis to extract operations from all nesting levels.

PARSED-COMMAND is the output from `jf/bash-parse'.
VAR-CONTEXT is an optional alist mapping variable names (symbols or strings) to values.

Signals error if PARSED-COMMAND is not a valid plist or if VAR-CONTEXT has invalid structure.

Returns a flat list of operation plists, each containing:
  :file - File path (may contain unresolved variables or patterns)
  :operation - Operation type (:read, :write, :delete, :modify, :create, :append,
                                :match-pattern, :read-directory, :read-metadata)
  :confidence - Confidence level (:high, :medium, :low)
  :source - Source of operation (:redirection, :positional-arg, :exec-block, etc.)
  :command - Command name that performs the operation

  Context flags (optional):
  :from-substitution - t if from command substitution
  :loop-context - t if in loop body
  :loop-variable - Loop variable name if in loop
  :conditional - t if in conditional branch
  :branch - :then or :else if in conditional
  :test-condition - t if in conditional test
  :pattern - t if file path is a glob pattern
  :pattern-source - Plist identifying pattern producer if applicable
  :heredoc-content - t if from heredoc with redirect
  :indirect - t if from nested/indirect execution

Handles arbitrary nesting depth up to 10 levels (configurable).
Operations are deduplicated by file+operation pair.

Examples:
  Simple: (jf/bash-extract-file-operations (jf/bash-parse \"cat file.txt\"))
  => ((:file \"file.txt\" :operation :read :confidence :high :command \"cat\"))

  Nested: (jf/bash-extract-file-operations (jf/bash-parse \"cat $(find . -name '*.log')\"))
  => ((:file \".\" :operation :read-directory :command \"find\" :from-substitution t)
      (:file \"*.log\" :operation :match-pattern :command \"find\" :pattern t :from-substitution t)
      (:file \"*.log\" :operation :read :command \"cat\" :pattern t :pattern-source (...)))

  Loop: (jf/bash-extract-file-operations (jf/bash-parse \"for f in *.txt; do rm $f; done\"))
  => ((:file \"*.txt\" :operation :delete :command \"rm\" :pattern t :loop-context t))"
  ;; Validate inputs
  (unless (listp parsed-command)
    (error "jf/bash-extract-file-operations: parsed-command must be a plist, got %S"
           (type-of parsed-command)))
  (when var-context
    (unless (jf/bash--valid-var-context-p var-context)
      (error "jf/bash-extract-file-operations: var-context must be an alist of (VAR . VALUE) pairs where VAR is symbol/string and VALUE is string, got %S"
             var-context)))

  ;; Normalize var-context
  (let ((context (jf/bash--normalize-var-context var-context)))
    ;; Use recursive analyzer (requires bash-parser-recursive)
    (require 'bash-parser-recursive)
    (let ((all-ops (jf/bash-analyze-file-operations-recursive
                   parsed-command context 0)))
      ;; Validate all operations before returning
      (dolist (op all-ops)
        (jf/bash--validate-operation-type op))
      ;; Deduplicate and return
      (jf/bash--deduplicate-operations all-ops))))

(defun jf/bash--validate-operation-type (operation)
  "Validate that OPERATION plist has valid structure and operation type.

OPERATION is a plist that should contain:
  :operation - Must be one of the valid operation types (required)
  :file      - File path (required)
  :confidence - Confidence level (required)
  :source    - Source of operation (required)
  :command   - Command name (required for most sources, optional for test-expression/loop-glob/command-name)

Returns t if valid, signals error with helpful message if invalid.

This validation ensures data integrity at extraction boundaries and
makes debugging easier by catching malformed entries early.

The :command field is optional for certain non-command sources:
  - :test-expression (conditional tests like [ -f file ])
  - :loop-glob (for loop glob patterns)
  - :command-name (self-executing paths like ./script.sh)

Examples:
  Valid operation with command:
    (:file \"foo.txt\" :operation :read :confidence :high
     :source :positional-arg :command \"cat\")
    => t

  Valid operation without command (test):
    (:file \"foo.txt\" :operation :read-metadata :confidence :high
     :source :test-expression :test-operator \"-f\")
    => t

  Invalid operation type:
    (:file \"foo.txt\" :operation :invalid :confidence :high
     :source :positional-arg :command \"cat\")
    => error: Invalid operation type :invalid

  Missing required field:
    (:file \"foo.txt\" :operation :read)
    => error: Missing required field :confidence"
  (let ((op-type (plist-get operation :operation))
        (file (plist-get operation :file))
        (confidence (plist-get operation :confidence))
        (source (plist-get operation :source))
        (command (plist-get operation :command)))

    ;; Check required fields are present
    (unless file
      (error "Operation validation failed: Missing required field :file in operation: %S" operation))
    (unless op-type
      (error "Operation validation failed: Missing required field :operation in operation: %S" operation))
    (unless confidence
      (error "Operation validation failed: Missing required field :confidence in operation: %S" operation))
    (unless source
      (error "Operation validation failed: Missing required field :source in operation: %S" operation))

    ;; Check :command field is present for sources that require it
    ;; Optional for: :test-expression, :loop-glob, :command-name (self-executing paths)
    (unless (or command
                (memq source '(:test-expression :loop-glob :command-name)))
      (error "Operation validation failed: Missing required field :command in operation: %S" operation))

    ;; Check operation type is valid
    (unless (memq op-type jf/bash-valid-operation-types)
      (error "Operation validation failed: Invalid operation type %S. Valid types: %S"
             op-type jf/bash-valid-operation-types))

    t))

(defun jf/bash-parser-has-feature-p (feature)
  "Check if bash-parser has FEATURE enabled.

Features:
  :recursive-analysis - Recursive semantic analysis
  :pattern-flow - Pattern flow tracking through substitutions (in recursive module)
  :loop-context - Loop variable binding and context tracking
  :conditional-context - Conditional branch context tracking
  :heredoc-context - Heredoc context disambiguation

Returns t if feature is available, nil otherwise."
  (pcase feature
    (:recursive-analysis (fboundp 'jf/bash-analyze-file-operations-recursive))
    (:pattern-flow (fboundp 'jf/bash-analyze-file-operations-recursive))
    (:loop-context (fboundp 'jf/bash--resolve-loop-variable))
    (:conditional-context (fboundp 'jf/bash--extract-file-test-operations))
    (:heredoc-context (fboundp 'jf/bash--determine-heredoc-context))
    (_ nil)))

(defun jf/bash--extract-from-single-command (command var-context)
  "Extract file operations from a single COMMAND with VAR-CONTEXT.

COMMAND is a single parsed command structure (from :all-commands or top-level).
VAR-CONTEXT is an alist of variable bindings.

Returns list of operation plists from all extraction sources.

Inline environment variables (from :env-vars field) are applied to var-context
for this command only. This implements bash semantics where env var prefixes
like 'PWD=/path cmd' only affect that specific command."
  ;; Skip compound command types - they're handled by recursive analyzer
  (let ((command-type (plist-get command :type)))
    (if (memq command-type '(:chain :pipeline))
        nil  ; Return nil for compound types
      ;; Process single command
      (let* (;; Apply inline env vars to context for this command only
             (env-vars (plist-get command :env-vars))
             ;; Resolve env var values (they might contain variables like $PWD)
             (resolved-env-vars
              (when (and env-vars (fboundp 'jf/bash--resolve-assignment-value))
                (mapcar (lambda (env-var)
                          (let* ((var-name (car env-var))
                                 (var-value (cdr env-var))
                                 ;; Resolve value using current context (before env vars)
                                 (resolved-value (jf/bash--resolve-assignment-value
                                                 var-value var-context)))
                            (cons var-name resolved-value)))
                        env-vars)))
             ;; Merge resolved env vars into context (env vars shadow existing vars)
             (effective-context (if resolved-env-vars
                                   (append resolved-env-vars var-context)
                                 var-context))
             (operations nil)
             (command-name (plist-get command :command-name))
             (positional-args (plist-get command :positional-args)))

        ;; Extract from redirections (high confidence)
        (when-let ((redir-ops (jf/bash-extract-operations-from-redirections command effective-context)))
          (setq operations (append operations redir-ops)))

        ;; Extract from positional arguments (command semantics)
        (when-let ((pos-ops (jf/bash-extract-operations-from-positional-args command effective-context)))
          (setq operations (append operations pos-ops)))

        ;; Extract from exec blocks (find -exec)
        (when-let ((exec-ops (jf/bash-extract-from-exec-blocks command effective-context)))
          (setq operations (append operations exec-ops)))

        ;; Check for self-execution (path-based commands)
        (when (and command-name (jf/bash--command-executes-self-p command-name))
          (let* ((resolved-path (jf/bash--resolve-path-variables command-name effective-context))
                 (final-path (if (stringp resolved-path)
                                resolved-path
                              (plist-get resolved-path :path)))
                 (unresolved-vars (when (listp resolved-path)
                                   (plist-get resolved-path :unresolved))))
            (push (append (list :file final-path
                               :operation :execute
                               :source :command-name
                               :confidence :low
                               :self-executing t
                               :script-args positional-args)
                         (when unresolved-vars
                           (list :unresolved t :unresolved-vars unresolved-vars)))
                  operations)))

        ;; Extract from nested commands (bash -c, sh -c, etc.)
        ;; Only if bash-parser-extensions is loaded
        (when (and (fboundp 'jf/bash-detect-command-injection)
                   (fboundp 'jf/bash-parse-nested-command))
          ;; Detect command injection and recursively extract operations
          (when-let ((injection-info (jf/bash-detect-command-injection command)))
            (let ((nested-cmd-string (plist-get injection-info :nested-command-string)))
              (when nested-cmd-string
                ;; Parse nested command and extract operations
                (let* ((nested-parsed (jf/bash-parse-nested-command nested-cmd-string))
                       (nested-ops (jf/bash-extract-file-operations nested-parsed effective-context)))
                  ;; Mark all nested operations as indirect
                  (dolist (op nested-ops)
                    (let ((indirect-op (copy-sequence op)))
                      (plist-put indirect-op :indirect t)
                      (push indirect-op operations))))))))

        ;; Validate all operations before returning
        (dolist (op operations)
          (jf/bash--validate-operation-type op))

        operations))))

(defun jf/bash--deduplicate-operations (operations)
  "Deduplicate OPERATIONS list by file + operation type + source context.

If multiple operations have the same :file and :operation values,
keep only the first occurrence. For exec-block operations, also
include the command name in the deduplication key to preserve
operations from different commands in multiple exec blocks.

This handles cases where a file appears multiple times in a command,
while preserving distinct operations from multiple exec blocks like:
  find . -exec grep pattern {} \\; -exec cat {} \\;

Returns deduplicated list maintaining original order."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (op operations)
      (let* ((file (plist-get op :file))
             (operation (plist-get op :operation))
             (source (plist-get op :source))
             (command (plist-get op :command))
             ;; For exec-block operations, include command in key
             ;; to preserve operations from different exec blocks
             (key (if (eq source :exec-block)
                      (list file operation source command)
                    (cons file operation))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push op result))))
    (nreverse result)))

(defun jf/bash--command-executes-self-p (command-name)
  "Return t if COMMAND-NAME is path-based executable.

A command is considered self-executing if it starts with path
prefixes that indicate direct file execution:
  ./  - Relative to current directory
  /   - Absolute path
  ../ - Relative to parent directory

This heuristic detects common shell idioms for executing scripts
and binaries without using an explicit interpreter.

COMMAND-NAME is the command name string from parsed command.

Returns t if path-based, nil otherwise.

Examples:
  (jf/bash--command-executes-self-p \"./script.sh\")      => t
  (jf/bash--command-executes-self-p \"/usr/bin/tool\")   => t
  (jf/bash--command-executes-self-p \"../bin/runner\")   => t
  (jf/bash--command-executes-self-p \"cat\")            => nil
  (jf/bash--command-executes-self-p \"script.sh\")      => nil"
  (and (stringp command-name)
       (or (string-prefix-p "./" command-name)
           (string-prefix-p "/" command-name)
           (string-prefix-p "../" command-name))))

(defun jf/bash--validate-semantics-entry (command-name semantics)
  "Validate SEMANTICS entry for COMMAND-NAME from semantics database.

COMMAND-NAME is the command symbol being validated.
SEMANTICS is the semantics plist from the database.

Validates:
  - :operations field is one of: list, :complex, :flag-dependent, :custom
  - For list specs: validate each operation spec has required fields
  - For :complex: validate :subcommand-handlers exists and is alist
  - For :flag-dependent: validate :flag-handlers exists and is list
  - For :custom: validate :handler exists and is symbol

Signals error with descriptive message if validation fails.
Returns t if valid.

This validation catches malformed database entries early (at extraction time)
instead of allowing cryptic downstream errors. Since the semantics database
is manually curated, validation helps catch typos and structural errors.

Examples:
  Valid simple command:
    (jf/bash--validate-semantics-entry
      'cat '(:operations ((:source :positional-args :operation :read))))
    => t

  Valid complex command:
    (jf/bash--validate-semantics-entry
      'git '(:operations :complex
             :subcommand-handlers ((add . (...)))))
    => t

  Invalid - missing :subcommand-handlers:
    (jf/bash--validate-semantics-entry
      'git '(:operations :complex))
    => error: Complex command git missing required :subcommand-handlers

  Invalid - malformed operation spec:
    (jf/bash--validate-semantics-entry
      'cat '(:operations ((:source :positional-args))))
    => error: Operation spec missing required :operation field"
  (let ((ops-spec (plist-get semantics :operations)))
    (cond
     ;; List of operation specs
     ((listp ops-spec)
      ;; Validate each operation spec in the list
      (dolist (spec ops-spec)
        (unless (plistp spec)
          (error "Semantics validation failed for %s: Operation spec is not a plist: %S"
                 command-name spec))
        ;; Check required fields
        (unless (plist-get spec :source)
          (error "Semantics validation failed for %s: Operation spec missing required :source field: %S"
                 command-name spec))
        (unless (plist-get spec :operation)
          (error "Semantics validation failed for %s: Operation spec missing required :operation field: %S"
                 command-name spec))))

     ;; Complex command (git, docker, etc.)
     ((eq ops-spec :complex)
      (let ((subcommand-handlers (plist-get semantics :subcommand-handlers)))
        (unless subcommand-handlers
          (error "Semantics validation failed for %s: Complex command missing required :subcommand-handlers field"
                 command-name))
        (unless (listp subcommand-handlers)
          (error "Semantics validation failed for %s: :subcommand-handlers must be an alist, got: %S"
                 command-name subcommand-handlers))))

     ;; Flag-dependent command (tar, sed, etc.)
     ((eq ops-spec :flag-dependent)
      (let ((flag-handlers (plist-get semantics :flag-handlers)))
        (unless flag-handlers
          (error "Semantics validation failed for %s: Flag-dependent command missing required :flag-handlers field"
                 command-name))
        (unless (listp flag-handlers)
          (error "Semantics validation failed for %s: :flag-handlers must be a list, got: %S"
                 command-name flag-handlers))))

     ;; Custom handler
     ((eq ops-spec :custom)
      (let ((handler (plist-get semantics :handler)))
        (unless handler
          (error "Semantics validation failed for %s: Custom command missing required :handler field"
                 command-name))
        (unless (symbolp handler)
          (error "Semantics validation failed for %s: :handler must be a symbol, got: %S"
                 command-name handler))))

     ;; Invalid operations spec type
     (t
      (error "Semantics validation failed for %s: :operations must be a list, :complex, :flag-dependent, or :custom, got: %S"
             command-name ops-spec)))

    t))

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
         (args (plist-get parsed-command :args))
         (operations nil))

    ;; Only proceed if we have a command and positional args
    (when (and command-name positional-args)
      (let ((semantics (jf/bash-lookup-command-semantics command-name)))
        (when semantics
          ;; Validate semantics entry (fail fast with clear errors)
          (condition-case err
              (jf/bash--validate-semantics-entry (intern command-name) semantics)
            (error
             ;; Log validation error instead of crashing
             (message "Warning: Semantics validation failed for command %s: %s"
                      command-name (error-message-string err))
             ;; Return early with empty operations
             (setq semantics nil)))
          (when semantics
            (let ((ops-spec (plist-get semantics :operations)))
            (cond
             ;; Complex command (git, docker, etc.) - handle subcommand
             ((eq ops-spec :complex)
              ;; For complex commands, subcommand is either explicitly set
              ;; or the first positional arg
              (let* ((effective-subcommand (or subcommand (car positional-args)))
                     (subcommand-args (if subcommand
                                         positional-args
                                       (cdr positional-args))))
                (when effective-subcommand
                  (let ((subcommand-handlers (plist-get semantics :subcommand-handlers)))
                    (when-let ((handler-spec (alist-get (intern effective-subcommand) subcommand-handlers)))
                      (setq operations
                            (jf/bash--extract-ops-from-positional-specs
                             handler-spec subcommand-args command-name var-context args)))))))

             ;; Flag-dependent command (tar, sed, etc.) - check flags
             ((eq ops-spec :flag-dependent)
              (let* ((flag-handlers (plist-get semantics :flag-handlers))
                     (matched-handler :unmatched))  ; Use sentinel value instead of nil
                ;; Find first matching flag handler
                (dolist (handler flag-handlers)
                  (when (eq matched-handler :unmatched)  ; Check for sentinel, not nil
                    (let ((trigger-flags (car handler))
                          (handler-spec (cdr handler)))
                      ;; Check if any trigger flag is present
                      (when (or (null trigger-flags)  ; Empty trigger matches always
                                (seq-some (lambda (f) (jf/bash--flag-present-p f flags)) trigger-flags))
                        (setq matched-handler handler-spec)))))  ; Can be () or operation spec list
                ;; Extract operations if handler matched (even if handler-spec is empty list)
                (unless (eq matched-handler :unmatched)  ; If not still unmatched, handler was found
                  (setq operations
                        (jf/bash--extract-ops-from-positional-specs
                         matched-handler positional-args command-name var-context args)))))

             ;; Custom command handler - delegate to custom function
             ((eq ops-spec :custom)
              (let ((custom-handler (plist-get semantics :handler)))
                (when (and custom-handler (fboundp custom-handler))
                  (setq operations (funcall custom-handler parsed-command var-context)))))

             ;; Simple command - direct operation specs
             ((listp ops-spec)
              (setq operations
                    (jf/bash--extract-ops-from-positional-specs
                     ops-spec positional-args command-name var-context args)))))))))

    operations))

(defun jf/bash--extract-ops-from-positional-specs (op-specs positional-args command-name var-context &optional args)
  "Apply OP-SPECS to POSITIONAL-ARGS to extract file operations.

OP-SPECS is a list of operation specification plists.
POSITIONAL-ARGS is list of argument strings.
COMMAND-NAME is the command performing the operations.
VAR-CONTEXT is optional variable resolution context.
ARGS is the original argument list (preserves flags and order).

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
                     ;; Pre-resolve command substitutions before checking for unresolvable patterns
                     ;; This allows $(pwd), $(basename $(pwd)), etc. to be statically resolved
                     (cmd-resolved (if var-context
                                       (jf/bash-resolve-command-substitution file-path var-context)
                                     file-path)))
                ;; Skip unresolvable command substitutions (marked with :unresolved or still containing $()
                ;; These are processed recursively and handled by pattern flow operations
                ;; Note: Deterministic commands are already resolved above, so won't be skipped
                (unless (or (eq cmd-resolved :unresolved)
                            (and (stringp cmd-resolved) (string-match-p "\\$(" cmd-resolved)))
                  (let* ((resolved-path (jf/bash--resolve-path-variables cmd-resolved var-context))
                         ;; Extract path and unresolved metadata
                         (final-path (if (stringp resolved-path)
                                        resolved-path
                                      (plist-get resolved-path :path)))
                         (unresolved-vars (when (listp resolved-path)
                                           (plist-get resolved-path :unresolved)))
                         (has-pattern (jf/bash--has-glob-pattern-p final-path))
                         ;; Capture script arguments for execute operations at index 0
                         ;; Use :args to preserve flag arguments in original order
                         (script-args (when (and (eq operation :execute)
                                                (eq index 0)
                                                args)
                                       (let ((script-pos (cl-position file-path args :test #'equal)))
                                         (if script-pos
                                             (nthcdr (1+ script-pos) args)
                                           '())))))
                    ;; Create operation plist
                    ;; Confidence degradation: operations with unresolved variables get :medium
                    ;; confidence instead of :high (security requirement - spec.md lines 196-199)
                    (push (append (list :file final-path
                                       :operation operation
                                       :confidence (if unresolved-vars :medium :high)
                                       :source :positional-arg
                                       :command command-name)
                                 (when unresolved-vars
                                   (list :unresolved t :unresolved-vars unresolved-vars))
                                 (when has-pattern
                                   (list :pattern t))
                                 ;; Always include :script-args for execute operations
                                 (when (eq operation :execute)
                                   (list :script-args (or script-args '()))))
                          operations)))))))))

    ;; Validate all operations before returning
    (let ((result (nreverse operations)))
      (dolist (op result)
        (jf/bash--validate-operation-type op))
      result)))

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
  "Resolve variables, command substitutions, and relative paths in FILE-PATH.

This function performs three-stage resolution:
  1. Variable resolution ($VAR, ${VAR}) using `jf/bash-resolve-variables'
  2. Command substitution ($(pwd), `pwd`) using `jf/bash-resolve-pwd-substitution'
  3. Relative path resolution (., ./, ../) using `jf/bash-resolve-relative-path'

FILE-PATH is the file path string (may contain variables, substitutions, and/or
relative paths). VAR-CONTEXT is optional alist mapping variable names to values.

The resolution order matters:
  - Variables first: $PWD/./file.txt → /base/dir/./file.txt
  - Then command substitutions: $(pwd)/file.txt → /base/dir/file.txt
  - Then relative paths: /base/dir/./file.txt → /base/dir/file.txt

Returns:
  - String: Fully resolved path (all variables, substitutions, and relative paths resolved)
  - Plist: Partially resolved with :unresolved metadata (some variables unresolved)

Examples:
  (jf/bash--resolve-path-variables \"/workspace/file.txt\" nil)
    => \"/workspace/file.txt\"

  (jf/bash--resolve-path-variables \"$WORKSPACE/file.txt\"
                                   '((WORKSPACE . \"/workspace\")))
    => \"/workspace/file.txt\"

  (jf/bash--resolve-path-variables \"$(pwd)/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash--resolve-path-variables \"./file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash--resolve-path-variables \"../other/file.txt\" '((PWD . \"/base/dir/sub\")))
    => \"/base/dir/other/file.txt\""
  ;; Stage 1: Resolve variables
  (let ((var-resolved (jf/bash-resolve-variables file-path var-context)))
    ;; Stage 2: Resolve command substitutions (pwd)
    ;; Handle both simple string results and plist results from variable resolution
    (let ((pwd-resolved
           (if (stringp var-resolved)
               ;; All variables resolved - apply pwd substitution
               (jf/bash-resolve-pwd-substitution var-resolved var-context)
             ;; Partial variable resolution - still apply pwd substitution to :path
             (let* ((path (plist-get var-resolved :path))
                    (unresolved-vars (plist-get var-resolved :unresolved))
                    (resolved-path (jf/bash-resolve-pwd-substitution path var-context)))
               (list :path resolved-path :unresolved unresolved-vars)))))
      ;; Stage 3: Resolve relative paths
      (if (stringp pwd-resolved)
          ;; All substitutions resolved - apply relative path resolution
          (jf/bash-resolve-relative-path pwd-resolved var-context)
        ;; Still has unresolved variables - apply relative path to :path
        (let* ((path (plist-get pwd-resolved :path))
               (unresolved-vars (plist-get pwd-resolved :unresolved))
               (resolved-path (jf/bash-resolve-relative-path path var-context)))
          ;; Return plist with resolved path and unresolved variables
          (list :path resolved-path :unresolved unresolved-vars))))))

(defun jf/bash-extract-operations-from-redirections (parsed-command &optional var-context)
  "Extract file operations from :redirections field with high confidence.

PARSED-COMMAND is the output of `jf/bash-parse'.
VAR-CONTEXT is an optional alist mapping variable names (symbols) to values (strings).

Returns list of operation plists:
  (:file \"path\" :operation :read/:write/:append
   :confidence :high :source :redirection
   :command \"cmd\" :metadata (:operator \">\" :descriptor nil ...))

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
       :command \"cat\" :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cmd >> log.txt\")
    nil)
  => ((:file \"log.txt\" :operation :append :confidence :high
       :command \"cmd\" :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cat < input.txt\")
    nil)
  => ((:file \"input.txt\" :operation :read :confidence :high
       :command \"cat\" :source :redirection :metadata (...)))

  (jf/bash-extract-operations-from-redirections
    (jf/bash-parse \"cmd > $OUTFILE\")
    '((OUTFILE . \"/workspace/output.txt\")))
  => ((:file \"/workspace/output.txt\" :operation :write :confidence :high
       :command \"cmd\" :source :redirection :metadata (...)))"
  (let ((operations nil))
    ;; Handle different command types
    (let ((command-type (plist-get parsed-command :type))
          (all-commands (plist-get parsed-command :all-commands)))

      (cond
       ;; Simple command: check for redirections
       ((eq command-type :simple)
        (when-let ((redirections (plist-get parsed-command :redirections))
                   (command-name (plist-get parsed-command :command-name)))
          (setq operations (jf/bash--extract-ops-from-redirect-list redirections command-name var-context))))

       ;; Pipeline or chain: process each command's redirections
       ((or (eq command-type :pipeline) (eq command-type :chain))
        (dolist (cmd all-commands)
          (when-let ((redirections (plist-get cmd :redirections))
                     (command-name (plist-get cmd :command-name)))
            (setq operations
                  (append operations
                         (jf/bash--extract-ops-from-redirect-list redirections command-name var-context))))))

       ;; Single command structure (no :type field)
       ;; This happens when called from jf/bash--extract-from-single-command
       ((null command-type)
        (when-let ((redirections (plist-get parsed-command :redirections))
                   (command-name (plist-get parsed-command :command-name)))
          (setq operations (jf/bash--extract-ops-from-redirect-list redirections command-name var-context))))))

    operations))

(defun jf/bash--extract-ops-from-redirect-list (redirections command-name var-context)
  "Extract operations from REDIRECTIONS list with COMMAND-NAME and VAR-CONTEXT.

REDIRECTIONS is a list of redirection plists from the parser.
COMMAND-NAME is the name of the command performing the redirections.
VAR-CONTEXT is optional variable resolution context.

Returns list of operation plists with resolved file paths."
  (let ((operations nil)
        (has-heredoc nil))
    ;; First pass: check if there's a heredoc redirect
    (dolist (redir redirections)
      (when (eq (plist-get redir :type) :heredoc)
        (setq has-heredoc t)))

    ;; Second pass: extract file operations
    (dolist (redir redirections)
      (let* ((redir-type (plist-get redir :type))
             (operator (plist-get redir :operator))
             (destination (plist-get redir :destination)))

        ;; Only process file redirections (not heredoc/herestring)
        (when (and (eq redir-type :file) destination)
          ;; Map operator to operation type
          (let ((operation-type (jf/bash--map-redirect-operator-to-operation operator)))
            (when operation-type
              ;; Resolve variables and relative paths in destination path
              (let* ((resolved-path (jf/bash--resolve-path-variables destination var-context))
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
                                   :command command-name
                                   :metadata redir)
                             (when unresolved-vars
                               (list :unresolved t :unresolved-vars unresolved-vars))
                             (when has-heredoc
                               (list :heredoc-content t)))
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
               ;; Extract operations using full semantics-aware extraction
               (extracted-ops (jf/bash-extract-operations-from-positional-args exec-block var-context)))
          ;; Adjust metadata for exec-block source
          (dolist (op extracted-ops)
            (plist-put op :source :exec-block)
            (plist-put op :indirect t)
            (plist-put op :exec-type exec-type)
            (push op operations)))))

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
  (when (and command-name
             ;; Don't try to intern assignment strings like "DIR=/tmp"
             (not (string-match-p "=" command-name)))
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
