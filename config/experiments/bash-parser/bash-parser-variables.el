;;; bash-parser-variables.el --- Variable context management -*- lexical-binding: t; -*-

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

Detects assignments in two patterns:

1. Unified pattern: VAR=value in command-name field
   Example: (:command-name "DIR=/tmp" ...) => ((DIR . "/tmp"))

2. Split pattern: VAR in command-name, value in first positional-arg
   This occurs when tree-sitter parses assignments in chains
   Example: (:command-name "DIR" :positional-args ("/tmp") ...)
   => ((DIR . "/tmp"))

The split pattern is detected by:
- Command name matches valid variable name pattern (^[A-Za-z_][A-Za-z0-9_]*$)
- Exactly one positional argument
- Command name not in semantics database (excludes known commands like 'cat')
- Value doesn't start with '-' (excludes flags)

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

    ;; Check for split assignment pattern: command-name is variable name only,
    ;; first positional arg is the value. This happens when tree-sitter parses
    ;; assignments in chains (e.g., "DIR=/tmp && cmd" becomes command-name="DIR", args=("/tmp"))
    (when (and command-name
               (string-match "^[A-Za-z_][A-Za-z0-9_]*$" command-name)  ; Valid var name (no =)
               positional-args
               (= (length positional-args) 1)  ; Exactly one positional arg
               ;; Heuristic: if command is in semantics DB, it's not an assignment
               (not (jf/bash-lookup-command-semantics command-name)))
      (let ((var-name command-name)
            (var-value (car positional-args)))
        ;; Only treat as assignment if value doesn't look like a flag
        (unless (string-prefix-p "-" var-value)
          (push (cons (intern var-name) var-value) assignments))))

    ;; Also check positional args for assignment patterns
    ;; (in case tree-sitter includes them there)
    (dolist (arg positional-args)
      (when (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" arg)
        (let ((var-name (match-string 1 arg))
              (var-value (match-string 2 arg)))
          (push (cons (intern var-name) var-value) assignments))))

    (nreverse assignments)))

(provide 'bash-parser-variables)
;;; bash-parser-variables.el ends here
