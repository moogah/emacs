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

        ;; Normalize multiple slashes in paths (e.g., *//config -> */config)
        (setq resolved (replace-regexp-in-string "/+" "/" resolved))

        ;; Return format based on resolution status
        (if unresolved
            ;; Partial or no resolution - return plist with metadata
            (list :path resolved :unresolved (nreverse unresolved))
          ;; Full resolution - return simple string
          resolved)))))

(defun jf/bash--navigate-parent-path (base-path levels)
  "Navigate up LEVELS directories from BASE-PATH.

Uses `file-name-directory' repeatedly to go up the directory tree.
Going up past root stops at \"/\".

Examples:
  (jf/bash--navigate-parent-path \"/a/b/c\" 1)  => \"/a/b/\"
  (jf/bash--navigate-parent-path \"/a/b/c\" 2)  => \"/a/\"
  (jf/bash--navigate-parent-path \"/a\" 5)      => \"/\"
  (jf/bash--navigate-parent-path \"/a/b/\" 1)   => \"/a/\""
  (let ((result base-path))
    (dotimes (_ levels)
      (let ((parent (file-name-directory (directory-file-name result))))
        (if (or (null parent) (equal parent result))
            ;; Reached root, stop navigating
            (setq result "/")
          (setq result parent))))
    result))

(defun jf/bash-resolve-relative-path (file-path var-context)
  "Resolve relative path (., ./, ../) in FILE-PATH using PWD from VAR-CONTEXT.

VAR-CONTEXT is an alist mapping variable names to values. Extracts PWD value
to resolve relative paths. If PWD is not in context, returns file-path unchanged.

Handles three relative path patterns:
  .           - Returns PWD value directly
  ./path      - Concatenates PWD with path after ./
  ../path     - Navigates up from PWD, then appends remaining path

Resolution behavior:
  - PWD in context: Resolves relative paths to absolute paths
  - No PWD in context: Returns original path unchanged (unresolved)
  - Absolute path: Returns original path unchanged (no resolution needed)

The function normalizes the result using `expand-file-name' to clean up
multiple slashes and resolve . components.

Examples:
  (jf/bash-resolve-relative-path \".\" '((PWD . \"/base/dir\")))
    => \"/base/dir\"

  (jf/bash-resolve-relative-path \"./file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash-resolve-relative-path \"../other/file.txt\" '((PWD . \"/base/dir/sub\")))
    => \"/base/dir/other/file.txt\"

  (jf/bash-resolve-relative-path \"../../file.txt\" '((PWD . \"/a/b/c\")))
    => \"/a/file.txt\"

  (jf/bash-resolve-relative-path \"./file.txt\" nil)
    => \"./file.txt\"  ; unchanged - no PWD context

  (jf/bash-resolve-relative-path \"/absolute/path.txt\" '((PWD . \"/base\")))
    => \"/absolute/path.txt\"  ; unchanged - already absolute

Security note: Relative paths must be resolved for scope validation. When
run_bash_command(cmd, dir) executes with PWD=dir, the shell resolves ./file.txt
to /dir/file.txt. The parser must extract the same absolute path for security
checks to work correctly."
  (let ((pwd (alist-get 'PWD var-context)))
    (cond
     ;; No PWD in context - return unchanged
     ((null pwd)
      file-path)

     ;; Already absolute path - return unchanged
     ((string-prefix-p "/" file-path)
      file-path)

     ;; Case 1: "." alone - return PWD directly
     ((equal file-path ".")
      pwd)

     ;; Case 2: "./" prefix - concat PWD with remainder
     ((string-prefix-p "./" file-path)
      (let ((remainder (substring file-path 2)))
        (expand-file-name (concat pwd "/" remainder))))

     ;; Case 3: "../" prefix - navigate up from PWD
     ((string-prefix-p "../" file-path)
      (let* ((components (split-string file-path "/" t))
             (parent-count 0)
             (remaining-components nil))
        ;; Count leading ".." components
        (while (and components (equal (car components) ".."))
          (setq parent-count (1+ parent-count))
          (setq components (cdr components)))
        ;; Remaining components are the path after all ../
        (setq remaining-components components)
        ;; Navigate up from PWD
        (let ((base (jf/bash--navigate-parent-path pwd parent-count)))
          (if remaining-components
              (expand-file-name (concat base (string-join remaining-components "/")))
            ;; No remaining components - just return the parent directory
            base))))

     ;; Not a relative path pattern we handle - return unchanged
     (t file-path))))

(defun jf/bash-resolve-pwd-substitution (file-path var-context)
  "Resolve $(pwd) and `pwd` command substitutions in FILE-PATH using VAR-CONTEXT.

VAR-CONTEXT is an alist mapping variable names to values. Extracts PWD value
to resolve pwd command substitutions. If PWD is not in context, returns
file-path unchanged.

Handles two pwd substitution patterns:
  $(pwd)/path   - Modern command substitution syntax
  `pwd`/path    - Legacy backtick syntax

Resolution behavior:
  - PWD in context: Replaces pwd substitutions with PWD value
  - No PWD in context: Returns original path unchanged (unresolved)
  - No pwd substitution: Returns original path unchanged

Examples:
  (jf/bash-resolve-pwd-substitution \"$(pwd)/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash-resolve-pwd-substitution \"`pwd`/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash-resolve-pwd-substitution \"$(pwd)\" '((PWD . \"/base/dir\")))
    => \"/base/dir\"

  (jf/bash-resolve-pwd-substitution \"cat $(pwd)/file.txt\" '((PWD . \"/base/dir\")))
    => \"cat /base/dir/file.txt\"

  (jf/bash-resolve-pwd-substitution \"$(pwd)/file.txt\" nil)
    => \"$(pwd)/file.txt\"  ; unchanged - no PWD context

Security note: $(pwd) substitutions must be resolved for scope validation. When
run_bash_command(cmd, dir) executes with PWD=dir, the shell evaluates $(pwd) to
dir. The parser must extract the same value for security checks to work correctly."
  (let ((pwd (alist-get 'PWD var-context)))
    (if (null pwd)
        ;; No PWD in context - return unchanged
        file-path
      ;; PWD available - resolve pwd substitutions
      (let ((result file-path))
        ;; Replace $(pwd) with PWD value
        ;; Use regexp-quote to properly escape the literal string
        (setq result (replace-regexp-in-string
                      (regexp-quote "$(pwd)")
                      pwd
                      result
                      t))    ; fixed-case (literal replacement)
        ;; Replace `pwd` with PWD value
        (setq result (replace-regexp-in-string
                      (regexp-quote "`pwd`")
                      pwd
                      result
                      t))    ; fixed-case (literal replacement)
        result))))

(defun jf/bash-track-assignments (parsed-command &optional initial-context)
  "Track variable assignments from PARSED-COMMAND, merging with INITIAL-CONTEXT.

Extracts VAR=value assignments and builds a context alist. Assignment values
are resolved using the current context before being added, enabling variable
chain tracking:

  BASE=$PWD; DIR=$BASE/sub; cat $DIR/file.txt
  Step 1: BASE=$PWD with PWD=/base → context: ((BASE . \"/base\") (PWD . \"/base\"))
  Step 2: DIR=$BASE/sub → resolves $BASE → context: ((DIR . \"/base/sub\") (BASE . \"/base\") ...)
  Step 3: cat $DIR/file.txt → resolves $DIR to /base/sub/file.txt

This is critical for security validation - assignments must be resolved to
absolute paths for scope checking."
  (let ((context (copy-alist initial-context))
        (command-type (plist-get parsed-command :type))
        (all-commands (plist-get parsed-command :all-commands)))

    (cond
     ;; Chain or pipeline: process all commands in order
     ((or (eq command-type :chain) (eq command-type :pipeline))
      (dolist (cmd all-commands)
        ;; Pass current context so assignments can resolve using earlier assignments
        (when-let ((assignments (jf/bash--extract-assignments-from-command cmd context)))
          ;; Prepend assignments to maintain left-to-right precedence
          (setq context (append assignments context)))))

     ;; Simple command: check for assignments
     ((eq command-type :simple)
      (when-let ((assignments (jf/bash--extract-assignments-from-command parsed-command context)))
        (setq context (append assignments context)))))

    context))

(defun jf/bash--resolve-assignment-value (value var-context)
  "Resolve assignment VALUE using VAR-CONTEXT.

Applies three-stage resolution:
  1. Variable resolution ($VAR, ${VAR})
  2. Command substitution ($(pwd), `pwd`)
  3. Relative path resolution (., ./, ../)

Returns resolved string. If resolution fails (unresolved variables), returns
original value with :unresolved marker or the partially resolved plist.

Examples:
  (jf/bash--resolve-assignment-value \"$PWD\" '((PWD . \"/base\")))
    => \"/base\"

  (jf/bash--resolve-assignment-value \"./sub\" '((PWD . \"/base\")))
    => \"/base/sub\"

  (jf/bash--resolve-assignment-value \"$UNKNOWN\" nil)
    => (:path \"$UNKNOWN\" :unresolved (\"UNKNOWN\"))"
  (let ((resolved (jf/bash-resolve-variables value var-context)))
    ;; Check if fully resolved (string) or partial (plist)
    (if (stringp resolved)
        ;; Fully resolved variables - apply pwd and relative path resolution
        (let ((pwd-resolved (jf/bash-resolve-pwd-substitution resolved var-context)))
          (jf/bash-resolve-relative-path pwd-resolved var-context))
      ;; Partial resolution - return as-is (plist with :unresolved)
      resolved)))

(defun jf/bash--extract-assignments-from-command (command &optional var-context)
  "Extract variable assignments from COMMAND structure with value resolution.

Returns alist of (VAR-SYMBOL . VALUE-STRING) or nil. Handles both unified
and split assignment patterns.

VAR-CONTEXT is an optional alist used to resolve assignment values. Assignment
values are resolved using the same logic as file paths:
  1. Variable resolution ($VAR, ${VAR})
  2. Command substitution ($(pwd), `pwd`)
  3. Relative path resolution (., ./, ../)

This enables variable chain tracking for security validation:
  BASE=$PWD; cat $BASE/file.txt → resolves BASE to PWD's value

If resolution results in :unresolved, the value is marked with :unresolved flag."
  (let ((assignments nil)
        (command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))

    ;; Check if command-name itself is an assignment (VAR=value)
    (when (and command-name (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" command-name))
      (let* ((var-name (match-string 1 command-name))
             (var-value (match-string 2 command-name))
             (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
        (push (cons (intern var-name) resolved-value) assignments)))

    ;; Check for split assignment pattern: command-name is variable name only,
    ;; first positional arg is the value. This happens when tree-sitter parses
    ;; assignments in chains (e.g., "DIR=/tmp && cmd" becomes command-name="DIR", args=("/tmp"))
    (when (and command-name
               (string-match "^[A-Za-z_][A-Za-z0-9_]*$" command-name)  ; Valid var name (no =)
               positional-args
               (= (length positional-args) 1)  ; Exactly one positional arg
               ;; Heuristic: if command is in semantics DB, it's not an assignment
               (not (jf/bash-lookup-command-semantics command-name)))
      (let* ((var-name command-name)
             (var-value (car positional-args))
             (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
        ;; Only treat as assignment if value doesn't look like a flag
        (unless (string-prefix-p "-" var-value)
          (push (cons (intern var-name) resolved-value) assignments))))

    ;; Also check positional args for assignment patterns
    ;; (in case tree-sitter includes them there)
    (dolist (arg positional-args)
      (when (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" arg)
        (let* ((var-name (match-string 1 arg))
               (var-value (match-string 2 arg))
               (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
          (push (cons (intern var-name) resolved-value) assignments))))

    (nreverse assignments)))

(provide 'bash-parser-variables)
;;; bash-parser-variables.el ends here
