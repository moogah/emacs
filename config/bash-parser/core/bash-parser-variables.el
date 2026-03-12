;; Author: Jeff Farr
;; Keywords: bash, parser, variables, resolution
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Variable context management for bash command parsing.
;; Handles variable references in file paths for scope-aware file operations.

;;; Code:

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
  (unless (stringp file-path)
    (signal 'wrong-type-argument (list 'stringp file-path)))
  (let ((var-names nil))
    (save-match-data
      ;; Match ${VAR} or $VAR patterns
      ;; Group 1: braced variable (${VAR})
      ;; Group 2: simple variable ($VAR)
      (cl-loop with start = 0
               while (string-match "\\${\\([A-Za-z_][A-Za-z0-9_]*\\)}\\|\\$\\([A-Za-z_][A-Za-z0-9_]*\\)"
                                   file-path start)
               do (push (or (match-string 1 file-path)
                           (match-string 2 file-path))
                        var-names)
               do (setq start (match-end 0))))
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
  (condition-case err
      (progn
        (unless (stringp file-path)
          (error "file-path must be a string, got %S" file-path))
        (unless (or (null var-context) (listp var-context))
          (error "var-context must be nil or alist, got %S" var-context))

        ;; Expand tilde in paths before variable resolution
        (let ((file-path-expanded
               (if (and (string-prefix-p "~/" file-path)
                        (alist-get 'HOME var-context))
                   (concat (alist-get 'HOME var-context)
                           (substring file-path 1))  ; Remove ~ keep /
                 file-path)))

          (let* ((detection-result (jf/bash-detect-variable-references file-path-expanded))
                 (has-variables (car detection-result))
                 (var-names (cdr detection-result)))

            ;; Fast path: no variables found
            (if (not has-variables)
                file-path-expanded

              ;; Process each variable reference
              (let ((resolved file-path-expanded)
                    (unresolved nil))
                (dolist (var-name var-names)
                  (let ((value (alist-get (intern var-name) var-context)))
                    (if value
                        (progn
                          ;; Validate resolved value for control characters
                          (when (string-match-p "[[:cntrl:]]" value)
                            (lwarn :bash-parser :warning
                                   "Variable %s contains control characters: %S"
                                   var-name value))
                          ;; Replace all occurrences of $VAR or ${VAR} with value
                          ;; Use word boundary \\b for $VAR to prevent partial matches
                          ;; (e.g., matching $VAR when the variable is $VARIABLE)
                          (setq resolved
                                (replace-regexp-in-string
                                 (format "\\${%s}\\|\\$%s\\b" var-name var-name)
                                 value
                                 resolved
                                 t  ; fixedcase - preserve case
                                 t))) ; literal - treat replacement string literally
                      ;; Track unresolved variable
                      (push var-name unresolved))))

                ;; Normalize multiple slashes in paths (e.g., *//config -> */config)
                (setq resolved (replace-regexp-in-string "/+" "/" resolved))

                ;; Return format based on resolution status
                (if unresolved
                    ;; Partial or no resolution - return plist with metadata
                    (list :path resolved :unresolved (nreverse unresolved))
                  ;; Full resolution - return simple string
                  resolved))))))
    (error (list :success nil
                 :error (format "Variable resolution error: %s" (error-message-string err))
                 :path file-path))))

(defun jf/bash--navigate-parent-path (base-path levels)
  "Navigate up LEVELS directories from BASE-PATH using Emacs file functions.

Repeatedly applies file-name-directory to move up the directory tree, stopping
at the filesystem root (\"/\") if LEVELS exceeds the depth of BASE-PATH.

BASE-PATH is a string representing an absolute or relative path.
LEVELS is an integer specifying how many directory levels to ascend.

Returns a string representing the resulting path after ascending LEVELS times.

Navigation behavior:
  - Uses file-name-directory to remove one level at a time
  - Normalizes with directory-file-name to handle trailing slashes
  - Stops at root (\"/\") if ascending beyond filesystem root
  - Returns trailing slash in result (directory form)

Examples:
  (jf/bash--navigate-parent-path \"/a/b/c\" 1)
    => \"/a/b/\"

  (jf/bash--navigate-parent-path \"/a/b/c\" 2)
    => \"/a/\"

  (jf/bash--navigate-parent-path \"/a\" 5)
    => \"/\" (stops at root)

  (jf/bash--navigate-parent-path \"/a/b/\" 1)
    => \"/a/\" (handles trailing slash)

  (jf/bash--navigate-parent-path \"/workspace/src/foo.txt\" 2)
    => \"/workspace/\"

Internal helper for jf/bash-resolve-relative-path.

BASE-PATH must be a string (caller responsibility).
LEVELS must be a non-negative integer (caller responsibility)."
  (let ((result base-path)
        (remaining levels))
    (while (> remaining 0)
      (let ((parent (file-name-directory (directory-file-name result))))
        (cond
         ;; Hit filesystem root - stop immediately (DOS prevention)
         ((or (null parent)
              (equal parent result)
              (string= result "/"))
          (setq result "/")
          (setq remaining 0))  ; Break loop

         ;; Normal case - ascend one level
         (t
          (setq result parent)
          (setq remaining (1- remaining))))))
    result))

(defun jf/bash-resolve-relative-path (file-path var-context &optional current-pwd)
  "Resolve relative path (., ./, ../) and bare filenames in FILE-PATH.

VAR-CONTEXT is an alist mapping variable names to values. Extracts PWD value
to resolve relative paths. If PWD is not in context, returns file-path unchanged.

CURRENT-PWD is an optional override for the directory context. If provided,
it takes precedence over PWD from VAR-CONTEXT.

Handles four relative path patterns:
  .             - Returns PWD value directly
  ./path        - Concatenates PWD with path after ./
  ../path       - Navigates up from PWD, then appends remaining path
  file.txt      - Bare filename: concatenates PWD with file path (NEW)
  subdir/file   - Relative path without ./ prefix: resolved against PWD (NEW)

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

  ;; NEW: Bare filenames resolved against PWD
  (jf/bash-resolve-relative-path \"file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash-resolve-relative-path \"subdir/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/subdir/file.txt\"

  ;; Using current-pwd parameter
  (jf/bash-resolve-relative-path \"file.txt\" nil \"/base/dir\")
    => \"/base/dir/file.txt\"

  (jf/bash-resolve-relative-path \"./file.txt\" nil)
    => \"./file.txt\"  ; unchanged - no PWD context

  (jf/bash-resolve-relative-path \"/absolute/path.txt\" '((PWD . \"/base\")))
    => \"/absolute/path.txt\"  ; unchanged - already absolute

Security note: Relative paths and bare filenames must be resolved for scope
validation. When run_bash_command(cmd, dir) executes with PWD=dir, the shell
resolves file.txt to /dir/file.txt. The parser must extract the same absolute
path for security checks to work correctly."
  (unless (stringp file-path)
    (signal 'wrong-type-argument (list 'stringp file-path)))
  (let ((pwd (or current-pwd (alist-get 'PWD var-context))))
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

     ;; Case 4: Bare filename or relative path (no ./ or ../ prefix)
     ;; NEW: Resolve against PWD for security validation
     ;; Examples: "file.txt" -> "/pwd/file.txt"
     ;;           "subdir/file.txt" -> "/pwd/subdir/file.txt"
     (t
      (expand-file-name (concat pwd "/" file-path))))))

(defun jf/bash--static-dirname (path)
  "Static evaluation of bash dirname command on PATH without executing shell.

Extracts the directory component of PATH using Emacs file-name-directory,
mimicking the behavior of the bash dirname command. This enables static
resolution of $(dirname PATH) command substitutions during parsing.

PATH is a string representing a file path (absolute or relative).

Returns a string containing the directory portion of PATH, with trailing slash
removed for consistency with bash dirname behavior. Returns empty string for
bare filenames without directory component.

Dirname behavior:
  - /path/to/file.txt → /path/to
  - /path/to/ → /path
  - file.txt → \"\" (empty string)
  - / → /

Examples:
  (jf/bash--static-dirname \"/path/to/file.txt\")
    => \"/path/to\"

  (jf/bash--static-dirname \"/path/to/\")
    => \"/path\"

  (jf/bash--static-dirname \"file.txt\")
    => \"\"

  (jf/bash--static-dirname \"/\")
    => \"/\"

  (jf/bash--static-dirname \"/workspace/src/foo.el\")
    => \"/workspace/src\"

Internal helper for jf/bash--resolve-command-substitution."
  (let ((dir (file-name-directory path)))
    (if dir
        ;; Remove trailing slash for consistency with bash dirname
        (directory-file-name dir)
      "")))

(defun jf/bash--static-basename (path &optional suffix)
  "Static evaluation of bash basename command on PATH without executing shell.

Extracts the filename component of PATH using Emacs file-name-nondirectory,
mimicking the behavior of the bash basename command. Optionally removes SUFFIX
from the result if PATH ends with it. This enables static resolution of
$(basename PATH [SUFFIX]) command substitutions during parsing.

PATH is a string representing a file path (absolute or relative).
SUFFIX is an optional string suffix to remove from the filename (e.g., \".txt\").

Returns a string containing the filename portion of PATH. If SUFFIX is provided
and matches the end of the filename, it is removed from the result.

Basename behavior:
  - /path/to/file.txt → file.txt
  - /path/to/file.txt with suffix \".txt\" → file
  - /path/to/ → \"\" (empty string)
  - file.txt → file.txt

Examples:
  (jf/bash--static-basename \"/path/to/file.txt\")
    => \"file.txt\"

  (jf/bash--static-basename \"/path/to/file.txt\" \".txt\")
    => \"file\"

  (jf/bash--static-basename \"/path/to/\")
    => \"\"

  (jf/bash--static-basename \"file.txt\")
    => \"file.txt\"

  (jf/bash--static-basename \"/workspace/src/foo.el\" \".el\")
    => \"foo\"

Internal helper for jf/bash--resolve-command-substitution."
  (let ((base (file-name-nondirectory (directory-file-name path))))
    (if (and suffix (string-suffix-p suffix base))
        (substring base 0 (- (length base) (length suffix)))
      base)))

(defun jf/bash--resolve-pwd-substitution (file-path var-context)
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
  (jf/bash--resolve-pwd-substitution \"$(pwd)/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash--resolve-pwd-substitution \"`pwd`/file.txt\" '((PWD . \"/base/dir\")))
    => \"/base/dir/file.txt\"

  (jf/bash--resolve-pwd-substitution \"$(pwd)\" '((PWD . \"/base/dir\")))
    => \"/base/dir\"

  (jf/bash--resolve-pwd-substitution \"cat $(pwd)/file.txt\" '((PWD . \"/base/dir\")))
    => \"cat /base/dir/file.txt\"

  (jf/bash--resolve-pwd-substitution \"$(pwd)/file.txt\" nil)
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

(defun jf/bash--resolve-command-substitution (file-path var-context)
  "Resolve command substitutions in FILE-PATH using static evaluation.

VAR-CONTEXT is an alist mapping variable names to values. This function
attempts to statically evaluate command substitutions for deterministic
commands. Complex runtime commands remain unresolved.

Supported commands:
  $(pwd)              - Returns PWD from var-context
  $(dirname PATH)     - Returns directory component of PATH
  $(basename PATH)    - Returns filename component of PATH

Resolution process:
  1. Detect command substitution patterns: $(command args) or `command args`
  2. Resolve any variables in arguments using var-context
  3. Attempt static evaluation of known commands
  4. If successful, replace substitution with result
  5. If command is unknown or arguments unresolved, return :unresolved marker

Examples:
  ;; dirname with variable argument
  (jf/bash--resolve-command-substitution
    \"$(dirname $FILE)\"
    '((FILE . \"/path/to/file.txt\")))
  => \"/path/to\"

  ;; dirname with literal argument
  (jf/bash--resolve-command-substitution
    \"$(dirname /path/to/file.txt)\"
    nil)
  => \"/path/to\"

  ;; basename with suffix removal
  (jf/bash--resolve-command-substitution
    \"$(basename $FILE .txt)\"
    '((FILE . \"/path/to/file.txt\")))
  => \"file\"

  ;; pwd substitution
  (jf/bash--resolve-command-substitution
    \"$(pwd)/file.txt\"
    '((PWD . \"/base/dir\")))
  => \"/base/dir/file.txt\"

  ;; Complex nested substitution
  (jf/bash--resolve-command-substitution
    \"$(dirname $(dirname $FILE))\"
    '((FILE . \"/a/b/c/file.txt\")))
  => \"/a/b\"

  ;; Unknown command - return :unresolved
  (jf/bash--resolve-command-substitution
    \"$(find . -name *.txt)\"
    nil)
  => :unresolved

  ;; Unresolved variable in argument - return :unresolved
  (jf/bash--resolve-command-substitution
    \"$(dirname $UNKNOWN)\"
    nil)
  => :unresolved

Security note: Only deterministic commands are evaluated. Runtime commands
like find, ls, or which cannot be statically evaluated and return :unresolved."
  (unless (stringp file-path)
    (signal 'wrong-type-argument (list 'stringp file-path)))
  (let ((result file-path)
        (max-iterations 10)  ; Prevent infinite loops in nested substitutions
        (iteration 0)
        (changed t))

    ;; Iterate until no more substitutions can be resolved
    (while (and changed (< iteration max-iterations))
      (setq iteration (1+ iteration))
      (setq changed nil)

      ;; Match $(command args...) or `command args...`
      ;; Process one substitution at a time (innermost first for nested cases)
      ;; Pattern [^)$]+ stops at first ) or $, matching innermost substitution
      ;; Nested support comes from the loop (lines 561-636), not the regex
      (when (string-match "\\$([^)$]+)\\|`[^`]+`" result)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (substitution (match-string 0 result))
               ;; Extract command and args from substitution
               (inner-cmd (cond
                           ;; $(command args) - extract between $( and )
                           ((string-prefix-p "$(" substitution)
                            (substring substitution 2 -1))
                           ;; `command args` - extract between ` and `
                           ((string-prefix-p "`" substitution)
                            (substring substitution 1 -1))
                           (t nil))))

          (when inner-cmd
            ;; Parse command and arguments
            (let* ((parts (split-string inner-cmd))
                   (cmd (car parts))
                   (args (cdr parts)))

              ;; Attempt to evaluate the command
              (let ((evaluation-result
                     (cond
                      ;; pwd command
                      ((equal cmd "pwd")
                       (alist-get 'PWD var-context))

                      ;; dirname command with one argument
                      ((and (equal cmd "dirname") (= (length args) 1))
                       (let* ((arg (car args))
                              ;; Resolve variables in argument
                              (resolved-arg (jf/bash-resolve-variables arg var-context)))
                         (cond
                          ;; Argument fully resolved
                          ((stringp resolved-arg)
                           (jf/bash--static-dirname resolved-arg))
                          ;; Argument has unresolved variables
                          (t :unresolved))))

                      ;; basename command with one or two arguments
                      ((and (equal cmd "basename") (or (= (length args) 1) (= (length args) 2)))
                       (let* ((arg (car args))
                              (suffix (when (= (length args) 2) (cadr args)))
                              ;; Resolve variables in argument
                              (resolved-arg (jf/bash-resolve-variables arg var-context)))
                         (cond
                          ;; Argument fully resolved
                          ((stringp resolved-arg)
                           (jf/bash--static-basename resolved-arg suffix))
                          ;; Argument has unresolved variables
                          (t :unresolved))))

                      ;; Unknown or unsupported command
                      (t :unresolved))))

                (cond
                 ;; Successfully evaluated - replace substitution with result
                 ((and evaluation-result (not (eq evaluation-result :unresolved)))
                  (setq result (concat
                                (substring result 0 match-start)
                                evaluation-result
                                (substring result match-end)))
                  (setq changed t))

                 ;; Could not evaluate - return :unresolved for entire path
                 (t
                  (setq result :unresolved)
                  (setq changed nil)))))))))

    result))

(defun jf/bash--track-assignments (parsed-command &optional initial-context)
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

  (jf/bash--resolve-assignment-value \"$(pwd)\" '((PWD . \"/base\")))
    => \"/base\"

  (jf/bash--resolve-assignment-value \"$(dirname $FILE)\" '((FILE . \"/a/b/c.txt\")))
    => \"/a/b\"

  (jf/bash--resolve-assignment-value \"$UNKNOWN\" nil)
    => (:path \"$UNKNOWN\" :unresolved (\"UNKNOWN\"))"
  (let ((resolved (jf/bash-resolve-variables value var-context)))
    ;; Check if fully resolved (string) or partial (plist)
    (if (stringp resolved)
        ;; Fully resolved variables - apply command substitution and relative path resolution
        (let ((cmd-resolved (jf/bash--resolve-command-substitution resolved var-context)))
          (cond
           ;; Command substitution could not be resolved - return :unresolved
           ((eq cmd-resolved :unresolved)
            (list :path resolved :unresolved (list "command-substitution")))
           ;; Command substitution resolved - apply relative path resolution
           (t
            (jf/bash-resolve-relative-path cmd-resolved var-context))))
      ;; Partial resolution - return as-is (plist with :unresolved)
      resolved)))

(defun jf/bash--extract-assignments-from-command (command &optional var-context)
  "Extract variable assignments from COMMAND structure with value resolution.

Returns alist of (VAR-SYMBOL . VALUE-STRING) or nil. Handles both unified
and split assignment patterns.

VAR-CONTEXT is an optional alist used to resolve assignment values. Assignment
values are resolved using the same logic as file paths:
  1. Variable resolution ($VAR, ${VAR})
  2. Command substitution ($(pwd), $(dirname $VAR), etc.)
  3. Relative path resolution (., ./, ../)

This enables variable chain tracking for security validation:
  BASE=$PWD; cat $BASE/file.txt → resolves BASE to PWD's value
  DIR=$(dirname $FILE); cat $DIR/data.txt → resolves DIR if FILE is known

Only fully-resolved assignments are added to the context. If resolution fails
(unresolved variables or command substitutions), the assignment is skipped to
prevent runtime errors when the variable is later referenced."
  (let ((assignments nil)
        (command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))

    ;; Check if command-name itself is an assignment (VAR=value)
    (when (and command-name (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" command-name))
      (let* ((var-name (match-string 1 command-name))
             (var-value (match-string 2 command-name))
             (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
        ;; Only add to context if value was fully resolved (string, not plist)
        (when (stringp resolved-value)
          (push (cons (intern var-name) resolved-value) assignments))))

    ;; Check for split assignment pattern: command-name is variable name only,
    ;; first positional arg is the value. This happens when tree-sitter parses
    ;; assignments in chains (e.g., "DIR=/tmp && cmd" becomes command-name="DIR", args=("/tmp"))
    (when (and command-name
               (string-match "^[A-Za-z_][A-Za-z0-9_]*$" command-name)  ; Valid var name (no =)
               positional-args
               (= (length positional-args) 1)  ; Exactly one positional arg
               ;; Heuristic: if command has registered handlers, it's not an assignment
               (not (jf/bash-lookup-command-handlers command-name)))
      (let* ((var-name command-name)
             (var-value (car positional-args))
             (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
        ;; Only treat as assignment if value doesn't look like a flag
        ;; AND value was fully resolved (string, not plist)
        (when (and (not (string-prefix-p "-" var-value))
                   (stringp resolved-value))
          (push (cons (intern var-name) resolved-value) assignments))))

    ;; Also check positional args for assignment patterns
    ;; (in case tree-sitter includes them there)
    (dolist (arg positional-args)
      (when (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" arg)
        (let* ((var-name (match-string 1 arg))
               (var-value (match-string 2 arg))
               (resolved-value (jf/bash--resolve-assignment-value var-value var-context)))
          ;; Only add to context if value was fully resolved (string, not plist)
          (when (stringp resolved-value)
            (push (cons (intern var-name) resolved-value) assignments)))))

    (nreverse assignments)))

(defun jf/bash--is-cd-command-p (command)
  "Return t if COMMAND structure represents a cd command.

Takes a parsed command structure (plist) and returns t if it represents
a cd command, nil otherwise.

Detection logic:
  - Checks :command-name equals \"cd\"
  - Handles 'builtin cd' invocations
  - Ignores cd as part of other commands (cdrom, abcd)

Examples:
  (jf/bash--is-cd-command-p '(:command-name \"cd\" :positional-args (\"/tmp\")))
    => t

  (jf/bash--is-cd-command-p '(:command-name \"builtin\" :positional-args (\"cd\" \"/tmp\")))
    => t

  (jf/bash--is-cd-command-p '(:command-name \"cdrom\" :positional-args (\"/dev\")))
    => nil

  (jf/bash--is-cd-command-p '(:command-name \"ls\" :positional-args (\"/tmp\")))
    => nil

This function operates on parsed command structures, unlike
`jf/bash-contains-cd-command-p' which operates on raw command strings."
  (let ((command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))
    (cond
     ;; Direct cd command
     ((equal command-name "cd")
      t)

     ;; Builtin cd invocation: builtin cd /path
     ((and (equal command-name "builtin")
           positional-args
           (equal (car positional-args) "cd"))
      t)

     ;; Not a cd command
     (t nil))))

(defun jf/bash--resolve-directory-target (target var-context current-pwd)
  "Resolve directory TARGET string to absolute path.

Takes a directory target string (e.g., from cd or pushd command), VAR-CONTEXT
for variable resolution, and CURRENT-PWD for relative path resolution. Returns
resolved absolute path or :unresolved marker if resolution fails.

TARGET is the directory path string to resolve.
VAR-CONTEXT is an alist of (SYMBOL . VALUE) for variable resolution.
CURRENT-PWD is the current working directory as an absolute path string.

Resolution steps (in order):
  a. Handle special forms:
     - nil or empty => :unresolved (or HOME for cd without args)
     - \"-\" => expand to OLDPWD (previous directory)
     - \"~\" => expand to HOME
     - \"~/path\" => expand ~ to HOME, then append path
  b. Resolve variables using jf/bash-resolve-variables
  c. Resolve command substitutions using jf/bash--resolve-command-substitution
  d. Resolve relative paths using jf/bash-resolve-relative-path
  e. If absolute path, return as-is

This shared logic is used by both jf/bash--extract-cd-target and
jf/bash--extract-pushd-target."
  (let ((home (alist-get 'HOME var-context)))
    (cond
     ;; No target provided - caller determines behavior (cd=>HOME, pushd=>unresolved)
     ((null target)
      nil)

     ;; Empty string target
     ((string-empty-p target)
      :unresolved)

     ;; cd/pushd - (switch to OLDPWD)
     ((equal target "-")
      (let ((oldpwd (alist-get 'OLDPWD var-context)))
        (if oldpwd
            oldpwd
          :unresolved)))

     ;; cd/pushd ~ (go to HOME)
     ((equal target "~")
      (if home
          home
        :unresolved))

     ;; cd/pushd ~/path (expand ~ to HOME, then append path)
     ((string-prefix-p "~/" target)
      (if home
          (let ((remainder (substring target 2)))  ; Remove "~/" prefix
            (expand-file-name (concat home "/" remainder)))
        :unresolved))

     ;; Have a regular target - resolve it
     (t
      ;; Step 1: Resolve variables
      (let ((var-resolved (jf/bash-resolve-variables target var-context)))
        (cond
         ;; Partial variable resolution - return :unresolved
         ((and (listp var-resolved)
               (plist-get var-resolved :unresolved))
          :unresolved)

         ;; Variables resolved - continue with path resolution
         (t
          ;; Step 2: Resolve command substitutions ($(pwd), $(dirname $VAR), etc.)
          (let ((cmd-resolved (jf/bash--resolve-command-substitution var-resolved var-context)))
            (cond
             ;; Command substitution could not be resolved
             ((eq cmd-resolved :unresolved)
              :unresolved)

             ;; Command substitution resolved - continue with relative path resolution
             (t
              ;; Step 3: Resolve relative paths using current-pwd
              (let ((path-resolved (jf/bash-resolve-relative-path cmd-resolved var-context current-pwd)))
                ;; Return the fully resolved path
                path-resolved)))))))))))

(defun jf/bash--extract-cd-target (command var-context current-pwd)
  "Extract and resolve target directory from cd COMMAND.

Takes a parsed cd command structure, VAR-CONTEXT for variable resolution,
and CURRENT-PWD for relative path resolution. Returns resolved absolute
path or :unresolved marker if resolution fails.

COMMAND is a parsed command plist with :command-name and :positional-args.
VAR-CONTEXT is an alist of (SYMBOL . VALUE) for variable resolution.
CURRENT-PWD is the current working directory as an absolute path string.

Extraction logic:
  - Gets target from first positional arg
  - For 'builtin cd', skips first arg and uses second
  - Returns nil if no target provided (to be handled by HOME expansion below)

Resolution steps (in order):
  a. Handle special forms:
     - cd (no args) => expand to HOME
     - cd - => expand to OLDPWD (previous directory)
     - cd ~ => expand to HOME
     - cd ~/path => expand ~ to HOME, then append path
  b. Resolve variables using jf/bash-resolve-variables
     Example: cd $DIR where DIR=/tmp => /tmp
  c. Resolve command substitutions using jf/bash--resolve-command-substitution
     Example: cd $(pwd)/sub => /current/sub
     Example: cd $(dirname $FILE) where FILE=/path/to/file.txt => /path/to
     Example: cd $(basename $DIR) => filename part of $DIR
  d. Resolve relative paths using jf/bash-resolve-relative-path
     Example: cd subdir with current-pwd=/base => /base/subdir
     Example: cd ../other with current-pwd=/base/sub => /base/other
  e. If absolute path, return as-is

Edge cases:
  - cd . => returns current-pwd unchanged
  - cd '' => returns :unresolved marker
  - cd (no args) => returns HOME
  - cd - => returns OLDPWD (previous directory)
  - cd ~ => returns HOME
  - cd ~/subdir => returns HOME/subdir
  - Unresolved variables => returns :unresolved marker
  - No HOME in context => returns :unresolved
  - cd - with no OLDPWD => returns :unresolved

Examples:
  ;; Absolute path
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"/tmp\"))
    nil \"/original\")
  => \"/tmp\"

  ;; Relative path
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"subdir\"))
    '((PWD . \"/base\")) \"/base\")
  => \"/base/subdir\"

  ;; Variable expansion
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"$DIR\"))
    '((DIR . \"/target\")) \"/original\")
  => \"/target\"

  ;; builtin cd invocation
  (jf/bash--extract-cd-target
    '(:command-name \"builtin\" :positional-args (\"cd\" \"/tmp\"))
    nil \"/original\")
  => \"/tmp\"

  ;; cd with no args (go to HOME)
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args nil)
    '((HOME . \"/home/user\")) \"/original\")
  => \"/home/user\"

  ;; cd ~ (go to HOME)
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"~\"))
    '((HOME . \"/home/user\")) \"/original\")
  => \"/home/user\"

  ;; cd ~/subdir (HOME expansion with path)
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"~/subdir\"))
    '((HOME . \"/home/user\")) \"/original\")
  => \"/home/user/subdir\"

  ;; cd - (switch to previous directory using OLDPWD)
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"-\"))
    '((PWD . \"/current\") (OLDPWD . \"/previous\")) \"/current\")
  => \"/previous\"

  ;; cd - with no OLDPWD in context
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"-\"))
    '((PWD . \"/current\")) \"/current\")
  => :unresolved

  ;; Unresolved variable
  (jf/bash--extract-cd-target
    '(:command-name \"cd\" :positional-args (\"$UNKNOWN\"))
    nil \"/original\")
  => :unresolved"
  (let* ((command-name (plist-get command :command-name))
         (positional-args (plist-get command :positional-args))
         (flags (plist-get command :flags))
         ;; Extract target - handle builtin cd specially
         (target (cond
                  ;; builtin cd: skip "cd" arg, use second arg
                  ((equal command-name "builtin")
                   (when (and positional-args
                              (>= (length positional-args) 2))
                     (nth 1 positional-args)))
                  ;; Regular cd: check positional args first, then flags
                  ;; (parser sometimes puts "-" in flags instead of positional-args)
                  (t
                   (or (car positional-args)
                       (car flags)))))
         ;; Extract HOME from var-context for special form handling
         (home (alist-get 'HOME var-context))
         ;; Resolve using shared logic
         (resolved (jf/bash--resolve-directory-target target var-context current-pwd)))

    ;; Handle cd-specific case: no target means go to HOME
    (if (null resolved)
        (if home home :unresolved)
      resolved)))

(defun jf/bash--is-pushd-command-p (command)
  "Return t if COMMAND structure represents a pushd command.

Takes a parsed command structure (plist) and returns t if it represents
a pushd command, nil otherwise.

Detection logic:
  - Checks :command-name equals \"pushd\"
  - Handles 'builtin pushd' invocations
  - Ignores pushd as part of other commands

Examples:
  (jf/bash--is-pushd-command-p '(:command-name \"pushd\" :positional-args (\"/tmp\")))
    => t

  (jf/bash--is-pushd-command-p '(:command-name \"builtin\" :positional-args (\"pushd\" \"/tmp\")))
    => t

  (jf/bash--is-pushd-command-p '(:command-name \"ls\" :positional-args (\"/tmp\")))
    => nil

This function operates on parsed command structures."
  (let ((command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))
    (cond
     ;; Direct pushd command
     ((equal command-name "pushd")
      t)

     ;; Builtin pushd invocation: builtin pushd /path
     ((and (equal command-name "builtin")
           positional-args
           (equal (car positional-args) "pushd"))
      t)

     ;; Not a pushd command
     (t nil))))

(defun jf/bash--is-popd-command-p (command)
  "Return t if COMMAND structure represents a popd command.

Takes a parsed command structure (plist) and returns t if it represents
a popd command, nil otherwise.

Detection logic:
  - Checks :command-name equals \"popd\"
  - Handles 'builtin popd' invocations
  - Ignores popd as part of other commands

Examples:
  (jf/bash--is-popd-command-p '(:command-name \"popd\" :positional-args nil))
    => t

  (jf/bash--is-popd-command-p '(:command-name \"builtin\" :positional-args (\"popd\")))
    => t

  (jf/bash--is-popd-command-p '(:command-name \"ls\" :positional-args (\"/tmp\")))
    => nil

This function operates on parsed command structures."
  (let ((command-name (plist-get command :command-name))
        (positional-args (plist-get command :positional-args)))
    (cond
     ;; Direct popd command
     ((equal command-name "popd")
      t)

     ;; Builtin popd invocation: builtin popd
     ((and (equal command-name "builtin")
           positional-args
           (equal (car positional-args) "popd"))
      t)

     ;; Not a popd command
     (t nil))))

(defun jf/bash--extract-pushd-target (command var-context current-pwd)
  "Extract and resolve target directory from pushd COMMAND.

Takes a parsed pushd command structure, VAR-CONTEXT for variable resolution,
and CURRENT-PWD for relative path resolution. Returns resolved absolute
path or :unresolved marker if resolution fails.

COMMAND is a parsed command plist with :command-name and :positional-args.
VAR-CONTEXT is an alist of (SYMBOL . VALUE) for variable resolution.
CURRENT-PWD is the current working directory as an absolute path string.

Extraction logic:
  - Gets target from first positional arg
  - For 'builtin pushd', skips first arg and uses second
  - Returns nil if no target provided (pushd with no args is complex - swaps dirs)

Resolution follows same logic as cd command:
  a. Handle special forms (~, -, no args)
  b. Resolve variables
  c. Resolve command substitutions
  d. Resolve relative paths
  e. Return absolute paths as-is

Examples:
  ;; Absolute path
  (jf/bash--extract-pushd-target
    '(:command-name \"pushd\" :positional-args (\"/tmp\"))
    nil \"/original\")
  => \"/tmp\"

  ;; Relative path
  (jf/bash--extract-pushd-target
    '(:command-name \"pushd\" :positional-args (\"subdir\"))
    '((PWD . \"/base\")) \"/base\")
  => \"/base/subdir\"

  ;; Variable expansion
  (jf/bash--extract-pushd-target
    '(:command-name \"pushd\" :positional-args (\"$DIR\"))
    '((DIR . \"/target\")) \"/original\")
  => \"/target\"

  ;; builtin pushd invocation
  (jf/bash--extract-pushd-target
    '(:command-name \"builtin\" :positional-args (\"pushd\" \"/tmp\"))
    nil \"/original\")
  => \"/tmp\"

  ;; pushd with no args (swaps top two dirs - complex, skip for now)
  (jf/bash--extract-pushd-target
    '(:command-name \"pushd\" :positional-args nil)
    nil \"/original\")
  => :unresolved"
  (let* ((command-name (plist-get command :command-name))
         (positional-args (plist-get command :positional-args))
         (flags (plist-get command :flags))
         ;; Extract target - handle builtin pushd specially
         (target (cond
                  ;; builtin pushd: skip "pushd" arg, use second arg
                  ((equal command-name "builtin")
                   (when (and positional-args
                              (>= (length positional-args) 2))
                     (nth 1 positional-args)))
                  ;; Regular pushd: check positional args first, then flags
                  (t
                   (or (car positional-args)
                       (car flags)))))
         ;; Extract HOME from var-context for special form handling
         (home (alist-get 'HOME var-context)))

    ;; Resolve using shared logic
    (let ((resolved (jf/bash--resolve-directory-target target var-context current-pwd)))
      ;; Handle pushd-specific case: no target means directory swap (unresolved)
      (if (null resolved)
          :unresolved
        resolved))))

(provide 'bash-parser-variables)
;;; bash-parser-variables.el ends here
