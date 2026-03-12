;; Author: Jeff Farr
;; Keywords: bash, parser, security, validation
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Security validation for bash commands using glob-based pattern matching.
;; Implements sandbox security validation for file operations and dangerous commands.

;;; Code:

;;; bash-parser-security.el --- Security validation for bash parser -*- lexical-binding: t; -*-

(require 'cl-lib)

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

(defun jf/bash--extract-unresolved-vars (file-path)
  "Extract list of unresolved variable names from FILE-PATH.

Searches for shell variable syntax ($VAR or ${VAR}) in the file path
and returns a list of variable names.

Returns nil if no variables found or if FILE-PATH is not a string.

Examples:
  (jf/bash--extract-unresolved-vars \"/path/to/file\")
    => nil
  (jf/bash--extract-unresolved-vars \"$HOME/file.txt\")
    => (\"HOME\")
  (jf/bash--extract-unresolved-vars \"${WORKSPACE}/src/$FILE\")
    => (\"WORKSPACE\" \"FILE\")
  (jf/bash--extract-unresolved-vars \"prefix-$VAR1-$VAR2-suffix\")
    => (\"VAR1\" \"VAR2\")"
  (when (stringp file-path)
    (let ((vars nil)
          (pos 0))
      (while (string-match "\\$\\(?:{\\([^}]+\\)}\\|\\([A-Za-z_][A-Za-z0-9_]*\\)\\)"
                          file-path pos)
        (let ((var-name (or (match-string 1 file-path)
                           (match-string 2 file-path))))
          (when var-name
            (push var-name vars)))
        (setq pos (match-end 0)))
      (nreverse vars))))

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

Special case: If :operations is the symbol :all, all operation types are allowed.

Denial conditions:
  1. No rule matched (path not in allowlist)
  2. Rule's :operations is not :all AND operation type not in list

Violation plist contains:
  :file - The file path
  :operation - The operation type (:read, :write, :delete, etc.)
  :matched-rule - The rule that was evaluated (or nil)
  :reason - Human-readable explanation
  :unresolved-vars - List of unresolved variable names (if any)
  :indirect - t if operation is indirect (from nested command)
  :nested-command - Original nested command string (if indirect)
  :guidance - Human-readable guidance for fixing the issue"
  (let* ((file (plist-get operation :file))
         (op-type (plist-get operation :operation))
         (allowed-ops (plist-get matched-rule :operations))
         (unresolved-vars (jf/bash--extract-unresolved-vars file))
         (indirect (plist-get operation :indirect))
         (nested-command (plist-get operation :nested-command)))
    (cond
     ;; No rule matched = violation (fail-safe: deny by default)
     ((null matched-rule)
      (let ((violation (list :file file
                            :operation op-type
                            :matched-rule nil
                            :reason "No allowlist rule matches this file path"
                            :guidance "Add a security rule pattern matching this path")))
        (when unresolved-vars
          (setq violation (plist-put violation :unresolved-vars unresolved-vars)))
        (when indirect
          (setq violation (plist-put violation :indirect t)))
        (when nested-command
          (setq violation (plist-put violation :nested-command nested-command)))
        violation))

     ;; Rule allows all operations
     ((eq allowed-ops :all)
      nil)  ; Allowed

     ;; Operation not in allowed list = violation
     ((not (memq op-type allowed-ops))
      (let ((violation (list :file file
                            :operation op-type
                            :matched-rule matched-rule
                            :reason (format "Operation %s not allowed (rule permits: %s)"
                                           op-type allowed-ops)
                            :guidance (format "Add %s to the allowed operations for this rule"
                                            op-type))))
        (when unresolved-vars
          (setq violation (plist-put violation :unresolved-vars unresolved-vars)))
        (when indirect
          (setq violation (plist-put violation :indirect t)))
        (when nested-command
          (setq violation (plist-put violation :nested-command nested-command)))
        violation))

     ;; Allowed
     (t nil))))

(defun jf/bash--walk-ast (ast-node visitor-fn)
  "Walk AST-NODE tree, calling VISITOR-FN on each command node.

VISITOR-FN is called with each node. If it returns non-nil, walking stops
and that value is returned.

AST-NODE should be a parsed result from jf/bash-parse (plist with :all-commands).

Returns the first non-nil result from VISITOR-FN, or nil if all visits return nil."
  (when (and ast-node (plist-get ast-node :success))
    (let ((all-commands (plist-get ast-node :all-commands)))
      (cl-some visitor-fn all-commands))))

(defun jf/bash-contains-cd-command-p (parsed-ast)
  "Return t if PARSED-AST contains a cd command.

PARSED-AST should be the result of jf/bash-parse (plist with :success and :all-commands).

Detects cd commands by examining the AST structure:
  - Simple command: \"cd /tmp\"
  - With flags: \"cd -P /tmp\"
  - In pipelines: \"cd /tmp && ls\"
  - In chains: \"cd /tmp; ls\"
  - Builtin invocation: \"builtin cd /tmp\"
  - In command substitutions: \"$(cd /tmp && pwd)\"

Does NOT detect (correct behavior):
  - cd as part of another word: \"cdrom\", \"abcd\"
  - cd in comments: \"# cd /tmp\"
  - cd in here-documents: \"cat <<EOF\\ncd /tmp\\nEOF\"

Returns t if cd command found, nil otherwise."
  (jf/bash--walk-ast
   parsed-ast
   (lambda (cmd)
     (let ((command-name (plist-get cmd :command-name)))
       ;; Check if this command is 'cd' or has nested commands with 'cd'
       (or (and command-name (string= command-name "cd"))
           ;; Check command substitutions recursively
           ;; Each substitution is a plist with :parsed field containing the AST
           (when-let ((substitutions (plist-get cmd :command-substitutions)))
             (cl-some (lambda (subst)
                       (when-let ((parsed-subst (plist-get subst :parsed)))
                         (jf/bash-contains-cd-command-p parsed-subst)))
                     substitutions))
           ;; Check nested commands (for command injection)
           (when-let ((nested (plist-get cmd :nested-command)))
             (jf/bash-contains-cd-command-p nested)))))))

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
  COMMAND-STRING - The bash command to validate (must be a string)
  RULES - List of security rule plists (from `jf/bash-match-rule')
  VAR-CONTEXT - Optional variable resolution context (alist of (VAR . VALUE) pairs).
                Variables are resolved by jf/bash-extract-file-operations before
                security validation. Unresolved variables cause operations to be
                marked as :unhandled, resulting in denial (fail-secure).
  INDIRECT-POLICY - How to handle indirect operations (:strict, :warn, :permissive)
                    :strict - Reject all indirect operations as violations
                    :warn - Flag indirect operations as unhandled
                    :permissive (default) - Validate indirect operations normally

Signals error if COMMAND-STRING is not a string, if RULES is not a list, if VAR-CONTEXT
has invalid structure, or if INDIRECT-POLICY is not a valid keyword.

Returns validation result plist:
  :allowed - t if command is safe, nil if denied
  :command - Original command string (for reference)
  :operations - All extracted file operations (for debugging)
  :violations - List of security violations (non-empty => denied)
  :unhandled - Operations that couldn't be validated
  :cd-detected - t if cd command was detected
  :denial-reason - Human-readable summary of why command was denied (nil if allowed)

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
  5. Allow only if no violations AND no unhandled operations"
  ;; Validate inputs
  (unless (stringp command-string)
    (error "jf/bash-sandbox-check: command-string must be a string, got %S"
           (type-of command-string)))
  (unless (listp rules)
    (error "jf/bash-sandbox-check: rules must be a list, got %S"
           (type-of rules)))
  (when var-context
    (unless (jf/bash--valid-var-context-p var-context)
      (error "jf/bash-sandbox-check: var-context must be an alist of (VAR . VALUE) pairs where VAR is symbol/string and VALUE is string, got %S"
             var-context)))
  (when (and indirect-policy
             (not (memq indirect-policy '(:strict :warn :permissive))))
    (error "jf/bash-sandbox-check: indirect-policy must be :strict, :warn, or :permissive, got %S"
           indirect-policy))

  (let ((violations nil)
        (unhandled nil)
        (operations nil)
        (cd-detected nil)
        (denial-reason nil)
        (parsed nil))

    ;; Parse command to AST first (needed for both cd detection and file operations)
    (setq parsed (jf/bash-parse command-string))

    ;; Check for cd command using AST (immediate rejection)
    (if (and (plist-get parsed :success)
             (jf/bash-contains-cd-command-p parsed))
        (setq cd-detected t
              denial-reason "cd command not allowed in sandbox - use absolute paths instead"
              violations (list (list :reason denial-reason
                                    :command command-string
                                    :guidance "Use absolute paths in commands instead of cd, or configure runtime working directory")))

      ;; Normal validation pipeline:
      ;; 1. AST already parsed above
      ;; 2. Extract file operations (with variable resolution via var-context)
      ;; 3. For each operation (with resolved paths):
      ;;    - Validate against security rules
      ;;    - Mark unresolved variables as unhandled
      (when (plist-get parsed :success)
        (setq operations (jf/bash-extract-file-operations parsed var-context))

        ;; Validate each operation
        (dolist (op operations)
            (cond
             ;; Indirect operation policy handling (check before other validations)
             ((and (plist-get op :indirect)
                   (eq indirect-policy :strict))
              ;; Strict policy: reject all indirect operations
              (let ((violation (list :file (plist-get op :file)
                                    :operation (plist-get op :operation)
                                    :reason "Indirect operation not allowed (strict policy)"
                                    :indirect t
                                    :guidance "Remove nested commands or use permissive indirect policy")))
                (when (plist-get op :nested-command)
                  (setq violation (plist-put violation :nested-command (plist-get op :nested-command))))
                (let ((unresolved-vars (jf/bash--extract-unresolved-vars (plist-get op :file))))
                  (when unresolved-vars
                    (setq violation (plist-put violation :unresolved-vars unresolved-vars))))
                (push violation violations)))

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
                  (push violation violations))))))))

    ;; Build denial reason summary if command was denied
    (when (or violations unhandled)
      (unless denial-reason  ; CD command may have already set this
        (setq denial-reason
              (cond
               ;; Multiple violations
               ((> (length violations) 1)
                (format "%d security violations detected" (length violations)))

               ;; Single violation
               ((= (length violations) 1)
                (plist-get (car violations) :reason))

               ;; Multiple unhandled operations
               ((> (length unhandled) 1)
                (format "%d unhandled operations (fail-secure denial)" (length unhandled)))

               ;; Single unhandled operation
               ((= (length unhandled) 1)
                (format "Unhandled operation: %s" (plist-get (car unhandled) :reason)))

               ;; Fallback
               (t "Security validation failed")))))

    ;; Return comprehensive result
    (list :allowed (and (null violations) (null unhandled))
          :command command-string
          :operations operations
          :violations (nreverse violations)
          :unhandled (nreverse unhandled)
          :cd-detected cd-detected
          :denial-reason denial-reason)))

(provide 'bash-parser-security)
;;; bash-parser-security.el ends here
