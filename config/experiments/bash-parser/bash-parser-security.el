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
  :reason - Human-readable explanation"
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
  VAR-CONTEXT - Optional variable resolution context (alist of (VAR . VALUE) pairs).
                Variables are resolved by jf/bash-extract-file-operations before
                security validation. Unresolved variables cause operations to be
                marked as :unhandled, resulting in denial (fail-secure).
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
  5. Allow only if no violations AND no unhandled operations"
  (let ((violations nil)
        (unhandled nil)
        (operations nil)
        (cd-detected nil))

    ;; Check for cd command (immediate rejection)
    (if (jf/bash-contains-cd-command-p command-string)
        (setq cd-detected t
              violations (list (list :reason "cd command not allowed in sandbox - use absolute paths instead"
                                    :command command-string)))

      ;; Normal validation pipeline:
      ;; 1. Parse command to AST
      ;; 2. Extract file operations (with variable resolution via var-context)
      ;; 3. For each operation (with resolved paths):
      ;;    - Validate against security rules
      ;;    - Mark unresolved variables as unhandled
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

(provide 'bash-parser-security)
;;; bash-parser-security.el ends here
